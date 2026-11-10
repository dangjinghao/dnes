#include "dnes.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Keep checks enabled in Release builds too.
#define CHECK(expr) do { \
  if (!(expr)) { \
    fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #expr); \
    exit(1); \
  } \
} while (0)

static struct bus test_bus;
static byte_t memory[65536];
static addr_t reads[8192];
static size_t read_count;

static byte_t sample_read(addr_t addr, bool read_only) {
  CHECK(!read_only);
  CHECK(read_count < sizeof(reads) / sizeof(reads[0]));
  reads[read_count++] = addr;
  return memory[addr];
}

static void setup(void) {
  bus_init(&test_bus, "APU test", 0xFFFF);
  apu_register(&test_bus);
  ctrl_register(&test_bus);
  dev_4017_register(&test_bus);
  struct bus_regparam rom = {.read = sample_read};
  bus_register(&test_bus, 0x8000, 0xFFFF, &rom);
  memset(memory, 0, sizeof(memory));
  read_count = 0;
  ctrl_reset();
}

static void write_reg(addr_t addr, byte_t value) {
  bus_write(&test_bus, addr, value);
}

static byte_t status(void) { return bus_read(&test_bus, 0x4015); }

static void tick(unsigned cpu_cycles) {
  for (unsigned i = 0; i < cpu_cycles; i++) {
    apu_clock();
    (void)apu_cpu_stall();
    apu_clock();
    apu_clock();
  }
}

static void test_status_and_length(void) {
  const addr_t length_regs[] = {0x4003, 0x4007, 0x400B, 0x400F};
  setup();
  for (unsigned i = 0; i < 4; i++)
    write_reg(length_regs[i], 0x18); // Length 2, ignored while disabled.
  CHECK(status() == 0);
  write_reg(0x4015, 0x0F);
  CHECK(status() == 0); // Enabling alone does not reload lengths.
  for (unsigned i = 0; i < 4; i++) {
    write_reg(length_regs[i], 0x18);
    CHECK(status() == (1u << (i + 1)) - 1);
  }
  tick(14912);
  CHECK(status() == 0x0F);
  tick(1); // First half-frame: 2 -> 1.
  CHECK(status() == 0x0F);
  tick(14916); // Second half-frame: 1 -> 0, plus frame IRQ.
  CHECK(status() == 0x40);

  for (unsigned i = 0; i < 4; i++)
    write_reg(length_regs[i], 0x18);
  write_reg(0x4015, 0x04);
  CHECK((status() & 0x0F) == 0x04);
  write_reg(0x4015, 0x08);
  CHECK((status() & 0x0F) == 0); // Triangle clears immediately.
  write_reg(0x400F, 0x18);
  CHECK((status() & 0x0F) == 0x08); // Noise is bit 3, not bit 2.
}

static void test_frame_counter(void) {
  setup();
  tick(29827);
  CHECK(!apu_irq_state());
  for (unsigned i = 0; i < 3; i++) {
    tick(1);
    CHECK(apu_irq_state());
    CHECK(bus_read_only(&test_bus, 0x4015) == 0x40);
    CHECK(apu_irq_state()); // Debug reads preserve IRQ.
    CHECK(status() == 0x40);
    CHECK(!apu_irq_state());
  }
  tick(1);
  CHECK(status() == 0);
  tick(29827);
  CHECK(apu_irq_state());
  write_reg(0x4015, 0);
  CHECK(apu_irq_state()); // $4015 writes only acknowledge DMC IRQ.
  write_reg(0x4017, 0x40);
  CHECK(!apu_irq_state());
  tick(60000);
  CHECK(!apu_irq_state());

  // Both write phases, including the delayed initial half-frame in mode 5.
  for (unsigned phase = 0; phase < 2; phase++) {
    setup();
    tick(phase);
    apu_clock(); // Match the APU-before-CPU order of dnes_clock.
    write_reg(0x4015, 1);
    write_reg(0x4003, 0x18);
    write_reg(0x4017, 0x80);
    apu_clock();
    apu_clock();
    unsigned delay = phase == 0 ? 3 : 4;
    tick(delay - 1);
    CHECK(status() == 1);
    tick(1); // Initial half-frame consumes one of two length ticks.
    tick(14912);
    CHECK(status() == 1);
    tick(1);
    CHECK(status() == 0);
    tick(100000);
    CHECK(!apu_irq_state());
  }

  setup();
  write_reg(0x4017, 0x80);
  tick(3);
  tick(14913);
  write_reg(0x4015, 1);
  write_reg(0x4003, 0x18);
  tick(29829 - 14913); // Step 4 in mode 5 must not clock lengths.
  CHECK(status() == 1);
  tick(37281 - 29829); // Step 5: 2 -> 1.
  CHECK(status() == 1);
  tick(1 + 14913); // Next sequence's first half-frame: 1 -> 0.
  CHECK(status() == 0);

  // $4017 still reads controller 2 through the shared bus device.
  ctrl_set_input(1, CTRL_A, true);
  write_reg(0x4016, 1);
  write_reg(0x4017, 0xC0);
  CHECK(bus_read(&test_bus, 0x4017) == 1);
  CHECK(bus_read(&test_bus, 0x4017) == 0);
}

static void test_triangle(void) {
  setup();
  write_reg(0x4015, 4);
  write_reg(0x4008, 0x81); // Halt length and continuously reload linear count.
  write_reg(0x400A, 2); // One sequencer step per 3 CPU cycles.
  write_reg(0x400B, 0);
  double baseline = apu_get_output_sample();
  tick(7456);
  CHECK(apu_get_output_sample() == baseline);
  tick(3); // Quarter-frame at 7457, next timer expiry at 7459.
  CHECK(apu_triangle_visual == 2);
  double wave[32];
  for (unsigned i = 0; i < 32; i++) {
    wave[i] = apu_get_output_sample();
    tick(3);
  }
  CHECK(fabs(wave[0] - apu_get_output_sample()) < 1e-12);
  CHECK(wave[0] > wave[1] && wave[14] > wave[15]);
  CHECK(wave[15] == wave[16]);
  CHECK(wave[16] < wave[17] && wave[30] < wave[31]);
  write_reg(0x4015, 0);
  double held = apu_get_output_sample();
  tick(100);
  CHECK(apu_get_output_sample() == held); // Hold DAC, do not reset phase.
  CHECK(status() == 0);

  setup();
  write_reg(0x4015, 4);
  write_reg(0x4008, 1); // Clear control: reload once, then count down.
  write_reg(0x400A, 2);
  write_reg(0x400B, 0);
  tick(14913);
  held = apu_get_output_sample();
  tick(100);
  CHECK(apu_get_output_sample() == held);
  CHECK(status() == 4); // Status reports length even with linear count zero.
  CHECK(apu_triangle_visual == 2047);
  write_reg(0x4008, 0x81); // Control write alone does not set reload flag.
  tick(7457);
  CHECK(apu_get_output_sample() == held);
  write_reg(0x400B, 0);
  tick(7457);
  CHECK(apu_triangle_visual == 2);
}

static void test_dmc_reader_and_irq(void) {
  setup();
  write_reg(0x4017, 0x40);
  write_reg(0x4010, 0x8F);
  write_reg(0x4012, 0xFF);
  write_reg(0x4013, 4); // 65 bytes, crosses $FFFF.
  write_reg(0x4015, 0x10);
  CHECK(status() == 0x10);
  tick(1);
  CHECK(read_count == 1 && reads[0] == 0xFFC0);
  write_reg(0x4015, 0x10); // Must not restart an active reader.
  tick(30000);
  CHECK(read_count == 65);
  CHECK(reads[63] == 0xFFFF && reads[64] == 0x8000);
  CHECK(status() == 0x80); // Reader finished, IRQ persists after read.
  CHECK(apu_irq_state());
  write_reg(0x4010, 0x0F);
  CHECK(!apu_irq_state());
  write_reg(0x4010, 0x8F);
  write_reg(0x4013, 0);
  write_reg(0x4015, 0x10);
  tick(1);
  CHECK(apu_irq_state());
  write_reg(0x4015, 0);
  CHECK(!apu_irq_state());

  setup();
  write_reg(0x4010, 0xCF); // Loop takes precedence over sample-end IRQ.
  write_reg(0x4012, 1);
  write_reg(0x4013, 0);
  write_reg(0x4015, 0x10);
  tick(10000);
  CHECK(read_count > 2);
  for (size_t i = 0; i < read_count; i++)
    CHECK(reads[i] == 0xC040);
  CHECK(status() == 0x10);
  write_reg(0x4015, 0);
  size_t stopped_count = read_count;
  tick(1000);
  CHECK(read_count == stopped_count && status() == 0);
}

static void test_dmc_output_and_rates(void) {
  const unsigned periods[] = {428, 380, 340, 320, 286, 254, 226, 214,
                             190, 160, 142, 128, 106, 84, 72, 54};
  for (unsigned rate = 0; rate < 16; rate++) {
    setup();
    memory[0xC000] = 0x01; // LSB first: +2 followed by -2.
    write_reg(0x4010, (byte_t)rate);
    write_reg(0x4011, 64);
    double initial = apu_get_output_sample();
    write_reg(0x4015, 0x10);
    // Timer starts at the power-on period; a rate write does not reset it.
    tick(428 + 7 * periods[rate]); // First silent byte ends.
    CHECK(apu_get_output_sample() == initial);
    tick(periods[rate] - 1);
    CHECK(apu_get_output_sample() == initial);
    tick(1);
    CHECK(apu_get_output_sample() > initial);
    write_reg(0x4015, 0); // Buffered bits still play after disabling reader.
    tick(periods[rate]);
    CHECK(fabs(apu_get_output_sample() - initial) < 1e-12);
    CHECK(read_count == 1);
  }

  for (unsigned high = 0; high < 2; high++) {
    setup();
    memory[0xC000] = high ? 0xFF : 0;
    write_reg(0x4010, 0x4F);
    write_reg(0x4011, high ? 0xFF : 0); // $4011 masks bit 7.
    double limit = apu_get_output_sample();
    write_reg(0x4015, 0x10);
    tick(10000);
    CHECK(apu_get_output_sample() == limit); // No 7-bit DAC wraparound.
  }
}

static void test_reset(void) {
  setup();
  double initial = apu_get_output_sample();
  write_reg(0x4010, 0x80);
  write_reg(0x4011, 127);
  write_reg(0x4015, 0x1F);
  write_reg(0x4008, 0xFF);
  write_reg(0x400B, 0xFF);
  tick(29830);
  CHECK(apu_irq_state());
  apu_reset();
  CHECK(status() == 0 && !apu_irq_state() && !apu_cpu_stall());
  CHECK(apu_get_output_sample() == initial);
  CHECK(apu_triangle_visual == 2047);
  tick(29827);
  CHECK(!apu_irq_state());
  tick(1);
  CHECK(apu_irq_state());
  write_reg(0x4015, 0x10); // Reset preserved the bus connection.
  tick(1);
  CHECK(reads[read_count - 1] == 0xC000);
}

static void system_tick(unsigned cpu_cycles) {
  for (unsigned i = 0; i < cpu_cycles * 3; i++)
    dnes_clock();
}

static void test_cpu_integration(void) {
  // A generated NROM with CLI/NOP/JMP and an IRQ handler at $8100.
  byte_t header[16] = {'N', 'E', 'S', 0x1A, 1, 1};
  byte_t prg[16384] = {0x58, 0xEA, 0x4C, 0x01, 0x80};
  byte_t chr[8192] = {0};
  prg[0x100] = 0x40; // RTI
  prg[0x3FFD] = 0x80;
  prg[0x3FFF] = 0x81;
  char rom_name[] = "apu-test.nes";
  FILE *rom = fopen(rom_name, "wb");
  CHECK(rom != NULL);
  CHECK(fwrite(header, sizeof(header), 1, rom) == 1);
  CHECK(fwrite(prg, sizeof(prg), 1, rom) == 1);
  CHECK(fwrite(chr, sizeof(chr), 1, rom) == 1);
  CHECK(fclose(rom) == 0);
  dnes_insert_cartridge(rom_name);
  CHECK(remove(rom_name) == 0);
  dnes_reset();
  dnes_set_sample_frequency(44100);
  apu_write(0x4010, 0x80);
  apu_write(0x4015, 0x10);
  system_tick(4); // DMC DMA stalls all four CPU clocks.
  CHECK(cpu_get_reg_PC() == 0x8000 && !cpu_inst_done());
  system_tick(8); // Finish reset; IRQ stays pending while I is set.
  CHECK(cpu_get_reg_PC() == 0x8000 && cpu_inst_done());
  CHECK(apu_irq_state());
  system_tick(1); // Execute CLI, but do not interrupt mid-instruction.
  CHECK(cpu_get_reg_PC() == 0x8001 && !cpu_inst_done());
  system_tick(1);
  CHECK(cpu_get_reg_PC() == 0x8001 && cpu_inst_done());
  system_tick(1);
  CHECK(cpu_get_reg_PC() == 0x8100 && cpu_get_flag(FLAG_I));
  CHECK(apu_irq_state());
  apu_write(0x4015, 0);
  CHECK(!apu_irq_state());
  system_tick(6 + 6); // Finish IRQ entry and RTI.
  CHECK(cpu_get_reg_PC() == 0x8001 && cpu_inst_done());
  cart_pop();
}

int main(void) {
  test_status_and_length();
  test_frame_counter();
  test_triangle();
  test_dmc_reader_and_irq();
  test_dmc_output_and_rates();
  test_reset();
  test_cpu_integration();
  puts("APU tests passed");
  return 0;
}
