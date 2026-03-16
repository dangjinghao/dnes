#include "dnes.h"
#include "mapper/mapper.h"
#include <stdio.h>

static size_t system_clock = 0;
static double audio_time = 0.0;
static double audio_global_time = 0.0;
static double audio_time_per_NES_clock = 0.0;
static double audio_time_per_system_sample = 0.0;

static void mbus_ready() {
  static struct bus mbus;
  printf("Preparing main bus...\n");
  bus_init(&mbus, "Main bus", 0xFFFF);
  ppu_register_mbus(&mbus);
  ram_register(&mbus);
  cart_register_mbus(&mbus);

  // 2a03 package
  ctrl_register(&mbus);
  apu_register(&mbus);
  dev_4017_register(&mbus);
  dma_register(&mbus);
  dma_mount_mbus(&mbus);
  // 2a03 package end

  bus_ready(&mbus);
  cpu_mount_mbus(&mbus);
}

static void pbus_ready() {
  static struct bus pbus;
  printf("Preparing PPU bus...\n");
  bus_init(&pbus, "PPU bus", 0x3FFF);
  cart_register_pbus(&pbus);
  ppu_ext_register(&pbus);
  bus_ready(&pbus);
  ppu_mount_pbus(&pbus);
}

void dnes_clock() {
  // The fastest clock frequency the digital system cares
  // about is equivalent to the PPU clock. So the PPU is clocked
  // each time this function is called...
  ppu_clock();

  apu_clock();

  if (system_clock % 3 == 0) {
    // Is the system performing a DMA transfer form CPU memory to
    // OAM memory on PPU?...
    if (dma_transfer) {
      dma_do_transfer(system_clock);
    } else {
      // No DMA happening, the CPU is in control of its
      // own destiny. Go forth my friend and calculate
      // awesomeness for many generations to come...
      cpu_clock();
    }
  }

  // Synchronising with Audio
  dnes_audio_sample_ready = false;
  audio_time += audio_time_per_NES_clock;
  if (audio_time >= audio_time_per_system_sample) {
    audio_time -= audio_time_per_system_sample;
    dnes_audio_sample = apu_get_output_sample();
    dnes_audio_sample_ready = true;
  }

  if (ppu_nmi) {
    ppu_nmi = false;
    cpu_nmi();
  }

  if (cart_get_mapper()->irq_state()) {
    cart_get_mapper()->irq_clear();
    cpu_irq();
  }

  // TODO: Check if cartridge is requesting IRQ
  system_clock += 1;
}

void dnes_reset() {
  cpu_reset();
  ppu_reset();
  cart_reset();
  ctrl_reset();
  apu_reset();
  system_clock = 0;
  audio_time = 0.0;
  audio_global_time = 0.0;
  // WARN: DO NOT reset these two variables, as they are set by the frontend when the
  // sample frequency is set, and should not be reset when the system is reset
  // audio_time_per_NES_clock = 0.0;
  // audio_time_per_system_sample = 0.0;
}

void dnes_insert_cartridge(char *rom_path) {
  cart_load(rom_path);
  mbus_ready();
  pbus_ready();
}

void dnes_set_sample_frequency(uint32_t sample_rate) {
  audio_time_per_system_sample = 1.0 / (double)sample_rate;
  const double ppu_clock_freq = 5369318.0; // PPU Clock Frequency
  audio_time_per_NES_clock = 1.0 / ppu_clock_freq;
}

double dnes_audio_sample = 0.0;
bool dnes_audio_sample_ready = false;
