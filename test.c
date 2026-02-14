#include "dnes.h"
// olc6502 test code
// Load Program (assembled at https://www.masswerk.at/6502/assembler.html)
/*
        *=$8000
        LDX #10
        STX $0000
        LDX #3
        STX $0001
        LDY $0000
        LDA #0
        CLC
        loop
        ADC $0001
        DEY
        BNE loop
        STA $0002
        NOP
        NOP
        NOP

Result:
  00: 0A
  01: 03
  02: 1E
*/

byte_t fake_rom_read(addr_t addr, bool read_only) {
  static byte_t data[] = {0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03,
                          0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9,
                          0x00, 0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0,
                          0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA};

  (void)read_only;
  return data[addr - 0x8000];
}


static addr_t fake_start_addr;
byte_t fake_start_read(addr_t addr, bool read_only) {
  (void)read_only;
  if (addr == 0xFFFC)
    return (byte_t)(fake_start_addr & 0xFF);
  else if (addr == 0xFFFD)
    return (byte_t)(fake_start_addr >> 8);
  else
    return 0;
}

void fake_start_register(addr_t addr) {
  struct bus_device param = {.read = fake_start_read, .write = NULL};
  fake_start_addr = addr;
  bus_register(0xFFFC, 0xFFFD, &param);
}

void fake_rom_register() {
  struct bus_device param = {.read = fake_rom_read, .write = NULL};
  bus_register(0x8000, 0x801B, &param);
}

int main() {
  fake_start_register(0x8000);
  fake_rom_register();
  ram_register();
  bus_ready();
  cpu_reset();
  while (true) {
    cpu_clock();
  }
}