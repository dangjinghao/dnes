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
*/
static byte_t data[] = {0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03,
                        0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9,
                        0x00, 0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0,
                        0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA};
int main() {
  size_t nOffset = 0x8000;

  for (size_t i = 0; i < sizeof(data) / sizeof(data[0]); i++) {
    ram[nOffset + i] = data[i];
  }
  ram[0xFFFC] = 0x00;
  ram[0xFFFD] = 0x80;

  cpu_reset();
  while (true) {
    cpu_clock();
  }
}