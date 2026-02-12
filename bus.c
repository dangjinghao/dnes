#include "dnes.h"

static inline bool is_addr_valid(addr_t addr) { return addr <= 0xFFFF; }

void bus_write(addr_t addr, byte_t data) {
  if (is_addr_valid(addr))
    ram[addr] = data;
  else
    errorf("bus_write: invalid address 0x%04X\n", addr);
}
byte_t bus_read(addr_t addr) {
  if (is_addr_valid(addr))
    return ram[addr];
  errorf("bus_read: invalid address 0x%04X\n", addr);
  return 0x00;
}

byte_t bus_read_only(addr_t addr) { return bus_read(addr); }