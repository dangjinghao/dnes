#include "dnes.h"

void bus_write(addr_t addr, byte_t data) { ram[addr] = data; }
byte_t bus_read(addr_t addr) { return ram[addr]; }

byte_t bus_read_only(addr_t addr) { return bus_read(addr); }