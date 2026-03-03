#include "dnes.h"
// TODO
static byte_t apu_read(addr_t addr, bool read_only) {
  (void)addr;
  (void)read_only;
  return 0x00;
}

static void apu_write(addr_t addr, byte_t data) {
  (void)addr;
  (void)data;
}

void apu_register(struct bus *bus) {
  struct bus_regparam p = {.read = apu_read, .write = apu_write};
  bus_register(bus, 0x4000, 0x4013, &p);
  bus_register(bus, 0x4015, 0x4015, &p);
}