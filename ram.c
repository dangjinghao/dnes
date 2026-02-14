#include "dnes.h"

#define RAM_SIZE (2 * 1024) // 2KiB

static byte_t ram[RAM_SIZE];

static inline addr_t ram_real_addr(addr_t addr) { return (addr & 0x07FF); }

static void ram_write(addr_t addr, byte_t data) {
  ram[ram_real_addr(addr)] = data;
}

static byte_t ram_read(addr_t addr, bool read_only) {
  (void)read_only;
  return ram[ram_real_addr(addr)];
}

void ram_register() {
  struct bus_device param = {.read = ram_read, .write = ram_write};
  bus_register(0x0000, 0x1FFF, &param);
}