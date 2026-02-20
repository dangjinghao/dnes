#include "dnes.h"

static size_t map_mbus_read(addr_t addr) {
  // if PRGROM is 16KB
  //     CPU Address Bus          PRG ROM
  //     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
  //     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
  // if PRGROM is 32KB
  //     CPU Address Bus          PRG ROM
  //     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF
  return addr & (mapper_prg_banks > 1 ? 0x7FFF : 0x3FFF);
}
static size_t map_mbus_write(addr_t addr) {
  return addr & (mapper_prg_banks > 1 ? 0x7FFF : 0x3FFF);
}
static size_t map_pbus_read(addr_t addr) {
  // There is no mapping required for PPU
  // PPU Address Bus          CHR ROM
  // 0x0000 -> 0x1FFF: Map    0x0000 -> 0x1FFF
  return addr;
}
static size_t map_pbus_write(addr_t addr) { return addr; }

static void reset() {}

static struct mapper mapper = {
    .map_mbus_read = map_mbus_read,
    .map_mbus_write = map_mbus_write,
    .map_pbus_read = map_pbus_read,
    .map_pbus_write = map_pbus_write,
    .reset = reset,
};

struct mapper *mapper_000(byte_t prg_banks, byte_t chr_banks) {
  mapper_prg_banks = prg_banks;
  mapper_chr_banks = chr_banks;
  return &mapper;
}