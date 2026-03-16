#include "dnes.h"
#include "mapper.h"
#include <stdbool.h>

static byte_t chr_bank_select = 0x00;

static bool map_mbus_read(addr_t addr, size_t *mapped_addr, byte_t *data) {
  (void)data;
  if (addr >= 0x8000 && addr <= 0xFFFF) {
    if (mapper_prg_banks == 1) // 16K ROM
      *mapped_addr = addr & 0x3FFF;
    if (mapper_prg_banks == 2) // 32K ROM
      *mapped_addr = addr & 0x7FFF;
    return true;
  } else
    return false;
}

static bool map_mbus_write(addr_t addr, size_t *mapped_addr, byte_t data) {
  if (addr >= 0x8000 && addr <= 0xFFFF) {
    chr_bank_select = data & 0x03;
    *mapped_addr = SIZE_MAX;
    return true;
  }

  // Mapper has handled write, but do not update ROMs
  return false;
}

static bool map_pbus_read(addr_t addr, size_t *mapped_addr) {
  if (addr < 0x2000) {
    *mapped_addr = chr_bank_select * 0x2000 + addr;
    return true;
  } else
    return false;
}
static bool map_pbus_write(addr_t addr, size_t *mapped_addr) {
  (void)addr;
  (void)mapped_addr;
  return false;
}

static void reset() { chr_bank_select = 0; }

static enum MIRROR mirror() { return M_HARDWARE; }

static struct mapper mapper = {
    .map_mbus_read = map_mbus_read,
    .map_mbus_write = map_mbus_write,
    .map_pbus_read = map_pbus_read,
    .map_pbus_write = map_pbus_write,
    .reset = reset,
    .mirror = mirror,
    .irq_state = mapper_defualt_irq_state,
    .irq_clear = mapper_defualt_irq_clear,
    .scanline = mapper_defualt_scanline,
    .mapper_pop = mapper_default_mapper_pop,
};

struct mapper *mapper_003(byte_t prg_banks, byte_t chr_banks) {
  mapper_default_cons(prg_banks, chr_banks, &mapper);

  return &mapper;
}