#include "dnes.h"

static byte_t prg_bank_select_lo = 0x00;
static byte_t prg_bank_select_hi = 0x00;

static bool map_mbus_read(addr_t addr, size_t *mapped_addr, byte_t *data) {
  (void)data;
  if (!mapped_addr) {
    return false;
  }
  if (addr >= 0x8000 && addr <= 0xBFFF) {
    *mapped_addr = prg_bank_select_lo * 0x4000 + (addr & 0x3FFF);
    return true;
  }

  if (addr >= 0xC000 && addr <= 0xFFFF) {
    *mapped_addr = prg_bank_select_hi * 0x4000 + (addr & 0x3FFF);
    return true;
  }
  return false;
}
static bool map_mbus_write(addr_t addr, size_t *mapped_addr, byte_t data) {
  (void)mapped_addr;
  if (addr >= 0x8000 && addr <= 0xFFFF) {
    prg_bank_select_lo = data & 0x0F;
  }
  // Mapper has handled write, but do not update ROMs
  return false;
}

static bool map_pbus_read(addr_t addr, size_t *mapped_addr) {
  if (addr < 0x2000) {
    *mapped_addr = addr;
    return true;
  }
  return false;
}
static bool map_pbus_write(addr_t addr, size_t *mapped_addr) {
  if (addr < 0x2000) {
    if (mapper_chr_banks == 0) // Treating as RAM
    {
      *mapped_addr = addr;
      return true;
    }
  }
  return false;
}

static void reset() {

  prg_bank_select_lo = 0;
  prg_bank_select_hi = mapper_prg_banks - 1;
}

static struct mapper mapper = {
    .map_mbus_read = map_mbus_read,
    .map_mbus_write = map_mbus_write,
    .map_pbus_read = map_pbus_read,
    .map_pbus_write = map_pbus_write,
    .reset = reset,
};

struct mapper *mapper_002(byte_t prg_banks, byte_t chr_banks) {
  mapper_prg_banks = prg_banks;
  mapper_chr_banks = chr_banks;
  return &mapper;
}