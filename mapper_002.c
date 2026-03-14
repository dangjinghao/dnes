#include "dnes.h"

static byte_t nPRGBankSelectLo = 0x00;
static byte_t nPRGBankSelectHi = 0x00;

static size_t map_mbus_read(addr_t addr, byte_t *data) {
  (void)data;
  if (addr >= 0x8000 && addr <= 0xBFFF) {
    return nPRGBankSelectLo * 0x4000 + (addr & 0x3FFF);
  }

  if (addr >= 0xC000 && addr <= 0xFFFF) {
    return nPRGBankSelectHi * 0x4000 + (addr & 0x3FFF);
  }
  return SIZE_MAX; // no mapping
}
static size_t map_mbus_write(addr_t addr, byte_t data) {
  if (addr >= 0x8000 && addr <= 0xFFFF) {
    nPRGBankSelectLo = data & 0x0F;
  }
  return SIZE_MAX; // no mapping
}

static size_t map_pbus_read(addr_t addr) {
  if (addr < 0x2000) {
    return addr;
  } else {
    return SIZE_MAX;
  }
}
static size_t map_pbus_write(addr_t addr) {
  if (addr < 0x2000) {
    if (mapper_chr_banks == 0) // Treating as RAM
    {
      return addr;
    }
  }
  return SIZE_MAX;
}

static void reset() {

  nPRGBankSelectLo = 0;
  nPRGBankSelectHi = mapper_prg_banks - 1;
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