#include "mapper.h"
#include <dnes.h>
#include <stdlib.h>

static byte_t nTargetRegister = 0x00;
static bool bPRGBankMode = false;
static bool bCHRInversion = false;
static enum MIRROR mirror_mode = M_HORIZONTAL;
static uint32_t pRegister[8];
static uint32_t pCHRBank[8];
static uint32_t pPRGBank[4];
static bool bIRQActive = false;
static bool bIRQEnable = false;
static bool bIRQUpdate = false;
static uint16_t nIRQCounter = 0x0000;
static uint16_t nIRQReload = 0x0000;
static byte_t *vRAMStatic;

static void mapper_pop() {
  free(vRAMStatic);
  vRAMStatic = NULL;
}

static void reset() {
  nTargetRegister = 0x00;
  bPRGBankMode = false;
  bCHRInversion = false;
  mirror_mode = M_HORIZONTAL;

  bIRQActive = false;
  bIRQEnable = false;
  bIRQUpdate = false;
  nIRQCounter = 0x0000;
  nIRQReload = 0x0000;

  for (int i = 0; i < 4; i++)
    pPRGBank[i] = 0;
  for (int i = 0; i < 8; i++) {
    pCHRBank[i] = 0;
    pRegister[i] = 0;
  }

  pPRGBank[0] = 0 * 0x2000;
  pPRGBank[1] = 1 * 0x2000;
  pPRGBank[2] = (mapper_prg_banks * 2 - 2) * 0x2000;
  pPRGBank[3] = (mapper_prg_banks * 2 - 1) * 0x2000;
}

static bool irq_state() { return bIRQActive; }

static void irq_clear() { bIRQActive = false; }

static void scanline() {
  if (nIRQCounter == 0) {
    nIRQCounter = nIRQReload;
  } else
    nIRQCounter--;

  if (nIRQCounter == 0 && bIRQEnable) {
    bIRQActive = true;
  }
}

static enum MIRROR mirror() { return mirror_mode; }

static bool map_pbus_read(addr_t addr, size_t *mapped_addr) {
  if (addr >= 0x0000 && addr <= 0x03FF) {
    *mapped_addr = pCHRBank[0] + (addr & 0x03FF);
    return true;
  }

  if (addr >= 0x0400 && addr <= 0x07FF) {
    *mapped_addr = pCHRBank[1] + (addr & 0x03FF);
    return true;
  }

  if (addr >= 0x0800 && addr <= 0x0BFF) {
    *mapped_addr = pCHRBank[2] + (addr & 0x03FF);
    return true;
  }

  if (addr >= 0x0C00 && addr <= 0x0FFF) {
    *mapped_addr = pCHRBank[3] + (addr & 0x03FF);
    return true;
  }

  if (addr >= 0x1000 && addr <= 0x13FF) {
    *mapped_addr = pCHRBank[4] + (addr & 0x03FF);
    return true;
  }

  if (addr >= 0x1400 && addr <= 0x17FF) {
    *mapped_addr = pCHRBank[5] + (addr & 0x03FF);
    return true;
  }

  if (addr >= 0x1800 && addr <= 0x1BFF) {
    *mapped_addr = pCHRBank[6] + (addr & 0x03FF);
    return true;
  }

  if (addr >= 0x1C00 && addr <= 0x1FFF) {
    *mapped_addr = pCHRBank[7] + (addr & 0x03FF);
    return true;
  }

  return false;
}

static bool map_pbus_write(addr_t addr, size_t *mapped_addr) {
  (void)addr;
  (void)mapped_addr;
  return false;
}

static bool map_mbus_read(addr_t addr, size_t *mapped_addr, byte_t *data) {
  if (addr >= 0x6000 && addr <= 0x7FFF) {

    // Write data to RAM
    *data = vRAMStatic[addr & 0x1FFF];

    // Signal mapper has handled request
    *mapped_addr = SIZE_MAX;
    return true;
  }

  if (addr >= 0x8000 && addr <= 0x9FFF) {
    *mapped_addr = pPRGBank[0] + (addr & 0x1FFF);
    return true;
  }

  if (addr >= 0xA000 && addr <= 0xBFFF) {
    *mapped_addr = pPRGBank[1] + (addr & 0x1FFF);
    return true;
  }

  if (addr >= 0xC000 && addr <= 0xDFFF) {
    *mapped_addr = pPRGBank[2] + (addr & 0x1FFF);
    return true;
  }

  if (addr >= 0xE000 && addr <= 0xFFFF) {
    *mapped_addr = pPRGBank[3] + (addr & 0x1FFF);
    return true;
  }

  return false;
}

static bool map_mbus_write(addr_t addr, size_t *mapped_addr, byte_t data) {
  if (addr >= 0x6000 && addr <= 0x7FFF) {
    // Signal mapper has handled request
    *mapped_addr = SIZE_MAX;

    // Write data to RAM
    vRAMStatic[addr & 0x1FFF] = data;

    return true;
  }

  if (addr >= 0x8000 && addr <= 0x9FFF) {
    // Bank Select
    if (!(addr & 0x0001)) {
      nTargetRegister = data & 0x07;
      bPRGBankMode = (data & 0x40);
      bCHRInversion = (data & 0x80);
    } else {
      // Update target register
      pRegister[nTargetRegister] = data;

      // Update Pointer Table
      if (bCHRInversion) {
        pCHRBank[0] = pRegister[2] * 0x0400;
        pCHRBank[1] = pRegister[3] * 0x0400;
        pCHRBank[2] = pRegister[4] * 0x0400;
        pCHRBank[3] = pRegister[5] * 0x0400;
        pCHRBank[4] = (pRegister[0] & 0xFE) * 0x0400;
        pCHRBank[5] = pRegister[0] * 0x0400 + 0x0400;
        pCHRBank[6] = (pRegister[1] & 0xFE) * 0x0400;
        pCHRBank[7] = pRegister[1] * 0x0400 + 0x0400;
      } else {
        pCHRBank[0] = (pRegister[0] & 0xFE) * 0x0400;
        pCHRBank[1] = pRegister[0] * 0x0400 + 0x0400;
        pCHRBank[2] = (pRegister[1] & 0xFE) * 0x0400;
        pCHRBank[3] = pRegister[1] * 0x0400 + 0x0400;
        pCHRBank[4] = pRegister[2] * 0x0400;
        pCHRBank[5] = pRegister[3] * 0x0400;
        pCHRBank[6] = pRegister[4] * 0x0400;
        pCHRBank[7] = pRegister[5] * 0x0400;
      }

      if (bPRGBankMode) {
        pPRGBank[2] = (pRegister[6] & 0x3F) * 0x2000;
        pPRGBank[0] = (mapper_prg_banks * 2 - 2) * 0x2000;
      } else {
        pPRGBank[0] = (pRegister[6] & 0x3F) * 0x2000;
        pPRGBank[2] = (mapper_prg_banks * 2 - 2) * 0x2000;
      }

      pPRGBank[1] = (pRegister[7] & 0x3F) * 0x2000;
      pPRGBank[3] = (mapper_prg_banks * 2 - 1) * 0x2000;
    }

    *mapped_addr = SIZE_MAX;
    return true;
  }

  if (addr >= 0xA000 && addr <= 0xBFFF) {
    if (!(addr & 0x0001)) {
      // Mirroring
      if (data & 0x01)
        mirror_mode = M_HORIZONTAL;
      else
        mirror_mode = M_VERTICAL;
    } else {
      // PRG Ram Protect
      // TODO:
    }
    *mapped_addr = SIZE_MAX;
    return true;
  }

  if (addr >= 0xC000 && addr <= 0xDFFF) {
    if (!(addr & 0x0001)) {
      nIRQReload = data;
    } else {
      nIRQCounter = 0x0000;
    }
    *mapped_addr = SIZE_MAX;
    return true;
  }

  if (addr >= 0xE000 && addr <= 0xFFFF) {
    if (!(addr & 0x0001)) {
      bIRQEnable = false;
      bIRQActive = false;
    } else {
      bIRQEnable = true;
    }
    *mapped_addr = SIZE_MAX;
    return true;
  }

  return false;
}

static struct mapper mapper = {
    .map_mbus_read = map_mbus_read,
    .map_mbus_write = map_mbus_write,
    .map_pbus_read = map_pbus_read,
    .map_pbus_write = map_pbus_write,
    .reset = reset,
    .mirror = mirror,
    .irq_state = irq_state,
    .irq_clear = irq_clear,
    .scanline = scanline,
    .mapper_pop = mapper_pop,
    // .opt_dump_ram = dump_ram,
    // .opt_load_ram = load_ram,
};

struct mapper *mapper_004(byte_t prg_banks, byte_t chr_banks) {
  mapper_default_cons(prg_banks, chr_banks, &mapper);
  vRAMStatic = malloc(0x2000); // in olc's implementation, this is 32 * 1024
  return &mapper;
}