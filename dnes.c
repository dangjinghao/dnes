#include "dnes.h"
#include <stdio.h>

static void mbus_ready() {
  static struct bus mbus;
  ppu_register_mbus(&mbus);
  ram_register(&mbus);
  cart_register_mbus(&mbus);
  ctrl_register(&mbus);
  // TODO: apu_register(&mbus);
  // TODO: stuff...
  bus_ready(&mbus);
  cpu_mount_mbus(&mbus);
}

static void pbus_ready() {
  static struct bus pbus;
  cart_register_pbus(&pbus);
  ppu_ext_register(&pbus);
  bus_ready(&pbus);
  ppu_mount_pbus(&pbus);
}

static size_t system_clock = 0;

void dnes_clock() {
  ppu_clock();
  if (system_clock % 3 == 0) {
    cpu_clock();
  }

  if (ppu_nmi) {
    ppu_nmi = false;
    cpu_nmi();
  }
  system_clock += 1;
}

void dnes_reset() {
  cpu_reset();
  ppu_reset();
  cart_reset();
  ctrl_reset();
  system_clock = 0;
}

void dnes_insert_cartridge(char *rom_path) {
  cart_load(rom_path);
  mbus_ready();
  pbus_ready();
}
