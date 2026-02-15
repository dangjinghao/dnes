#include "dnes.h"

void mbus_ready() {
  static struct bus mbus;
  ppu_register_mbus(&mbus);
  ram_register(&mbus);
  cart_register_mbus(&mbus);
  // TODO: apu_register(&mbus);
  // TODO: controls
  // TODO: stuff...
  bus_ready(&mbus);
  cpu_mount_mbus(&mbus);
}

void pbus_ready() {
  static struct bus pbus;
  cart_register_pbus(&pbus);
  // TODO: name_table_register
  // TODO: palettes_register
  bus_ready(&pbus);
  ppu_mount_pbus(&pbus);
}

static size_t system_clock = 0;

void dnes_clock() { system_clock += 1; }

void dnes_reset() {
  cpu_reset();
  system_clock = 0;
}

void dnes_insert_cartridge(char *rom_path) {
  cart_load(rom_path);
  mbus_ready();
  pbus_ready();
}