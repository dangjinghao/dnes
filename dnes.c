#include "dnes.h"
#include <stdio.h>

static void mbus_ready() {
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

static void pbus_ready() {
  static struct bus pbus;
  cart_register_pbus(&pbus);
  // TODO: name_table_register
  // TODO: palettes_register
  bus_ready(&pbus);
  ppu_mount_pbus(&pbus);
}

static size_t system_clock = 0;

void dnes_clock() {
  ppu_clock();
  if (system_clock % 3 == 0) {
    cpu_clock();
  }
  system_clock += 1;
}

void dnes_reset() {
  cpu_reset();
  system_clock = 0;
}

void dnes_insert_cartridge(char *rom_path) {
  cart_load(rom_path);
  mbus_ready();
  pbus_ready();
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    errorfln("Empty arguments");
    return 1;
  }

  dnes_insert_cartridge(argv[1]);
  dnes_reset();
  while (true) {
    dnes_clock();
  }
  return 0;
}