#include "dnes.h"

void mbus_ready() {
  static struct bus mbus;
  ram_register(&mbus);
  cart_register_mbus(&mbus);
  ppu_register_mbus(&mbus);
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