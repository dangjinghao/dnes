#include "dnes.h"
#include <stdio.h>

static void mbus_ready() {
  static struct bus mbus;
  printf("Preparing main bus...\n");
  bus_init(&mbus, "Main bus", 0xFFFF);
  ppu_register_mbus(&mbus);
  ram_register(&mbus);
  cart_register_mbus(&mbus);
  ctrl_register(&mbus);
  apu_register(&mbus);
  dma_register(&mbus);
  dma_mount_mbus(&mbus);
  // stuff...
  bus_ready(&mbus);
  cpu_mount_mbus(&mbus);
}

static void pbus_ready() {
  static struct bus pbus;
  printf("Preparing PPU bus...\n");
  bus_init(&pbus, "PPU bus", 0x3FFF);
  cart_register_pbus(&pbus);
  ppu_ext_register(&pbus);
  bus_ready(&pbus);
  ppu_mount_pbus(&pbus);
}

static size_t system_clock = 0;

void dnes_clock() {
  ppu_clock();
  if (system_clock % 3 == 0) {
    // Is the system performing a DMA transfer form CPU memory to
    // OAM memory on PPU?...
    if (dma_transfer) {
      dma_do_transfer(system_clock);
    } else {
      // No DMA happening, the CPU is in control of its
      // own destiny. Go forth my friend and calculate
      // awesomeness for many generations to come...
      cpu_clock();
    }
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
