#include "dnes.h"
#include <stdio.h>

// A simple form of Direct Memory Access is used to swiftly
// transfer data from CPU bus memory into the OAM memory. It would
// take too long to sensibly do this manually using a CPU loop, so
// the program prepares a page of memory with the sprite info required
// for the next frame and initiates a DMA transfer. This suspends the
// CPU momentarily while the PPU gets sent data at PPU clock speeds.
// Note here, that dma_page and dma_addr form a 16-bit address in
// the CPU bus address space
byte_t dma_page = 0x00;
byte_t dma_addr = 0x00;
byte_t dma_data = 0x00;

// DMA transfers need to be timed accurately. In principle it takes
// 512 cycles to read and write the 256 bytes of the OAM memory, a
// read followed by a write. However, the CPU needs to be on an "even"
// clock cycle, so a dummy cycle of idleness may be required
bool dma_dummy = true;

// Finally a flag to indicate that a DMA transfer is happening
bool dma_transfer = false;

static void dma_write(addr_t addr, byte_t data) {
  (void)addr;
  dma_page = data;
  dma_addr = 0x00;
  dma_transfer = true;
}

static void dma_register(struct bus *mbus) {
  bus_register(mbus, 0x4014, 0x4014,
               (&(struct bus_regparam){.write = dma_write}));
}

static struct bus mbus;

static void mbus_ready() {
  printf("Preparing main bus...\n");
  bus_init(&mbus, "Main bus", 0xFFFF);
  ppu_register_mbus(&mbus);
  ram_register(&mbus);
  cart_register_mbus(&mbus);
  ctrl_register(&mbus);
  apu_register(&mbus);
  dma_register(&mbus);
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
      // ...Yes! We need to wait until the next even CPU clock cycle
      // before it starts...
      if (dma_dummy) {
        // ...So hang around in here each clock until 1 or 2 cycles
        // have elapsed...
        if (system_clock % 2 == 1) {
          // ...and finally allow DMA to start
          dma_dummy = false;
        }
      } else {
        // DMA can take place!
        if (system_clock % 2 == 0) {
          // On even clock cycles, read from CPU bus
          dma_data = bus_read(&mbus, (addr_t)(dma_page << 8 | dma_addr));
        } else {
          // On odd clock cycles, write to PPU OAM
          ppu_oam_start[dma_addr] = dma_data;
          // Increment the lo byte of the address
          dma_addr++;
          // If this wraps around, we know that 256
          // bytes have been written, so end the DMA
          // transfer, and proceed as normal
          if (dma_addr == 0x00) {
            dma_transfer = false;
            dma_dummy = true;
          }
        }
      }
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
