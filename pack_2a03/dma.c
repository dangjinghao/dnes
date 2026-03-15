#include "dnes.h"

// A simple form of Direct Memory Access is used to swiftly
// transfer data from CPU bus memory into the OAM memory. It would
// take too long to sensibly do this manually using a CPU loop, so
// the program prepares a page of memory with the sprite info required
// for the next frame and initiates a DMA transfer. This suspends the
// CPU momentarily while the PPU gets sent data at PPU clock speeds.
// Note here, that dma_page and dma_addr form a 16-bit address in
// the CPU bus address space
static byte_t dma_page = 0x00;
static byte_t dma_addr = 0x00;
static byte_t dma_data = 0x00;

// DMA transfers need to be timed accurately. In principle it takes
// 512 cycles to read and write the 256 bytes of the OAM memory, a
// read followed by a write. However, the CPU needs to be on an "even"
// clock cycle, so a dummy cycle of idleness may be required
static bool dma_dummy = true;

static void dma_write(addr_t addr, byte_t data) {
  (void)addr;
  dma_page = data;
  dma_addr = 0x00;
  dma_transfer = true;
}

static struct bus *mbus;

void dma_register(struct bus *bus) {
  bus_register(bus, 0x4014, 0x4014,
               (&(struct bus_regparam){.write = dma_write, .read = NULL}));
}

void dma_mount_mbus(struct bus *bus) { mbus = bus; }

void dma_do_transfer(size_t system_clock) {
  // We need to wait until the next even CPU clock cycle
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
      dma_data = bus_read(mbus, (addr_t)(dma_page << 8 | dma_addr));
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
}

// Finally a flag to indicate that a DMA transfer is happening
bool dma_transfer = false;
