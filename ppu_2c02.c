
#include "dnes.h"

static struct bus *pbus;

static byte_t name_table[2][1024];
static byte_t palette_table[32];

static inline addr_t ppu_mbus_real_addr(addr_t addr) { return (addr & 0x0007); }

static byte_t ppu_mbus_read(addr_t addr, bool read_only) {
  (void)read_only;
  addr = ppu_mbus_real_addr(addr);
  errorfln("Reading PPU address: %#04X", addr);
  switch (addr) {
  case 0x0000: // control
    break;
  case 0x0001: // mask
    break;
  case 0x0002: // status
    break;
  case 0x0003: // OAM address
    break;
  case 0x0004: // OAM data
    break;
  case 0x0005: // scroll
    break;
  case 0x0006: // PPU address
    break;
  case 0x0007: // PPU data
    break;
  default:
    errorfln("Unknown PPU address to read: %#04X.", addr);
  }
  return 0x00;
}
static void ppu_mbus_write(addr_t addr, byte_t data) {
  (void)data;
  addr = ppu_mbus_real_addr(addr);
  errorfln("Writing PPU address: %#04X", addr);
  switch (addr) {
  case 0x0000: // control
    break;
  case 0x0001: // mask
    break;
  case 0x0002: // status
    break;
  case 0x0003: // OAM address
    break;
  case 0x0004: // OAM data
    break;
  case 0x0005: // scroll
    break;
  case 0x0006: // PPU address
    break;
  case 0x0007: // PPU data
    break;
  default:
    errorfln("Unknown PPU address to write: %#04X.", addr);
  }
}

void ppu_register_mbus(struct bus *mbus) {
  bus_register(
      mbus, 0x2000, 0x3FFF,
      &(struct bus_regparam){.read = ppu_mbus_read, .write = ppu_mbus_write});
}

void ppu_mount_pbus(struct bus *bus) { pbus = bus; }

void ppu_clock() {}