
#include "dnes.h"
#include <SDL3/SDL.h>

static union {
  struct {
    byte_t unused : 5;
    byte_t sprite_overflow : 1;
    byte_t sprite_zero_hit : 1;
    byte_t vertical_blank : 1;
  };

  byte_t reg;
} status;

static union {
  struct {
    byte_t grayscale : 1;
    byte_t render_background_left : 1;
    byte_t render_sprites_left : 1;
    byte_t render_background : 1;
    byte_t render_sprites : 1;
    byte_t enhance_red : 1;
    byte_t enhance_green : 1;
    byte_t enhance_blue : 1;
  };

  byte_t reg;
} mask;

static union PPUCTRL {
  struct {
    byte_t nametable_x : 1;
    byte_t nametable_y : 1;
    byte_t increment_mode : 1;
    byte_t pattern_sprite : 1;
    byte_t pattern_background : 1;
    byte_t sprite_size : 1;
    byte_t slave_mode : 1; // unused
    byte_t enable_nmi : 1;
  };

  byte_t reg;
} control;

union loopy_register {
  // Credit to Loopy for working this out :D
  struct {
    uint16_t coarse_x : 5;
    uint16_t coarse_y : 5;
    uint16_t nametable_x : 1;
    uint16_t nametable_y : 1;
    uint16_t fine_y : 3;
    uint16_t unused : 1;
  };

  uint16_t reg;
};



static union loopy_register
    vram_addr; // Active "pointer" address into nametable to extract
               // background tile info
static union loopy_register
    tram_addr; // Temporary store of information to be "transferred"
               // into "pointer" at various times

// Pixel offset horizontally
static byte_t fine_x = 0x00;

// Internal communications
static byte_t address_latch = 0x00;
static byte_t ppu_data_buffer = 0x00;

// Pixel "dot" position information
static int16_t scanline = 0;
static int16_t cycle = 0;

// Background rendering
static byte_t bg_next_tile_id = 0x00;
static byte_t bg_next_tile_attrib = 0x00;
static byte_t bg_next_tile_lsb = 0x00;
static byte_t bg_next_tile_msb = 0x00;
static uint16_t bg_shifter_pattern_lo = 0x0000;
static uint16_t bg_shifter_pattern_hi = 0x0000;
static uint16_t bg_shifter_attrib_lo = 0x0000;
static uint16_t bg_shifter_attrib_hi = 0x0000;

static struct bus *pbus;

static byte_t name_table[2][1024];
static byte_t palette_table[32];

static struct SDL_Color screen_color[0x40] = {
    {84, 84, 84, 255},    {0, 30, 116, 255},    {8, 16, 144, 255},
    {48, 0, 136, 255},    {68, 0, 100, 255},    {92, 0, 48, 255},
    {84, 4, 0, 255},      {60, 24, 0, 255},     {32, 42, 0, 255},
    {8, 58, 0, 255},      {0, 64, 0, 255},      {0, 60, 0, 255},
    {0, 50, 60, 255},     {0, 0, 0, 255},       {0, 0, 0, 255},
    {0, 0, 0, 255},       {152, 150, 152, 255}, {8, 76, 196, 255},
    {48, 50, 236, 255},   {92, 30, 228, 255},   {136, 20, 176, 255},
    {160, 20, 100, 255},  {152, 34, 32, 255},   {120, 60, 0, 255},
    {84, 90, 0, 255},     {40, 114, 0, 255},    {8, 124, 0, 255},
    {0, 118, 40, 255},    {0, 102, 120, 255},   {0, 0, 0, 255},
    {0, 0, 0, 255},       {0, 0, 0, 255},       {236, 238, 236, 255},
    {76, 154, 236, 255},  {120, 124, 236, 255}, {176, 98, 236, 255},
    {228, 84, 236, 255},  {236, 88, 180, 255},  {236, 106, 100, 255},
    {212, 136, 32, 255},  {160, 170, 0, 255},   {116, 196, 0, 255},
    {76, 208, 32, 255},   {56, 204, 108, 255},  {56, 180, 204, 255},
    {60, 60, 60, 255},    {0, 0, 0, 255},       {0, 0, 0, 255},
    {236, 238, 236, 255}, {168, 204, 236, 255}, {188, 188, 236, 255},
    {212, 178, 236, 255}, {236, 174, 236, 255}, {236, 174, 212, 255},
    {236, 180, 176, 255}, {228, 196, 144, 255}, {204, 210, 120, 255},
    {180, 222, 120, 255}, {168, 226, 144, 255}, {152, 226, 180, 255},
    {160, 214, 228, 255}, {160, 162, 160, 255}, {0, 0, 0, 255},
    {0, 0, 0, 255},
};

/**
 * @brief Get the corresponded color from palette by pixel 2 bit index
 *
 * @param palette_idx
 * @param px the 2b pixel index
 * @return struct SDL_Color*
 */
static struct SDL_Color *get_color_from_palette(byte_t palette_idx, byte_t px) {
  const addr_t start_addr = 0x3F00;
  return &screen_color[bus_read(pbus,
                                start_addr + (byte_t)(palette_idx * 4) + px) &
                       0x3F];
}

static inline addr_t ppu_mbus_real_addr(addr_t addr) { return (addr & 0x0007); }

static byte_t ppu_mbus_read(addr_t addr, bool read_only) {
  addr = ppu_mbus_real_addr(addr);
  byte_t data = 0x00;
  errorfln("Reading PPU address: %#04X", addr);
  if (read_only) {
    // Reading from PPU registers can affect their contents
    // so this read only option is used for examining the
    // state of the PPU without changing its state. This is
    // really only used in debug mode.
    switch (addr) {
    case 0x0000: // control
      data = control.reg;
      break;
    case 0x0001: // mask
      data = mask.reg;
      break;
    case 0x0002: // status
      data = status.reg;
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
  } else {
    switch (addr) {
    case 0x0000: // control, not readable
      break;
    case 0x0001: // mask, not readable
      break;
    case 0x0002: // status
      // Reading from the status register has the effect of resetting
      // different parts of the circuit. Only the top three bits
      // contain status information, however it is possible that
      // some "noise" gets picked up on the bottom 5 bits which
      // represent the last PPU bus transaction. Some games "may"
      // use this noise as valid data (even though they probably
      // shouldn't)
      data = (status.reg & 0xE0) | (ppu_data_buffer & 0x1F);
      // Clear the vertical blanking flag
      status.vertical_blank = 0;

      // Reset Loopy's Address latch flag
      address_latch = 0;
      break;
    case 0x0003: // OAM address
      break;
    case 0x0004: // OAM data
      break;
    case 0x0005: // scroll, not readable
      break;
    case 0x0006: // PPU address, not readable
      break;
    case 0x0007: // PPU data
      // Reads from the NameTable ram get delayed one cycle,
      // so output buffer which contains the data from the
      // previous read request
      data = ppu_data_buffer;
      // then update the buffer for next time
      ppu_data_buffer = bus_read(pbus, vram_addr.reg);
      // However, if the address was in the palette range, the
      // data is not delayed, so it returns immediately
      if (vram_addr.reg >= 0x3F00)
        data = ppu_data_buffer;
      // All reads from PPU data automatically increment the nametable
      // address depending upon the mode set in the control register.
      // If set to vertical mode, the increment is 32, so it skips
      // one whole nametable row; in horizontal mode it just increments
      // by 1, moving to the next column
      vram_addr.reg += (control.increment_mode ? 32 : 1);
      break;
    default:
      errorfln("Unknown PPU address to read: %#04X.", addr);
    }
  }
  return data;
}
static void ppu_mbus_write(addr_t addr, byte_t data) {
  addr = ppu_mbus_real_addr(addr);
  errorfln("Writing PPU address: %#04X", addr);
  switch (addr) {
  case 0x0000: // control
    control.reg = data;
    tram_addr.nametable_x = control.nametable_x;
    tram_addr.nametable_y = control.nametable_y;
    break;
  case 0x0001: // mask
    mask.reg = data;
    break;
  case 0x0002: // status
    break;
  case 0x0003: // OAM address
    break;
  case 0x0004: // OAM data
    break;
  case 0x0005: // scroll
    if (address_latch == 0) {
      // First write to scroll register contains X offset in pixel space
      // which we split into coarse and fine x values
      fine_x = data & 0x07;
      tram_addr.coarse_x = data >> 3;
      address_latch = 1;
    } else {
      // First write to scroll register contains Y offset in pixel space
      // which we split into coarse and fine Y values
      tram_addr.fine_y = data & 0x07;
      tram_addr.coarse_y = data >> 3;
      address_latch = 0;
    }
    break;
  case 0x0006: // PPU address
    if (address_latch == 0) {
      // PPU address bus can be accessed by CPU via the ADDR and DATA
      // registers. The fisrt write to this register latches the high byte
      // of the address, the second is the low byte. Note the writes
      // are stored in the tram register...
      tram_addr.reg = (uint16_t)((data & 0x3F) << 8) | (tram_addr.reg & 0x00FF);
      address_latch = 1;
    } else {
      // ...when a whole address has been written, the internal vram address
      // buffer is updated. Writing to the PPU is unwise during rendering
      // as the PPU will maintam the vram address automatically whilst
      // rendering the scanline position.
      tram_addr.reg = (tram_addr.reg & 0xFF00) | data;
      vram_addr = tram_addr;
      address_latch = 0;
    }
    break;
  case 0x0007: // PPU data
    bus_write(pbus, vram_addr.reg, data);
    // All writes from PPU data automatically increment the nametable
    // address depending upon the mode set in the control register.
    // If set to vertical mode, the increment is 32, so it skips
    // one whole nametable row; in horizontal mode it just increments
    // by 1, moving to the next column
    vram_addr.reg += (control.increment_mode ? 32 : 1);
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

static void ppu_nametable_write(addr_t addr, byte_t data) {
  addr &= 0x0FFF;
  if (cart_get_mirror_mode() == M_VERTICAL) {
    // Vertical
    if (addr >= 0x0000 && addr <= 0x03FF)
      name_table[0][addr & 0x03FF] = data;
    if (addr >= 0x0400 && addr <= 0x07FF)
      name_table[1][addr & 0x03FF] = data;
    if (addr >= 0x0800 && addr <= 0x0BFF)
      name_table[0][addr & 0x03FF] = data;
    if (addr >= 0x0C00 && addr <= 0x0FFF)
      name_table[1][addr & 0x03FF] = data;
  } else if (cart_get_mirror_mode() == M_HORIZONTAL) {
    // Horizontal
    if (addr >= 0x0000 && addr <= 0x03FF)
      name_table[0][addr & 0x03FF] = data;
    if (addr >= 0x0400 && addr <= 0x07FF)
      name_table[0][addr & 0x03FF] = data;
    if (addr >= 0x0800 && addr <= 0x0BFF)
      name_table[1][addr & 0x03FF] = data;
    if (addr >= 0x0C00 && addr <= 0x0FFF)
      name_table[1][addr & 0x03FF] = data;
  }
}

static byte_t ppu_nametable_read(addr_t addr, bool readonly) {
  (void)readonly;
  byte_t data = 0x00;
  addr &= 0x0FFF;

  if (cart_get_mirror_mode() == M_VERTICAL) {
    // Vertical
    if (addr >= 0x0000 && addr <= 0x03FF)
      data = name_table[0][addr & 0x03FF];
    if (addr >= 0x0400 && addr <= 0x07FF)
      data = name_table[1][addr & 0x03FF];
    if (addr >= 0x0800 && addr <= 0x0BFF)
      data = name_table[0][addr & 0x03FF];
    if (addr >= 0x0C00 && addr <= 0x0FFF)
      data = name_table[1][addr & 0x03FF];
  } else if (cart_get_mirror_mode() == M_HORIZONTAL) {
    // Horizontal
    if (addr >= 0x0000 && addr <= 0x03FF)
      data = name_table[0][addr & 0x03FF];
    if (addr >= 0x0400 && addr <= 0x07FF)
      data = name_table[0][addr & 0x03FF];
    if (addr >= 0x0800 && addr <= 0x0BFF)
      data = name_table[1][addr & 0x03FF];
    if (addr >= 0x0C00 && addr <= 0x0FFF)
      data = name_table[1][addr & 0x03FF];
  }
  return data;
}

static void ppu_palette_write(addr_t addr, byte_t data) {
  addr &= 0x001F;
  if (addr == 0x0010)
    addr = 0x0000;
  if (addr == 0x0014)
    addr = 0x0004;
  if (addr == 0x0018)
    addr = 0x0008;
  if (addr == 0x001C)
    addr = 0x000C;
  palette_table[addr] = data;
}

static byte_t ppu_palette_read(addr_t addr, bool readonly) {
  (void)readonly;
  byte_t data = 0x00;
  addr &= 0x001F;
  if (addr == 0x0010)
    addr = 0x0000;
  if (addr == 0x0014)
    addr = 0x0004;
  if (addr == 0x0018)
    addr = 0x0008;
  if (addr == 0x001C)
    addr = 0x000C;
  data = palette_table[addr] & (mask.grayscale ? 0x30 : 0x3F);
  return data;
}

void ppu_ext_register(struct bus *pbus) {
  // name table register
  bus_register(pbus, 0x2000, 0x3EFF,
               &(struct bus_regparam){.read = ppu_nametable_read,
                                      .write = ppu_nametable_write});
  // palette register
  bus_register(pbus, 0x3F00, 0x3FFF,
               &(struct bus_regparam){.read = ppu_palette_read,
                                      .write = ppu_palette_write});
}

void ppu_reset() {
  fine_x = 0x00;
  address_latch = 0x00;
  ppu_data_buffer = 0x00;
  scanline = 0;
  cycle = 0;
  bg_next_tile_id = 0x00;
  bg_next_tile_attrib = 0x00;
  bg_next_tile_lsb = 0x00;
  bg_next_tile_msb = 0x00;
  bg_shifter_pattern_lo = 0x0000;
  bg_shifter_pattern_hi = 0x0000;
  bg_shifter_attrib_lo = 0x0000;
  bg_shifter_attrib_hi = 0x0000;
  status.reg = 0x00;
  mask.reg = 0x00;
  control.reg = 0x00;
  vram_addr.reg = 0x0000;
  tram_addr.reg = 0x0000;
}
// ==============================================================================
// Increment the background tile "pointer" one tile/column horizontally
static void clock_increment_scroll_x() {
  // Note: pixel perfect scrolling horizontally is handled by the
  // data shifters. Here we are operating in the spatial domain of
  // tiles, 8x8 pixel blocks.

  // Ony if rendering is enabled
  if (mask.render_background || mask.render_sprites) {
    // A single name table is 32x30 tiles. As we increment horizontally
    // we may cross into a neighbouring nametable, or wrap around to
    // a neighbouring nametable
    if (vram_addr.coarse_x == 31) {
      // Leaving nametable so wrap address round
      vram_addr.coarse_x = 0;
      // Flip target nametable bit
      vram_addr.nametable_x = ~vram_addr.nametable_x;
    } else {
      // Staying in current nametable, so just increment
      vram_addr.coarse_x++;
    }
  }
}

// ==============================================================================
// Increment the background tile "pointer" one scanline vertically
static void clock_increment_scroll_y() {
  // Incrementing vertically is more complicated. The visible nametable
  // is 32x30 tiles, but in memory there is enough room for 32x32 tiles.
  // The bottom two rows of tiles are in fact not tiles at all, they
  // contain the "attribute" information for the entire table. This is
  // information that describes which palettes are used for different
  // regions of the nametable.

  // In addition, the NES doesnt scroll vertically in chunks of 8 pixels
  // i.e. the height of a tile, it can perform fine scrolling by using
  // the fine_y component of the register. This means an increment in Y
  // first adjusts the fine offset, but may need to adjust the whole
  // row offset, since fine_y is a value 0 to 7, and a row is 8 pixels high

  // Ony if rendering is enabled
  if (mask.render_background || mask.render_sprites) {
    // If possible, just increment the fine y offset
    if (vram_addr.fine_y < 7) {
      vram_addr.fine_y++;
    } else {
      // If we have gone beyond the height of a row, we need to
      // increment the row, potentially wrapping into neighbouring
      // vertical nametables. Dont forget however, the bottom two rows
      // do not contain tile information. The coarse y offset is used
      // to identify which row of the nametable we want, and the fine
      // y offset is the specific "scanline"

      // Reset fine y offset
      vram_addr.fine_y = 0;

      // Check if we need to swap vertical nametable targets
      if (vram_addr.coarse_y == 29) {
        // We do, so reset coarse y offset
        vram_addr.coarse_y = 0;
        // And flip the target nametable bit
        vram_addr.nametable_y = ~vram_addr.nametable_y;
      } else if (vram_addr.coarse_y == 31) {
        // In case the pointer is in the attribute memory, we
        // just wrap around the current nametable
        vram_addr.coarse_y = 0;
      } else {
        // None of the above boundary/wrapping conditions apply
        // so just increment the coarse y offset
        vram_addr.coarse_y++;
      }
    }
  }
}
// ==============================================================================
// Transfer the temporarily stored horizontal nametable access information
// into the "pointer". Note that fine x scrolling is not part of the "pointer"
// addressing mechanism
static void clock_transfer_address_x() {
  // Ony if rendering is enabled
  if (mask.render_background || mask.render_sprites) {
    vram_addr.nametable_x = tram_addr.nametable_x;
    vram_addr.coarse_x = tram_addr.coarse_x;
  }
}

// ==============================================================================
// Transfer the temporarily stored vertical nametable access information
// into the "pointer". Note that fine y scrolling is part of the "pointer"
// addressing mechanism

static void clock_transfer_address_y() {
  // Ony if rendering is enabled
  if (mask.render_background || mask.render_sprites) {
    vram_addr.fine_y = tram_addr.fine_y;
    vram_addr.nametable_y = tram_addr.nametable_y;
    vram_addr.coarse_y = tram_addr.coarse_y;
  }
}
// ==============================================================================
// Prime the "in-effect" background tile shifters ready for outputting next
// 8 pixels in scanline.
static void clock_load_background_shifters() {
  // Each PPU update we calculate one pixel. These shifters shift 1 bit along
  // feeding the pixel compositor with the binary information it needs. Its
  // 16 bits wide, because the top 8 bits are the current 8 pixels being drawn
  // and the bottom 8 bits are the next 8 pixels to be drawn. Naturally this
  // means the required bit is always the MSB of the shifter. However, "fine x"
  // scrolling plays a part in this too, whcih is seen later, so in fact we can
  // choose any one of the top 8 bits.
  bg_shifter_pattern_lo = (bg_shifter_pattern_lo & 0xFF00) | bg_next_tile_lsb;
  bg_shifter_pattern_hi = (bg_shifter_pattern_hi & 0xFF00) | bg_next_tile_msb;

  // Attribute bits do not change per pixel, rather they change every 8 pixels
  // but are synchronised with the pattern shifters for convenience, so here
  // we take the bottom 2 bits of the attribute word which represent which
  // palette is being used for the current 8 pixels and the next 8 pixels, and
  // "inflate" them to 8 bit words.
  bg_shifter_attrib_lo = (bg_shifter_attrib_lo & 0xFF00) |
                         ((bg_next_tile_attrib & 0b01) ? 0xFF : 0x00);
  bg_shifter_attrib_hi = (bg_shifter_attrib_hi & 0xFF00) |
                         ((bg_next_tile_attrib & 0b10) ? 0xFF : 0x00);
}
// ==============================================================================
// Every cycle the shifters storing pattern and attribute information shift
// their contents by 1 bit. This is because every cycle, the output progresses
// by 1 pixel. This means relatively, the state of the shifter is in sync
// with the pixels being drawn for that 8 pixel section of the scanline.

static void clock_update_shifters() {
  if (mask.render_background) {
    // Shifting background tile pattern row
    bg_shifter_pattern_lo <<= 1;
    bg_shifter_pattern_hi <<= 1;

    // Shifting palette attributes by 1
    bg_shifter_attrib_lo <<= 1;
    bg_shifter_attrib_hi <<= 1;
  }
}
static bool frame_complete = false;
static bool nmi = false;

bool ppu_is_frame_complete() { return frame_complete; }

void ppu_clock() {
  // All but 1 of the secanlines is visible to the user. The pre-render scanline
  // at -1, is used to configure the "shifters" for the first visible scanline,
  // 0.
  if (scanline >= -1 && scanline < 240) {
    if (scanline == 0 && cycle == 0) {
      // "Odd Frame" cycle skip
      cycle = 1;
    }

    if (scanline == -1 && cycle == 1) {
      // Effectively start of new frame, so clear vertical blank flag
      status.vertical_blank = 0;
    }

    if ((cycle >= 2 && cycle < 258) || (cycle >= 321 && cycle < 338)) {
      clock_update_shifters();

      // In these cycles we are collecting and working with visible data
      // The "shifters" have been preloaded by the end of the previous
      // scanline with the data for the start of this scanline. Once we
      // leave the visible region, we go dormant until the shifters are
      // preloaded for the next scanline.

      // Fortunately, for background rendering, we go through a fairly
      // repeatable sequence of events, every 2 clock cycles.
      switch ((cycle - 1) % 8) {
      case 0:
        // Load the current background tile pattern and attributes into the
        // "shifter"
        clock_load_background_shifters();

        // Fetch the next background tile ID
        // "(vram_addr.reg & 0x0FFF)" : Mask to 12 bits that are relevant
        // "| 0x2000"                 : Offset into nametable space on PPU
        // address bus
        bg_next_tile_id = bus_read(pbus, 0x2000 | (vram_addr.reg & 0x0FFF));

        // Explanation:
        // The bottom 12 bits of the loopy register provide an index into
        // the 4 nametables, regardless of nametable mirroring configuration.
        // nametable_y(1) nametable_x(1) coarse_y(5) coarse_x(5)
        //
        // Consider a single nametable is a 32x32 array, and we have four of
        // them
        //   0                1
        // 0 +----------------+----------------+
        //   |                |                |
        //   |                |                |
        //   |    (32x32)     |    (32x32)     |
        //   |                |                |
        //   |                |                |
        // 1 +----------------+----------------+
        //   |                |                |
        //   |                |                |
        //   |    (32x32)     |    (32x32)     |
        //   |                |                |
        //   |                |                |
        //   +----------------+----------------+
        //
        // This means there are 4096 potential locations in this array, which
        // just so happens to be 2^12!
        break;
      case 2:
        // Fetch the next background tile attribute. OK, so this one is a bit
        // more involved :P

        // Recall that each nametable has two rows of cells that are not tile
        // information, instead they represent the attribute information that
        // indicates which palettes are applied to which area on the screen.
        // Importantly (and frustratingly) there is not a 1 to 1 correspondance
        // between background tile and palette. Two rows of tile data holds
        // 64 attributes. Therfore we can assume that the attributes affect
        // 8x8 zones on the screen for that nametable. Given a working
        // resolution of 256x240, we can further assume that each zone is 32x32
        // pixels in screen space, or 4x4 tiles. Four system palettes are
        // allocated to background rendering, so a palette can be specified
        // using just 2 bits. The attribute byte therefore can specify 4
        // distinct palettes. Therefore we can even further assume that a single
        // palette is applied to a 2x2 tile combination of the 4x4 tile zone.
        // The very fact that background tiles "share" a palette locally is the
        // reason why in some games you see distortion in the colours at screen
        // edges.

        // As before when choosing the tile ID, we can use the bottom 12 bits of
        // the loopy register, but we need to make the implementation "coarser"
        // because instead of a specific tile, we want the attribute byte for a
        // group of 4x4 tiles, or in other words, we divide our 32x32 address
        // by 4 to give us an equivalent 8x8 address, and we offset this address
        // into the attribute section of the target nametable.

        // Reconstruct the 12 bit loopy address into an offset into the
        // attribute memory

        // "(vram_addr.coarse_x >> 2)"        : integer divide coarse x by 4,
        //                                      from 5 bits to 3 bits
        // "((vram_addr.coarse_y >> 2) << 3)" : integer divide coarse y by 4,
        //                                      from 5 bits to 3 bits,
        //                                      shift to make room for coarse x

        // Result so far: YX00 00yy yxxx

        // All attribute memory begins at 0x03C0 within a nametable, so OR with
        // result to select target nametable, and attribute byte offset. Finally
        // OR with 0x2000 to offset into nametable address space on PPU bus.
        bg_next_tile_attrib =
            bus_read(pbus, (addr_t)(0x23C0 | (vram_addr.nametable_y << 11) |
                                    (vram_addr.nametable_x << 10) |
                                    ((vram_addr.coarse_y >> 2) << 3) |
                                    (vram_addr.coarse_x >> 2)));
        // Right we've read the correct attribute byte for a specified address,
        // but the byte itself is broken down further into the 2x2 tile groups
        // in the 4x4 attribute zone.

        // The attribute byte is assembled thus: BR(76) BL(54) TR(32) TL(10)
        //
        // +----+----+			    +----+----+
        // | TL | TR |			    | ID | ID |
        // +----+----+ where TL =   +----+----+
        // | BL | BR |			    | ID | ID |
        // +----+----+			    +----+----+
        //
        // Since we know we can access a tile directly from the 12 bit address,
        // we can analyse the bottom bits of the coarse coordinates to provide
        // us with the correct offset into the 8-bit word, to yield the 2 bits
        // we are actually interested in which specifies the palette for the 2x2
        // group of tiles. We know if "coarse y % 4" < 2 we are in the top half
        // else bottom half. Likewise if "coarse x % 4" < 2 we are in the left
        // half else right half. Ultimately we want the bottom two bits of our
        // attribute word to be the palette selected. So shift as required...
        if (vram_addr.coarse_y & 0x02)
          bg_next_tile_attrib >>= 4;
        if (vram_addr.coarse_x & 0x02)
          bg_next_tile_attrib >>= 2;
        bg_next_tile_attrib &= 0x03;
        break;

        // Compared to the last two, the next two are the easy ones... :P

      case 4:
        // Fetch the next background tile LSB bit plane from the pattern memory
        // The Tile ID has been read from the nametable. We will use this id to
        // index into the pattern memory to find the correct sprite (assuming
        // the sprites lie on 8x8 pixel boundaries in that memory, which they do
        // even though 8x16 sprites exist, as background tiles are always 8x8).
        //
        // Since the sprites are effectively 1 bit deep, but 8 pixels wide, we
        // can represent a whole sprite row as a single byte, so offsetting
        // into the pattern memory is easy. In total there is 8KB so we need a
        // 13 bit address.

        // "(control.pattern_background << 12)"  : the pattern memory selector
        //                                         from control register, either
        //                                         0K or 4K offset
        // "((uint16_t)bg_next_tile_id << 4)"    : the tile id multiplied by 16,
        // as
        //                                         2 lots of 8 rows of 8 bit
        //                                         pixels
        // "(vram_addr.fine_y)"                  : Offset into which row based
        // on
        //                                         vertical scroll offset
        // "+ 0"                                 : Mental clarity for plane
        // offset Note: No PPU address bus offset required as it starts at
        // 0x0000
        bg_next_tile_lsb =
            bus_read(pbus, (byte_t)((control.pattern_background << 12) +
                                    ((uint16_t)bg_next_tile_id << 4) +
                                    (vram_addr.fine_y) + 0));
        break;
      case 6:
        // Fetch the next background tile MSB bit plane from the pattern memory
        // This is the same as above, but has a +8 offset to select the next bit
        // plane
        bg_next_tile_msb =
            bus_read(pbus, (byte_t)((control.pattern_background << 12) +
                                    ((uint16_t)bg_next_tile_id << 4) +
                                    (vram_addr.fine_y) + 8));
        break;
      case 7:
        // Increment the background tile "pointer" to the next tile horizontally
        // in the nametable memory. Note this may cross nametable boundaries
        // which is a little complex, but essential to implement scrolling
        clock_increment_scroll_x();
        break;
      }
    }

    // End of a visible scanline, so increment downwards...
    if (cycle == 256) {
      clock_increment_scroll_y();
    }

    //...and reset the x position
    if (cycle == 257) {
      clock_load_background_shifters();
      clock_transfer_address_x();
    }

    // Superfluous reads of tile id at end of scanline
    if (cycle == 338 || cycle == 340) {
      bg_next_tile_id = bus_read(pbus, 0x2000 | (vram_addr.reg & 0x0FFF));
    }

    if (scanline == -1 && cycle >= 280 && cycle < 305) {
      // End of vertical blank period so reset the Y address ready for rendering
      clock_transfer_address_y();
    }
  }

  if (scanline == 240) {
    // Post Render Scanline - Do Nothing!
  }

  if (scanline >= 241 && scanline < 261) {
    if (scanline == 241 && cycle == 1) {
      // Effectively end of frame, so set vertical blank flag
      status.vertical_blank = 1;

      // If the control register tells us to emit a NMI when
      // entering vertical blanking period, do it! The CPU
      // will be informed that rendering is complete so it can
      // perform operations with the PPU knowing it wont
      // produce visible artefacts
      if (control.enable_nmi)
        nmi = true;
    }
  }

  // Composition - We now have background pixel information for this cycle
  // At this point we are only interested in background

  byte_t bg_pixel = 0x00;   // The 2-bit pixel to be rendered
  byte_t bg_palette = 0x00; // The 3-bit index of the palette the pixel indexes

  // We only render backgrounds if the PPU is enabled to do so. Note if
  // background rendering is disabled, the pixel and palette combine
  // to form 0x00. This will fall through the colour tables to yield
  // the current background colour in effect
  if (mask.render_background) {
    // Handle Pixel Selection by selecting the relevant bit
    // depending upon fine x scolling. This has the effect of
    // offsetting ALL background rendering by a set number
    // of pixels, permitting smooth scrolling
    uint16_t bit_mux = 0x8000 >> fine_x;

    // Select Plane pixels by extracting from the shifter
    // at the required location.
    byte_t p0_pixel = (bg_shifter_pattern_lo & bit_mux) > 0;
    byte_t p1_pixel = (bg_shifter_pattern_hi & bit_mux) > 0;

    // Combine to form pixel index
    bg_pixel = (byte_t)(p1_pixel << 1) | p0_pixel;

    // Get palette
    byte_t bg_pal0 = (bg_shifter_attrib_lo & bit_mux) > 0;
    byte_t bg_pal1 = (bg_shifter_attrib_hi & bit_mux) > 0;
    bg_palette = (byte_t)(bg_pal1 << 1) | bg_pal0;
  }

  // Now we have a final pixel colour, and a palette for this cycle
  // of the current scanline. Let's at long last, draw that ^&%*er :P

  // sprScreen->SetPixel(cycle - 1, scanline,
  //                     get_color_from_palette(bg_palette, bg_pixel));

  // Fake some noise for now
  // sprScreen.SetPixel(cycle - 1, scanline, palScreen[(rand() % 2) ? 0x3F :
  // 0x30]);

  // Advance renderer - it never stops, it's relentless
  cycle++;
  if (cycle >= 341) {
    cycle = 0;
    scanline++;
    if (scanline >= 261) {
      scanline = -1;
      frame_complete = true;
    }
  }
}