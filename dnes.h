#ifndef _DNES_H
#define _DNES_H
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <SDL3/SDL.h>
// The width of address is 16b
typedef uint16_t addr_t;
typedef uint8_t byte_t;

/// bus.c
//

struct bus_regparam {
  void (*write)(addr_t addr, byte_t data);
  byte_t (*read)(addr_t addr, bool read_only);
};
struct bus_device {
  addr_t start, end;
  struct bus_regparam device;
};
struct bus {
  struct bus_device devices[16];
  size_t dev_count;
};

void bus_write(struct bus *bus, addr_t addr, byte_t data);
byte_t bus_read(struct bus *bus, addr_t addr);
byte_t bus_read_only(struct bus *bus, addr_t addr);
void bus_register(struct bus *bus, addr_t start, addr_t end,
                  struct bus_regparam *p);
void bus_ready(struct bus *bus);

/// cpu_6502.c
//

void cpu_mount_mbus(struct bus *bus);
void cpu_clock();
void cpu_reset();
void cpu_irq();
void cpu_nmi();
bool cpu_inst_done();

enum CPU_P_FLAGS {
  FLAG_C = 1 << 0,
  FLAG_Z = 1 << 1,
  FLAG_I = 1 << 2,
  FLAG_D = 1 << 3,
  FLAG_B = 1 << 4,
  FLAG_U = 1 << 5,
  FLAG_V = 1 << 6,
  FLAG_N = 1 << 7,
};

bool cpu_get_flag(enum CPU_P_FLAGS flag);
addr_t cpu_get_reg_PC();
byte_t cpu_get_reg_A();
byte_t cpu_get_reg_X();
byte_t cpu_get_reg_Y();
byte_t cpu_get_reg_STKP();
void cpu_disasm_code(addr_t addr, char *buf, size_t buf_size,
                     size_t *str_used_len, size_t *inst_byte_len);

/// ram.c
//

void ram_register(struct bus *bus);

/// ppu_2c02.c
//

void ppu_register_mbus(struct bus *mbus);
void ppu_mount_pbus(struct bus *pbus);
void ppu_ext_register(struct bus *pbus);
bool ppu_is_frame_complete();
void ppu_clock();
void ppu_reset();
bool ppu_nmi_is_enabled();
void ppu_nmi_disable();
void ppu_nmi_enable();
struct SDL_Color *ppu_get_color_from_palette(byte_t palette_idx, byte_t px);
extern struct SDL_Color ppu_screen_output[240][256];
/// cartridge.c
//

enum mirroring_mode {
  M_HORIZONTAL,
  M_VERTICAL,
  M_ONESCREEN_LO,
  M_ONESCREEN_HI
};

void cart_register_mbus(struct bus *mbus);
void cart_register_pbus(struct bus *pbus);
void cart_load(const char *rom_path);
void cart_pop();
void cart_reset();
extern byte_t mapper_prg_banks, mapper_chr_banks;

enum mirroring_mode cart_get_mirror_mode();

/// utils.c
//

// format: `<file_name>:<line_number> message`
void serrorf(char *file_name, size_t line, char *fmt, ...)
    __attribute__((format(printf, 3, 4)));
#define errorf(fmt, ...) serrorf(__FILE_NAME__, __LINE__, fmt, __VA_ARGS__)
#define errorfln(fmt, ...)                                                     \
  serrorf(__FILE_NAME__, __LINE__, fmt "\n", ##__VA_ARGS__)

#define TODO() assert(0 && "todo")

extern const uint32_t COLOR_RED;
extern const uint32_t COLOR_GREEN;
extern const uint32_t COLOR_BLUE;
extern const uint32_t COLOR_WHITE;
extern const uint32_t COLOR_BLACK;
extern const uint32_t COLOR_CYAN;

static inline byte_t color_extract_red(uint32_t color) {
  return (color >> 24) & 0xFF;
}
static inline byte_t color_extract_green(uint32_t color) {
  return (color >> 16) & 0xFF;
}
static inline byte_t color_extract_blue(uint32_t color) {
  return (color >> 8) & 0xFF;
}
static inline byte_t color_extract_alpha(uint32_t color) {
  return (color >> 0) & 0xFF;
}

// mapper_*.c

struct mapper {
  size_t (*map_mbus_read)(addr_t addr);
  size_t (*map_mbus_write)(addr_t addr);
  size_t (*map_pbus_read)(addr_t addr);
  size_t (*map_pbus_write)(addr_t addr);
  void (*reset)();
};

struct mapper *mapper_000(byte_t prg_banks, byte_t chr_banks);

/// dnes.c
//

void dnes_reset();
void dnes_clock();
void dnes_insert_cartridge(char *rom_path);

#endif