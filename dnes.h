#ifndef _DNES_H
#define _DNES_H
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// The width of address is 16b
typedef uint16_t addr_t;
// 1 byte is 8b
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
  const char *reg_func_name;
};
struct bus {
  struct bus_device devices[16];
  size_t dev_count;
  addr_t mask; // mask of the bus address, e.g. 0x3FFF for 14-bit address
  const char *name;
};

void bus_write(struct bus *bus, addr_t addr, byte_t data);
byte_t bus_read(struct bus *bus, addr_t addr);
byte_t bus_read_only(struct bus *bus, addr_t addr);
void bus_ready(struct bus *bus);
void bus_register_2(struct bus *bus, addr_t start, addr_t end,
                    struct bus_regparam *p, const char *reg_func_name);
#define bus_register(bus, start, end, p)                                       \
  bus_register_2((bus), (start), (end), (p), __FUNCTION__)
void bus_init(struct bus *bus, const char *name, addr_t mask);

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

extern bool ppu_nmi;
extern bool ppu_frame_complete;
extern byte_t *ppu_oam_start;

void ppu_clock();
void ppu_reset();
void ppu_gen_pattern_table(byte_t i, byte_t palette);

struct ppu_color {
  byte_t r, g, b, a;
} __attribute__((packed));

struct ppu_color *ppu_get_color_from_palette(byte_t palette_idx, byte_t px);
extern struct ppu_color ppu_screen_output[240][256];
extern struct ppu_color ppu_pattern_table[2][128][128];

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

static inline bool is_byte_neg(byte_t B) { return (B & 0x80) != 0; }

// format: `<file_name>:<line_number> message`
void serrorf(char *file_name, size_t line, char *fmt, ...)
    __attribute__((format(printf, 3, 4)));
#define errorf(fmt, ...) serrorf(__FILE_NAME__, __LINE__, fmt, __VA_ARGS__)
#define errorfln(fmt, ...)                                                     \
  serrorf(__FILE_NAME__, __LINE__, fmt "\n", ##__VA_ARGS__)

void todo_exit(const char *file_name, size_t line);
#define TODO() todo_exit(__FILE_NAME__, __LINE__)

#define ctx_mgr(st, ed)                                                        \
  for (bool _run = ({                                                          \
         st;                                                                   \
         true;                                                                 \
       });                                                                     \
       _run; _run = ({                                                         \
               ed;                                                             \
               false;                                                          \
             }))

// color bytes
extern struct ppu_color *COLOR_RED;
extern struct ppu_color *COLOR_GREEN;
extern struct ppu_color *COLOR_BLUE;
extern struct ppu_color *COLOR_WHITE;
extern struct ppu_color *COLOR_BLACK;
extern struct ppu_color *COLOR_CYAN;

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

/// controller.c
//
enum ctrl_button {
  CTRL_RIGHT = 0,
  CTRL_LEFT = 1,
  CTRL_DOWN = 2,
  CTRL_UP = 3,
  CTRL_START = 4,
  CTRL_SELECT = 5,
  CTRL_B = 6,
  CTRL_A = 7,
};

void ctrl_register(struct bus *bus);
void ctrl_set_input(byte_t player, enum ctrl_button b, bool pressed);
void ctrl_reset();
void ctrl_clear_input(byte_t player);

// apu.c

void apu_register(struct bus *bus);

// dma.c

void dma_register(struct bus *bus);
void dma_mount_mbus(struct bus *bus);
void dma_do_transfer(size_t system_clock);
extern bool dma_transfer;

#endif