#ifndef _DNES_H
#define _DNES_H
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
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