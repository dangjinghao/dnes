#ifndef _MAPPER_H
#define _MAPPER_H

#include <dnes.h>

struct mapper {
  bool (*map_mbus_read)(addr_t addr, size_t *mapped_addr, byte_t *data);
  bool (*map_mbus_write)(addr_t addr, size_t *mapped_addr, byte_t data);
  bool (*map_pbus_read)(addr_t addr, size_t *mapped_addr);
  bool (*map_pbus_write)(addr_t addr, size_t *mapped_addr);
  void (*reset)();
  enum MIRROR (*mirror)();

  bool (*irq_state)();
  void (*irq_clear)();
  void (*scanline)();

  size_t (*opt_dump_ram)(byte_t **ram_ref);
  void (*opt_load_ram)(byte_t *ram, size_t ram_size);
  void (*mapper_pop)();
};

void mapper_default_mapper_pop();
bool mapper_defualt_irq_state();
void mapper_defualt_irq_clear();
void mapper_defualt_scanline();
void mapper_default_cons(byte_t prg_banks, byte_t chr_banks,
                         struct mapper *mapper);

struct mapper *mapper_000(byte_t prg_banks, byte_t chr_banks);
struct mapper *mapper_001(byte_t prg_banks, byte_t chr_banks);
struct mapper *mapper_002(byte_t prg_banks, byte_t chr_banks);
struct mapper *mapper_003(byte_t prg_banks, byte_t chr_banks);
struct mapper *mapper_004(byte_t prg_banks, byte_t chr_banks);

extern byte_t mapper_prg_banks, mapper_chr_banks;

#endif