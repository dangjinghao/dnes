#include "dnes.h"

byte_t mapper_prg_banks, mapper_chr_banks;

void mapper_default_cons(byte_t prg_banks, byte_t chr_banks,
                         struct mapper *mapper) {
  mapper_prg_banks = prg_banks;
  mapper_chr_banks = chr_banks;
  mapper->reset();
}

bool mapper_defualt_irq_state() { return false; }
void mapper_defualt_irq_clear() {}
void mapper_defualt_scanline() {}