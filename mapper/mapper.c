#include "dnes.h"

byte_t mapper_prg_banks, mapper_chr_banks;

void mapper_default_build(byte_t prg_banks, byte_t chr_banks,
                          struct mapper *mapper) {
  mapper_prg_banks = prg_banks;
  mapper_chr_banks = chr_banks;
  mapper->reset();
}