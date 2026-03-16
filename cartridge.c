#include "dnes.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct __attribute__((packed)) nes_hdr {
  byte_t magic[4];
  byte_t prg_rom_chunks; // n*16KiB
  byte_t chr_rom_chunks; // n*8KiB
  byte_t flag1;
  byte_t flag2;
  byte_t prg_ram_size;
  byte_t tv_system1;
  byte_t tv_system2;
  byte_t unused[5];
};

static byte_t prg_banks;
static byte_t *prg_memory;
static byte_t chr_banks;
static byte_t *chr_memory;
static struct mapper *mapper;

static enum MIRROR hw_mirror;

static inline bool nes_hdr_mirroring(struct nes_hdr *hdr) {
  return hdr->flag1 & (1);
}

static inline bool nes_hdr_battery_sram(struct nes_hdr *hdr) {
  return hdr->flag1 & (1 << 1);
}

static inline bool nes_hdr_trainer(struct nes_hdr *hdr) {
  return hdr->flag1 & (1 << 2);
}

static inline bool nes_hdr_4_screen(struct nes_hdr *hdr) {
  return hdr->flag1 & (1 << 3);
}

static inline byte_t nes_hdr_mapper_id(struct nes_hdr *hdr) {
  return (hdr->flag2 & 0b11110000) | (hdr->flag1 >> 4);
}

static inline bool nes_hdr_vs_unisystem(struct nes_hdr *hdr) {
  return (hdr->flag2 & (1));
}

static inline bool nes_hdr_play_choice_10(struct nes_hdr *hdr) {
  return (hdr->flag2 & (1 << 1));
}

static inline byte_t nes_hdr_nes_version(struct nes_hdr *hdr) {
  return ((hdr->flag2 >> 2) & (0b11));
}

static inline byte_t nes_2_hdr_prg_banks(struct nes_hdr *hdr) {
  return (byte_t)(((hdr->prg_ram_size & 0x07) << 8) | hdr->prg_rom_chunks);
}

static inline byte_t nes_2_hdr_chr_banks(struct nes_hdr *hdr) {
  return (byte_t)(((hdr->prg_ram_size & 0x38) << 8) | hdr->chr_rom_chunks);
}

static void cart_mbus_write(addr_t addr, byte_t data) {
  size_t mapped_addr = 0;
  if (!mapper->map_mbus_write(addr, &mapped_addr, data)) {
    // not a valid address for this mapper
    return;
  }
  if (mapped_addr == SIZE_MAX) {
    // mapper writes the data directly, no need to write to memory
    return;
  }
  prg_memory[mapped_addr] = data;
}

static byte_t cart_mbus_read(addr_t addr, bool read_only) {
  (void)read_only;
  byte_t data = 0;
  size_t mapped_addr = 0;
  if (!mapper->map_mbus_read(addr, &mapped_addr, &data)) {
    // not a valid address for this mapper
    return 0;
  }
  if (mapped_addr == SIZE_MAX) {
    // mapper reads the data directly, no need to read from memory, just return
    // it
    return data;
  }
  return prg_memory[mapped_addr];
}

static void cart_pbus_write(addr_t addr, byte_t data) {
  size_t mapped_addr = 0;
  if (!mapper->map_pbus_write(addr, &mapped_addr)) {
    return;
  }
  chr_memory[mapped_addr] = data;
}

static byte_t cart_pbus_read(addr_t addr, bool read_only) {
  (void)read_only;
  size_t mapped_addr = 0;
  if (!mapper->map_pbus_read(addr, &mapped_addr)) {
    return 0;
  }
  return chr_memory[mapped_addr];
}

void cart_register_mbus(struct bus *mbus) {
  assert(prg_memory);
  assert(chr_memory);
  bus_register(mbus, 0x4020, 0xFFFF,
               (&(struct bus_regparam){.read = cart_mbus_read,
                                       .write = cart_mbus_write}));
}

void cart_register_pbus(struct bus *pbus) {
  assert(prg_memory);
  assert(chr_memory);
  bus_register(pbus, 0x0000, 0x1FFF,
               (&(struct bus_regparam){.read = cart_pbus_read,
                                       .write = cart_pbus_write}));
}

void cart_load(const char *rom_path) {
  size_t sz;
  FILE *rom = fopen(rom_path, "r");
  if (!rom) {
    errorfln("Failed to open rom file: %s", strerror(errno));
    exit(1);
  }
  struct nes_hdr hdr;
  sz = fread(&hdr, sizeof(hdr), 1, rom);
  if (sz != 1) {
    errorfln("Failed to load rom header: file is too small");
    exit(1);
  }
  if (memcmp(hdr.magic, "NES\x1A", 4)) {
    errorfln("Failed to load rom: invalid hdr magic number");
    exit(1);
  }
  if (nes_hdr_trainer(&hdr)) {
    if (fseeko(rom, 512, SEEK_CUR)) {
      errorfln("Failed to seek position in trainer rom");
      exit(1);
    }
  }
  byte_t mapper_id = nes_hdr_mapper_id(&hdr);
  hw_mirror = nes_hdr_mirroring(&hdr) ? M_VERTICAL : M_HORIZONTAL;

  byte_t rom_type = nes_hdr_nes_version(&hdr);
  switch (rom_type) {
  case 0:
  case 1: {
    prg_banks = hdr.prg_rom_chunks;
    prg_memory = malloc(prg_banks * 1024 * 16);
    if (fread(prg_memory, sizeof(byte_t), prg_banks * 1024 * 16, rom) !=
        prg_banks * 1024 * 16) {
      errorfln("Failed to read PRG ROM data");
      exit(1);
    }

    chr_banks = hdr.chr_rom_chunks;
    if (chr_banks == 0) {
      chr_memory = malloc(1024 * 8);
    } else {
      chr_memory = malloc(chr_banks * 1024 * 8);
      if (fread(chr_memory, sizeof(byte_t), chr_banks * 1024 * 8, rom) !=
          chr_banks * 1024 * 8) {
        errorfln("Failed to read CHR ROM data");
        exit(1);
      }
    }
    break;
  }
  case 2: {
    prg_banks = nes_2_hdr_prg_banks(&hdr);
    prg_memory = malloc(prg_banks * 1024 * 16);
    if (fread(prg_memory, sizeof(byte_t), prg_banks * 1024 * 16, rom) !=
        prg_banks * 1024 * 16) {
      errorfln("Failed to read PRG ROM data");
      exit(1);
    }
    chr_banks = nes_2_hdr_chr_banks(&hdr);
    chr_memory = malloc(chr_banks * 1024 * 8);
    if (fread(chr_memory, sizeof(byte_t), chr_banks * 1024 * 8, rom) !=
        chr_banks * 1024 * 8) {
      errorfln("Failed to read CHR ROM data");
      exit(1);
    }
    break;
  }
  default:
    errorfln("Unsupported NES version: %hhd", rom_type);
    exit(1);
  }

  switch (mapper_id) {
  case 0:
    mapper = mapper_000(prg_banks, chr_banks);
    break;
  case 1:
    mapper = mapper_001(prg_banks, chr_banks);
    break;
  case 2:
    mapper = mapper_002(prg_banks, chr_banks);
    break;
  case 3:
    mapper = mapper_003(prg_banks, chr_banks);
    break;
  default:
    errorfln("Unsupported mapper: %hhd", mapper_id);
    exit(1);
  }
  printf("ROM loaded: mapper %hhd, prg banks %hhd, chr banks %hhd, rom type "
         "%hhd\n",
         mapper_id, prg_banks, chr_banks, rom_type);
  fclose(rom);
}

void cart_pop() {
  mapper->reset();
  mapper = NULL;
  free(prg_memory);
  free(chr_memory);
}

void cart_reset() {
  if (mapper)
    mapper->reset();
}

struct mapper *cart_get_mapper() { return mapper; }

enum MIRROR cart_get_mirror_mode() {
  enum MIRROR m = mapper->mirror();
  if (m == M_HARDWARE) {
    // Mirror configuration was defined
    // in hardware via soldering
    return hw_mirror;
  } else {
    // Mirror configuration can be
    // dynamically set via mapper
    return m;
  }
}