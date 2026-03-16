#include "mapper.h"
#include <dnes.h>
#include <stdlib.h>

static byte_t nTargetRegister = 0x00;
static bool bPRGBankMode = false;
static bool bCHRInversion = false;
static enum MIRROR mirrormode = M_HORIZONTAL;
static uint32_t pRegister[8];
static uint32_t pCHRBank[8];
static uint32_t pPRGBank[4];
static bool bIRQActive = false;
static bool bIRQEnable = false;
static bool bIRQUpdate = false;
static uint16_t nIRQCounter = 0x0000;
static uint16_t nIRQReload = 0x0000;
static byte_t *vRAMStatic;

// static struct mapper mapper = {
//     .map_mbus_read = map_mbus_read,
//     .map_mbus_write = map_mbus_write,
//     .map_pbus_read = map_pbus_read,
//     .map_pbus_write = map_pbus_write,
//     .reset = reset,
//     .mirror = mirror,
//     .irq_state = mapper_defualt_irq_state,
//     .irq_clear = mapper_defualt_irq_clear,
//     .scanline = mapper_defualt_scanline,
//     .opt_mapper_pop = mapper_pop,
//     .opt_dump_ram = dump_ram,
//     .opt_load_ram = load_ram,
// };

// struct mapper *mapper_004(byte_t prg_banks, byte_t chr_banks) {
//   mapper_default_cons(prg_banks, chr_banks, &mapper);
//   vRAMStatic = malloc(0x2000); // in olc's implementation, this is 32 * 1024
//   return &mapper;
// }