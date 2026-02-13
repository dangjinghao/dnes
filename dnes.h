#ifndef _DNES_H
#define _DNES_H
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
// The width of address is 16b
typedef uint16_t addr_t;
typedef uint8_t byte_t;

/// bus.c
//

void bus_write(addr_t addr, byte_t data);
byte_t bus_read(addr_t addr);
byte_t bus_read_only(addr_t addr);

/// cpu_6502.c
//

void cpu_clock();
void cpu_reset();
void cpu_irq();
void cpu_nmi();
void cpu_disasm_log(addr_t st, addr_t ed);
bool cpu_inst_done();

/// ram.c
//
#define RAM_SIZE (64 * 1024) // 64KiB

extern byte_t ram[RAM_SIZE];

/// utils.c
//

// format: `<file_name>:<line_number> message`
void serrorf(char *file_name, size_t line, char *fmt, ...)
    __attribute__((format(printf, 3, 4)));
#define errorf(fmt, ...) serrorf(__FILE_NAME__, __LINE__, fmt, __VA_ARGS__)

#endif