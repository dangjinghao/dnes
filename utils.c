#include "dnes.h"
#include <stdarg.h>
#include <stdio.h>
void serrorf(char *file_name, size_t line, char *fmt, ...) {
  fprintf(stderr, "%s:%lu ", file_name, line);
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}

struct ppu_color *COLOR_RED = &(struct ppu_color){0xFF, 0x00, 0x00, 0xFF};
struct ppu_color *COLOR_GREEN = &(struct ppu_color){0x00, 0xFF, 0x00, 0xFF};
struct ppu_color *COLOR_BLUE = &(struct ppu_color){0x00, 0x00, 0xFF, 0xFF};
struct ppu_color *COLOR_WHITE = &(struct ppu_color){0xFF, 0xFF, 0xFF, 0xFF};
struct ppu_color *COLOR_BLACK = &(struct ppu_color){0x00, 0x00, 0x00, 0xFF};
struct ppu_color *COLOR_CYAN = &(struct ppu_color){0x00, 0xFF, 0xFF, 0xFF};