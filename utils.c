#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include "dnes.h"
void serrorf(char *file_name, size_t line, char *fmt, ...) {
  fprintf(stderr, "%s:%lu ", file_name, line);
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}

const uint32_t COLOR_RED = 0x00FF0000;
const uint32_t COLOR_GREEN = 0x0000FF00;
const uint32_t COLOR_BLUE = 0x000000FF;
const uint32_t COLOR_WHITE = 0x00FFFFFF;
const uint32_t COLOR_BLACK = 0xFF000000;
const uint32_t COLOR_CYAN = 0x0000FFFF;