#include "dnes.h"
#include <SDL3/SDL.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
void serrorf(char *file_name, size_t line, char *fmt, ...) {
  fprintf(stderr, "%s:%lu ", file_name, line);
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}

const uint32_t COLOR_RED = 0xFF0000FF;
const uint32_t COLOR_GREEN = 0x00FF00FF;
const uint32_t COLOR_BLUE = 0x0000FFFF;
const uint32_t COLOR_WHITE = 0xFFFFFFFF;
const uint32_t COLOR_BLACK = 0x000000FF;
const uint32_t COLOR_CYAN = 0x00FFFFFF;
