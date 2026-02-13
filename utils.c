#include <stdarg.h>
#include <stdio.h>

void serrorf(char *file_name, size_t line, char *fmt, ...) {
  fprintf(stderr, "%s:%lu ", file_name, line);
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}