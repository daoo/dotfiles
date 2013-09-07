#include "files.h"

#include "logging.h"
#include <stdio.h>

void file_print(const char* file)
{
  FILE* f = fopen(file, "r");
  if (f) {
    char c = getc(f);
    while (c != EOF && c != '\n') {
      putchar(c);
      c = getc(f);
    }
    fclose(f);
  } else {
    ERROR_FILE("io error while reading file", file);
  }
}

void file_write(const char* file, const char* str)
{
  FILE* f = fopen(file, "w");
  if (f) {
    fputs(str, stdout);
    fclose(f);
  } else {
    ERROR_FILE("io error while writing file", file);
  }
}
