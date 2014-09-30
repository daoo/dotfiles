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

char file_equal(const char* file, const char* str)
{
  FILE* f = fopen(file, "r");
  if (f) {
    int i = 0;
    char c = getc(f);
    while (c != EOF && str[i] != 0) {
      if (c != str[i])
        return 0;

      c = getc(f);
      ++i;
    }

    return 1;
  } else {
    ERROR_FILE("io error while reading file", file);
    return 0;
  }
}

void file_write(const char* file, const char* str)
{
  FILE* f = fopen(file, "w");
  if (f) {
    fputs(str, f);
    fclose(f);
  } else {
    ERROR_FILE("io error while writing file", file);
  }
}
