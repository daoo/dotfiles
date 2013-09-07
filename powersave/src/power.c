#include "power.h"

#include "files.h"
#include <stdio.h>

void power_opt(const char* file, const char* value)
{
  file_write(file, value);
}

void power_check(const char* file)
{
  printf("%s: ", file);
  file_print(file);
  putchar('\n');
}
