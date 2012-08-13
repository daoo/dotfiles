#include "power.h"
#include <stdio.h>
#include <assert.h>

void opt(const char* file, const char* value, size_t length) {
  assert(file && "file ptr is null");
  assert(value && "value ptr is null");
  assert(length > 0 && "length must be greater than zero");

  switch (write_string(file, value, length)) {
    case FAIL_OPEN_WRITE:
      printf("[error] cannot open %s for writing\n", file);
      break;
    case FAIL_WRITE:
      printf("[error] cannot write %s to %s\n", value, file);
      break;
    case WRITE_SUCCESS:
      break;
  }
}

void check(const char* file) {
  assert(file && "file ptr is null");

  FILE* fptr;
  fptr = fopen(file, "r");
  if (!fptr) {
    printf("[error] cannot open %s for reading\n", file);
    return;
  }

  printf("%s: ", file);
  int c;
  while ((c = fgetc(fptr)) != EOF) {
    putchar(c);
  }
  fclose(fptr);
}
