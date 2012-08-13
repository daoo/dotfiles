#include "functions.h"

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

write_error write_string(const char* file, const char* value, size_t length) {
  assert(file && "file string is null");
  assert(value && "value string is null");
  assert(length > 0 && "length must be greater than zero");

  FILE* fptr;
  fptr = fopen(file, "w");
  if (!fptr)
    return FAIL_OPEN_WRITE;

  size_t written = fwrite(value, sizeof(char), length, fptr);
  fclose(fptr);

  return written == length ? WRITE_SUCCESS : FAIL_WRITE;
}

read_error read_char(const char* file, int* ptr) {
  assert(file && "file string is null");
  assert(ptr && "result ptr is null");

  FILE* fptr = fopen(file, "r");
  if (!fptr)
    return FAIL_OPEN_READ;

  int chr = fgetc(fptr);
  fclose(fptr);

  if (chr == EOF)
    return FILE_EMPTY;

  *ptr = chr;
  return READ_SUCCESS;
}

bool path_exists(const char* file) {
  assert(file && "file string is null");
  return access(file, F_OK) == 0 ? true : false;
}
