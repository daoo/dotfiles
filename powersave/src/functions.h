#ifndef FUNCTIONS_H_ZPC8J5AU
#define FUNCTIONS_H_ZPC8J5AU

#include <stddef.h>
#include <stdbool.h>

#define COUNT_OF(x) ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))
#define opt_(f, v) opt((f), (v), COUNT_OF((v)))

typedef enum read_error_enum {
  READ_SUCCESS, FAIL_OPEN_READ, FILE_EMPTY
} read_error;

typedef enum write_error_enum {
  WRITE_SUCCESS, FAIL_OPEN_WRITE, FAIL_WRITE
} write_error;

write_error write_string(const char* file, const char* value, size_t length);
read_error read_char(const char* file, int* ptr);
bool path_exists(const char* file);

#endif /* end of include guard: FUNCTIONS_H_ZPC8J5AU */
