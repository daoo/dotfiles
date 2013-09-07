#ifndef LOGGING_H_BEILFOH7
#define LOGGING_H_BEILFOH7

#ifdef LOGGING
#include <cstdio>
#define ERROR(s) fprintf(stderr, "[error] %s\n", (s))
#define ERROR_FILE(s, f) fprintf(stderr, "[error] %s: %f\n", (s), (f))
#define ERROR_PRINTF(...) fprintf(stderr, __VA_ARGS__)
#else
#define ERROR(s)
#define ERROR_FILE(s, f)
#define ERROR_PRINTF(...)
#endif

#endif /* end of include guard: LOGGING_H_BEILFOH7 */
