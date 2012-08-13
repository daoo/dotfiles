#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "functions.h"
#include "settings.h"

typedef enum adapter_state_enum {
  AC, BATTERY
} adapter_state;

typedef enum lid_state_enum {
  OPEN, CLOSED
} lid_state;

typedef enum save_state_enum {
  FULL, SAVE
} save_state;

const char ADAPTER_STATE_FILE[] = "/run/powersave/adapter";
const char LID_STATE_FILE[]     = "/run/powersave/lid";
const char SAVE_STATE_FILE[]    = "/run/powersave/save";

void get_state(const char* file, void* state) {
  read_char(file, (int*) state);
}

void set_state(const char* file, int state) {
  write_char(file, state);
}

int main(int argc, char const* argv[]) {
  if (argc > 1) {
    if (strcmp(argv[0], "--ac") == 0) {
    } else if (strcmp(argv[0], "--battery") == 0) {
    } else if (strcmp(argv[0], "--lid") == 0) {
      if (strcmp(argv[1], "open") == 0) {
      } else if (strcmp(argv[1], "close") == 0) {
      }
    }
  } else {
    settings_check();
  }
  return 0;
}
