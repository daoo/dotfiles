#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "functions.h"
#include "settings.h"

bool eq(const char* a, const char* b) {
  return strcmp(a, b) == 0;
}

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

save_state calculate_new_state(save_state old, adapter_state adpt, lid_state lid) {
  return lid == OPEN && adpt == AC ? FULL : SAVE;
}

int main(int argc, char const* argv[]) {
  save_state old = FULL;
  save_state new = FULL;

  lid_state lid;
  adapter_state adapt;

  if (argc > 1) {
    if (eq(argv[1], "--ac")) {
      new = calculate_new_state(old, AC, lid);
    } else if (eq(argv[1], "--battery")) {
      new = calculate_new_state(old, BATTERY, lid);
    } else if (eq(argv[1], "--lid-open")) {
      new = calculate_new_state(old, adapt, OPEN);
    } else if (eq(argv[1], "--lid-close")) {
      new = calculate_new_state(old, adapt, CLOSED);
    } else if (eq(argv[1], "--power-save")) {
      new = SAVE;
    } else if (eq(argv[1], "--power-full")) {
      new = FULL;
    }

    if (new != old) {
      switch (new) {
        case FULL:
          settings_full();
          break;
        case SAVE:
          settings_save();
          break;
      }
    }
  } else {
    settings_check();
  }
  return 0;
}
