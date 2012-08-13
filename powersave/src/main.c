#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "settings.h"

typedef enum power_state_enum {
  BATTERY, AC
} power_state;

typedef enum lid_state_enum {
  OPEN_CLOSED
} lid_state;

const char POWER_STATE_FILE[] = "/run/powersave/power";
const char LID_STATE_FILE[]   = "/run/powersave/lid";

bool get_power_state(power_state* state) {
  FILE* fptr = fopen(POWER_STATE_FILE, "r");
  if (fptr) {
    int c = fgetc(fptr);
    fclose(fptr);
    *state = (power_state) c;
    return true;
  }

  return false;
}

void set_power_state(power_state state) {
  FILE* fptr = fopen(POWER_STATE_FILE, "w");
  if (fptr) {
    fputc(state, fptr);
    fclose(fptr);
  }
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
