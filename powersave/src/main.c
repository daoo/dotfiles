#include "native.h"

#include <stdio.h>
#include <string.h>

const char* SAVE_POWER_MSG = "Entering power saving mode.\n";
const char* FULL_POWER_MSG = "Entering full power mode.\n";

const char* HELP_MSG =
  "Usage: powersave ACTION\n\n"
  "Available actions are:\n"
  "   auto   save power depending on battery status\n"
  "   full   force full power\n"
  "   print  print the state of all power saving settings\n"
  "   save   force power saving\n";

void print_help()
{
  fputs(HELP_MSG, stdout);
}

int main(int argc, char const* argv[])
{
  if (argc == 2) {
    const char* cmd = argv[1];

    if (strcmp(cmd, "save") == 0) {
      fputs(SAVE_POWER_MSG, stdout);
      save_power();
    } else if (strcmp(cmd, "full") == 0) {
      fputs(FULL_POWER_MSG, stdout);
      full_power();
    } else if (strcmp(cmd, "auto") == 0) {
      if (on_battery()) {
        fputs(SAVE_POWER_MSG, stdout);
        save_power();
      } else {
        fputs(FULL_POWER_MSG, stdout);
        full_power();
      }
    } else if (strcmp(cmd, "print") == 0) {
      print_state();
    } else {
      fprintf(stderr, "Unknown action %s\n", cmd);
      print_help();
      return 1;
    }
  } else {
    print_help();
    return 1;
  }

  return 0;
}
