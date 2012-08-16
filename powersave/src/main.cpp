#include "files.hpp"
#include "functions.hpp"
#include "settings.hpp"
#include "state.hpp"

#include <iostream>
#include <string>

using namespace files;
using namespace settings;
using namespace state;
using namespace std;

const save_state DEFAULT_SAVE_STATE       = FULL;
const lid_state DEFAULT_LID_STATE         = OPEN;
const adapter_state DEFAULT_ADAPTER_STATE = AC;

const string RUN_DIR            = "/run/powersave";
const string ADAPTER_STATE_FILE = "/run/powersave/adapter";
const string LID_STATE_FILE     = "/run/powersave/lid";
const string SAVE_STATE_FILE    = "/run/powersave/save";

int main(int argc, char const* argv[]) {
  save_state old = get_state<save_state>(SAVE_STATE_FILE, FULL);
  save_state state_new = old;

  lid_state lid = get_state<lid_state>(LID_STATE_FILE, OPEN);
  adapter_state adapt = get_state<adapter_state>(ADAPTER_STATE_FILE, AC);

  if (argc > 1) {
    string argv1(argv[1]);

    if (argv1 == "--ac") {
      state_new = calculate_new_state(AC, lid);
    } else if (argv1 == "--battery") {
      state_new = calculate_new_state(BATTERY, lid);
    } else if (argv1 == "--lid-open") {
      state_new = calculate_new_state(adapt, OPEN);
    } else if (argv1 == "--lid-close") {
      state_new = calculate_new_state(adapt, CLOSED);
    } else if (argv1 == "--power-save") {
      state_new = SAVE;
    } else if (argv1 == "--power-full") {
      state_new = FULL;
    } else {
      cerr << "Unknown parameter " << argv1 << "\n";
    }

    if (state_new != old) {
      switch (state_new) {
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
