#include "state.hpp"

namespace state {
  save_state calculate_new_state(adapter_state adpt, lid_state lid) {
    return lid == OPEN && adpt == AC ? FULL : SAVE;
  }
}
