#ifndef STATE_HPP_G3RVFVND
#define STATE_HPP_G3RVFVND

#include "files.hpp"
#include "io_exception.hpp"

#include <string>

typedef enum {
  AC, BATTERY
} adapter_state;

typedef enum {
  OPEN, CLOSED
} lid_state;

typedef enum {
  FULL, SAVE
} save_state;

template <typename T>
T get_state(const std::string& file, T def) {
  try {
    return static_cast<T>(files::read<char>(file));
  } catch (const files::io_exception& ex) {
    return def;
  }
}

template <typename T>
void set_state(const std::string& file, T state) {
  files::write<char>(file, static_cast<char>(state));
}

save_state calculate_new_state(adapter_state adpt, lid_state lid) {
  return lid == OPEN && adpt == AC ? FULL : SAVE;
}

#endif /* end of include guard: STATE_HPP_G3RVFVND */
