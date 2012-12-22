#ifndef FILES_HPP_8GUXMS0N
#define FILES_HPP_8GUXMS0N

#include "io_exception.hpp"

#include <cstring>
#include <dirent.h>
#include <fstream>
#include <string>
#include <unistd.h>

namespace files {
  inline std::string read(const std::string& file) {
    std::ifstream f(file);
    if (f.is_open()) {
      std::string line;
      getline(f, line);
      return line;
    } else {
      throw files::io_exception(file);
    }
  }

  inline void write(const std::string& file, const char* str) {
    std::ofstream f(file);
    if (f.is_open()) {
      f << str;
    } else {
      throw files::io_exception(file);
    }
  }

  inline bool path_exists(const char* file) {
    return access(file, F_OK) == 0;
  }
}

#endif /* end of include guard: FILES_HPP_8GUXMS0N */
