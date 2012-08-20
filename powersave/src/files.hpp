#ifndef FILES_HPP_8GUXMS0N
#define FILES_HPP_8GUXMS0N

#include "io_exception.hpp"

#include <fstream>
#include <string>
#include <vector>

namespace files {
  template <typename T>
  T read(const std::string& file) {
    std::ifstream f(file);
    if (f.is_open()) {
      T a;
      f >> a;
      return a;
    } else {
      throw files::io_exception(file);
    }
  }

  template <typename T>
  void write(const std::string& file, T chr) {
    std::ofstream f(file);
    if (f.is_open()) {
      f << chr;
    } else {
      throw files::io_exception(file);
    }
  }

  std::vector<std::string> list_dir(const std::string& path);

  std::string path_join(const std::string& a, const std::string& b);
  std::string path_join(const std::string& a, const std::string& b, const std::string& c);

  bool path_exists(const std::string& file);
}

#endif /* end of include guard: FILES_HPP_8GUXMS0N */
