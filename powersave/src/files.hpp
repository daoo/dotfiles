#ifndef FILES_HPP_8GUXMS0N
#define FILES_HPP_8GUXMS0N

#include <string>

namespace files {
  std::string read(const std::string& file);
  void write(const std::string& file, const char* str);

  bool path_exists(const char* file);
}

#endif /* end of include guard: FILES_HPP_8GUXMS0N */
