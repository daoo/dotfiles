#ifndef FILES_HPP_8GUXMS0N
#define FILES_HPP_8GUXMS0N

#include <iostream>
#include <string>

template <typename T>
T read(const std::string& file) {
  std::ifstream f(file);
  T a;
  f >> a;
  return a;
}

template <typename T>
void write(const std::string& file, T chr) {
  std::ofstream f(file);
  f << chr;
}

bool path_exists(const std::string& file);

#endif /* end of include guard: FILES_HPP_8GUXMS0N */
