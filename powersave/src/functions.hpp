#ifndef FUNCTIONS_HPP_JLBIVHA3
#define FUNCTIONS_HPP_JLBIVHA3

#include <sstream>

template <typename T>
std::string to_string(T v) {
  std::stringstream ss;
  ss << v;
  return ss.str();
}

#endif /* end of include guard: FUNCTIONS_HPP_JLBIVHA3 */
