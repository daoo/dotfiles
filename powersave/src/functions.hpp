#ifndef FUNCTIONS_HPP_JLBIVHA3
#define FUNCTIONS_HPP_JLBIVHA3

#include <sstream>

bool char_is_int(char c);
bool str_is_int(const std::string& str);

template <typename T>
std::string to_string(T v) {
  std::stringstream ss;
  ss << v;
  return ss.str();
}

#endif /* end of include guard: FUNCTIONS_HPP_JLBIVHA3 */
