#include "functions.hpp"

#include <algorithm>

bool char_is_int(char c) {
  return c == '0' ||
         c == '1' ||
         c == '2' ||
         c == '3' ||
         c == '4' ||
         c == '5' ||
         c == '6' ||
         c == '7' ||
         c == '8' ||
         c == '9';
}

bool str_is_int(const std::string& str) {
  return str.length() > 0 && all_of(str.begin(), str.end(), char_is_int);
}
