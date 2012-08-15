#include "files.hpp"

#include <unistd.h>

using namespace std;

bool path_exists(const string& file) {
  return access(file.c_str(), F_OK) == 0 ? true : false;
}