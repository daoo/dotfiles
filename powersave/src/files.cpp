#include "files.hpp"
#include "io_exception.hpp"

#include <cstring>
#include <dirent.h>
#include <unistd.h>

using namespace std;

namespace files {
  bool path_exists(const string& file) {
    return access(file.c_str(), F_OK) == 0;
  }
}
