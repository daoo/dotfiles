#include "files.hpp"

#include "io_exception.hpp"
#include <cstring>
#include <dirent.h>
#include <fstream>
#include <unistd.h>

using namespace std;

namespace files
{
  string read(const string& file)
  {
    ifstream f(file);
    if (f.is_open()) {
      string line;
      getline(f, line);
      return line;
    } else {
      throw io_exception(file);
    }
  }

  void write(const string& file, const char* str)
  {
    ofstream f(file);
    if (f.is_open()) {
      f << str;
    } else {
      throw io_exception(file);
    }
  }

  bool path_exists(const char* file)
  {
    return access(file, F_OK) == 0;
  }
}
