#include "io_exception.hpp"

using namespace std;

namespace files {
  io_exception::io_exception(const string& file) : file(file) { }

  const string& io_exception::get_file() const
  {
    return file;
  }

  const char* io_exception::what() const throw()
  {
    return file.c_str();
  }
}
