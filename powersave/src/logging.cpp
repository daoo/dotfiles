#include "logging.hpp"
#include <iostream>

using namespace std;

namespace logging {
  void error(const string& str)
  {
#ifndef NLOGGING
    cerr << "[error] " << str << "\n";
#endif
  }
}
