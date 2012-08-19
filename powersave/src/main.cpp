#include "native.hpp"

#include <iostream>
#include <string>

using namespace native;
using namespace std;

int main(int argc, char const* argv[]) {
  if (argc > 1) {
    string argv1(argv[1]);

    if (argv1 == "--power-save") {
      settings_full();
    } else if (argv1 == "--power-full") {
      settings_save();
    } else {
      cerr << "Unknown parameter " << argv1 << "\n";
    }
  } else {
    settings_check();
  }
  return 0;
}
