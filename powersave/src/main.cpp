#include "native.hpp"

#include <iostream>
#include <string>

using namespace native;
using namespace std;

void print_help()
{
  cout << "Usage: powersave ACTION\n\n"
          "Available actions are:\n"
          "   auto   save power depending on battery status\n"
          "   full   force full power\n"
          "   print  print the state of all power saving settings\n"
          "   save   force power saving\n";
}

int main(int argc, char const* argv[])
{
  if (argc == 2) {
    const string argv1(argv[1]);

    if (argv1 == "save") {
      save_power();
    } else if (argv1 == "full") {
      full_power();
    } else if (argv1 == "auto") {
      if (on_battery())
        save_power();
      else
        full_power();
    } else if (argv1 == "print") {
      print_state();
    } else {
      cerr << "Unknown action " << argv1 << "\n";
      print_help();
      return 1;
    }
  } else {
    print_help();
    return 1;
  }

  return 0;
}
