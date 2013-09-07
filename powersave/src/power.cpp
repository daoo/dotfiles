#include "power.hpp"

#include "files.hpp"
#include "functions.hpp"
#include "io_exception.hpp"
#include "logging.hpp"
#include <cstdlib>
#include <iostream>
#include <sys/wait.h>
#include <unistd.h>

using namespace files;
using namespace std;

namespace power
{
  void opt(const string& file, const char* value)
  {
    try {
      write(file, value);
    } catch (io_exception& ex) {
      logging::error("io error while writing to file " + file);
    }
  }

  void check(const string& file)
  {
    try {
      string str = read(file);
      cout << file << ": " << str << "\n";
    } catch (io_exception& ex) {
      logging::error("io error while reading from file " + file);
    }
  }

  void run(const string& cmd)
  {
    int ecode = system(cmd.c_str());
    if (ecode != 0) {
      logging::error("failed running '" + cmd + "', exit code " + to_string(ecode));
    }
  }

  int run2(const string& cmd, const string& param)
  {
    pid_t pid = fork();
    if (pid < 0) {
      logging::error("fork failed");
      return -1;
    } else if (pid == 0) {
      // child
      exit(execlp(cmd.c_str(), cmd.c_str(), param.c_str(), 0));
    } else {
      int e;
      wait(&e);
      return e;
    }
  }

  void load(const string& module)
  {
    int e = run2("modprobe", module);
    if (e != 0) {
      logging::error("loading module " + module + " failed, modprobe exited with " + to_string(e));
    }
  }

  void unload(const string& module)
  {
    int e = run2("rmmod", module);
    if (e != 0) {
      logging::error("loading module " + module + " failed, modprobe exited with " + to_string(e));
    }
  }

  void is_loaded(const string&)
  {
    logging::error("is_loaded is not implemented");
  }
}
