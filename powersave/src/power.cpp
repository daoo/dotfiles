#include "power.hpp"

#include "files.hpp"
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

using namespace files;
using namespace std;

namespace power {
  void opt(const string& file, const string& value) {
    try {
      write<string>(file, value);
    } catch (io_exception& ex) {
      cerr << "[error] io error while writing to file " << file << "\n";
    }
  }

  void check(const string& file) {
    try {
      string str = read<string>(file);
      cout << file << ": " << str << "\n";
    } catch (io_exception& ex) {
      cerr << "[error] io error while reading from file " << file << "\n";
    }
  }

  void run(const string& cmd) {
    int ecode = system(cmd.c_str());
    if (ecode != 0) {
      cerr << "[error] failed running '" << cmd << "', exit code " << ecode << "\n";
    }
  }

  int run2(const string& cmd, const string& param) {
    pid_t pid = fork();
    if (pid < 0) {
      cerr << "[error] fork failed\n";
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

  void load(const string& module) {
    int e = run2("modprobe", module);
    if (e != 0) {
      cerr << "[error] loading module " << module << " failed, modprobe exited with " << e << "\n";
    }
  }

  void unload(const string& module) {
    int e = run2("rmmod", module);
    if (e != 0) {
      cerr << "[error] loading module " << module << " failed, modprobe exited with " << e << "\n";
    }
  }

  void is_loaded(const string&) {
    cerr << "[error] is_loaded is not implemented\n";
  }
}
