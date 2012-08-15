#include "power.hpp"

#include "files.hpp"
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

using namespace std;

void opt(const string& file, const string& value) {
  write<string>(file, value);
  //printf("[error] cannot open %s for writing\n", file);
  //printf("[error] cannot write %s to %s\n", value, file);
}

void check(const string& file) {
  cout << read<string>(file);
  //cout << "[error] cannot open " << file << " for reading\n";
}

void run(const string& cmd) {
  int ecode = system(cmd.c_str());
  if (ecode != 0) {
    cout << "[error] failed running '" << cmd << "', exit code " << ecode << "\n";
  }
}

int run2(const string& cmd, const string& param) {
  pid_t pid = fork();
  if (pid < 0) {
    cout << "[error] fork failed\n";
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
    cout << "[error] loading module " << module << " failed, modprobe exited with " << e << "\n";
  }
}

void unload(const string& module) {
  int e = run2("rmmod", module);
  if (e != 0) {
    cout << "[error] loading module " << module << " failed, modprobe exited with " << e << "\n";
  }
}

void is_loaded(const string&) {
}
