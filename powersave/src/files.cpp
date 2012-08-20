#include "files.hpp"
#include "io_exception.hpp"

#include <cstring>
#include <dirent.h>
#include <unistd.h>

using namespace std;

namespace files {
  static bool not_dot(const char* str) {
    return str[0] != '.' && str[1] != '.';
  }

  vector<string> list_dir(const string& path) {
    DIR* dir = opendir (path.c_str());
    if (dir != 0) {
      vector<string> paths;
      struct dirent* ent;
      while ((ent = readdir(dir)) != 0) {
        if (not_dot(ent->d_name)) {
          paths.push_back(string(ent->d_name));
        }
      }
      closedir(dir);

      return paths;
    } else {
      throw io_exception("Could not open directory " + path);
    }
  }

  string path_join(const string& a, const string& b) {
    if (a.back() != '/' && b.front() != '/') {
      return a + "/" + b;
    } else if (a.back() == '/' && b.front() == '/') {
      return a + b.substr(1);
    } else {
      return a + b;
    }
  }

  string path_join(const string& a, const string& b, const string& c) {
    return path_join(path_join(a, b), c);
  }

  bool path_exists(const string& file) {
    return access(file.c_str(), F_OK) == 0;
  }
}
