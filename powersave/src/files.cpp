#include "files.hpp"
#include "io_exception.hpp"

#include <dirent.h>
#include <unistd.h>

using namespace std;

namespace files {
  vector<string> list_dir(const string& path) {
    DIR* dir = opendir (path.c_str());
    if (dir != 0) {
      vector<string> paths;
      struct dirent* ent;
      while ((ent = readdir(dir)) != 0) {
        paths.push_back(string(ent->d_name));
      }
      closedir(dir);

      return paths;
    } else {
      throw io_exception("Could not open directory");
    }
  }

  bool path_exists(const string& file) {
    return access(file.c_str(), F_OK) == 0;
  }
}
