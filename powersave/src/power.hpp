#ifndef POWER_HPP_Q6Q3NRSC
#define POWER_HPP_Q6Q3NRSC

#include <string>

namespace power
{
  void opt(const std::string& file, const char* value);
  void check(const std::string& file);

  void run(const std::string& cmd);

  void is_loaded(const std::string& module);
  void load(const std::string& module);
  void unload(const std::string& module);
}

#endif /* end of include guard: POWER_HPP_Q6Q3NRSC */
