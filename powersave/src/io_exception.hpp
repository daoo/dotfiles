#ifndef IO_EXCEPTION_HPP_SX7MRZYY
#define IO_EXCEPTION_HPP_SX7MRZYY

#include <exception>
#include <string>

namespace files {
  class io_exception : public std::exception
  {
    public:
      io_exception(const std::string& file);

      const std::string& get_file() const;

      virtual const char* what() const throw();
    private:
      const std::string& file;
  };
}

#endif /* end of include guard: IO_EXCEPTION_HPP_SX7MRZYY */
