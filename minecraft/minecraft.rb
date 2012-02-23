require "fileutils"
require 'tempfile'

# {{{ Utils
module Utils
  # Yield each file in a directory.
  # Excluding "." and "..".
  def Utils.each_file(dir)
    Dir.entries(dir).each do |f|
      if f != "." and f != ".."
        yield f
      end
    end
  end

  # Convert seconds to the hh:mm:ss format.
  def Utils.seconds_to_str(time)
    hours   = (time / 3600).to_i
    minutes = (time / 60 - (hours * 3600)).to_i
    seconds = (time - (minutes * 60 + hours * 3600)).to_i

    return "%02d:%02d:%02d" % [hours, minutes, seconds]
  end

  # Unzip the contents of a zip file to a target directory.
  # Overwrites anything in the target directory. Syscalls the unzip program.
  def Utils.unzip(file, target)
    system("unzip -oqq #{file} -d #{target}")
  end

  # Zip the contents of a directory.
  # Overwrites the target zip file. Syscalls the zip program.
  def Utils.zip(dir, target)
    pwd = Dir.pwd
    Dir.chdir(dir)
    system("zip -rqq #{target} *")
    Dir.chdir(pwd)
  end
end
# }}}
# {{{ Minecraft
module Minecraft
  INSTALL_DIR = File.join(ENV["HOME"], "games/minecraft")
  JAVAOPTS    = "-server -Xmx1024M -Xms512M"
end

module Client
  include Minecraft

  DOT_DIR         = File.join(ENV["HOME"], ".minecraft")
  BIN_DIR         = File.join(DOT_DIR, "bin")
  JAR_FILE        = File.join(BIN_DIR, "minecraft.jar")
  JAR_BACKUP_FILE = File.join(BIN_DIR, "minecraft.jar.bak")

  LAUNCHER_JAR = File.join(INSTALL_DIR, "launcher.jar")
  CLIENTS_DIR  = File.join(INSTALL_DIR, "client")
  MODS_DIR     = File.join(INSTALL_DIR, "mods")
  LOG_FILE     = File.join(INSTALL_DIR, "minecraft.log")

  TMP_DIR = "/tmp/minecraft.jar/"
  TMP_JAR = "/tmp/tmp-minecraft.jar"
end

module Server
  include Minecraft
end
# }}}
# {{{ Dialog
class Dialog
  def initialize()
    @result = nil
    @exit   = nil
  end

  def get_result()
    return @result
  end

  def get_exit()
    return @exit
  end

  def self.execute(cmd)
    tmp     = Tempfile.new("tmp")
    cmd_err = cmd + " 2> " + tmp.path

    res     = nil
    success = false
    begin
      success = system(cmd_err)
      if success
        res = tmp.gets
      end
    ensure
      tmp.close
      tmp.unlink
    end

    return res, success
  end
end

class YesNoDialog < Dialog
  def initialize(title)
    @title = title
  end

  def run()
    @result, @exit = Dialog.execute("dialog --yesno '#{@title}' 10 50")
  end
end

class InputDialog < Dialog
  def initialize(title, init)
    @title = title
    if init == nil
      @init = ""
    else
      @init = init
    end
  end

  def run()
    @result, @exit = Dialog.execute("dialog --inputbox '#{@title}' 10 50 #{@init}")
  end
end

class MenuDialog < Dialog
  def initialize(title, options)
    @title   = title
    @options = build_options(options)
  end

  def run()
    @result, @exit = Dialog.execute("dialog --menu '#{@title}' 0 50 10 #{@options}")
  end

  private

  def build_options(options)
    str = ""
    options.each do |opt|
      str << '"'
      str << opt
      str << "\" \""
      str << "todo"
      str << '" '
    end

    return str
  end
end

class SelectFileDialog < Dialog
  # TODO file desc
  def initialize(title, dir)
    @title = title
    @dir   = dir
  end

  def run()
    options = build_options()
    @result, @exit = Dialog.execute("dialog --menu '#{@title}' 0 50 10 #{options}")
  end

  private

  def build_options()
    str = ""
    Utils.each_file(@dir) do |f|
      str << '"'
      str << f
      str << "\" \""
      str << "todo"
      str << '" '
    end

    return str
  end
end

class SelectManyFilesDialog < Dialog
  # TODO file desc
  def initialize(title, dir)
    @title = title
    @dir   = dir
  end

  def run()
    options = build_options()
    @result, @exit = Dialog.execute("dialog --checklist '#{@title}' 0 50 10 #{options}")
  end

  private

  def build_options()
    str = ""
    Utils.each_file(@dir) do |f|
      str << '"'
      str << f
      str << "\" \""
      str << "todo"
      str << '" off '
    end

    puts str

    return str
  end
end
# }}}

diag = MenuDialog.new("Welcome!", ["client", "sever", "check for updates"])
diag.run()
puts diag.get_exit()
puts diag.get_result()

# vim: set fdm=marker :
