#!/usr/bin/env ruby

require "tempfile"

# {{{ Utils
module Utils
  def Utils.list_files(dir)
    res = []
    Dir.entries(dir).each do |f|
      if f != "." and f != ".."
        res << f
      end
    end

    return res.sort
  end

  def Utils.dir_empty?(dir)
    Dir.entries(dir).length == 2
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
  def Utils.unzip(file, target, filters = ["*"], junk_paths = false)
    filters = filters.map { |f| "'#{f}'" }.join(" ")
    if junk_paths
      system("unzip -joqq #{file} #{filters} -d #{target}")
    else
      system("unzip -oqq #{file} #{filters} -d #{target}")
    end
  end

  # Zip the contents of a directory.
  # Overwrites the target zip file. Syscalls the zip program.
  def Utils.zip(dir, target)
    pwd = Dir.pwd
    Dir.chdir(dir)
    system("zip -rqq #{target} *")
    Dir.chdir(pwd)
  end

  def Utils.is_same?(file1, file2)
    if File.exists?(file1) and File.exists?(file2)
      return system("diff -q #{file1} #{file2} > /dev/null")
    else
      return false
    end
  end

  # Download an URL to a file.
  # Raises error if http request fails.
  def Utils.download(url, file)
    File.write(file, open(url).read())
  end
end

class RSSParser 
  def initialize(url)
    @url = url

    data = ""
    open(url) do |f|
      f.each do |line|
        data << line
      end
    end
    @doc = REXML::Document.new(data)
  end

  def get_newer(top_filter, data_item, data_regex, date_item, some_date)
    if some_date == nil
      # Default to downloading everything that have been released the latest
      # week, hence the -7
      some_date = Date.today() - 7
    end

    newer = []
    @doc.elements.each(top_filter) do |e|
      data = e.elements[data_item].text
      if data.match(data_regex) != nil
        date = Date.parse(e.elements[date_item].text)
        if date > some_date
          newer << { :data => data, :date => date }
        end
      end
    end

    return newer
  end

  def get_latest(top_filter, data_item, data_regex, date_item)
    latest = nil
    data   = nil
    @doc.elements.each(top_filter) do |e|
      tmp = e.elements[data_item].text
      if tmp.match(data_regex) != nil
        date = Date.parse(e.elements[date_item].text)
        if latest == nil or date > latest
          latest = date
          data   = { :data => tmp, :date => date }
        end
      end
    end

    return data
  end
end

class Tempdir
  def initialize(dir)
    @dir = dir

    FileUtils.mkdir_p(dir)
    yield self
    FileUtils.rm_rf(dir)
  end

  def path()
    return @dir
  end
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

class MessageBoxDialog < Dialog
  def initialize(message)
    @message = message
    @finished = false
  end

  def <<(str)
    @message << str
    run()
  end

  def finish()
    @finished = true
    run()
  end

  def run()
    if @finished
      @result, @exit = Dialog.execute("dialog --msgbox '#{@message}' 10 75")
    else
      @result, @exit = Dialog.execute("dialog --infobox '#{@message}' 10 75")
    end
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
  def initialize(title, options, cancel = "Cancel")
    @title   = title
    @options = build_options(options)
    @cancel  = cancel
  end

  def run()
    @result, @exit = Dialog.execute(
      "dialog --cancel-label #{@cancel} --menu '#{@title}' 0 50 10 #{@options}")
  end

  private

  def build_options(options)
    str = ""
    options.each do |opt|
      str << '"'
      str << opt[:tag]
      str << "\" \""
      str << opt[:desc]
      str << '" '
    end

    return str
  end
end

class SelectFileDialog < Dialog
  def initialize(title, dir, get_desc)
    @title    = title
    @dir      = dir
    @get_desc = get_desc
  end

  def run()
    options = build_options()
    result, @exit = Dialog.execute(
      "dialog --menu '#{@title}' 0 50 10 #{options}")
    @result = File.join(@dir, result)
  end

  private

  def build_options()
    str = ""
    Utils.list_files(@dir).each do |f|
      str << '"'
      str << f
      str << '" "'
      str << @get_desc.call(File.join(@dir, f))
      str << '" '
    end

    return str
  end
end

class SelectManyFilesDialog < SelectFileDialog
  # TODO file desc
  def initialize(title, dir, get_desc)
    super(title, dir, get_desc)
  end

  def run()
    options = build_options()
    result, @exit = Dialog.execute(
      "dialog --checklist '#{@title}' 0 50 10 #{options}")
    @result = File.join(@dir, result)
  end
end
# }}}
# {{{ Minecraft
module Minecraft
  JAVAOPTS = "-server -Xmx1024M -Xms512M"

  # ~/.minecraft
  DOT_DIR         = File.join(ENV["HOME"], ".minecraft")
  BIN_DIR         = File.join(DOT_DIR, "bin")
  NATIVES_DIR     = File.join(BIN_DIR, "natives")
  JAR_FILE        = File.join(BIN_DIR, "minecraft.jar")
  JAR_BACKUP_FILE = File.join(BIN_DIR, "minecraft.jar.bak")

  # install dir
  INSTALL_DIR  = File.join(ENV["HOME"], "games/minecraft")
  LAUNCHER_JAR = File.join(INSTALL_DIR, "launcher.jar")

  CLIENT_JARS_DIR    = File.join(INSTALL_DIR, "clients")
  CLIENT_MODS_DIR    = File.join(INSTALL_DIR, "mods")
  CLIENT_RELEASE_JAR = File.join(CLIENT_JARS_DIR, "release.jar")

  SERVER_JARS_DIR    = File.join(INSTALL_DIR, "servers")
  SERVER_CONFIGS_DIR = File.join(INSTALL_DIR, "configs")
  SERVER_WORLDS_DIR  = File.join(INSTALL_DIR, "worlds")
  SERVER_RELEASE_JAR = File.join(SERVER_JARS_DIR, "release.jar")
end
# }}}

# vim: set fdm=marker :
