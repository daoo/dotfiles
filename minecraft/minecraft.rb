#!/usr/bin/env ruby
#
# A minecraft launcher written by daoo.
# Fully featured, can update the client and the server, fetch snapshots, update
# update lwjgl, etc.
#

require "date"
require "fileutils"
require "open-uri"
require "rexml/document"
require "tempfile"

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
    open(url.to_s) do |f|
      File.open(file, "w") do |out|
        out.write(f.readlines)
      end
    end
  end
end
# }}}
# {{{ Minecraft
module Minecraft
  JAVAOPTS    = "-server -Xmx1024M -Xms512M"

  # ~/.minecraft
  DOT_DIR         = File.join(ENV["HOME"], ".minecraft")
  BIN_DIR         = File.join(DOT_DIR, "bin")
  NATIVES_DIR     = File.join(BIN_DIR, "natives")
  JAR_FILE        = File.join(BIN_DIR, "minecraft.jar")
  JAR_BACKUP_FILE = File.join(BIN_DIR, "minecraft.jar.bak")

  # install dir
  INSTALL_DIR  = File.join(ENV["HOME"], "games/minecraft")
  LAUNCHER_JAR = File.join(INSTALL_DIR, "launcher.jar")
  CLIENTS_DIR  = File.join(INSTALL_DIR, "client")
  SERVERS_DIR  = File.join(INSTALL_DIR, "server")
  MODS_DIR     = File.join(INSTALL_DIR, "mods")
  LOG_FILE     = File.join(INSTALL_DIR, "minecraft.log")

  RELEASE_CLIENT_JAR = File.join(CLIENTS_DIR, "release.jar")
  RELEASE_SERVER_JAR = File.join(SERVERS_DIR, "release.jar")

  # urls, lwjgl
  LWJGL_RSS = "http://sourceforge.net/api/file/index/project-id/58488/mtime/desc/limit/20/rss"

  # urls, release versions
  RELEASE_LAUNCHER_URL = "https://s3.amazonaws.com/MinecraftDownload/launcher/minecraft.jar"
  RELEASE_SERVER_URL   = "https://s3.amazonaws.com/MinecraftDownload/launcher/minecraft_server.jar"

  # urls, snapshots, needs an advanced downloading mechanism
  SNAPSHOT_URL = "http://assets.minecraft.net"

  # tmp
  TMP_MINECRAFT_DIR = "/tmp/minecraft.jar/"
  TMP_MINECRAFT_JAR = "/tmp/tmp-minecraft.jar"
  TMP_LWJGL_DIR     = "/tmp/lwjgl.zip/"

  # lwjgl files
  LWJGL_JARS = ["jinput.jar", "lwjgl.jar", "lwjgl_util.jar"].map { |jar|
    File.join(TMP_LWJGL_DIR, jar)
  }
  LWJGL_LIBS = ["libjinput-linux64.so", "libjinput-linux.so", "liblwjgl64.so", "liblwjgl.so", "libopenal64.so", "libopenal.so"].map { |lib|
    File.join(TMP_LWJGL_DIR, lib)
  }

  # helper files
  LWJGL_VERSION = File.join(DOT_DIR, "lwjgl-version")
end
# }}}
# {{{ Updater
module Updater
  # Parse an xml file for the latest of element.
  def Updater.get_latest(url, top_filter, result_regexp, date_item, result_item)
    data = ""
    open(url) do |f|
      f.each do |line|
        data << line
      end
    end
    doc = REXML::Document.new(data)

    latest = nil
    res    = nil
    doc.elements.each(top_filter) do |e|
      k = e.elements[result_item].text
      if k.match(result_regexp) != nil
        d = Date.parse(e.elements[date_item].text)
        if latest == nil or d > latest
          latest = d
          res    = k
        end
      end
    end

    return res
  end

  def Updater.get_latest_release(url, target_jar)
    res = false

    Tempfile.open("release.jar") do |tmp|
      open(url) do |f|
        tmp.write(f.readlines)
      end
      tmp.flush

      if not Utils.is_same?(tmp.path, target_jar)
        FileUtils.cp(tmp.path, target_jar)

        res = true
      end
    end

    return res
  end

  # Connect to assets.minecraft.net and retrive the latest snapshot version.
  def Updater.get_latest_snapshot()
    key = Updater.get_latest(
      Minecraft::SNAPSHOT_URL,
      "ListBucketResult/Contents",
      /\d\dw\d\d[a-z]\/$/,
      "LastModified",
      "Key")

    # Remove the trailing slash
    return key[0, key.length - 1]
  end

  # Get the latest version of lwjgl
  def Updater.get_latest_lwjgl()
    return Updater.get_latest(
      Minecraft::LWJGL_RSS,
      "rss/channel/item",
      /lwjgl-[\.0-9]+\.zip\/download$/,
      "pubDate",
      "link")
  end

  # Download a specific client snapshot.
  # Returns true if a new snapshot was downloaded.
  def Updater.download_client_snapshot(snapshot)
    client_file = File.join(Minecraft::CLIENTS_DIR, snapshot + ".jar")
    if not File.exists?(client_file)
      client_url = "#{Minecraft::SNAPSHOT_URL}/#{snapshot}/minecraft.jar"
      Utils.download(client_url, client_file)

      return true
    end

    return false
  end

  # Download a specific server snapshot.
  # Returns true if a new snapshot was downloaded.
  def Updater.download_server_snapshot(snapshot)
    server_file = File.join(Minecraft::SERVERS_DIR, snapshot + ".jar")
    if not File.exists?(server_file)
      server_url = "#{Minecraft::SNAPSHOT_URL}/#{snapshot}/minecraft_server.jar"
      Utils.download(server_url, server_file)

      return true
    end

    return false
  end

  # Download and update lwjgl if needed.
  def Updater.download_lwjgl()
    lwjgl_version = nil
    if File.exists?(Minecraft::LWJGL_VERSION)
      File.open(Minecraft::LWJGL_VERSION, "r") do |f|
        lwjgl_version = f.readline
      end
    end

    lwjgl_url = Updater.get_latest_lwjgl()
    if lwjgl_version == nil or not lwjgl_url.include?(lwjgl_version)
      # OK, we can download a new version of LWJGL
      # Carefully extract the version from the URL
      new_version = lwjgl_url.match(/\d\.\d\.\d/)[0]

      Tempfile.open("lwjgl.zip") do |tmp|
        open(lwjgl_url) do |zip|
          tmp.write(zip.read)
        end
        tmp.flush

        # Okay, now we got the zip, extract it
        FileUtils.rm_rf(Minecraft::TMP_LWJGL_DIR)
        Utils.unzip(tmp.path, Minecraft::TMP_LWJGL_DIR, ["*.jar", "*.so"], true)

        # Copy the correct files
        FileUtils.cp(Minecraft::LWJGL_JARS, Minecraft::BIN_DIR)
        FileUtils.cp(Minecraft::LWJGL_LIBS, Minecraft::NATIVES_DIR)

        FileUtils.rm_rf(Minecraft::TMP_LWJGL_DIR)

        File.open(Minecraft::LWJGL_VERSION, "w") do |f|
          f.write(new_version)
        end

        return true
      end
    end

    return false
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
    @result, @exit = Dialog.execute("dialog --cancel-label #{@cancel} --menu '#{@title}' 0 50 10 #{@options}")
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

while true do
  diag = MenuDialog.new("Welcome!", ["client", "sever", "check for updates"], "Exit")
  diag.run()
  if diag.get_exit()
    action = diag.get_result()
    if action == "client"
    elsif action == "server"
    elsif action == "check for updates"
      msgBox = MessageBoxDialog.new("")

      msgBox << "Checking for launcher release... "
      if Updater.get_latest_release(Minecraft::RELEASE_LAUNCHER_URL, Minecraft::LAUNCHER_JAR)
        msgBox << "Updated!\n"
      else
        msgBox << "No update found.\n"
      end

      msgBox << "Checking for server release... "
      if Updater.get_latest_release(Minecraft::RELEASE_SERVER_URL, Minecraft::RELEASE_SERVER_JAR)
        msgBox << "Updated!\n"
      else
        msgBox << "No update found.\n"
      end

      snapshot = Updater.get_latest_snapshot()
      msgBox << "Checking for client snapshot... "
      if Updater.download_client_snapshot(snapshot)
        msgBox << "Downloaded client #{snapshot}!\n"
      else
        msgBox << "No new snapshots found.\n"
      end

      msgBox << "Checking for server snapshot... "
      if Updater.download_server_snapshot(snapshot)
        msgBox << "Downloaded server #{snapshot}!\n"
      else
        msgBox << "No new snapshots found.\n"
      end

      msgBox << "Checking for new lwjgl... "
      if Updater.download_lwjgl()
        msgBox << "Downloaded new lwjgl!\n"
      else
        msgBox << "No new lwjgl found.\n"
      end

      msgBox.finish()
    end
  else
    break
  end
end

# vim: set fdm=marker :
