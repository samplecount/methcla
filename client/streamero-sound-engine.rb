#!/usr/bin/env ruby

require 'rubygems'
require 'fileutils'
require 'json'
require 'readline'
require 'socket'
require 'tempfile'

def read_terminal
  while line = Readline.readline('> ', true)
    yield line unless line.empty?
  end
end

def read_file(path)
  IO.foreach(path) { |line| yield line }
end

tmpfile = Tempfile.new('streamero-sound-engine-')
socket_path = tmpfile.path
tmpfile.close!

server = UNIXServer.new(socket_path)
$stdout << "Started UNIX server " << server.path << "\n"

pid = fork
if pid.nil?
  exec("./dist/build/streamero-sound-engine/streamero-sound-engine #{server.path}")
end

io = server.accept

begin
  read_terminal { |msg|
    io << [msg.size].pack("N")
    io << msg
    n = io.read(4).unpack("N")[0]
    $stdout << io.read(n)
  }
ensure
  io.close
  server.close
  FileUtils.rm_f(socket_path)
  # Process.waitpid(pid)
end
