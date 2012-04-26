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

def write_msg(io, msg)
    io << [msg.size].pack("N")
    io << msg
    io.flush
end

def read_msg(io)
    n = io.read(4).unpack("N")[0]
    io.read(n)
end

begin
  read_terminal { |msg|
    write_msg(io, msg)
    $stdout << read_msg(io) << "\n"
  }
ensure
  write_msg(io, { "request" => "Quit" }.to_json)
  io.close
  server.close
  Process.waitpid(pid)
  FileUtils.rm_f(socket_path)
end
