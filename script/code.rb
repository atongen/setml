#!/usr/bin/env ruby

parts = ENV["PATH"].split(":")
opam = parts.detect { |p| p =~ /opam/ }
nvm = parts.detect { |p| p =~ /nvm/ }
parts.delete(opam)
parts.delete(nvm)
parts.uniq!

env = case ARGV[0]
when "opam"
  ([opam, nvm] + parts)
when "nvm"
  ([nvm, opam] + parts)
else
  STDERR.puts "arg must be 'opam' or 'nvm'"
  exit 1
end

code = `which code`.strip
dir = File.expand_path("../..", __FILE__)
Dir.chdir(dir)
system({ "PATH" => env.join(":") }, code)
