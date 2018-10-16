#!/usr/bin/env ruby

dir = File.expand_path("../..", __FILE__)
Dir.chdir(dir)

def select_parts(parts, strs)
  regexs = strs.map { |s| %r(#{s}) }
  parts.select do |p|
    regexs.any? { |r| r.match(p) } &&
      File.exists?(p)
  end
end

parts = ENV["PATH"].split(":")

ocaml = select_parts(parts, %w( opam ocaml ))
js = select_parts(parts, %w( nvm node ))
rest = parts - (ocaml + js)

env = case ARGV[0]
when "ocaml", "opam"
  (ocaml + js + rest).uniq
when "js", "npm", "nvm", "node"
  (js + ocaml + rest).uniq
else
  []
end

if env.empty?
  STDERR.puts "arg must be 'ocaml' or 'js'"
  exit 1
end

code = `which code`.strip
path = env.join(":")
system({ "PATH" => path }, code)
