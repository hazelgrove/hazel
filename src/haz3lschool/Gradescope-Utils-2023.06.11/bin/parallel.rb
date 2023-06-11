#!/usr/bin/env ruby

require 'parallel'
require 'json'
require 'open3'
require 'optparse'

argparser = OptionParser.new
help = ->(){
  puts argparser.help
  puts <<~__EOF
  SYNOPSIS
      #{$0} : token2uniqname -> json
          stdin: a token2uniqname json hash
          stdout: a merged json hash, for all students
          args: a cmd w/ args to run, where `cmd` should return a json hash
      cat token2uniqname.json | #{$0} cmd args
  DESCRIPTION
      This is a thin wrapper to run other Gradescope-Utils scripts in parallel.
      The fundamental assumption is that `cmd` is some sort of map, so each student is really independent.
      `#{$0}` splits `token2uniqname` into singleton hashes per student, and runs `cmd args < singleton_token2uniqname` in parallel, merging the json hashes at the end.

      `#{$0}` passes `args` to `cmd` exactly as is-- as we get it, post shell-- so you shouldn't need to do anything fancy here.

  see the toplevel Gradescope-Utils README for more information
__EOF
  exit
}
argparser.on('-h', '--help', 'prints this help') { ||
  help.call
}
# don't touch ARGV so we can pass pristine ARGV exactly as-is to child processes
begin
  _nonoptions = argparser.parse(ARGV)
rescue OptionParser::InvalidOption
  # hack to have OptionParser only handle the help message, and allow everything else
end
if ARGV.length == 0
  help.call
end

token2uniqname = JSON.parse(STDIN.read)
tokens = token2uniqname.keys
# let the OS deal w/ the scheduling
results = Parallel.map(tokens, in_threads: tokens.length) { |token|
  singleton_token2uniqname = token2uniqname.slice(token)
  stdout, _status = Open3.capture2(*ARGV, :stdin_data => JSON.pretty_generate(singleton_token2uniqname))
  JSON.parse(stdout)
}
# fold each student's data into a single json hash
puts JSON.pretty_generate({}.merge(*results))
