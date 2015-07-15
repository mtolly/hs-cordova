#!/usr/bin/env ruby

# Removes any "RSomething" qualified imports
# if they aren't used in the file.

lines = STDIN.readlines
new_lines = lines.select do |ln|
  words = ln.split /\s+/
  next true unless words[0] == 'import'
  next true unless words[1] == 'qualified'
  next true unless words[3] == 'as'
  qualifier = words[4]
  next true unless qualifier =~ /^R[A-Z]/
  lines.any? { |someLine| someLine =~ /\b#{qualifier}\./ }
end
puts new_lines
