STDIN.each do |line|
  a, b = line.split(/\s+/)
  if a =~ /\d/
    direction = 2
    slot = a
    card = b
  else
    direction = 1
    slot = b
    card = a
  end
  STDOUT.puts direction
  STDOUT.puts slot
  STDOUT.puts card
  STDOUT.flush
end
