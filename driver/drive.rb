STDIN.each do |line|
  line = line.gsub(/[\[\]]/,' ')
  a, b = line.strip.split(/\s+/)
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
