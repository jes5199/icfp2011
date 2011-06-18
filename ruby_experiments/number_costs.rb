numbers = [0]
c = 0
seen = {0=>true}
while !numbers.empty? do
  new_numbers = []
  numbers.each do |n|
    puts( ("%5d" % n) + ("%5d" % c) )
    n1 = n + 1
    n2 = n * 2
    [n1,n2].each do |nn|
      new_numbers << nn if nn < 65535 and !seen[nn]
      seen[nn] = true
    end
  end
  numbers = new_numbers
  c += 1
end
