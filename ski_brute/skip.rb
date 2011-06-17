require 'set'
require 'pp'
require 'yaml'
STDOUT.sync = true
STDERR.sync = true

def nski(expr)
  $c = 0
  ski(expr)
end

def ski(expr)
  $c ||= 0
  $c += 1
  if $c > 1000
    raise SystemStackError
  end
  #puts expr.inspect
  return expr unless expr.is_a?(Array)
  expr = expr.map do |ex|
    ski(ex)
  end
  if expr.length == 1
    return expr[0]
  end
  case expr[0]
    when Array
      return ski( expr[0] + expr[1..-1] )
    when "s"
      if expr.length >= 4
        s, g, f, x, *rest = expr
        return ski([g, x, [f, x], *rest])
      end
    when "k"
      if expr.length >= 3
        k, a, b, *rest = expr
        return ski([b, *rest])
      end
    when "i"
      if expr.length >= 2
        i, x, *rest = expr
        return ski([x, *rest])
      end
    when "p"
      if expr.length >= 2
        k, a, *rest = expr
        return ski(["i", *rest])
      end
  end
  return expr
end

letters = %w/s k i p/

things = Set["i"]

evils = Set[]
tried = Set[]

print "-: "
p things
1024.times do |n|
  STDERR.puts(n)
  STDERR.puts( things.length )

  STDERR.puts( "CREATING" )

  new_things = []
  things.each do |thing|
    if thing.is_a?(String)
      thing = [thing]
    end
    letters.each do |letter|
      new_things << (thing.dup << letter)
      new_things << [letter, thing.dup]
    end
  end

  STDERR.puts( "DELETING" )

  new_things.delete_if{|th| evils.include?(th) }
  new_things.delete_if{|th| tried.include?(th) }
  tried += new_things

  STDERR.puts(new_things.length)
  STDERR.puts( "CALCULATING" )

  new_things.map! do |thing|
    #puts "skiing #{thing.inspect}"
    begin
      nthing = nski(thing)
      puts(thing.inspect + " -> " + nthing.inspect)
      nthing
    rescue SystemStackError
      STDOUT.puts( "Non-terminating: " + thing.inspect )
      #STDERR.print( "." )
      evils << thing
    end
  end.uniq!
  things += new_things
  puts "#{n}: "
  #pp things

  STDERR.puts( "DELETING" )

  new_things.delete_if{|th| evils.include?(th) }

  STDERR.puts(new_things.length)
  STDERR.puts( "APPLYING" )

  new_things.each do |thing|
    begin
      real = thing.dup
      thing = [thing] + ['a', 'b']
      out = nski(thing)
      puts(thing.inspect + " => " + out.inspect)
      if out == ['s', ['k', 'a'], ['k', 'b'] ]
        STDERR.puts("Candidate: " + real.inspect)
        STDOUT.puts("Candidate: " + real.inspect)
      end
    rescue SystemStackError
      STDOUT.puts( "Non-terminating: " + thing.inspect )
      #STDERR.print( "," )
      evils << real
    end
  end
end
