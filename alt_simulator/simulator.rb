Size = 256
Starting_vitality = 10000
class Cells
    def vitality
        @vitality ||= [Starting_vitality]*Size
      end
    def field
        @field ||= ['I']*Size
      end
  end

class Object
    def to_lisp
        to_s
      end
  end

class Array
    def to_lisp
        first.to_lisp + self[1..-1].collect { |x| "("+x.to_lisp+")" }.join
      end
  end


def ask(prompt,type)
    while true
        print prompt,"\n"
        input = (gets || abort).chomp
        result = send("#{type}",input) rescue "***"
        p result
        return result if result.to_s == input
      end
    end

class GameState
    def player
        @player ||= 0
      end
    def proponent
        @proponent ||= Cells.new
      end
    def opponent
        @opponent ||= Cells.new
      end
    def swap
        return if @mode == :only
        @proponent,@opponent = @opponent,@proponent
        @player = 1-player
      end
    def initialize(mode)
        @turn = -1
        @mode = mode
        puts "Lambda: The Gathering version $Date:: 2011-06-15 18:44:34 +0900\#\$"
      end
    def header
        puts "###### turn #{@turn+=1}"
        puts "*** player #{player}'s turn, with slots:"
        (0...Size).each { |i| 
            puts "#{i}={#{proponent.vitality[i]},#{proponent.field[i].to_lisp}}" unless proponent.vitality[i] == Starting_vitality && proponent.field[i] == "I"
          }
        puts "(slots {#{Starting_vitality},I} are omitted)"
      end
    #
    # Cards
    #
    def I x
        x
      end
    def zero
        0
      end
    def succ n
        p [:succ,n]
        n+1
      end
    def dbl n
        2*n
      end
    def get i
        p [:get,i]
        proponent.field[i] || fail
      end
    def put x,y
        y
      end
    def S f,g,x
        h = apply(f,x)
        y = apply(g,x)
        z = apply(h,y)
        z
      end
    def K x,y
        x
      end
    def inc i
        proponent.vitality[i] += 1 if proponent.vitality[i] > 0 
        "I"
      end
    def dec i
        opponent.vitality[i] -= 1 if opponent.vitality[i] < 0
        "I"
      end
    def attack i,j,n
        "I"
      end
    def help i,j,n
        "I"
      end
    def copy i
        opponent.field[i] || fail
      end
    def revive i
        if proponent.vitality[i] == 0
            proponent.vitaility[i] = 1
            proponent.field[i] = ['I']
          end
        "I"
      end
    def zombie i,x
        "I"
      end
    #
    #
    #
    def arity(x)
        case x
          when Integer; 0
          when String; respond_to?(x) ? method(x).arity : fail("Invalid card: '#{x}'")
          else fail
          end
      end
    def apply(x,y)
        result = (x.is_a? Array) ? (x + [y]) : [x,y]
        f = result.first
        if result.length <= arity(f)
            # not enough to apply
          elsif result.length == arity(f) + 1
            result = send(*result.collect { |x| (x == 'zero') ? 0 : x}) if arity(f) > 0
          else
            fail unless result.length == 1 
          end
        result
      end
    def move(side=nil,card=nil,cell=nil)
        header
        side ||= ask("(1) apply card to slot, or (2) apply slot to card?",Integer)
        if side == 1
            card ||= ask("card name?",String)
            cell ||= ask("slot no?",Integer)
            message = "player 0 applied card #{card} to slot #{cell}"
          else
            cell ||= ask("slot no?",Integer)
            card ||= ask("card name?",String)
            message = "player 0 applied slot #{cell} to card #{card}"
          end
        proponent.field[cell] = (side == 1) ? apply(card,proponent.field[cell]) : apply(proponent.field[cell],card) #rescue "I"
        puts message
        swap
      end
  end

gs = GameState.new(:only)
1000.times { gs.move }


__END__

(1) apply card to slot, or (2) apply slot to card?
1
card name?
inc
slot no?
3
Exception: Native.Error
slot 3 reset to I
