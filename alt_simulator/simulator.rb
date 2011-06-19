require "brains"

Size = 256
Starting_vitality = 10000
Max_vitality = 65535
Max_applications = 1000
Max_turns = 100000

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
        result = result.to_i if type == String and result =~ /^\d+$/
        #p result
        return result if result.to_s == input
      end
    end

class GameState
    def zombie?
        @zombie
      end
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
        (0...Size).each { |i|
            if proponent.vitality[i] == -1
                begin
                    @applications = 0
                    @zombie = true
                    apply(proponent.field[i],'I')
                  ensure
                    @zombie = false
                    proponent.vitality[i] = 0
                    proponent.field[i] = "I"
                  end
              end
          }
      end
    def initialize(mode)
        @turn = -1
        @mode = mode
        puts "Lambda: The Gathering version $Date:: #{Time.now.to_s }2011-06-15 18:44:34 +0900\#\$"
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
        n+1
      end
    def dbl n
        2*n
      end
    def get i
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
        if zombie?
            proponent.vitality[i] -= 1 if proponent.vitality[i] > 0 
            proponent.vitality[i] = Max_vitality if proponent.vitality[i] > Max_vitality
          else
            proponent.vitality[i] += 1 if proponent.vitality[i] > 0 
            proponent.vitality[i] = Max_vitality if proponent.vitality[i] > Max_vitality
          end
        "I"
      end
    def dec i
        if zombie?
            opponent.vitality[i] += 1 if opponent.vitality[i] > 0
            opponent.vitality[i] = Max_vitality if proponent.vitality[i] > Max_vitality
          else
            opponent.vitality[i] -= 1 if opponent.vitality[i] > 0
          end
        "I"
      end
    def attack i,j,n
        proponent.vitality[i] -= n
        if zombie?
            opponent.vitality[255-j] += (n*9)/10 
            opponent.vitality[255-j] = Max_vitality if opponent.vitality[255-j] > Max_vitality
          else
            opponent.vitality[255-j] -= (n*9)/10 
            opponent.vitality[255-j] = 0 if opponent.vitality[255-j] < 0
          end
        "I"
      end
    def help i,j,n
        proponent.vitality[i] -= n
        if zombie?
            proponent.vitality[j] -= (n*11)/10 
            proponent.vitality[j] = 0 if proponent.vitality[j] < 0
          else
            proponent.vitality[j] += (n*11)/10 
            proponent.vitality[j] = Max_vitality if proponent.vitality[j] > Max_vitality
            if proponent.vitality[j] > Max_vitality
                fail
              end
          end
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
        fail unless opponent.vitality[255-j] <= 0
        opponent.vitality[255-j] = -1
        opponent.field[255-j] = x
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
        @applications += 1
        fail if @applications > Max_applications
        result = (x.is_a? Array) ? (x + [y]) : [x,y]
        p result
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
        @applications = 0
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
        proponent.field[cell] = (side == 1) ? apply(card,proponent.field[cell]) : apply(proponent.field[cell],card) rescue "I"
        puts message
        print "...in #{@applications} applications\n"
        swap
      end
  end

class Game
    attr_accessor :game_state,:players
    def initialize(players)
        fail unless [1,2].include? players.length
        @players = players
        @game_state = GameState.new( (players.length == 1) ? :only : :alt)
      end
    def run
        (0...Max_turns).each { |turn|
            player = players[turn % players.length]
            if player
                game_state.move(*player.choose_move(game_state))
              else 
                game_state.move
              end
          }
      end
  end

Game.new(ARGV.collect { |player_desc| (player_desc == '-') ? nil : Brain.new(player_desc)}).run

__END__

gs = GameState.new(:only)
#gs.proponent.field[0] = ['S','get',['S',['S',['K',['help',0,0]],['K',8196]],['I']]]
#gs.proponent.field[0] = ['S','get',['S',['S',['K',['help',0,0]],['K',8196]],['I']]]

gs.proponent.field[16] = ['S','get',['S',['S',['K',['help',0,0]],['K',4096]],['I']]]

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
