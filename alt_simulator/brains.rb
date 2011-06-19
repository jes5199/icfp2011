
class Goal
    attr_accessor :category,:args
    def initialize(game_state,category,*args)
        @game_state = game_state
        @category = category
        @args = args
      end
    def moves
        @moves ||= (
            limit,target_cell = *args
            current = @game_state.proponent.field[target_cell].to_lisp
            current_num = (current =~ /^\d+$/ && current.to_i) || (current == 'zero' && 0)
            if current == 'I'
                [[2,'zero',target_cell]]
              elsif current_num && current_num < limit
                [[1,'succ',target_cell]]
              elsif current_num == limit
                []
              else
                [[1,'put',target_cell]]
              end
          )
      end
  end

class Brain
    attr_accessor :options,:goal_agents,:goal_filters
    def initialize(options)
        @options = options
        @goal_agents  = methods.grep(/^ga/)
        @goal_filters = methods.grep(/^gf/) + ['highlander']
      end
    def choose_move(game_state)
        @game_state = game_state
        goals = goal_agents.inject([]) { |gs,ga| gs + send(ga) }
        goal  = goal_filters.inject(goals) { |gs,gf| send(gf,gs) }.first
        goal.moves.first
      end
    def gaCount
        puts "I suggest we count!"
        [Goal.new(@game_state,:count_to,10,17)]
      end
    def highlander(goals)
        puts "Highlander saw #{goals.length} goals"
        fail "There were no goals!" unless goals.length >= 1
        [goals.first]
      end
  end
