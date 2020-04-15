open Deck
open Game

module Make = functor(G:Game)->
struct
  type chip = Chip.t
  type modes = Classic | Switch | European
  type category = Card



end
