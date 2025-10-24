open State
open Meld

val board_valid : board -> bool

val apply_play : t -> meld list -> (t, string) result
val apply_draw : t -> (t, string) result
val next_turn  : t -> t
val is_game_over : t -> bool
val get_winner : t -> player option
