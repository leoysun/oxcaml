open Tile

type meld = tile list

val is_group : meld -> bool
val is_run   : meld -> bool
val is_meld  : meld -> bool
val meld_points_for_initial : meld -> int
