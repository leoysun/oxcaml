type color = Red | Blue | Black | Orange
type rank = int  (* 1..13 *)
type tile = Tile of color * rank | Joker

val all_colors : color list
val pp_tile : tile -> string
val deck : unit -> tile list
