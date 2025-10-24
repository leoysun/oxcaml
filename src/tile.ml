type color = Red | Blue | Black | Orange
type rank = int
type tile = Tile of color * rank | Joker

let all_colors = [Red; Blue; Black; Orange]

let pp_color = function Red->"R" | Blue->"B" | Black->"K" | Orange->"O"
let pp_rank r = Printf.sprintf "%02d" r
let pp_tile = function Tile(c,r)-> pp_color c ^ pp_rank r | Joker-> "Jk"

let deck () =
  let base =
    List.concat_map all_colors (fun c ->
      List.init 13 (fun i -> Tile (c, i+1)))
  in
  base @ base @ [Joker; Joker]
