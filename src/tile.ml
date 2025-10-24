type color = Red | Blue | Black | Orange
type rank = int
type tile = Tile of color * rank | Joker

let all_colors = [Red; Blue; Black; Orange]

let pp_color = function Red->"R" | Blue->"B" | Black->"K" | Orange->"O"
let pp_rank r = Printf.sprintf "%02d" r
let pp_tile = function Tile(c,r)-> pp_color c ^ pp_rank r | Joker-> "Jk"

let deck () =
  let base =
    List.concat_map (fun c ->
      List.init 13 (fun i -> Tile (c, i+1))) all_colors
  in
  base @ base @ [Joker; Joker]

let compare_tile t1 t2 = match t1, t2 with
  | Joker, Joker -> 0
  | Joker, _ -> 1
  | _, Joker -> -1
  | Tile (c1, r1), Tile (c2, r2) ->
      let c_comp = compare c1 c2 in
      if c_comp <> 0 then c_comp else compare r1 r2
