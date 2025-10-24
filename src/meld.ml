open Tile

type meld = tile list

let is_group m =
  let joker_count = List.length (List.filter (function Joker -> true | _ -> false) m) in
  let n = List.length m in
  if n < 3 || n > 4 then false else
  let rec process_tiles rank_opt colors = function
    | [] -> 
        (match rank_opt with
         | None -> false
         | Some _ -> 
             let unique_colors = List.sort_uniq Stdlib.compare colors in
             List.length unique_colors + joker_count = n)
    | Joker::rest -> process_tiles rank_opt colors rest
    | Tile(c,r)::rest ->
        (match rank_opt with
         | None -> process_tiles (Some r) [c] rest
         | Some r0 when r = r0 -> process_tiles rank_opt (c::colors) rest
         | Some _ -> false)
  in
  process_tiles None [] m

let is_run m =
  let joker_count = List.length (List.filter (function Joker -> true | _ -> false) m) in
  if List.length m < 3 then false else
  let rec process_run color_opt ranks = function
    | [] ->
        (match color_opt with
         | None -> false
         | Some _ ->
             let sorted_ranks = List.sort Stdlib.compare ranks in
             let rec gaps acc = function
               | a::b::xs -> if a=b then max_int else gaps (acc + (b-a-1)) (b::xs)
               | _ -> acc
             in
             let g = gaps 0 sorted_ranks in
             g <> max_int
             && g <= joker_count
             && (match sorted_ranks with
                 | [] | [_] -> true
                 | _ ->
                     let rmax = List.hd (List.rev sorted_ranks) in
                     rmax + joker_count <= 13))
    | Joker::rest -> process_run color_opt ranks rest
    | Tile(c,r)::rest ->
        (match color_opt with
         | None -> process_run (Some c) [r] rest
         | Some c0 when c = c0 -> process_run color_opt (r::ranks) rest
         | Some _ -> false)
  in
  process_run None [] m

let is_meld m = is_group m || is_run m

let meld_points m =
  List.fold_left
    (fun s -> function Tile(_,r)->s+r | Joker-> s+10)
    0 m

(* For the initial-30 rule we just over-approximate joker value as 10. *)
let meld_points_for_initial = meld_points
