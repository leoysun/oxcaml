open Tile

type meld = tile list

let is_group m =
  let non_j = List.filter ((<>) Joker) m in
  let jokers = List.length m - List.length non_j in
  let n = List.length m in
  if n < 3 || n > 4 then false else
  match non_j with
  | [] -> false
  | Tile(_,r0)::_ ->
    let same_rank = List.for_all (function Tile(_,r)->r=r0 | _-> true) non_j in
    if not same_rank then false else
    let colors =
      non_j |> List.filter_map (function Tile(c,_) -> Some c | _ -> None)
            |> List.sort_uniq Stdlib.compare
    in
    List.length colors + jokers = n

let is_run m =
  let non_j = List.filter ((<>) Joker) m in
  let jokers = List.length m - List.length non_j in
  if List.length m < 3 then false else
  match non_j with
  | [] -> false
  | Tile(c0,_)::_ ->
    if not (List.for_all (function Tile(c,_) -> c=c0 | _-> true) non_j)
    then false
    else
      let ranks = non_j
        |> List.filter_map (function Tile(_,r)->Some r | _-> None)
        |> List.sort Stdlib.compare in
      let rec gaps acc = function
        | a::b::xs -> if a=b then max_int else gaps (acc + (b-a-1)) (b::xs)
        | _ -> acc
      in
      let g = gaps 0 ranks in
      g <> max_int
      && g <= jokers
      && (match ranks with
          | [] | [_] -> true
          | _ ->
            let rmax = List.hd (List.rev ranks) in
            rmax + jokers <= 13)

let is_meld m = is_group m || is_run m

(* For the initial-30 rule we just over-approximate joker value as 10. *)
let meld_points_for_initial m =
  List.fold_left
    (fun s -> function Tile(_,r)->s+r | Joker-> s+10)
    0 m
