open Tile
open Meld

module TileOrd = struct type t = tile let compare = Stdlib.compare end
module M = Map.Make(TileOrd)

module TileMultiset = struct
  type t = int M.t
  let empty = M.empty
  let count x m = Option.value (M.find_opt x m) ~default:0
  let add x m = M.add x (count x m + 1) m
  let remove_one x m =
    let n = count x m in
    if n=0 then None
    else Some (if n=1 then M.remove x m else M.add x (n-1) m)
  let of_list xs = List.fold_left (fun m x -> add x m) empty xs
  let to_list m =
    M.bindings m |> List.concat_map (fun (t,n)-> List.init n (fun _->t))
end

type player = {
  name : string;
  hand : TileMultiset.t;
  met_initial_30 : bool;
}

type board = meld list
type move = Play of meld list | Draw | Pass

type t = {
  deck   : tile list;
  board  : board;
  players: player array;
  turn   : int;
}

let shuffle (rng:Random.State.t) (lst:'a list) =
  let a = Array.of_list lst in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int rng (i+1) in
    let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp
  done; Array.to_list a

let initial_state rng =
  let d = shuffle rng (deck ()) in
  let take n xs =
    let rec go acc n xs =
      if n=0 then (List.rev acc, xs)
      else match xs with y::ys -> go (y::acc) (n-1) ys | [] -> (List.rev acc, [])
    in go [] n xs
  in
  let p1_tiles, rest1 = take 14 d in
  let p2_tiles, rest2 = take 14 rest1 in
  {
    deck = rest2;
    board = [];
    players = [|
      { name="You"; hand = TileMultiset.of_list p1_tiles; met_initial_30=false };
      { name="Bot"; hand = TileMultiset.of_list p2_tiles; met_initial_30=false };
    |];
    turn = 0;
  }
