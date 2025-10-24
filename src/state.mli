open Tile
open Meld

module TileMultiset : sig
  type t
  val empty : t
  val add : tile -> t -> t
  val remove_one : tile -> t -> t option
  val of_list : tile list -> t
  val to_list : t -> tile list
end

type player = {
  name : string;
  hand : TileMultiset.t;
  met_initial_30 : bool;
}

type board = meld list

type move =
  | Play of meld list
  | Draw
  | Pass

type t = {
  deck   : tile list;
  board  : board;
  players: player array;
  turn   : int;
}

val initial_state : Random.State.t -> t
