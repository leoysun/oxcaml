open Tile
open State

(* Serialization functions for Firestore *)

let tile_to_json = function
  | Tile (Red, r) -> `Assoc [("type", `String "tile"); ("color", `String "red"); ("rank", `Int r)]
  | Tile (Blue, r) -> `Assoc [("type", `String "tile"); ("color", `String "blue"); ("rank", `Int r)]
  | Tile (Black, r) -> `Assoc [("type", `String "tile"); ("color", `String "black"); ("rank", `Int r)]
  | Tile (Orange, r) -> `Assoc [("type", `String "tile"); ("color", `String "orange"); ("rank", `Int r)]
  | Joker -> `Assoc [("type", `String "joker")]

let tile_of_json = function
  | `Assoc fields ->
      (match List.assoc_opt "type" fields with
       | Some (`String "joker") -> Some Joker
       | Some (`String "tile") ->
           (match List.assoc_opt "color" fields, List.assoc_opt "rank" fields with
            | Some (`String "red"), Some (`Int r) -> Some (Tile (Red, r))
            | Some (`String "blue"), Some (`Int r) -> Some (Tile (Blue, r))
            | Some (`String "black"), Some (`Int r) -> Some (Tile (Black, r))
            | Some (`String "orange"), Some (`Int r) -> Some (Tile (Orange, r))
            | _ -> None)
       | _ -> None)
  | _ -> None

let meld_to_json meld = `List (List.map tile_to_json meld)

let meld_of_json = function
  | `List tiles -> 
      let parsed = List.filter_map tile_of_json tiles in
      if List.length parsed = List.length tiles then Some parsed else None
  | _ -> None

let player_to_json player =
  let hand_list = TileMultiset.to_list player.hand in
  `Assoc [
    ("name", `String player.name);
    ("hand", `List (List.map tile_to_json hand_list));
    ("met_initial_30", `Bool player.met_initial_30);
  ]

let player_of_json = function
  | `Assoc fields ->
      (match List.assoc_opt "name" fields,
             List.assoc_opt "hand" fields,
             List.assoc_opt "met_initial_30" fields with
       | Some (`String name),
         Some (`List hand_json),
         Some (`Bool met_initial_30) ->
           (match List.filter_map tile_of_json hand_json with
            | hand_list when List.length hand_list = List.length hand_json ->
                Some {
                  name;
                  hand = TileMultiset.of_list hand_list;
                  met_initial_30;
                }
            | _ -> None)
       | _ -> None)
  | _ -> None

let state_to_json state =
  `Assoc [
    ("deck", `List (List.map tile_to_json state.deck));
    ("board", `List (List.map meld_to_json state.board));
    ("players", `List (Array.to_list state.players |> List.map player_to_json));
    ("turn", `Int state.turn);
  ]

let state_of_json = function
  | `Assoc fields ->
      (match List.assoc_opt "deck" fields,
             List.assoc_opt "board" fields,
             List.assoc_opt "players" fields,
             List.assoc_opt "turn" fields with
       | Some (`List deck_json),
         Some (`List board_json),
         Some (`List players_json),
         Some (`Int turn) ->
           (match List.filter_map tile_of_json deck_json,
                  List.filter_map meld_of_json board_json,
                  List.filter_map player_of_json players_json with
            | deck, board, players 
              when List.length deck = List.length deck_json
                && List.length board = List.length board_json
                && List.length players = List.length players_json ->
                Some {
                  deck;
                  board;
                  players = Array.of_list players;
                  turn;
                }
            | _ -> None)
       | _ -> None)
  | _ -> None

(* Convert JSON to/from JavaScript objects for Firestore *)
module Js = Js_of_ocaml.Js

let json_to_js_value json =
  let rec convert = function
    | `String s -> Js.string s |> Js.Unsafe.inject
    | `Int i -> Js.number_of_float (float_of_int i) |> Js.Unsafe.inject
    | `Bool b -> Js.bool b |> Js.Unsafe.inject
    | `List l -> 
        let arr = Js.array (Array.of_list (List.map convert l)) in
        Js.Unsafe.inject arr
    | `Assoc assoc ->
        let obj = Js.Unsafe.obj [||] in
        List.iter (fun (k, v) ->
          Js.Unsafe.set obj (Js.string k) (convert v)
        ) assoc;
        Js.Unsafe.inject obj
    | `Null -> Js.Unsafe.inject Js.null
    | `Float f -> Js.number_of_float f |> Js.Unsafe.inject
  in
  convert json

let js_value_to_json js_val =
  let rec convert v =
    let v_type = Js.to_string (Js.typeof v) in
    if v_type = "string" then
      `String (Js.to_string (Js.Unsafe.coerce v))
    else if v_type = "number" then
      `Float (Js.float_of_number (Js.Unsafe.coerce v))
    else if v_type = "boolean" then
      `Bool (Js.to_bool (Js.Unsafe.coerce v))
    else if v_type = "object" then
      let array_global = Js.Unsafe.get Js.Unsafe.global "Array" in
      if Js.instanceof v array_global then
        let arr = Js.to_array v in
        `List (Array.to_list arr |> List.map convert)
      else if (try Js.to_bool (Js.Unsafe.get v "data") with _ -> false) then
        (* Firestore DocumentSnapshot - extract data() *)
        let data = Js.Unsafe.meth_call v "data" [||] in
        convert data
      else
        (* Plain object *)
        let obj = Js.Unsafe.coerce v in
        let object_global = Js.Unsafe.get Js.Unsafe.global "Object" in
        let keys = Js.Unsafe.meth_call object_global "keys" [|obj|] in
        let keys_arr = Js.to_array keys in
        let assoc = Array.to_list keys_arr |> List.map (fun k ->
          let key_str = Js.to_string k in
          let value = Js.Unsafe.get obj k in
          (key_str, convert value)
        ) in
        `Assoc assoc
    else
      `Null
  in
  convert js_val

let state_to_firestore state =
  state_to_json state |> json_to_js_value

let state_of_firestore js_val =
  js_value_to_json js_val |> state_of_json

