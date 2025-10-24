(* Main Rummikub library module *)

module Tile = struct
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
end

module Meld = struct
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

  let meld_points_for_initial = meld_points
end

module State = struct
  open Tile
  open Meld

  module TileOrd = struct type t = tile let compare = Tile.compare_tile end
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
end

module Rules = struct
  open Tile
  open Meld
  open State

  let board_valid b = List.for_all is_meld b

  let remove_meld_from_hand (h:TileMultiset.t) (m:meld) =
    let open TileMultiset in
    let rec step h = function
      | [] -> Some h
      | t::ts -> Option.bind (remove_one t h) (fun h' -> step h' ts)
    in step h m

  let initial_30_ok melds =
    let pts = List.fold_left (fun s m -> s + meld_points_for_initial m) 0 melds in
    pts >= 30 && List.for_all is_meld melds

  let validate_meld m =
    let n = List.length m in
    if n < 3 then Error (Printf.sprintf "Meld too short: need at least 3 tiles, got %d" n)
    else if n > 13 then Error (Printf.sprintf "Meld too long: max 13 tiles, got %d" n)
    else
      (* Check if it's a valid group *)
      let is_valid_group = is_group m in
      let is_valid_run = is_run m in
      if is_valid_group || is_valid_run then Ok ()
      else
        (* Provide detailed error *)
        let colors = List.filter_map (function Tile (c, _) -> Some c | Joker -> None) m in
        let ranks = List.filter_map (function Tile (_, r) -> Some r | Joker -> None) m in
        let unique_colors = List.sort_uniq Stdlib.compare colors in
        let unique_ranks = List.sort_uniq Stdlib.compare ranks in
        
        if List.length unique_ranks = 1 then
          (* Trying for a group *)
          if List.length unique_colors < List.length (List.filter (function Tile _ -> true | _ -> false) m) then
            Error "Group: Each tile must have a different color"
          else if n > 4 then
            Error "Group: Maximum 4 tiles (one of each color)"
          else
            Error "Group: All tiles must have the same rank with different colors"
        else if List.length unique_colors = 1 then
          (* Trying for a run *)
          let sorted_ranks = List.sort Int.compare ranks in
          let rec check_consecutive = function
            | [] | [_] -> true
            | a :: b :: rest ->
                if b = a then false  (* Duplicate rank *)
                else if b <> a + 1 && b <> a + 2 then false  (* Gap too large *)
                else check_consecutive (b :: rest)
          in
          let has_dup ranks =
            let sorted = List.sort Int.compare ranks in
            let rec check = function
              | [] | [_] -> false
              | a :: b :: rest -> a = b || check (b :: rest)
            in check sorted
          in
          if has_dup ranks then
            Error "Run: Cannot have duplicate ranks"
          else if not (check_consecutive sorted_ranks) then
            let gaps = List.fold_left (fun acc r ->
              match acc with
              | [] -> [r]
              | prev :: _ -> if r - prev > 1 then r :: acc else acc
            ) [] sorted_ranks in
            Error (Printf.sprintf "Run: Ranks must be consecutive (gaps at: %s)" 
              (String.concat ", " (List.map Int.to_string gaps)))
          else if (match List.rev sorted_ranks with [] -> false | max_rank :: _ -> max_rank > 13) then
            Error "Run: Ranks cannot exceed 13"
          else
            Error "Run: All tiles must be same color with consecutive ranks"
        else
          Error "Meld must be either a Group (same rank, different colors) or a Run (same color, consecutive ranks)"

  (* New version that supports table manipulation *)
  let apply_play_with_table_manipulation st new_board tiles_from_hand =
    let p = st.players.(st.turn) in
    
    (* Validate all melds in the new board *)
    let validation_errors = List.filter_map (fun m ->
      match validate_meld m with
      | Ok () -> None
      | Error msg -> Some msg
    ) new_board in
    if validation_errors <> [] then
      Error (String.concat "\n" validation_errors)
    else
    
    (* Get all tiles from old board and new board *)
    let old_board_tiles = List.concat st.board in
    let new_board_tiles = List.concat new_board in
    
    (* Calculate tiles added to board (tiles in new board but not in old board) *)
    let tiles_added = 
      let rec remove_tiles_from_list tiles_to_remove from_list =
        match tiles_to_remove with
        | [] -> from_list
        | t :: rest ->
            let from_list' = 
              let rec remove_one tile = function
                | [] -> []
                | x :: xs -> if Tile.compare_tile x tile = 0 then xs else x :: remove_one tile xs
              in remove_one t from_list
            in
            remove_tiles_from_list rest from_list'
      in
      remove_tiles_from_list old_board_tiles new_board_tiles
    in
    
    (* Verify tiles added match tiles from hand *)
    let tiles_added_sorted = List.sort Tile.compare_tile tiles_added in
    let tiles_from_hand_sorted = List.sort Tile.compare_tile tiles_from_hand in
    if tiles_added_sorted <> tiles_from_hand_sorted then
      Error "Tiles on board don't match: you must only add tiles from your hand"
    else
    
    (* Check initial 30 rule for first play *)
    let passes_initial =
      if p.met_initial_30 then true 
      else if List.is_empty tiles_from_hand then false  (* Must play something on first turn *)
      else
        (* Calculate points from newly played tiles *)
        let new_melds = List.filter (fun meld ->
          List.exists (fun tile ->
            List.exists (fun hand_tile -> Tile.compare_tile tile hand_tile = 0) tiles_from_hand
          ) meld
        ) new_board in
        initial_30_ok new_melds
    in
    if not passes_initial then Error "Initial meld total < 30 points" else
    
    (* Remove tiles from hand *)
    let new_hand_opt = 
      List.fold_left
        (fun acc tile -> Option.bind acc (fun h -> TileMultiset.remove_one tile h))
        (Some p.hand) tiles_from_hand
    in
    match new_hand_opt with
    | None -> Error "Tried to play tiles you don't have in your hand"
    | Some h' ->
      let p' = { p with hand = h'; met_initial_30 = p.met_initial_30 || passes_initial } in
      let players' = Array.copy st.players in
      players'.(st.turn) <- p';
      Ok { st with board = new_board; players = players' }
  
  (* Legacy version - just add melds to board (for backward compatibility) *)
  let apply_play st melds =
    let tiles_in_melds = List.concat melds in
    let new_board = st.board @ melds in
    apply_play_with_table_manipulation st new_board tiles_in_melds

  let apply_draw st =
    match st.deck with
    | [] -> Error "No tiles left in deck"
    | t::rest ->
      let p = st.players.(st.turn) in
      let p' = { p with hand = TileMultiset.add t p.hand } in
      let players' = Array.copy st.players in
      players'.(st.turn) <- p';
      Ok { st with deck = rest; players = players' }

  let next_turn st = { st with turn = (st.turn + 1) mod Array.length st.players }

  let is_game_over st =
    Array.exists (fun p -> TileMultiset.to_list p.hand = []) st.players

  let get_winner st =
    if is_game_over st then
      let rec find_winner i =
        if i >= Array.length st.players then None
        else if TileMultiset.to_list st.players.(i).hand = [] then Some st.players.(i)
        else find_winner (i + 1)
      in find_winner 0
    else None
end

module AI = struct
  open Tile
  open Meld
  open State
  open Rules

  let all_candidate_melds_from_hand (h:TileMultiset.t) : meld list =
    let tiles = TileMultiset.to_list h in
    (* naive enumeration: all subsets up to size 4 for groups, size up to 13 for runs by color *)
    let rec subsets_of_size k xs =
      if k=0 then [ [] ] else
      match xs with
      | [] -> []
      | y::ys ->
          let with_y = List.map (fun s -> y::s) (subsets_of_size (k-1) ys) in
          let without = subsets_of_size k ys in
          with_y @ without
    in
    let groups =
      List.concat_map (fun k -> subsets_of_size k tiles) [3;4]
      |> List.filter is_group
    in
    let by_color c =
      List.filter (function Tile(c',_) when c'=c -> true | Joker -> true | _ -> false) tiles
    in
  let runs =
    List.concat_map (fun c ->
      let cs = by_color c in
      List.concat_map (fun k -> subsets_of_size k cs) (List.init 11 (fun i->i+3))
      |> List.filter is_run) all_colors
    in
    (* de-dup roughly by sorting tiles as strings *)
    let norm m =
      m |> List.map pp_tile |> List.sort String.compare |> String.concat "," in
    let tbl = Hashtbl.create 97 in
    let add_unique acc m =
      let key = norm m in
      if Hashtbl.mem tbl key then acc else (Hashtbl.add tbl key (); m::acc)
    in
    List.fold_left add_unique [] (groups @ runs)

  let random_move st =
    let p = st.players.(st.turn) in
    let melds = all_candidate_melds_from_hand p.hand in
    let legal =
      List.filter_map (fun m ->
        match apply_play st [m] with
        | Ok _ ->
            if p.met_initial_30 || initial_30_ok [m] then Some m else None
        | Error _ -> None)
        melds
    in
    match legal with
    | [] -> Draw
    | ms ->
        let best = List.hd (List.sort (fun a b -> compare (meld_points b) (meld_points a)) ms) in
        Play [best]

  let smart_move st =
    let p = st.players.(st.turn) in
    let melds = all_candidate_melds_from_hand p.hand in
    let legal =
      List.filter_map (fun m ->
        match apply_play st [m] with
        | Ok _ ->
            if p.met_initial_30 || initial_30_ok [m] then Some m else None
        | Error _ -> None)
        melds
    in
    match legal with
    | [] -> Draw
    | ms ->
        (* Try to find the best combination of melds *)
        let rec find_best_combination remaining_melds current_melds =
          if List.length current_melds >= 3 then current_melds else
          match remaining_melds with
          | [] -> current_melds
          | m::rest ->
              let new_combination = m::current_melds in
              match apply_play st new_combination with
              | Ok _ -> find_best_combination rest new_combination
              | Error _ -> find_best_combination rest current_melds
        in
        let best_combination = find_best_combination ms [] in
        if best_combination = [] then
          let best = List.hd (List.sort (fun a b -> compare (meld_points b) (meld_points a)) ms) in
          Play [best]
        else
          Play best_combination
end
