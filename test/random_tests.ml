open Rummikub
open QCheck

let gen_tile =
  let open Tile in
  let gen_color = Gen.oneofl [Red;Blue;Black;Orange] in
  Gen.frequency
    [ 9, Gen.map (fun (c,r)-> Tile (c,r)) Gen.(pair gen_color (int_range 1 13));
      1, Gen.pure Joker ]

let gen_meld = Gen.(list_size (int_range 3 4) gen_tile)

let test_meld_validity =
  Test.make ~name:"meld_validity_is_sound" ~count:500
    (make gen_meld)
    (fun m -> 
      let is_valid = Meld.is_meld m in
      let is_group = Meld.is_group m in
      let is_run = Meld.is_run m in
      (* A meld is valid if it's either a group or a run *)
      is_valid = (is_group || is_run))

let test_meld_points_positive =
  Test.make ~name:"meld_points_always_positive" ~count:500
    (make gen_meld)
    (fun m -> Meld.meld_points m >= 0)

let test_initial_state_validity =
  Test.make ~name:"initial_state_is_valid" ~count:100
    (make Gen.(int_range 0 1000))
    (fun seed ->
      let rng = Random.State.make [|seed|] in
      let st = State.initial_state rng in
      (* Check that initial state is valid *)
      List.length st.deck = 54 && (* 2 copies of 26 tiles + 2 jokers *)
      Array.length st.players = 2 &&
      st.turn = 0 &&
      st.board = [] &&
      Array.for_all (fun p -> 
        List.length (State.TileMultiset.to_list p.hand) = 14 &&
        not p.met_initial_30) st.players)

let test_hand_operations =
  Test.make ~name:"hand_operations_are_consistent" ~count:200
    (make Gen.(list_size (int_range 1 20) gen_tile))
    (fun tiles ->
      let hand = State.TileMultiset.of_list tiles in
      let tiles_back = State.TileMultiset.to_list hand in
      (* Converting to list and back should preserve the multiset *)
      List.sort Tile.compare_tile tiles = List.sort Tile.compare_tile tiles_back)

let test_play_legality =
  Test.make ~name:"play_legality_is_consistent" ~count:100
    (make Gen.(int_range 0 1000))
    (fun seed ->
      let rng = Random.State.make [|seed|] in
      let st = State.initial_state rng in
      let current_player = st.players.(st.turn) in
      let hand_tiles = State.TileMultiset.to_list current_player.hand in
      
      (* Try to create a valid meld from hand tiles *)
      let rec find_valid_meld tiles =
        match tiles with
        | [] -> None
        | t1::rest ->
            (* Try groups of same rank *)
            let same_rank = List.filter (function 
              | Tile.Tile(_,r1), Tile.Tile(_,r2) when r1 = r2 -> true
              | Tile.Joker, _ | _, Tile.Joker -> true
              | _ -> false) (List.map (fun t -> (t1, t)) rest) in
            if List.length same_rank >= 2 then
              Some (t1::(List.take 2 (List.map snd same_rank)))
            else
              (* Try runs of same color *)
              let same_color = List.filter (function
                | Tile.Tile(c1,_), Tile.Tile(c2,_) when c1 = c2 -> true
                | Tile.Joker, _ | _, Tile.Joker -> true
                | _ -> false) (List.map (fun t -> (t1, t)) rest) in
              if List.length same_color >= 2 then
                Some (t1::(List.take 2 (List.map snd same_color)))
              else
                find_valid_meld rest
      in
      
      match find_valid_meld hand_tiles with
      | None -> true (* No valid meld found, which is fine *)
      | Some meld ->
          match Rules.apply_play st [meld] with
          | Ok _ -> true
          | Error _ -> 
              (* If we can't play, it should be because of initial 30 rule *)
              Meld.meld_points meld < 30)

let test_draw_operation =
  Test.make ~name:"draw_operation_is_safe" ~count:100
    (make Gen.(int_range 0 1000))
    (fun seed ->
      let rng = Random.State.make [|seed|] in
      let st = State.initial_state rng in
      let initial_deck_size = List.length st.deck in
      let initial_hand_size = List.length (State.TileMultiset.to_list st.players.(st.turn).hand) in
      
      match Rules.apply_draw st with
      | Ok st' ->
          let new_deck_size = List.length st'.deck in
          let new_hand_size = List.length (State.TileMultiset.to_list st'.players.(st'.turn).hand) in
          new_deck_size = initial_deck_size - 1 &&
          new_hand_size = initial_hand_size + 1
      | Error _ -> false)

let test_turn_progression =
  Test.make ~name:"turn_progression_is_correct" ~count:100
    (make Gen.(int_range 0 1000))
    (fun seed ->
      let rng = Random.State.make [|seed|] in
      let st = State.initial_state rng in
      let st' = Rules.next_turn st in
      st'.turn = (st.turn + 1) mod Array.length st.players)

let test_game_state_invariants =
  Test.make ~name:"game_state_invariants_are_maintained" ~count:100
    (make Gen.(int_range 0 1000))
    (fun seed ->
      let rng = Random.State.make [|seed|] in
      let st = State.initial_state rng in
      
      (* Check that all board melds are valid *)
      let board_valid = Rules.board_valid st.board in
      
      (* Check that all players have valid hands *)
      let hands_valid = Array.for_all (fun p ->
        let tiles = State.TileMultiset.to_list p.hand in
        List.for_all (fun t -> match t with
          | Tile.Tile(c,r) -> List.mem c Tile.all_colors && r >= 1 && r <= 13
          | Tile.Joker -> true) tiles) st.players in
      
      (* Check that turn index is valid *)
      let turn_valid = st.turn >= 0 && st.turn < Array.length st.players in
      
      board_valid && hands_valid && turn_valid)

let test_ai_move_safety =
  Test.make ~name:"ai_move_is_safe" ~count:50
    (make Gen.(int_range 0 1000))
    (fun seed ->
      let rng = Random.State.make [|seed|] in
      let st = State.initial_state rng in
      let move = AI.random_move st in
      
      match move with
      | State.Draw -> true
      | State.Play melds ->
          (* If AI wants to play, the move should be legal *)
          (match Rules.apply_play st melds with
           | Ok _ -> true
           | Error _ -> false)
      | State.Pass -> true)

let suite = [
  test_meld_validity;
  test_meld_points_positive;
  test_initial_state_validity;
  test_hand_operations;
  test_play_legality;
  test_draw_operation;
  test_turn_progression;
  test_game_state_invariants;
  test_ai_move_safety;
]

let () = QCheck_base_runner.run_tests_main suite
