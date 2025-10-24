open Rummikub

(* Test states for Rummikub game *)

(* State 1: Initial game state *)
let initial_state : State.t =
  let rng = Random.State.make [|42|] in
  State.initial_state rng

(* State 2: After first valid play (initial 30 points) *)
let state_after_first_play : State.t =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  (* Create a player with a valid initial meld *)
  let open Tile in
  let p0 = { State.name="Player1"
           ; hand = State.TileMultiset.of_list [Tile(Red,10); Tile(Red,11); Tile(Red,12)]
           ; met_initial_30=false } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  match Rules.apply_play st [[Tile(Red,10); Tile(Red,11); Tile(Red,12)]] with
  | Ok st' -> st'
  | Error _ -> st

(* State 3: Near endgame with one player almost winning *)
let near_endgame_state : State.t =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let open Tile in
  (* Player 1 has only 2 tiles left *)
  let p0 = { State.name="Player1"
           ; hand = State.TileMultiset.of_list [Tile(Red,1); Tile(Blue,1)]
           ; met_initial_30=true } in
  (* Player 2 has many tiles *)
  let p1 = { State.name="Player2"
           ; hand = State.TileMultiset.of_list 
               [Tile(Red,2); Tile(Red,3); Tile(Red,4); Tile(Blue,2); Tile(Blue,3); Tile(Blue,4)]
           ; met_initial_30=true } in
  let st = { st with players=[| p0; p1 |]; turn=0 } in
  st

(* State 4: Game over state *)
let game_over_state : State.t =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let open Tile in
  (* Player 1 has no tiles (winner) *)
  let p0 = { State.name="Player1"
           ; hand = State.TileMultiset.empty
           ; met_initial_30=true } in
  (* Player 2 has remaining tiles *)
  let p1 = { State.name="Player2"
           ; hand = State.TileMultiset.of_list 
               [Tile(Red,2); Tile(Red,3); Tile(Blue,2); Tile(Blue,3)]
           ; met_initial_30=true } in
  let st = { st with players=[| p0; p1 |]; turn=0 } in
  st

(* State 5: State with jokers in hand *)
let state_with_jokers : State.t =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let open Tile in
  let p0 = { State.name="Player1"
           ; hand = State.TileMultiset.of_list [Joker; Tile(Red,5); Tile(Red,7)]
           ; met_initial_30=false } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  st

(* Test functions for each state *)

let test_initial_state () =
  Alcotest.(check int) "initial deck size" 54 (List.length initial_state.deck);
  Alcotest.(check int) "initial board empty" 0 (List.length initial_state.board);
  Alcotest.(check int) "initial turn" 0 initial_state.turn;
  Alcotest.(check int) "initial players count" 2 (Array.length initial_state.players);
  Alcotest.(check bool) "initial game not over" false (Rules.is_game_over initial_state)

let test_state_after_first_play () =
  Alcotest.(check int) "board has one meld" 1 (List.length state_after_first_play.board);
  Alcotest.(check bool) "player met initial 30" true state_after_first_play.players.(0).met_initial_30;
  Alcotest.(check bool) "board is valid" true (Rules.board_valid state_after_first_play.board)

let test_near_endgame_state () =
  Alcotest.(check int) "player1 hand size" 2 
    (List.length (State.TileMultiset.to_list near_endgame_state.players.(0).hand));
  Alcotest.(check int) "player2 hand size" 6 
    (List.length (State.TileMultiset.to_list near_endgame_state.players.(1).hand));
  Alcotest.(check bool) "game not over yet" false (Rules.is_game_over near_endgame_state)

let test_game_over_state () =
  Alcotest.(check bool) "game is over" true (Rules.is_game_over game_over_state);
  Alcotest.(check int) "winner hand empty" 0 
    (List.length (State.TileMultiset.to_list game_over_state.players.(0).hand));
  Alcotest.(check string) "winner name" "Player1" 
    (match Rules.get_winner game_over_state with
     | Some winner -> winner.name
     | None -> "No winner")

let test_state_with_jokers () =
  let hand = State.TileMultiset.to_list state_with_jokers.players.(0).hand in
  Alcotest.(check bool) "hand contains joker" true (List.mem Tile.Joker hand);
  Alcotest.(check int) "hand size" 3 (List.length hand);
  (* Test that joker can be used in a valid meld *)
  let open Tile in
  let meld = [Joker; Tile(Red,5); Tile(Red,7)] in
  Alcotest.(check bool) "joker meld is valid" true (Meld.is_meld meld)

let test_state_transitions () =
  (* Test that we can transition from initial to first play *)
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let open Tile in
  let p0 = { State.name="Player1"
           ; hand = State.TileMultiset.of_list [Tile(Red,10); Tile(Red,11); Tile(Red,12)]
           ; met_initial_30=false } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  match Rules.apply_play st [[Tile(Red,10); Tile(Red,11); Tile(Red,12)]] with
  | Ok st' ->
      Alcotest.(check bool) "transition successful" true true;
      Alcotest.(check int) "board updated" 1 (List.length st'.board);
      Alcotest.(check bool) "player met initial 30" true st'.players.(0).met_initial_30
  | Error e -> Alcotest.fail ("Transition failed: " ^ e)

let test_illegal_state_transitions () =
  (* Test that invalid moves are rejected *)
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let open Tile in
  let p0 = { State.name="Player1"
           ; hand = State.TileMultiset.of_list [Tile(Red,1); Tile(Red,2); Tile(Red,3)]
           ; met_initial_30=false } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  (* Try to play insufficient points *)
  match Rules.apply_play st [[Tile(Red,1); Tile(Red,2); Tile(Red,3)]] with
  | Ok _ -> Alcotest.fail "Should have failed - insufficient points"
  | Error e -> Alcotest.(check string) "error message" "Initial meld total < 30" e

let suite = [
  "initial_state", `Quick, test_initial_state;
  "state_after_first_play", `Quick, test_state_after_first_play;
  "near_endgame_state", `Quick, test_near_endgame_state;
  "game_over_state", `Quick, test_game_over_state;
  "state_with_jokers", `Quick, test_state_with_jokers;
  "state_transitions", `Quick, test_state_transitions;
  "illegal_state_transitions", `Quick, test_illegal_state_transitions;
]

let () = Alcotest.run "rummikub_states" [ "states", suite ]
