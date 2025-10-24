open Rummikub

let test_initial_30 () =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  (* Construct a synthetic state for a deterministic test *)
  let open Tile in
  let p0 = { State.name="P0"
           ; hand = State.TileMultiset.of_list [Tile(Red,10); Tile(Red,9); Tile(Red,11)]
           ; met_initial_30=false } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  match Rules.apply_play st [[Tile(Red,9); Tile(Red,10); Tile(Red,11)]] with
  | Ok st' ->
      Alcotest.(check bool) "board valid" true (Rules.board_valid st'.board);
      Alcotest.(check bool) "met init" true st'.players.(0).met_initial_30
  | Error e -> Alcotest.fail e

let test_invalid_meld () =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let open Tile in
  let p0 = { State.name="P0"
           ; hand = State.TileMultiset.of_list [Tile(Red,10); Tile(Red,9); Tile(Blue,11)]
           ; met_initial_30=false } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  match Rules.apply_play st [[Tile(Red,9); Tile(Red,10); Tile(Blue,11)]] with
  | Ok _ -> Alcotest.fail "Should have failed - invalid meld"
  | Error e -> Alcotest.(check string) "error message" "Invalid meld(s)" e

let test_insufficient_initial_points () =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let open Tile in
  let p0 = { State.name="P0"
           ; hand = State.TileMultiset.of_list [Tile(Red,1); Tile(Red,2); Tile(Red,3)]
           ; met_initial_30=false } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  match Rules.apply_play st [[Tile(Red,1); Tile(Red,2); Tile(Red,3)]] with
  | Ok _ -> Alcotest.fail "Should have failed - insufficient points"
  | Error e -> Alcotest.(check string) "error message" "Initial meld total < 30" e

let test_draw_tile () =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let initial_deck_size = List.length st.deck in
  match Rules.apply_draw st with
  | Ok st' ->
      Alcotest.(check int) "deck size decreased" (initial_deck_size - 1) (List.length st'.deck);
      Alcotest.(check int) "hand size increased" 
        (List.length (State.TileMultiset.to_list st.players.(st.turn).hand) + 1)
        (List.length (State.TileMultiset.to_list st'.players.(st'.turn).hand))
  | Error e -> Alcotest.fail e

let test_game_over_detection () =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let p0 = { State.name="P0"
           ; hand = State.TileMultiset.empty
           ; met_initial_30=true } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  Alcotest.(check bool) "game over" true (Rules.is_game_over st)

let test_winner_detection () =
  let rng = Random.State.make [|42|] in
  let st = State.initial_state rng in
  let p0 = { State.name="P0"
           ; hand = State.TileMultiset.empty
           ; met_initial_30=true } in
  let st = { st with players=[| p0; st.players.(1) |] } in
  match Rules.get_winner st with
  | Some winner -> Alcotest.(check string) "winner name" "P0" winner.name
  | None -> Alcotest.fail "Should have found a winner"

let suite =
  [ "initial-30", `Quick, test_initial_30;
    "invalid-meld", `Quick, test_invalid_meld;
    "insufficient-initial-points", `Quick, test_insufficient_initial_points;
    "draw-tile", `Quick, test_draw_tile;
    "game-over-detection", `Quick, test_game_over_detection;
    "winner-detection", `Quick, test_winner_detection ]

let () = Alcotest.run "rules" [ "rules", suite ]
