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

let suite =
  [ "initial-30", `Quick, test_initial_30 ]

let () = Alcotest.run "rules" [ "rules", suite ]
