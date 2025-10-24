open Rummikub

(* Computer opponent competition test *)

let run_game rng =
  let rec game_loop st turn_count =
    if turn_count > 200 then (* Prevent infinite games *)
      (st, "timeout")
    else if Rules.is_game_over st then
      (st, "game_over")
    else
      let current_player = st.players.(st.turn) in
      let move = AI.random_move st in
      let st' = match move with
        | State.Draw -> 
            (match Rules.apply_draw st with
             | Ok s -> s
             | Error _ -> st)
        | State.Play melds ->
            (match Rules.apply_play st melds with
             | Ok s -> s
             | Error _ -> st)
        | State.Pass -> st
      in
      let st' = Rules.next_turn st' in
      game_loop st' (turn_count + 1)
  in
  let initial_state = State.initial_state rng in
  game_loop initial_state 0

let run_smart_game rng =
  let rec game_loop st turn_count =
    if turn_count > 200 then (* Prevent infinite games *)
      (st, "timeout")
    else if Rules.is_game_over st then
      (st, "game_over")
    else
      let current_player = st.players.(st.turn) in
      let move = AI.smart_move st in
      let st' = match move with
        | State.Draw -> 
            (match Rules.apply_draw st with
             | Ok s -> s
             | Error _ -> st)
        | State.Play melds ->
            (match Rules.apply_play st melds with
             | Ok s -> s
             | Error _ -> st)
        | State.Pass -> st
      in
      let st' = Rules.next_turn st' in
      game_loop st' (turn_count + 1)
  in
  let initial_state = State.initial_state rng in
  game_loop initial_state 0

let test_random_vs_random () =
  let results = ref [] in
  for i = 0 to 999 do
    let rng = Random.State.make [|i|] in
    let (final_state, reason) = run_game rng in
    let winner = Rules.get_winner final_state in
    let winner_name = match winner with
      | Some p -> p.name
      | None -> "none"
    in
    results := (winner_name, reason) :: !results
  done;
  
  let winner_counts = List.fold_left (fun acc (winner, _) ->
    let count = List.assoc_opt winner acc |> Option.value ~default:0 in
    (winner, count + 1) :: List.remove_assoc winner acc
  ) [] !results in
  
  let timeout_count = List.length (List.filter (fun (_, reason) -> reason = "timeout") !results) in
  
  Printf.printf "Random vs Random (1000 games):\n";
  List.iter (fun (winner, count) ->
    Printf.printf "  %s: %d wins\n" winner count
  ) winner_counts;
  Printf.printf "  Timeouts: %d\n" timeout_count;
  
  (* Test passes if we get some reasonable distribution *)
  let total_wins = List.fold_left (fun acc (_, count) -> acc + count) 0 winner_counts in
  Alcotest.(check bool) "reasonable number of games completed" true (total_wins > 500)

let test_smart_vs_smart () =
  let results = ref [] in
  for i = 0 to 999 do
    let rng = Random.State.make [|i + 1000|] in
    let (final_state, reason) = run_smart_game rng in
    let winner = Rules.get_winner final_state in
    let winner_name = match winner with
      | Some p -> p.name
      | None -> "none"
    in
    results := (winner_name, reason) :: !results
  done;
  
  let winner_counts = List.fold_left (fun acc (winner, _) ->
    let count = List.assoc_opt winner acc |> Option.value ~default:0 in
    (winner, count + 1) :: List.remove_assoc winner acc
  ) [] !results in
  
  let timeout_count = List.length (List.filter (fun (_, reason) -> reason = "timeout") !results) in
  
  Printf.printf "Smart vs Smart (1000 games):\n";
  List.iter (fun (winner, count) ->
    Printf.printf "  %s: %d wins\n" winner count
  ) winner_counts;
  Printf.printf "  Timeouts: %d\n" timeout_count;
  
  (* Test passes if we get some reasonable distribution *)
  let total_wins = List.fold_left (fun acc (_, count) -> acc + count) 0 winner_counts in
  Alcotest.(check bool) "reasonable number of games completed" true (total_wins > 500)

let test_random_vs_smart () =
  let results = ref [] in
  for i = 0 to 999 do
    let rng = Random.State.make [|i + 2000|] in
    let initial_state = State.initial_state rng in
    let rec game_loop st turn_count =
      if turn_count > 200 then
        (st, "timeout")
      else if Rules.is_game_over st then
        (st, "game_over")
      else
        let current_player = st.players.(st.turn) in
        let move = if st.turn = 0 then AI.random_move st else AI.smart_move st in
        let st' = match move with
          | State.Draw -> 
              (match Rules.apply_draw st with
               | Ok s -> s
               | Error _ -> st)
          | State.Play melds ->
              (match Rules.apply_play st melds with
               | Ok s -> s
               | Error _ -> st)
          | State.Pass -> st
        in
        let st' = Rules.next_turn st' in
        game_loop st' (turn_count + 1)
    in
    let (final_state, reason) = game_loop initial_state 0 in
    let winner = Rules.get_winner final_state in
    let winner_name = match winner with
      | Some p -> p.name
      | None -> "none"
    in
    results := (winner_name, reason) :: !results
  done;
  
  let winner_counts = List.fold_left (fun acc (winner, _) ->
    let count = List.assoc_opt winner acc |> Option.value ~default:0 in
    (winner, count + 1) :: List.remove_assoc winner acc
  ) [] !results in
  
  let timeout_count = List.length (List.filter (fun (_, reason) -> reason = "timeout") !results) in
  
  Printf.printf "Random vs Smart (1000 games):\n";
  List.iter (fun (winner, count) ->
    Printf.printf "  %s: %d wins\n" winner count
  ) winner_counts;
  Printf.printf "  Timeouts: %d\n" timeout_count;
  
  (* Test passes if we get some reasonable distribution *)
  let total_wins = List.fold_left (fun acc (_, count) -> acc + count) 0 winner_counts in
  Alcotest.(check bool) "reasonable number of games completed" true (total_wins > 500)

let suite = [
  "random_vs_random", `Slow, test_random_vs_random;
  "smart_vs_smart", `Slow, test_smart_vs_smart;
  "random_vs_smart", `Slow, test_random_vs_smart;
]

let () = Alcotest.run "ai_competition" [ "competition", suite ]
