open Rummikub

let print_tile = function
  | Tile.Tile (Tile.Red, r) -> Printf.sprintf "R%02d" r
  | Tile.Tile (Tile.Blue, r) -> Printf.sprintf "B%02d" r
  | Tile.Tile (Tile.Black, r) -> Printf.sprintf "K%02d" r
  | Tile.Tile (Tile.Orange, r) -> Printf.sprintf "O%02d" r
  | Tile.Joker -> "Jk"

let print_meld meld =
  "[" ^ (String.concat " " (List.map print_tile meld)) ^ "]"

let print_board board =
  if List.is_empty board then
    print_endline "Table: (empty)"
  else
    print_endline ("Table: " ^ String.concat " " (List.map print_meld board))

let print_hand hand =
  let tiles = State.TileMultiset.to_list hand in
  let tile_strings = List.mapi (fun i tile -> Printf.sprintf "%d:%s" i (print_tile tile)) tiles in
  print_endline ("Hand: " ^ String.concat " " tile_strings)

let parse_tile s =
  if s = "Jk" then Tile.Joker else
  let color = match s.[0] with
    | 'R' -> Tile.Red | 'B' -> Tile.Blue | 'K' -> Tile.Black | 'O' -> Tile.Orange
    | _ -> failwith "Invalid color"
  in
  let rank = int_of_string (String.sub s 1 (String.length s - 1)) in
  Tile.Tile (color, rank)

let rec game_loop st =
  let current = st.players.(st.turn) in
  Printf.printf "\n=== %s's turn ===\n" current.name;
  print_board st.board;
  
  if current.name = "You" then begin
    print_hand current.hand;
    print_endline "\nCommands:";
    print_endline "  'draw' - draw a tile";
    print_endline "  'pass' - pass turn";
    print_endline "  'play <tile1> <tile2> ...' - play a meld";
    print_endline "  'quit' - quit game";
    print_string "> ";
    let line = read_line () in
    
    if line = "quit" then
      print_endline "Thanks for playing!"
    else if line = "draw" then
      match Rules.apply_draw st with
      | Ok st' -> game_loop (Rules.next_turn st')
      | Error e -> print_endline ("Error: " ^ e); game_loop st
    else if line = "pass" then
      game_loop (Rules.next_turn st)
    else if String.starts_with line ~prefix:"play " then
      let tiles_str = String.sub line 5 (String.length line - 5) in
      let tile_strings = String.split_on_char ' ' tiles_str |> List.filter ((<>) "") in
      try
        let meld = List.map parse_tile tile_strings in
        match Rules.apply_play st [meld] with
        | Ok st' -> 
            print_endline ("Played: " ^ print_meld meld);
            game_loop (Rules.next_turn st')
        | Error e -> print_endline ("Error: " ^ e); game_loop st
      with
      | _ -> print_endline "Invalid tile format"; game_loop st
    else
      print_endline "Invalid command"; game_loop st
  end else begin
    (* AI turn *)
    print_endline "Bot is thinking...";
    let move = AI.random_move st in
    match move with
    | State.Draw ->
        print_endline "Bot draws a tile.";
        let st' = match Rules.apply_draw st with Ok s -> s | Error _ -> st in
        game_loop (Rules.next_turn st')
    | State.Play melds ->
        print_endline ("Bot plays: " ^ String.concat " " (List.map print_meld melds));
        let st' = match Rules.apply_play st melds with Ok s -> s | Error _ -> st in
        game_loop (Rules.next_turn st')
    | State.Pass -> 
        print_endline "Bot passes.";
        game_loop (Rules.next_turn st)
  end

let () =
  Random.self_init ();
  let rng = Random.State.make_self_init () in
  let st = State.initial_state rng in
  print_endline "Welcome to Rummikub!";
  print_endline "You are Player 1, Bot is Player 2.";
  print_endline "First play must total at least 30 points.";
  game_loop st
