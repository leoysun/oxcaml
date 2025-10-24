open Meld
open State

let board_valid b = List.for_all Meld.is_meld b

let remove_meld_from_hand (h:State.TileMultiset.t) (m:meld) =
  let open State.TileMultiset in
  let rec step h = function
    | [] -> Some h
    | t::ts -> Option.bind (remove_one t h) (fun h' -> step h' ts)
  in step h m

let initial_30_ok melds =
  let pts = List.fold_left (fun s m -> s + Meld.meld_points_for_initial m) 0 melds in
  pts >= 30 && List.for_all Meld.is_meld melds

let apply_play st melds =
  let p = st.players.(st.turn) in
  if not (List.for_all Meld.is_meld melds) then Error "Invalid meld(s)" else
  let passes_initial =
    if p.met_initial_30 then true else initial_30_ok melds
  in
  if not passes_initial then Error "Initial meld total < 30" else
  (* remove from hand *)
  let new_hand_opt =
    List.fold_left
      (fun acc m -> Option.bind acc (fun h -> remove_meld_from_hand h m))
      (Some p.hand) melds
  in
  match new_hand_opt with
  | None -> Error "Tried to play tiles you don't have"
  | Some h' ->
    let p' = { p with hand = h'; met_initial_30 = p.met_initial_30 || initial_30_ok melds } in
    let players' = Array.copy st.players in
    players'.(st.turn) <- p';
    let b' = st.board @ melds in
    if not (board_valid b') then Error "Board invalid after play"
    else Ok { st with board = b' }

let apply_draw st =
  match st.deck with
  | [] -> Ok st
  | t::rest ->
    let p = st.players.(st.turn) in
    let p' = { p with hand = State.TileMultiset.add t p.hand } in
    let players' = Array.copy st.players in
    players'.(st.turn) <- p';
    Ok { st with deck = rest }

let next_turn st = { st with turn = (st.turn + 1) mod Array.length st.players }

let is_game_over st =
  Array.exists (fun p -> State.TileMultiset.to_list p.hand = []) st.players

let get_winner st =
  if is_game_over st then
    let rec find_winner i =
      if i >= Array.length st.players then None
      else if State.TileMultiset.to_list st.players.(i).hand = [] then Some st.players.(i)
      else find_winner (i + 1)
    in find_winner 0
  else None
