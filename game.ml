(* ---------- Tile ---------- *)
type color = Red | Blue | Black | Orange
type rank = int  (* 1..13 *)
type tile =
  | Tile of color * rank
  | Joker

let all_colors = [Red; Blue; Black; Orange]

let deck () : tile list =
  let copies = 2 in
  let base =
    List.concat_map all_colors (fun c ->
      List.init 13 (fun i -> Tile (c, i+1)))
  in
  let tiles = List.concat (List.init copies (fun _ -> base)) @ [Joker; Joker] in
  tiles

let string_of_color = function
  | Red -> "R" | Blue -> "B" | Black -> "K" | Orange -> "O"

let string_of_tile = function
  | Tile (c, r) -> Printf.sprintf "%s%02d" (string_of_color c) r
  | Joker -> "Jk"

(* Fisher–Yates shuffle *)
let shuffle (rng: Random.State.t) (xs: 'a array) =
  for i = Array.length xs - 1 downto 1 do
    let j = Random.State.int rng (i+1) in
    let tmp = xs.(i) in
    xs.(i) <- xs.(j); xs.(j) <- tmp
  done

(* ---------- Meld ---------- *)
type meld = tile list

let is_group (m: meld) : bool =
  let tiles = List.filter (fun t -> t <> Joker) m in
  let jokers = List.length m - List.length tiles in
  if List.length m < 3 || List.length m > 4 then false else
  match tiles with
  | [] -> false  (* group of only jokers not allowed *)
  | Tile (_, r0) :: _ ->
      let same_rank =
        List.for_all (function Tile(_,r)-> r=r0 | Joker-> true) tiles in
      let colors =
        List.filter_map (function Tile(c,_) -> Some c | _ -> None) tiles in
      let unique_colors =
        List.sort_uniq compare colors in
      same_rank && (List.length unique_colors + jokers = List.length m)
  | _ -> false

let is_run (m: meld) : bool =
  let tiles = List.filter (fun t -> t <> Joker) m in
  let jokers = List.length m - List.length tiles in
  if List.length m < 3 then false else
  match tiles with
  | [] -> false
  | Tile (c0, _) :: _ ->
      let same_color =
        List.for_all (function Tile(c,_) -> c=c0 | Joker-> true) tiles in
      if not same_color then false else
      let ranks =
        List.filter_map (function Tile(_,r)->Some r | _->None) tiles
        |> List.sort compare in
      (* Count gaps between consecutive ranks; jokers must cover them, and no duplicates *)
      let rec gaps acc = function
        | a::b::xs ->
            if a=b then max_int else gaps (acc + (b - a - 1)) (b::xs)
        | _ -> acc
      in
      let gap = gaps 0 ranks in
      gap <> max_int
      && (gap <= jokers)
      && (match ranks with
          | [] | [_] -> true
          | rmin::_ ->
              (* prevent wrap; also ensure we’re not exceeding 13 with jokers at the top *)
              rmin >= 1 && (List.hd (List.rev ranks)) + jokers <= 13)

let is_meld m = is_group m || is_run m

let meld_points (m: meld) : int =
  (* Jokers count as the value they stand for; in v1, approximate: average of neighbors or 10.
     Good enough for initial-30 check; replace with exact assignment once table manipulation is added. *)
  let ranks =
    List.filter_map (function Tile(_,r)->Some r | Joker-> None) m in
  let base = List.fold_left ( + ) 0 ranks in
  base + (List.length (List.filter ((=) Joker) m)) * 10

(* ---------- Hand ---------- *)
module TileOrd = struct
  type t = tile
  let compare = compare
end
module MSet = Map.Make(TileOrd)

type hand = int MSet.t

let hand_empty : hand = MSet.empty
let hand_count t h = match MSet.find_opt t h with Some n -> n | None -> 0
let hand_add t h = MSet.add t (hand_count t h + 1) h
let hand_remove t h =
  let n = hand_count t h in
  if n <= 0 then None else
  Some (if n=1 then MSet.remove t h else MSet.add t (n-1) h)

let hand_of_list ts = List.fold_left (fun h t -> hand_add t h) hand_empty ts
let list_of_hand h =
  MSet.bindings h |> List.concat_map (fun (t,n) -> List.init n (fun _->t))

(* ---------- Board ---------- *)
type board = meld list

let board_valid (b: board) = List.for_all is_meld b

(* ---------- Moves ---------- *)
type move =
  | Play of meld list          (* all melds played from hand this turn *)
  | Draw
  | Pass                       (* optional rule; usually draw ends turn *)

type player =
  { name : string
  ; hand : hand
  ; has_met_initial_30 : bool
  }

type state =
  { deck : tile list
  ; discard : tile list          (* unused in standard Rummikub; kept for extension *)
  ; board : board
  ; players : player array
  ; turn : int                   (* index into players *)
  }

(* ---------- Dealing & setup ---------- *)
let deal rng =
  let d = deck () |> Array.of_list in
  shuffle rng d;
  let draw i = d.(i) in
  let hand_size = 14 in
  let p1 = Array.init hand_size (fun i -> draw i) |> Array.to_list in
  let p2 = Array.init hand_size (fun i -> draw (hand_size + i)) |> Array.to_list in
  let pos = 2*hand_size in
  let rest = Array.sub d pos (Array.length d - pos) |> Array.to_list in
  { deck = rest
  ; discard = []
  ; board = []
  ; players = [|
      { name = "You"; hand = hand_of_list p1; has_met_initial_30=false };
      { name = "Bot"; hand = hand_of_list p2; has_met_initial_30=false };
    |]
  ; turn = 0
  }

(* ---------- Hand/move legality (v1: only plays from hand, no table rearrangement) ---------- *)
let can_play_initial_30 melds =
  let pts = List.fold_left (fun s m -> s + meld_points m) 0 melds in
  pts >= 30 && List.for_each ignore (List.map (fun m -> is_meld m) melds) = ()

let remove_meld_from_hand (h:hand) (m:meld) : hand option =
  let rec step h = function
    | [] -> Some h
    | t::ts ->
        begin match hand_remove t h with
        | None -> None
        | Some h' -> step h' ts
        end
  in step h m

let apply_play (st:state) (melds: meld list) : (state, string) result =
  let p = st.players.(st.turn) in
  if not (List.for_all is_meld melds) then Error "Invalid meld(s)" else
  let can_play =
    if p.has_met_initial_30 then true else can_play_initial_30 melds
  in
  if not can_play then Error "Initial meld total < 30" else
  (* remove tiles from hand *)
  let new_hand_opt =
    List.fold_left
      (fun acc m -> acc |> Option.bind (fun h -> remove_meld_from_hand h m))
      (Some p.hand) melds
  in
  match new_hand_opt with
  | None -> Error "You don’t have the tiles you tried to play"
  | Some h' ->
      let p' =
        { p with
          hand = h';
          has_met_initial_30 = p.has_met_initial_30 || can_play_initial_30 melds
        }
      in
      let players' = Array.copy st.players in
      players'.(st.turn) := p';
      let b' = st.board @ melds in
      if not (board_valid b') then Error "Board invalid after play"
      else Ok { st with board = b' }

let apply_draw (st:state) : (state, string) result =
  match st.deck with
  | [] -> Ok st  (* no tile to draw; effectively pass *)
  | t::rest ->
      let p = st.players.(st.turn) in
      let p' = { p with hand = hand_add t p.hand } in
      let players' = Array.copy st.players in
      players'.(st.turn) := p';
      Ok { st with deck = rest }

let next_turn (st:state) : state =
  { st with turn = (st.turn + 1) mod Array.length st.players }

(* ---------- AI (greedy): play the single highest-scoring meld from hand; else draw ---------- *)
let all_candidate_melds_from_hand (h:hand) : meld list =
  let tiles = list_of_hand h in
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
    List.concat_map all_colors (fun c ->
      let cs = by_color c in
      List.concat_map (fun k -> subsets_of_size k cs) (List.init 11 (fun i->i+3))
      |> List.filter is_run)
  in
  (* de-dup roughly by sorting tiles as strings *)
  let norm m =
    m |> List.map string_of_tile |> List.sort String.compare |> String.concat "," in
  let tbl = Hashtbl.create 97 in
  let add_unique acc m =
    let key = norm m in
    if Hashtbl.mem tbl key then acc else (Hashtbl.add tbl key (); m::acc)
  in
  List.fold_left add_unique [] (groups @ runs)

let ai_move (st:state) : move =
  let p = st.players.(st.turn) in
  let melds = all_candidate_melds_from_hand p.hand in
  let legal =
    List.filter_map (fun m ->
      match apply_play st [m] with
      | Ok _ ->
          if p.has_met_initial_30 || can_play_initial_30 [m] then Some m else None
      | Error _ -> None)
      melds
  in
  match legal with
  | [] -> Draw
  | ms ->
      let best = List.hd (List.sort (fun a b -> compare (meld_points b) (meld_points a)) ms) in
      Play [best]

(* ---------- CLI (very simple) ---------- *)
let print_board (b:board) =
  let show m = "[" ^ (String.concat " " (List.map string_of_tile m)) ^ "]" in
  print_endline ("Table: " ^ String.concat " " (List.map show b))

let print_hand (h:hand) =
  list_of_hand h |> List.map string_of_tile |> String.concat " " |> print_endline

let rec game_loop (st:state) =
  let current = st.players.(st.turn) in
  Printf.printf "\n-- %s's turn --\n" current.name;
  print_board st.board;
  if current.name = "You" then begin
    print_endline "Your hand:"; print_hand current.hand;
    print_endline "Enter: 'draw' or a meld like 'R03 R04 R05' or 'R05 B05 K05'";
    let line = read_line () in
    if line = "draw" then
      match apply_draw st with
      | Ok st' -> game_loop (next_turn st')
      | Error e -> print_endline e; game_loop st
    else
      let parse_token s =
        if s = "Jk" then Joker else
        let c = match s.[0] with
          | 'R' -> Red | 'B' -> Blue | 'K' -> Black | 'O' -> Orange | _ -> Red in
        let r = int_of_string (String.sub s 1 (String.length s - 1)) in
        Tile (c, r)
      in
      let meld =
        line |> String.split_on_char ' ' |> List.filter ((<>) "")
             |> List.map parse_token
      in
      match apply_play st [meld] with
      | Ok st' -> game_loop (next_turn st')
      | Error e -> print_endline e; game_loop st
  end else begin
    match ai_move st with
    | Draw ->
        print_endline "Bot draws.";
        let st' = match apply_draw st with Ok s -> s | Error _ -> st in
        game_loop (next_turn st')
    | Play melds ->
        print_endline "Bot plays:";
        List.iter (fun m ->
          print_endline ("  " ^ String.concat " " (List.map string_of_tile m))) melds;
        let st' = match apply_play st melds with Ok s -> s | Error _ -> st in
        game_loop (next_turn st')
    | Pass -> game_loop (next_turn st)  (* unused in v1 *)
  end

let () =
  Random.self_init ();
  let rng = Random.State.make_self_init () in
  let st = deal rng in
  game_loop st
