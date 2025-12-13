open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Rummikub
(* Don't open Firebase/Auth/Firestore here to avoid namespace conflicts with Bonsai type inference *)
(* We'll use fully qualified names: Firebase.xxx, Auth.xxx, Firestore.xxx *)

(* js_of_ocaml for DOM event handling *)
module Js = Js_of_ocaml.Js
module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html

(* Helper to create drag/drop attrs that prevent default *)
let on_dragover_prevent_default effect =
  Vdom.Attr.on_dragover (fun evt ->
    (* Prevent default behavior to allow drop *)
    let js_evt = Js.Unsafe.coerce evt in
    ignore (Js.Unsafe.meth_call js_evt "preventDefault" [||]);
    effect
  )

let on_drop_prevent_default effect =
  Vdom.Attr.on_drop (fun evt ->
    (* Prevent default behavior *)
    let js_evt = Js.Unsafe.coerce evt in
    ignore (Js.Unsafe.meth_call js_evt "preventDefault" [||]);
    ignore (Js.Unsafe.meth_call js_evt "stopPropagation" [||]);
    effect
  )

(* Game mode type *)
type game_mode = VsComputer | PassAndPlay | ThreePlayer | FourPlayer [@@deriving sexp_of]

module Model = struct
  type drag_source = 
    | FromHand of int  (* tile index in hand *)
    | FromStagingMeld of int * int  (* (meld_index, tile_index) in staging area *)
  [@@deriving sexp_of]

  type t = {
    game_state : Rummikub.State.t option;
    selected_tiles : int list;  (* Indices of selected tiles from current player's hand *)
    message : string;
    game_mode : game_mode option;
    num_players : int;
    last_drawn_tile_index : int option;  (* Index of the most recently drawn tile *)
    tiles_played_this_turn : bool;  (* Whether any tiles were played this turn - affects Draw/Pass availability *)
    rearrange_mode : bool;  (* Whether table manipulation mode is active *)
    staging_melds : Rummikub.Tile.tile list list;  (* Work-in-progress melds during rearrangement *)
    dragging_tile : drag_source option;  (* Currently dragged tile *)
    drag_over_meld : int option;  (* Meld index being hovered over *)
    tiles_moved_from_hand : Rummikub.Tile.tile list;  (* Tiles moved from hand to staging during rearrange *)
    jokers_taken_from_board : Rummikub.Tile.tile list;  (* Jokers taken from board/staging and should be added to hand *)
    (* Firebase/authentication fields *)
    current_user : Auth.user option;
    game_id : string option;  (* Firestore game document ID *)
    firebase_initialized : bool;
    unsubscribe_listener : (unit -> unit) option;  (* Unsubscribe function for real-time listener *)
    email_input : string;  (* For email/password login *)
    password_input : string;
    player_index : int option;  (* Which player am I? (0-based index) - only set in multiplayer *)
    join_game_id_input : string;  (* Input field for joining game by ID *)
    show_auth_ui : bool;  (* Whether to show authentication UI *)
    show_multiplayer_ui : bool;  (* Whether to show multiplayer create/join UI *)
    in_matchmaking : bool;  (* Whether currently searching for a match *)
    matchmaking_unsubscribe : (unit -> unit) option;  (* Unsubscribe function for matchmaking queue listener *)
  }
  
  let equal _t1 _t2 = false
  let sexp_of_t _ = Core.Sexp.Atom "model"
end

module Action = struct
  type t =
    | SelectMode of game_mode
    | StartGame
    | ToggleTile of int
    | PlaySelected
    | DrawTile
    | PassTurn
    | NewGame
    | BotMove
    | ToggleRearrangeMode
    | AddToNewMeld
    | StartDragFromHand of int
    | StartDragFromStaging of int * int
    | DragOver of int option
    | DropOnMeld of int
    | DropOnNewMeld
    | EndDrag
    | RemoveTileFromStaging of int * int
    | AddTileFromBoard of int * int
    | TakeJokerFromStaging of int * int
    | SubmitRearrangement
    | CancelRearrangement
    | InitFirebase
    | SignInWithGoogle
    | SignInWithFacebook
    | SignInWithEmail
    | SignInAnonymously
    | CreateAccountWithEmail
    | SignOut
    | UpdateEmailInput of string
    | UpdatePasswordInput of string
    | UpdateJoinGameId of string
    | CreateOnlineGame
    | SetOnlinePlayerCount of int
    | JoinOnlineGame
    | QuickMatch
    | CancelQuickMatch
    | GameStateUpdated
    | AuthError of string
    | GameCreated of string
    | GameJoined of string * int
    | ToggleAuthUI
    | ToggleMultiplayerUI
    | AuthStateChangedSignedIn of string
    | AuthStateChangedSignedOut
  [@@deriving sexp_of]
end

(* Simple style helpers using string-based CSS *)
let style_string s = Vdom.Attr.create "style" s

let tile_to_string = function
  | Tile.Tile (Tile.Red, r) -> Printf.sprintf "R%02d" r
  | Tile.Tile (Tile.Blue, r) -> Printf.sprintf "B%02d" r
  | Tile.Tile (Tile.Black, r) -> Printf.sprintf "K%02d" r
  | Tile.Tile (Tile.Orange, r) -> Printf.sprintf "O%02d" r
  | Tile.Joker -> "Jk"

let tile_color = function
  | Tile.Tile (Tile.Red, _) -> "#dc3545"
  | Tile.Tile (Tile.Blue, _) -> "#007bff"
  | Tile.Tile (Tile.Black, _) -> "#343a40"
  | Tile.Tile (Tile.Orange, _) -> "#fd7e14"
  | Tile.Joker -> "#6f42c1"

let render_tile ~tile ~selected ~newly_drawn ~inject ~action =
  let tile_text = tile_to_string tile in
  let color = tile_color tile in
  let transform = if selected then "scale(1.1)" else "scale(1)" in
  let box_shadow = if newly_drawn then 
    "0 0 15px 3px #ffd700, 0 4px 8px rgba(0,0,0,0.2)" 
  else if selected then
    "0 4px 8px rgba(0,0,0,0.2)"
  else
    "0 2px 4px rgba(0,0,0,0.1)"
  in
  let animation = if newly_drawn then "pulse 1.5s ease-in-out" else "none" in
  let style = Printf.sprintf
    "background: white; border: 2px solid %s; color: %s; border-radius: 8px; \
     padding: 0.5rem 0.625rem; font-weight: bold; font-size: 0.9rem; \
     min-width: 40px; text-align: center; cursor: pointer; transform: %s; \
     transition: all 0.2s ease; box-shadow: %s; animation: %s;"
    color color transform box_shadow animation
  in
  Vdom.Node.button
    ~attrs:[style_string style; Vdom.Attr.on_click (fun _ -> inject action)]
    [Vdom.Node.text tile_text]

let render_staging_meld ~meld ~meld_index ~is_drop_target ~inject =
  (* Sort tiles for display while keeping track of original indices *)
  (* Create list of (original_index, tile) pairs and sort by tile *)
  let indexed_tiles = List.mapi meld ~f:(fun i t -> (i, t)) in
  let sorted_indexed = List.sort indexed_tiles ~compare:(fun (_, t1) (_, t2) -> Tile.compare_tile t1 t2) in
  
  let meld_tiles = List.map sorted_indexed ~f:(fun (orig_idx, tile) ->
    let tile_text = tile_to_string tile in
    let color = tile_color tile in
    let is_joker = match tile with Tile.Joker -> true | _ -> false in
    let style = Printf.sprintf
      "background: white; border: 2px solid %s; color: %s; border-radius: 8px; \
       padding: 0.5rem 0.625rem; font-weight: bold; font-size: 0.9rem; \
       min-width: 40px; text-align: center; margin: 0 2px; cursor: %s; \
       transition: all 0.2s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.1);%s"
      color color
      (if is_joker then "pointer" else "move")
      (if is_joker then " border-width: 3px;" else "")
    in
    Vdom.Node.div
      ~attrs:(List.filter_opt [
        Some (style_string style);
        Some (Vdom.Attr.create "draggable" "true");
        Some (Vdom.Attr.on_dragstart (fun _evt -> inject (Action.StartDragFromStaging (meld_index, orig_idx))));
        Some (Vdom.Attr.on_dragend (fun _evt -> inject (Action.EndDrag)));
        (if is_joker then Some (Vdom.Attr.on_click (fun _ -> inject (Action.TakeJokerFromStaging (meld_index, orig_idx)))) else None);
        (if is_joker then Some (Vdom.Attr.title "Click to take joker to hand") else None);
      ])
      [Vdom.Node.text tile_text]
  ) in
  
  (* Re-validate after every change - joker purpose may have changed *)
  let meld_validation = 
    if Meld.is_meld meld then
      Vdom.Node.span 
        ~attrs:[style_string "color: #28a745; font-weight: bold;"] 
        [Vdom.Node.text " ✓"]
    else
      Vdom.Node.span 
        ~attrs:[style_string "color: #dc3545; font-weight: bold;"] 
        [Vdom.Node.text " ✗"]
  in
  
  let meld_style =
    "display: inline-block; background: white; border: 3px solid #333; \
     border-radius: 8px; padding: 0.625rem; margin: 5px; min-width: 100px; \
     transition: all 0.2s ease;"
  in
  
  Vdom.Node.div
    ~attrs:[
      style_string (if is_drop_target then 
        "display: inline-block; background: #e8f5e9; border: 3px dashed #28a745; \
         border-radius: 8px; padding: 0.625rem; margin: 5px; min-width: 100px; \
         transition: all 0.2s ease;"
      else meld_style);
      on_dragover_prevent_default (inject (Action.DragOver (Some meld_index)));
      Vdom.Attr.on_dragleave (fun _evt -> inject (Action.DragOver None));
      on_drop_prevent_default (inject (Action.DropOnMeld meld_index));
    ]
    [
      Vdom.Node.div
        ~attrs:[style_string "font-size: 0.8rem; color: #666; margin-bottom: 5px; text-align: center;"]
        [Vdom.Node.text (Printf.sprintf "Meld (%d pts)" (Meld.meld_points meld)); meld_validation];
      Vdom.Node.div
        ~attrs:[style_string "display: flex; gap: 3px; flex-wrap: wrap;"]
        meld_tiles
    ]

(* Helper to convert tile lists from Rummikub.State.TileMultiset.to_list to Tile.tile list *)
let tiles_from_hand (hand : 'a) : Rummikub.Tile.tile list =
  let hand_cast = (Obj.magic hand : Rummikub.State.TileMultiset.t) in
  Obj.magic (Rummikub.State.TileMultiset.to_list hand_cast)

(* Helper to safely extract game state, casting from any internal representation *)
let get_game_state (game_state_opt : 'a option) : Rummikub.State.t option =
  match Obj.magic game_state_opt with
  | None -> None
  | Some s -> Some (Obj.magic s : Rummikub.State.t)

let render_draggable_hand ~hand ~selected_tiles ~inject ~tiles_moved_from_hand =
  let tiles = tiles_from_hand hand in
  let tiles_moved : Rummikub.Tile.tile list = Obj.magic tiles_moved_from_hand in
  (* Filter out tiles that have been moved to staging during rearrange *)
  let filtered_tiles = 
    if List.is_empty tiles_moved then tiles
    else
      let rec remove_tiles hand_tiles moved_tiles =
        match moved_tiles with
        | [] -> hand_tiles
        | moved_tile :: rest ->
            let rec remove_first tile tiles acc =
              match tiles with
              | [] -> List.rev acc
              | h :: t -> if Tile.compare_tile h tile = 0 then List.rev acc @ t else remove_first tile t (h :: acc)
            in
            remove_tiles (remove_first moved_tile hand_tiles []) rest
      in
      remove_tiles tiles tiles_moved
  in
  if List.is_empty filtered_tiles then
    Vdom.Node.div
      ~attrs:[style_string "display: flex; justify-content: center; align-items: center; \
                            min-height: 50px; color: #28a745; font-weight: bold; \
                            font-size: 1.2rem; margin-top: 0.625rem;"]
      [Vdom.Node.text "🎉 EMPTY HAND! 🎉"]
  else
    Vdom.Node.div
      ~attrs:[style_string "display: flex; flex-wrap: wrap; gap: 5px; margin-top: 0.625rem;"]
      (List.mapi filtered_tiles ~f:(fun i tile ->
        (* Find original index in unfiltered hand for drag handler *)
        let original_idx = List.findi tiles ~f:(fun _ t -> Tile.compare_tile t tile = 0)
          |> Option.value_map ~default:i ~f:fst
        in
        let tile_text = tile_to_string tile in
        let color = tile_color tile in
        let selected = List.mem selected_tiles original_idx ~equal:Int.equal in
        let transform = if selected then "scale(1.1)" else "scale(1)" in
        let box_shadow = if selected then "0 4px 8px rgba(0,0,0,0.2)" else "0 2px 4px rgba(0,0,0,0.1)" in
        let style = Printf.sprintf
          "background: white; border: 2px solid %s; color: %s; border-radius: 8px; \
           padding: 0.5rem 0.625rem; font-weight: bold; font-size: 0.9rem; \
           min-width: 40px; text-align: center; cursor: move; \
           transition: all 0.2s ease; box-shadow: %s; transform: %s;"
          color color box_shadow transform
        in
        Vdom.Node.div
          ~attrs:[
            style_string style;
            Vdom.Attr.create "draggable" "true";
            Vdom.Attr.on_dragstart (fun _evt -> inject (Action.StartDragFromHand original_idx));
            Vdom.Attr.on_dragend (fun _evt -> inject (Action.EndDrag));
            Vdom.Attr.on_click (fun _ -> inject (Action.ToggleTile original_idx));
          ]
          [Vdom.Node.text tile_text]
      ))

let render_hand ~hand ~selected_tiles ~last_drawn_tile_index ~inject ~is_current:_ ~hide_tiles ~tiles_moved_from_hand =
  let tiles = tiles_from_hand hand in
  let tiles_moved : Rummikub.Tile.tile list = Obj.magic tiles_moved_from_hand in
  (* Filter out tiles that have been moved to staging during rearrange *)
  let filtered_tiles = 
    if List.is_empty tiles_moved then tiles
    else
      (* Remove tiles from hand that are in tiles_moved_from_hand *)
      let rec remove_tiles hand_tiles moved_tiles =
        match moved_tiles with
        | [] -> hand_tiles
        | moved_tile :: rest ->
            (* Remove first occurrence of moved_tile from hand_tiles *)
            let rec remove_first tile tiles acc =
              match tiles with
              | [] -> List.rev acc
              | h :: t -> if Tile.compare_tile h tile = 0 then List.rev acc @ t else remove_first tile t (h :: acc)
            in
            remove_tiles (remove_first moved_tile hand_tiles []) rest
      in
      remove_tiles tiles tiles_moved
  in
  if List.is_empty filtered_tiles then
    Vdom.Node.div
      ~attrs:[style_string "display: flex; justify-content: center; align-items: center; \
                            min-height: 50px; color: #28a745; font-weight: bold; \
                            font-size: 1.2rem; margin-top: 0.625rem;"]
      [Vdom.Node.text "🎉 EMPTY HAND! 🎉"]
  else if hide_tiles then
    (* Show face-down tiles for hidden hands *)
    Vdom.Node.div
      ~attrs:[style_string "display: flex; flex-wrap: wrap; gap: 5px; margin-top: 0.625rem;"]
      (List.init (List.length filtered_tiles) ~f:(fun _ ->
        Vdom.Node.span
          ~attrs:[style_string "background: #999; color: white; border: 2px solid #333; \
                                border-radius: 8px; padding: 0.5rem 0.625rem; font-weight: bold; \
                                min-width: 40px; text-align: center;"]
          [Vdom.Node.text "??"]
      ))
  else
    Vdom.Node.div
      ~attrs:[style_string "display: flex; flex-wrap: wrap; gap: 5px; margin-top: 0.625rem;"]
      (List.mapi filtered_tiles ~f:(fun i tile ->
        let selected = List.mem selected_tiles i ~equal:Int.equal in
        let newly_drawn = match last_drawn_tile_index with
          | Some idx -> idx = i
          | None -> false
        in
        render_tile ~tile ~selected ~newly_drawn ~inject ~action:(Action.ToggleTile i)
      ))

let render_player ~(player : Rummikub.State.player) ~is_current ~is_winner ~is_me ~selected_tiles ~last_drawn_tile_index ~inject ~hide_tiles ~rearrange_mode ~tiles_moved_from_hand =
  (* Dark minimalistic styling with Rummikub accent colors *)
  let bg_color =
    if is_winner then "#1a1a1a"
    else if is_me then "#2d2d2d"  (* Slightly lighter for "me" *)
    else if is_current then "#2d2d2d"
    else "#242424"
  in
  let border_color =
    if is_winner then "#28a745"
    else if is_current then "#dc3545"  (* Red for active turn *)
    else if is_me then "#007bff"  (* Blue for "me" *)
    else "#333"
  in
  let indicator_color =
    if is_winner then "#28a745"
    else if is_current then "#dc3545"  (* Red for active player *)
    else "#555"
  in
  let player_style = Printf.sprintf
    "background: %s; border-radius: 4px; padding: 0.75rem; border-left: 3px solid %s;"
    bg_color border_color
  in
  (* Display name with (You) indicator if this is the current user *)
  let display_name = 
    if is_winner then player.name ^ " 🏆"
    else if is_me then player.name ^ " (You)"
    else player.name
  in
  let name_color = 
    if is_winner then "#28a745"
    else if is_me then "#007bff"
    else if is_current then "#dc3545"
    else "#999"
  in
  Vdom.Node.div
    ~attrs:[style_string player_style]
    [
      Vdom.Node.h3
        ~attrs:[style_string (Printf.sprintf "color: %s; margin-bottom: 0.5rem; display: flex; \
                              align-items: center; gap: 0.4rem; font-size: 0.9rem; font-weight: 500;" name_color)]
        [
          Vdom.Node.span
            ~attrs:[style_string (Printf.sprintf "width: 6px; height: 6px; \
                                                   border-radius: 50%%; background: %s;"
                                                   indicator_color)]
            [];
          Vdom.Node.text display_name;
        ];
      Vdom.Node.p
        ~attrs:[style_string "color: #666; font-size: 0.75rem; margin-bottom: 0.5rem;"]
        [Vdom.Node.text (Printf.sprintf "%d tiles" 
          (List.length (Rummikub.State.TileMultiset.to_list player.hand)))];
      (if rearrange_mode && is_current then
        render_draggable_hand ~hand:player.hand ~selected_tiles ~inject ~tiles_moved_from_hand
      else
        render_hand ~hand:player.hand ~selected_tiles ~last_drawn_tile_index ~inject ~is_current ~hide_tiles ~tiles_moved_from_hand);
    ]

let render_staging_area ~staging_melds ~drag_over_meld ~inject =
  let staging_display = 
    if List.is_empty staging_melds then
      [Vdom.Node.div
        ~attrs:[style_string "text-align: center; color: #6c757d; font-style: italic; padding: 1rem;"]
        [Vdom.Node.text "Drag tiles from your hand or between melds to rearrange"]]
    else
      List.mapi staging_melds ~f:(fun idx meld ->
        let is_drop_target = match drag_over_meld with
          | Some target_idx -> target_idx = idx
          | None -> false
        in
        render_staging_meld ~meld ~meld_index:idx ~is_drop_target ~inject
      )
  in
  
  (* Add a "new meld" drop zone *)
  let new_meld_is_target = match drag_over_meld with
    | Some idx -> idx = List.length staging_melds
    | None -> false
  in
  let new_meld_zone =
    Vdom.Node.div
      ~attrs:[
        style_string (if new_meld_is_target then
          "display: inline-block; background: #e8f5e9; border: 2px dashed #28a745; \
           border-radius: 8px; padding: 0.625rem; margin: 5px; min-width: 100px; \
           min-height: 60px; text-align: center; color: #6c757d; \
           transition: all 0.2s ease;"
        else
          "display: inline-block; background: #f8f9fa; border: 2px dashed #dee2e6; \
           border-radius: 8px; padding: 0.625rem; margin: 5px; min-width: 100px; \
           min-height: 60px; text-align: center; color: #6c757d; \
           transition: all 0.2s ease;"
        );
        on_dragover_prevent_default (inject (Action.DragOver (Some (List.length staging_melds))));
        Vdom.Attr.on_dragleave (fun _evt -> inject (Action.DragOver None));
        on_drop_prevent_default (inject (Action.DropOnNewMeld));
      ]
      [Vdom.Node.text "+ Drop here for new meld"]
  in
  
  Vdom.Node.div ~attrs:[] (staging_display @ [new_meld_zone])

let render_board ~board ~rearrange_mode ~selected_board_tiles:_ ~inject =
  if List.is_empty board then
    Vdom.Node.div
      ~attrs:[style_string "text-align: center; color: #6c757d; font-style: italic; padding: 2.5rem;"]
      [Vdom.Node.text "No tiles on the table yet"]
  else
    Vdom.Node.div ~attrs:[] (List.mapi board ~f:(fun meld_idx meld ->
      if rearrange_mode then
        (* In rearrange mode, make tiles clickable to add to staging *)
        let meld_style = "display: inline-block; background: white; border: 2px solid #dee2e6; \
                         border-radius: 8px; padding: 0.625rem; margin: 5px;"
        in
        Vdom.Node.div
          ~attrs:[style_string meld_style]
          (List.mapi meld ~f:(fun tile_idx tile ->
            let tile_text = tile_to_string tile in
            let color = tile_color tile in
            let style = Printf.sprintf
              "background: white; border: 2px solid %s; color: %s; border-radius: 8px; \
               padding: 0.5rem 0.625rem; font-weight: bold; font-size: 0.9rem; \
               min-width: 40px; text-align: center; cursor: pointer; \
               transition: all 0.2s ease;"
              color color
            in
            Vdom.Node.span
              ~attrs:[
                style_string style;
                Vdom.Attr.on_click (fun _ -> inject (Action.AddTileFromBoard (meld_idx, tile_idx)));
              ]
              [Vdom.Node.text tile_text]
          ))
      else
        render_staging_meld ~meld ~meld_index:meld_idx ~is_drop_target:false ~inject
    ))

(* Simple AI *)
module SimpleAI = struct
  module ColorKey = struct
    type t = Tile.color
    let compare = Stdlib.compare
    let sexp_of_t = function
      | Tile.Red -> Core.Sexp.Atom "Red"
      | Tile.Blue -> Core.Sexp.Atom "Blue"
      | Tile.Black -> Core.Sexp.Atom "Black"
      | Tile.Orange -> Core.Sexp.Atom "Orange"
    let hash = function
      | Tile.Red -> 0
      | Tile.Blue -> 1
      | Tile.Black -> 2
      | Tile.Orange -> 3
  end
  
  let find_groups tiles =
    let by_rank = Hashtbl.create (module Int) in
    List.iter tiles ~f:(fun tile ->
      match tile with
      | Tile.Tile (_, rank) -> Hashtbl.add_multi by_rank ~key:rank ~data:tile
      | Tile.Joker -> ()
    );
    Hashtbl.fold by_rank ~init:[] ~f:(fun ~key:_ ~data:group acc ->
      if List.length group >= 3 && Meld.is_meld group then group :: acc else acc
    )

  let find_runs tiles =
    let by_color = Hashtbl.create (module ColorKey) in
    List.iter tiles ~f:(fun tile ->
      match tile with
      | Tile.Tile (color, _) -> Hashtbl.add_multi by_color ~key:color ~data:tile
      | Tile.Joker -> ()
    );
    Hashtbl.fold by_color ~init:[] ~f:(fun ~key:_ ~data:color_tiles acc ->
      let sorted = List.sort color_tiles ~compare:Tile.compare_tile in
      let rec find_run current acc_runs = function
        | [] ->
            if List.length current >= 3 && Meld.is_meld current then current :: acc_runs
            else acc_runs
        | tile :: rest ->
            match tile, List.hd current with
            | Tile.Tile (_, r1), Some (Tile.Tile (_, r2)) when r1 = r2 + 1 ->
                find_run (tile :: current) acc_runs rest
            | Tile.Tile _, Some _ ->
                let new_acc = if List.length current >= 3 && Meld.is_meld current then
                  current :: acc_runs else acc_runs in
                find_run [tile] new_acc rest
            | Tile.Tile _, None -> find_run [tile] acc_runs rest
            | _ -> find_run current acc_runs rest
      in
      find_run [] acc sorted @ acc
    )

  (* Find a single valid meld to play, if any *)
  let find_one_meld (state : Rummikub.State.t) player_idx =
    let player = state.players.(player_idx) in
    let tiles = tiles_from_hand player.hand in
    let groups = find_groups tiles in
    let runs = find_runs tiles in
    let all_melds = groups @ runs in
    
    if not player.met_initial_30 then
      (* Need to meet 30-point requirement *)
      let valid_initial = List.filter all_melds ~f:(fun meld ->
        Rummikub.Rules.initial_30_ok [meld]
      ) in
      List.hd valid_initial
    else
      List.hd all_melds

  (* Play all valid melds and return the final state, plus count of melds played *)
  let play_all_melds (initial_state : Rummikub.State.t) player_idx =
    let rec loop state melds_played =
      match find_one_meld state player_idx with
      | Some meld ->
          (match Rummikub.Rules.apply_play state [meld] with
           | Ok new_state -> loop new_state (melds_played + 1)
           | Error _ -> (state, melds_played))  (* Meld failed, stop *)
      | None -> (state, melds_played)  (* No more valid melds *)
    in
    loop initial_state 0
end

(* Mutable refs to store pending updates from Firestore/Auth (outside Bonsai system) *)
let pending_state_update : Firestore.State.t option ref = ref None
let pending_auth_user : Auth.user option ref = ref None

(* Helper to save game state to Firestore if in multiplayer mode *)
let save_game_state_if_online ~schedule_event (model : Model.t) (new_state : Rummikub.State.t) =
  match model.game_id, model.firebase_initialized with
  | Some game_id, true ->
      (* Save to Firestore - errors are handled silently (could add error action) *)
      (* Type cast: State.t (from open Rummikub) needs to be cast to Firestore's State.t *)
      Firestore.save_game_state game_id (Obj.magic new_state : Firestore.State.t)
        (fun () -> ())  (* Success - no action needed, real-time listener will update *)
        (fun error -> 
          schedule_event (Action.AuthError (Printf.sprintf "Failed to save game: %s" error)))
  | _ -> ()  (* Not in multiplayer mode or Firebase not initialized *)

let apply_action ~schedule_event (model : Model.t) (action : Action.t) : Model.t =
  match action with
  | SelectMode mode ->
      let num_players = match mode with
        | VsComputer | PassAndPlay -> 2
        | ThreePlayer -> 3
        | FourPlayer -> 4
      in
      (* Automatically start the game when mode is selected *)
      let model = { model with game_mode = Some mode; num_players; last_drawn_tile_index = None } in
      (* Start the game immediately with the selected mode *)
      let rng = Stdlib.Random.State.make_self_init () in
      let full_deck = Rummikub.State.shuffle rng (Rummikub.Tile.deck ()) in
      let rec deal_hands n deck acc =
        if n = 0 then (List.rev acc, deck)
        else
          let hand_tiles, remaining = List.split_n deck 14 in
          let player_name = match mode with
            | VsComputer -> 
                if List.length acc = 0 then "You" else "Computer"
            | PassAndPlay | ThreePlayer | FourPlayer ->
                Printf.sprintf "Player %d" (List.length acc + 1)
          in
          (* Create player record - use type annotation to disambiguate *)
          let hand_multiset = Rummikub.State.TileMultiset.of_list (Obj.magic hand_tiles : Rummikub.Tile.tile list) in
          let player = ({
            name = player_name;
            hand = hand_multiset;
            met_initial_30 = false;
          } : Rummikub.State.player) in
          deal_hands (n - 1) remaining (player :: acc)
      in
      let players_list, remaining_deck = deal_hands num_players full_deck [] in
      let base_state = Rummikub.State.initial_state rng in
      let players_array : Rummikub.State.player array = Obj.magic (Array.of_list players_list) in
      let deck_list = (Obj.magic remaining_deck : Rummikub.Tile.tile list) in
      let game_state : Rummikub.State.t = { base_state with 
        players = players_array;
        deck = deck_list;
      }
      in
      { model with
        game_state = Some game_state;
        selected_tiles = [];
        tiles_played_this_turn = false;
        message = "Game started! Make your first move.";
        last_drawn_tile_index = None;
      }

  | StartGame ->
      let mode = Option.value model.game_mode ~default:VsComputer in
      let rng = Stdlib.Random.State.make_self_init () in
      let num_players = model.num_players in
      
      (* Create game state with correct number of players *)
      let full_deck = Rummikub.State.shuffle rng (Rummikub.Tile.deck ()) in
      let rec deal_hands n deck acc =
        if n = 0 then (List.rev acc, deck)
        else
          let hand_tiles, remaining = List.split_n deck 14 in
          let player_name = match mode with
            | VsComputer -> 
                if List.length acc = 0 then "You" else "Computer"
            | PassAndPlay | ThreePlayer | FourPlayer ->
                Printf.sprintf "Player %d" (List.length acc + 1)
          in
          (* Create player record - use type annotation to disambiguate *)
          let hand_multiset = Rummikub.State.TileMultiset.of_list (Obj.magic hand_tiles : Rummikub.Tile.tile list) in
          let player = ({
            name = player_name;
            hand = hand_multiset;
            met_initial_30 = false;
          } : Rummikub.State.player) in
          deal_hands (n - 1) remaining (player :: acc)
      in
      let players_list, remaining_deck = deal_hands num_players full_deck [] in
      let base_state = Rummikub.State.initial_state rng in
      let players_array : Rummikub.State.player array = Obj.magic (Array.of_list players_list) in
      let deck_list = (Obj.magic remaining_deck : Rummikub.Tile.tile list) in
      let game_state : Rummikub.State.t = { base_state with 
        players = players_array;
        deck = deck_list;
      }
      in
      { model with 
        game_state = Some game_state;
        selected_tiles = [];
        tiles_played_this_turn = false;
        message = "Game started! Make your first move.";
        last_drawn_tile_index = None;
      }
  
  | ToggleTile idx ->
      (match model.game_state with
      | None -> model
      | Some state_any ->
          let state : Rummikub.State.t = Obj.magic state_any in
          if Rummikub.Rules.is_game_over state then model
          else
            let current_player = state.players.(state.turn) in
            let tiles = tiles_from_hand current_player.hand in
            if idx < List.length tiles then
              let new_selected = 
                if List.mem model.selected_tiles idx ~equal:Int.equal then
                  List.filter model.selected_tiles ~f:(fun i -> i <> idx)
                else
                  idx :: model.selected_tiles
              in
              { model with 
                selected_tiles = new_selected;
                message = if List.is_empty new_selected then "Select tiles to form a meld" 
                          else Printf.sprintf "%d tile(s) selected" (List.length new_selected);
                last_drawn_tile_index = None;  (* Clear highlight when selecting *)
              }
            else model
      )
  
  | PlaySelected ->
      (match model.game_state with
      | None -> model
      | Some state ->
          (* In multiplayer, only allow moves if it's this player's turn *)
          (* Cast state to Rummikub.State.t for type compatibility *)
          let state_cast = (Obj.magic state : Rummikub.State.t) in
          let is_valid_turn = match model.player_index, model.game_id with
          | Some player_idx, Some _ -> state_cast.turn = player_idx
          | _ -> true  (* Single player or local game - always valid *)
          in
          if not is_valid_turn then
            { model with message = "Not your turn! Please wait for your opponent." }
          else if Rummikub.Rules.is_game_over state_cast || List.is_empty model.selected_tiles then model
          else
            let current_player = state_cast.players.(state_cast.turn) in
            let tiles = tiles_from_hand current_player.hand in
            let selected_tiles_list = List.filter_map model.selected_tiles ~f:(fun i ->
              if i < List.length tiles then Some (List.nth_exn tiles i) else None
            ) in
            let meld = selected_tiles_list in
            match Rummikub.Rules.apply_play state_cast [meld] with
            | Ok new_state ->
                (* Save to Firestore if in multiplayer mode *)
                save_game_state_if_online ~schedule_event model new_state;
                (* Don't end turn - player must explicitly end turn *)
                { model with
                  game_state = Some (Obj.magic new_state : Rummikub.State.t);
                  selected_tiles = [];
                  tiles_played_this_turn = true;
                  message = "Move played! Continue playing or end your turn.";
                  last_drawn_tile_index = None;
                }
            | Error error_msg ->
                { model with message = "Error: " ^ error_msg; last_drawn_tile_index = None }
      )
  
  | DrawTile ->
      (match model.game_state with
      | None -> model
      | Some state ->
          (* Cast state to Rummikub.State.t for type compatibility *)
          let state_cast = (Obj.magic state : Rummikub.State.t) in
          (* In multiplayer, only allow moves if it's this player's turn *)
          let is_valid_turn = match model.player_index, model.game_id with
          | Some player_idx, Some _ -> state_cast.turn = player_idx
          | _ -> true  (* Single player or local game - always valid *)
          in
          if not is_valid_turn then
            { model with message = "Not your turn! Please wait for your opponent." }
          else if Rummikub.Rules.is_game_over state_cast then model
          else
            let old_hand_size = List.length (Rummikub.State.TileMultiset.to_list state_cast.players.(state_cast.turn).hand) in
            match Rummikub.Rules.apply_draw state_cast with
            | Ok new_state ->
                (* The newly drawn tile is at the end of the hand *)
                let new_hand_size = List.length (Rummikub.State.TileMultiset.to_list new_state.players.(state_cast.turn).hand) in
                let drawn_tile_index = if new_hand_size > old_hand_size then Some (new_hand_size - 1) else None in
                let new_state = Rummikub.Rules.next_turn new_state in
                (* Save to Firestore if in multiplayer mode *)
                save_game_state_if_online ~schedule_event model new_state;
                (* Trigger bot move if it's now computer's turn *)
                (match model.game_mode with
                 | Some VsComputer when new_state.turn = 1 ->
                     let _ = Js.Unsafe.meth_call Js.Unsafe.global "setTimeout" [|
                       Js.Unsafe.inject (Js.wrap_callback (fun () -> schedule_event Action.BotMove));
                       Js.Unsafe.inject (Js.number_of_float 500.0)
                     |] in ()
                 | _ -> ());
                { model with
                  game_state = Some (Obj.magic new_state : Rummikub.State.t);
                  selected_tiles = [];
                  tiles_played_this_turn = false;  (* Reset for next turn *)
                  message = "Drew a tile";
                  last_drawn_tile_index = drawn_tile_index;
                }
            | Error error_msg ->
                { model with message = "Error: " ^ error_msg; last_drawn_tile_index = None }
      )

  | PassTurn ->
      (match model.game_state with
      | None -> model
      | Some state ->
          (* Cast state to Rummikub.State.t for type compatibility *)
          let state_cast = (Obj.magic state : Rummikub.State.t) in
          (* In multiplayer, only allow moves if it's this player's turn *)
          let is_valid_turn = match model.player_index, model.game_id with
          | Some player_idx, Some _ -> state_cast.turn = player_idx
          | _ -> true  (* Single player or local game - always valid *)
          in
          if not is_valid_turn then
            { model with message = "Not your turn! Please wait for your opponent." }
          else if Rummikub.Rules.is_game_over state_cast then model
          else
            let new_state = Rummikub.Rules.next_turn state_cast in
            (* Save to Firestore if in multiplayer mode *)
            save_game_state_if_online ~schedule_event model new_state;
            (* Trigger bot move if it's now computer's turn *)
            (match model.game_mode with
             | Some VsComputer when new_state.turn = 1 ->
                 let _ = Js.Unsafe.meth_call Js.Unsafe.global "setTimeout" [|
                   Js.Unsafe.inject (Js.wrap_callback (fun () -> schedule_event Action.BotMove));
                   Js.Unsafe.inject (Js.number_of_float 500.0)
                 |] in ()
             | _ -> ());
            { model with
              game_state = Some (Obj.magic new_state : Rummikub.State.t);
              selected_tiles = [];
              tiles_played_this_turn = false;  (* Reset for next turn *)
              message = "Turn ended";
              last_drawn_tile_index = None;
            }
      )

  | NewGame ->
      { model with
        game_state = None;
        selected_tiles = [];
        tiles_played_this_turn = false;
        message = "Select game mode";
        game_mode = None;
        last_drawn_tile_index = None;
      }
  
  | BotMove ->
      (match model.game_state, model.game_mode with
      | Some state_any, Some VsComputer ->
          let state = (Obj.magic state_any : Rummikub.State.t) in
          if state.turn = 1 && not (Rummikub.Rules.is_game_over state) then
            (* Play all valid melds *)
            let (state_after_melds, melds_played) = SimpleAI.play_all_melds state 1 in
            if melds_played > 0 then
              (* Computer played melds, now pass to end turn *)
              let new_state = Rummikub.Rules.next_turn state_after_melds in
              { model with 
                game_state = Some new_state;
                message = Printf.sprintf "Computer played %d meld(s) and passed" melds_played;
                last_drawn_tile_index = None;
              }
            else
              (* No valid melds - draw a tile *)
              match Rummikub.Rules.apply_draw state with
              | Ok new_state ->
                  let new_state = Rummikub.Rules.next_turn new_state in
                  { model with 
                    game_state = Some new_state;
                    message = "Computer drew a tile";
                    last_drawn_tile_index = None;
                  }
              | Error _ ->
                  (* Can't draw (deck empty), just pass *)
                  let new_state = Rummikub.Rules.next_turn state in
                  { model with 
                    game_state = Some new_state;
                    message = "Computer passed (no tiles to draw)";
                    last_drawn_tile_index = None;
                  }
          else model
      | _ -> model
      )
  
  | ToggleRearrangeMode ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if Rummikub.Rules.is_game_over state then model
          else
            let new_rearrange_mode = not model.rearrange_mode in
            { model with 
              rearrange_mode = new_rearrange_mode;
              selected_tiles = [];
              staging_melds = if new_rearrange_mode then state.board else [];
              dragging_tile = None;
              drag_over_meld = None;
              tiles_moved_from_hand = if new_rearrange_mode then [] else [];
              jokers_taken_from_board = if new_rearrange_mode then [] else [];
              message = if new_rearrange_mode then "Rearrange Mode: Drag tiles to rearrange. Drag from hand to add tiles."
                        else "Rearrange mode cancelled";
            }
      )
  
  | AddToNewMeld ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if not model.rearrange_mode || List.is_empty model.selected_tiles then model
          else
            let current_player = state.players.(state.turn) in
            let hand_tiles = Rummikub.State.TileMultiset.to_list current_player.hand in
            
            (* Get selected tiles from hand *)
            let new_meld_tiles = List.filter_map model.selected_tiles ~f:(fun idx ->
              if idx < List.length hand_tiles then Some (List.nth_exn hand_tiles idx) else None
            ) in
            
            if List.is_empty new_meld_tiles then model
            else
              { model with
                staging_melds = model.staging_melds @ [new_meld_tiles];
                tiles_moved_from_hand = new_meld_tiles @ model.tiles_moved_from_hand;
                selected_tiles = [];
                message = "Meld added to staging area. Continue or Submit.";
              }
      )
  
  | StartDragFromHand idx ->
      { model with dragging_tile = Some (Model.FromHand idx) }
  
  | StartDragFromStaging (meld_idx, tile_idx) ->
      { model with dragging_tile = Some (Model.FromStagingMeld (meld_idx, tile_idx)) }
  
  | DragOver meld_idx_opt ->
      { model with drag_over_meld = meld_idx_opt }
  
  | EndDrag ->
      { model with dragging_tile = None; drag_over_meld = None }
  
  | DropOnMeld meld_idx ->
      (match model.dragging_tile, model.game_state with
      | Some drag_source, Some state ->
          let current_player = state.players.(state.turn) in
          let hand_tiles = Rummikub.State.TileMultiset.to_list current_player.hand in
          
          (* Get the dragged tile *)
          let tile_opt = match drag_source with
            | Model.FromHand idx ->
                if idx < List.length hand_tiles then Some (List.nth_exn hand_tiles idx) else None
            | Model.FromStagingMeld (src_meld_idx, src_tile_idx) ->
                if src_meld_idx < List.length model.staging_melds then
                  let src_meld = List.nth_exn model.staging_melds src_meld_idx in
                  if src_tile_idx < List.length src_meld then Some (List.nth_exn src_meld src_tile_idx)
                  else None
                else None
          in
          
          (match tile_opt with
          | None -> { model with dragging_tile = None; drag_over_meld = None }
          | Some tile ->
              (* Track if tile came from hand *)
              let new_tiles_from_hand = match drag_source with
                | Model.FromHand _ -> tile :: model.tiles_moved_from_hand
                | Model.FromStagingMeld _ -> model.tiles_moved_from_hand
              in
              
              (* Remove tile from source if from staging *)
              let new_staging = match drag_source with
                | Model.FromHand _ -> model.staging_melds (* Don't remove from hand yet *)
                | Model.FromStagingMeld (src_meld_idx, src_tile_idx) ->
                    (* Remove from staging meld *)
                    let staging = List.mapi model.staging_melds ~f:(fun i meld ->
                      if i = src_meld_idx then
                        List.filteri meld ~f:(fun j _ -> j <> src_tile_idx)
                      else meld
                    ) in
                    (* Filter out empty melds *)
                    List.filter staging ~f:(fun m -> not (List.is_empty m))
              in
              
              (* Add tile to target meld *)
              let final_staging = 
                if meld_idx < List.length new_staging then
                  List.mapi new_staging ~f:(fun i meld ->
                    if i = meld_idx then meld @ [tile] else meld
                  )
                else
                  new_staging @ [[tile]]
              in
              
              { model with 
                staging_melds = final_staging;
                tiles_moved_from_hand = new_tiles_from_hand;
                dragging_tile = None;
                drag_over_meld = None;
                message = "Tile moved. Continue rearranging or Submit when done.";
              }
          )
      | _ -> { model with dragging_tile = None; drag_over_meld = None }
      )
  
  | DropOnNewMeld ->
      (match model.dragging_tile, model.game_state with
      | Some drag_source, Some state ->
          let current_player = state.players.(state.turn) in
          let hand_tiles = Rummikub.State.TileMultiset.to_list current_player.hand in
          
          (* Get the dragged tile *)
          let tile_opt = match drag_source with
            | Model.FromHand idx ->
                if idx < List.length hand_tiles then Some (List.nth_exn hand_tiles idx) else None
            | Model.FromStagingMeld (src_meld_idx, src_tile_idx) ->
                if src_meld_idx < List.length model.staging_melds then
                  let src_meld = List.nth_exn model.staging_melds src_meld_idx in
                  if src_tile_idx < List.length src_meld then Some (List.nth_exn src_meld src_tile_idx)
                  else None
                else None
          in
          
          (match tile_opt with
          | None -> { model with dragging_tile = None; drag_over_meld = None }
          | Some tile ->
              (* Track if tile came from hand *)
              let new_tiles_from_hand = match drag_source with
                | Model.FromHand _ -> tile :: model.tiles_moved_from_hand
                | Model.FromStagingMeld _ -> model.tiles_moved_from_hand
              in
              
              (* Remove tile from source if from staging *)
              let new_staging = match drag_source with
                | Model.FromHand _ -> model.staging_melds
                | Model.FromStagingMeld (src_meld_idx, src_tile_idx) ->
                    let staging = List.mapi model.staging_melds ~f:(fun i meld ->
                      if i = src_meld_idx then
                        List.filteri meld ~f:(fun j _ -> j <> src_tile_idx)
                      else meld
                    ) in
                    List.filter staging ~f:(fun m -> not (List.is_empty m))
              in
              
              (* Create new meld with this tile *)
              let final_staging = new_staging @ [[tile]] in
              
              { model with 
                staging_melds = final_staging;
                tiles_moved_from_hand = new_tiles_from_hand;
                dragging_tile = None;
                drag_over_meld = None;
                message = "New meld created. Add more tiles or Submit when done.";
              }
          )
      | _ -> { model with dragging_tile = None; drag_over_meld = None }
      )
  
  | RemoveTileFromStaging (meld_idx, tile_idx) ->
      if not model.rearrange_mode then model
      else
        let new_staging = List.mapi model.staging_melds ~f:(fun i meld ->
          if i = meld_idx then
            List.filteri meld ~f:(fun j _ -> j <> tile_idx)
          else meld
        ) in
        let new_staging = List.filter new_staging ~f:(fun m -> not (List.is_empty m)) in
        { model with 
          staging_melds = new_staging;
          message = "Tile removed from meld";
        }
  
  | AddTileFromBoard (board_meld_idx, tile_idx) ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if not model.rearrange_mode then model
          else
            (* Get tile from board *)
            if board_meld_idx < List.length state.board then
              let board_meld = List.nth_exn state.board board_meld_idx in
              if tile_idx < List.length board_meld then
                let tile = List.nth_exn board_meld tile_idx in
                (* Add to new meld in staging *)
                { model with
                  staging_melds = model.staging_melds @ [[tile]];
                  message = "Tile added from board. Continue rearranging or Submit when done.";
                }
              else model
            else model
      )
  
  | TakeJokerFromStaging (meld_idx, tile_idx) ->
      (match model.game_state with
      | None -> model
      | Some _state ->
          if not model.rearrange_mode then model
          else
            (* Check if meld_idx is valid *)
            if meld_idx < List.length model.staging_melds then
              let meld = List.nth_exn model.staging_melds meld_idx in
              if tile_idx < List.length meld then
                let tile = List.nth_exn meld tile_idx in
                (* Check if it's a joker *)
                match tile with
                | Tile.Joker ->
                    (* Create meld without this joker *)
                    let meld_without_joker = List.filteri meld ~f:(fun i _ -> i <> tile_idx) in
                    (* Check if meld is still valid without joker *)
                    (* Allow taking if: 1) meld is valid without joker (and has >= 3 tiles), OR 2) meld becomes < 3 tiles (can be fixed before submit) *)
                    if List.length meld_without_joker < 3 || Meld.is_meld meld_without_joker then
                      (* Valid - remove joker from staging and add to jokers_taken_from_board *)
                      let new_staging = List.mapi model.staging_melds ~f:(fun i m ->
                        if i = meld_idx then meld_without_joker else m
                      ) in
                      (* Filter out melds with less than 3 tiles (they'll be validated on submit) *)
                      let new_staging = List.filter new_staging ~f:(fun m -> not (List.is_empty m)) in
                      { model with
                        staging_melds = new_staging;
                        jokers_taken_from_board = tile :: model.jokers_taken_from_board;
                        message = "Joker taken to hand. Meld remains valid without it. Submit to confirm.";
                      }
                    else
                      { model with
                        message = "Cannot take joker: meld would be invalid without it.";
                      }
                | _ ->
                    { model with
                      message = "This tile is not a joker. Use drag-and-drop to move regular tiles.";
                    }
              else model
            else model
      )
  
  | SubmitRearrangement ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if not model.rearrange_mode then model
          else
            (* Validate: All melds must have at least 3 tiles *)
            let invalid_meld_indices = 
              List.mapi model.staging_melds ~f:(fun i meld -> (i, meld))
              |> List.filter ~f:(fun (_, meld) -> List.length meld < 3)
              |> List.map ~f:fst
            in
            
            if not (List.is_empty invalid_meld_indices) then
              { model with
                message = Printf.sprintf 
                  "Error: All melds must have at least 3 tiles. Meld(s) %s have fewer than 3 tiles."
                  (String.concat ~sep:", " 
                    (List.map invalid_meld_indices ~f:(fun idx -> Int.to_string (idx + 1))))
              }
            else
              (* Use tracked tiles from hand *)
              let tiles_from_hand = model.tiles_moved_from_hand in
              
              (* Check 30-point first-move rule if needed *)
              let current_player = state.players.(state.turn) in
              if not current_player.met_initial_30 && not (List.is_empty tiles_from_hand) then
                (* Check if new melds (those with tiles from hand) meet 30-point requirement *)
                let new_melds = List.filter model.staging_melds ~f:(fun meld ->
                  (* Check if this meld has any tiles from hand *)
                  List.exists meld ~f:(fun tile -> List.exists tiles_from_hand ~f:(fun h_tile -> Tile.compare_tile tile h_tile = 0))
                ) in
                if not (Rummikub.Rules.initial_30_ok new_melds) then
                  { model with
                    message = "Error: First play must total at least 30 points. Your new melds total " ^
                              (new_melds |> List.map ~f:Meld.meld_points |> List.sum (module Int) ~f:Fn.id |> Int.to_string) ^
                              " points."
                  }
                else
                  (* Submit using table manipulation function *)
                  (match Rummikub.Rules.apply_play_with_table_manipulation state model.staging_melds tiles_from_hand with
                  | Ok new_state ->
                      (* Add jokers taken from board to player's hand *)
                      let updated_state = 
                        if List.is_empty model.jokers_taken_from_board then new_state
                        else
                          let current_player_idx = new_state.turn in
                          let current_player = new_state.players.(current_player_idx) in
                          let updated_hand = List.fold_left model.jokers_taken_from_board 
                            ~init:current_player.hand 
                            ~f:(fun acc joker -> Rummikub.State.TileMultiset.add joker acc) in
                          let updated_player = { current_player with hand = updated_hand } in
                          let updated_players = Array.copy new_state.players in
                          updated_players.(current_player_idx) <- updated_player;
                          { new_state with players = updated_players }
                      in
                      (* Save to Firestore if in multiplayer mode *)
                      save_game_state_if_online ~schedule_event model updated_state;
                      (* Don't end turn - player must explicitly end turn *)
                      { model with 
                        game_state = Some updated_state;
                        selected_tiles = [];
                        tiles_played_this_turn = not (List.is_empty tiles_from_hand);  (* Tiles played if any from hand *)
                        rearrange_mode = false;
                        staging_melds = [];
                        tiles_moved_from_hand = [];
                        jokers_taken_from_board = [];
                        dragging_tile = None;
                        drag_over_meld = None;
                        message = "Table rearranged! Continue playing or end your turn.";
                        last_drawn_tile_index = None;
                      }
                  | Error error_msg ->
                      { model with message = "Error: " ^ error_msg; }
                  )
              else
                (* Already met initial 30 or no tiles from hand - just validate *)
                (* Submit using table manipulation function *)
                (match Rummikub.Rules.apply_play_with_table_manipulation state model.staging_melds tiles_from_hand with
                | Ok new_state ->
                    (* Add jokers taken from board to player's hand *)
                    let updated_state = 
                      if List.is_empty model.jokers_taken_from_board then new_state
                      else
                        let current_player_idx = new_state.turn in
                        let current_player = new_state.players.(current_player_idx) in
                        let updated_hand = List.fold_left model.jokers_taken_from_board 
                          ~init:current_player.hand 
                          ~f:(fun acc joker -> Rummikub.State.TileMultiset.add joker acc) in
                        let updated_player = { current_player with hand = updated_hand } in
                        let updated_players = Array.copy new_state.players in
                        updated_players.(current_player_idx) <- updated_player;
                        { new_state with players = updated_players }
                    in
                    (* Save to Firestore if in multiplayer mode *)
                    save_game_state_if_online ~schedule_event model updated_state;
                    (* Don't end turn - player must explicitly end turn *)
                    { model with 
                      game_state = Some updated_state;
                      selected_tiles = [];
                      tiles_played_this_turn = not (List.is_empty tiles_from_hand);  (* Tiles played if any from hand *)
                      rearrange_mode = false;
                      staging_melds = [];
                      tiles_moved_from_hand = [];
                      jokers_taken_from_board = [];
                      dragging_tile = None;
                      drag_over_meld = None;
                      message = "Table rearranged! Continue playing or end your turn.";
                      last_drawn_tile_index = None;
                    }
                | Error error_msg ->
                    { model with message = "Error: " ^ error_msg; }
                )
      )
  
  | CancelRearrangement ->
      { model with 
        rearrange_mode = false;
        selected_tiles = [];
        staging_melds = [];
        dragging_tile = None;
        drag_over_meld = None;
        tiles_moved_from_hand = [];
        jokers_taken_from_board = [];
        message = "Rearrange mode cancelled";
        last_drawn_tile_index = None;
      }

  (* Firebase/authentication actions *)
  | InitFirebase ->
      if model.firebase_initialized then model
      else
        (try
          let config_js = Js.Unsafe.get Js.Unsafe.global "firebaseConfig" in
          let config_type = Js.to_string (Js.typeof config_js) in
          if String.equal config_type "undefined" then
            { model with message = "Firebase config not found. Please set window.firebaseConfig in HTML." }
          else
            let json = Js.Unsafe.get Js.Unsafe.global "JSON" in
            let config_str = Js.Unsafe.meth_call json "stringify" [|config_js|] |> Js.to_string in
            Firebase.init config_str;
            (* Auth state listener will be set up in component lifecycle after init *)
            { model with 
              firebase_initialized = true;
              message = "Firebase initialized. Please sign in to play online.";
            }
        with
        | e -> { model with message = Printf.sprintf "Firebase init error: %s" (Exn.to_string e) })

  (* Google and Facebook sign-in disabled for now *)
  | SignInWithGoogle ->
      { model with message = "Google sign-in is currently disabled." }

  | SignInWithFacebook ->
      { model with message = "Facebook sign-in is currently disabled." }

  | SignInWithEmail ->
      if not model.firebase_initialized then
        { model with message = "Please initialize Firebase first" }
      else if String.is_empty model.email_input || String.is_empty model.password_input then
        { model with message = "Please enter email and password" }
      else (
        Auth.sign_in_with_email model.email_input model.password_input
          (fun user ->
            pending_auth_user := Some user;
            pending_auth_user := Some user;
            schedule_event (Action.AuthStateChangedSignedIn user.uid))
          (fun error ->
            schedule_event (Action.AuthError error));
        { model with message = "Signing in..." }
      )

  | SignInAnonymously ->
      if not model.firebase_initialized then
        { model with message = "Please initialize Firebase first" }
      else (
        Auth.sign_in_anonymously
          (fun user ->
            pending_auth_user := Some user;
            pending_auth_user := Some user;
            schedule_event (Action.AuthStateChangedSignedIn user.uid))
          (fun error ->
            schedule_event (Action.AuthError error));
        { model with message = "Signing in as guest..." }
      )

  | CreateAccountWithEmail ->
      if not model.firebase_initialized then
        { model with message = "Please initialize Firebase first" }
      else if String.is_empty model.email_input || String.is_empty model.password_input then
        { model with message = "Please enter email and password" }
      else (
        Auth.create_user_with_email model.email_input model.password_input
          (fun user ->
            pending_auth_user := Some user;
            pending_auth_user := Some user;
            schedule_event (Action.AuthStateChangedSignedIn user.uid))
          (fun error ->
            schedule_event (Action.AuthError error));
        { model with message = "Creating account..." }
      )

  | SignOut ->
      if not model.firebase_initialized then
        model
      else
        (Option.iter model.unsubscribe_listener ~f:(fun unsub -> unsub ());
        Auth.sign_out
          (fun () -> ())
          (fun _error -> ());
        { model with 
          current_user = None;
          game_id = None;
          unsubscribe_listener = None;
          message = "Signed out";
        })

  | UpdateEmailInput email ->
      { model with email_input = email }

  | UpdatePasswordInput password ->
      { model with password_input = password }

  | (UpdateJoinGameId game_id) ->
      { model with join_game_id_input = game_id }

  | SetOnlinePlayerCount count ->
      { model with num_players = count }

  | CreateOnlineGame ->
      (* Start a new game and then create it online *)
      if not model.firebase_initialized then
        { model with message = "Firebase not initialized. Please sign in first." }
      else if Option.is_none model.current_user then
        { model with message = "Please sign in to create an online game." }
      else
        (* First start a local game, then save it to Firestore *)
        let rng = Stdlib.Random.State.make_self_init () in
        let num_players = model.num_players in
        
        (* Create game state *)
        let full_deck = Rummikub.State.shuffle rng (Rummikub.Tile.deck ()) in
        let rec deal_hands n deck acc =
          if n = 0 then (List.rev acc, deck)
          else
            let hand_tiles, remaining = List.split_n deck 14 in
            (* For online games, always use "Player N" naming *)
            let player_name = Printf.sprintf "Player %d" (List.length acc + 1) in
            (* Create player record - use type annotation to disambiguate *)
            let hand_multiset = Rummikub.State.TileMultiset.of_list (Obj.magic hand_tiles : Rummikub.Tile.tile list) in
            let player = ({
              name = player_name;
              hand = hand_multiset;
              met_initial_30 = false;
            } : Rummikub.State.player) in
            deal_hands (n - 1) remaining (player :: acc)
        in
        let players_list, remaining_deck = deal_hands num_players full_deck [] in
        let base_state = Rummikub.State.initial_state rng in
        let players_array : Rummikub.State.player array = Obj.magic (Array.of_list players_list) in
        let deck_list = (Obj.magic remaining_deck : Rummikub.Tile.tile list) in
        let game_state : Rummikub.State.t = { base_state with 
          players = players_array;
          deck = deck_list;
        } in
        (* Save to Firestore - creator is player 0 *)
        let user_id = match model.current_user with
          | Some user when not (String.is_empty user.uid) -> user.uid
          | _ -> ""
        in
        if String.is_empty user_id then
          { model with message = "❌ Invalid user session. Please sign out and sign in again." }
        else begin
          (* Log user_id for debugging *)
          let _ = Js.Unsafe.meth_call (Js.Unsafe.get Js.Unsafe.global "console") "log" 
            [|Js.Unsafe.inject (Js.string (Printf.sprintf "[Create] User ID: %s" user_id))|] in
          Firestore.create_game_with_user (Obj.magic game_state : Firestore.State.t) user_id num_players
            (fun game_id ->
              schedule_event (Action.GameCreated game_id))
            (fun error ->
              schedule_event (Action.AuthError error));
          { model with 
            game_state = Some (Obj.magic game_state : Rummikub.State.t);
            player_index = Some 0;  (* Creator is always player 0 *)
            message = "Creating online game...";
          }
        end

  | JoinOnlineGame ->
      if not model.firebase_initialized then
        { model with message = "Firebase not initialized. Please sign in first." }
      else if Option.is_none model.current_user then
        { model with message = "Please sign in to join a game." }
      else if String.is_empty model.join_game_id_input then
        { model with message = "Please enter a game ID." }
      else
        let game_id = model.join_game_id_input in
        let user_id = match model.current_user with
          | Some user when not (String.is_empty user.uid) -> user.uid
          | _ -> ""
        in
        if String.is_empty user_id then
          { model with message = "❌ Invalid user session. Please sign out and sign in again." }
        else begin
          (* Log user_id for debugging *)
          let _ = Js.Unsafe.meth_call (Js.Unsafe.get Js.Unsafe.global "console") "log" 
            [|Js.Unsafe.inject (Js.string (Printf.sprintf "[Join] User ID: %s, Game ID: %s" user_id game_id))|] in
          Firestore.join_game game_id user_id
            (fun state player_idx ->
              (* Store the state and trigger both GameJoined and GameStateUpdated *)
              pending_state_update := Some state;
              schedule_event (Action.GameJoined (game_id, player_idx));
              schedule_event (Action.GameStateUpdated))
            (fun error ->
              schedule_event (Action.AuthError error));
          { model with 
            game_id = Some game_id;
            message = Printf.sprintf "Joining game %s..." game_id 
          }
        end

  (* QuickMatch feature is disabled for now *)
  | QuickMatch -> 
      { model with message = "Quick match is currently disabled. Use 'Join Existing Game' instead." }

  | CancelQuickMatch -> 
      model

  | GameStateUpdated ->
      (* Update game state from pending_state_update ref *)
      (match !pending_state_update with
      | Some firestore_state ->
          let state_cast = (Obj.magic firestore_state : Rummikub.State.t) in
          pending_state_update := None;  (* Clear after processing *)
          (* Also update num_players to match the actual game *)
          let actual_num_players = Array.length state_cast.players in
          (* Check if turn changed - if so, reset tiles_played_this_turn *)
          let turn_changed = match model.game_state with
            | Some old_state -> old_state.turn <> state_cast.turn
            | None -> true
          in
          { model with 
            game_state = Some state_cast; 
            num_players = actual_num_players;
            tiles_played_this_turn = if turn_changed then false else model.tiles_played_this_turn;
          }
      | None -> model)  (* No pending state - ignore *)

  | AuthStateChangedSignedIn _user_id ->
      (* Get the actual user object from the ref *)
      let user_opt = !pending_auth_user in
      pending_auth_user := None;  (* Clear after processing *)
      (* Log user info for debugging *)
      (match user_opt with
      | Some user ->
          let _ = Js.Unsafe.meth_call (Js.Unsafe.get Js.Unsafe.global "console") "log" 
            [|Js.Unsafe.inject (Js.string (Printf.sprintf "[AuthStateChanged] User signed in - uid: %s, email: %s" 
              user.uid (Option.value user.email ~default:"<no email>")))|] in
          ()
      | None ->
          let _ = Js.Unsafe.meth_call (Js.Unsafe.get Js.Unsafe.global "console") "log" 
            [|Js.Unsafe.inject (Js.string "[AuthStateChanged] WARNING: User object is None!")|] in
          ());
      { model with
        current_user = user_opt;
        message = (match user_opt with
          | Some user -> Printf.sprintf "Signed in as %s" (Option.value user.email ~default:user.uid)
          | None -> "Signed in");
      }
  | AuthStateChangedSignedOut ->
      pending_auth_user := None;
      { model with
        current_user = None;
        message = "Signed out";
      }

  | (AuthError error_msg) ->
      (* Display error with ❌ prefix for visibility *)
      { model with message = Printf.sprintf "❌ %s" error_msg }

  | GameCreated game_id ->
      (* Set up real-time listener first *)
      let unsubscribe = Firestore.listen_to_game_state game_id
        (fun updated_state ->
          pending_state_update := Some updated_state;
          schedule_event (Action.GameStateUpdated))
        (fun error ->
          schedule_event (Action.AuthError error))
      in
      (* Load the game state to determine player_index (async - will trigger GameJoined action) *)
      Firestore.load_game_state game_id
        (fun state_opt ->
          match state_opt with
          | Some state ->
              (* Determine player_index - in quickmatch, first player is 0, second is 1 *)
              let player_idx = if model.in_matchmaking then 1 else 0 in
              (* Store state and trigger GameJoined *)
              pending_state_update := Some state;
              schedule_event (Action.GameJoined (game_id, player_idx));
              schedule_event (Action.GameStateUpdated)
          | None ->
              schedule_event (Action.AuthError "Game not found after creation"))
        (fun error ->
            schedule_event (Action.AuthError error));
      (* Return model immediately with game_id and listener set up *)
      { model with 
        game_id = Some game_id;
        in_matchmaking = false;  (* No longer in matchmaking *)
        matchmaking_unsubscribe = None;
        unsubscribe_listener = Some unsubscribe;
        show_multiplayer_ui = false;  (* Hide the multiplayer UI *)
        show_auth_ui = false;
        message = "Game created! Waiting for opponent...";
      }

  | GameJoined (game_id_str, player_idx) ->
      (* Set up real-time listener first *)
      let unsubscribe = Firestore.listen_to_game_state game_id_str
        (fun updated_state ->
          (* Store in mutable ref and trigger GameStateUpdated *)
          pending_state_update := Some updated_state;
          schedule_event (Action.GameStateUpdated))
        (fun error ->
          schedule_event (Action.AuthError error))
      in
      (* Load initial game state *)
      Firestore.load_game_state game_id_str
        (fun state_opt ->
          match state_opt with
          | Some firestore_state ->
              (* Store initial state in ref and trigger update *)
              pending_state_update := Some firestore_state;
              schedule_event (Action.GameStateUpdated)
          | None ->
              schedule_event (Action.AuthError "Game not found"))
        (fun error ->
          schedule_event (Action.AuthError error));
      (* Return model with game_id, player_index, listener, and hide multiplayer UI *)
      { model with 
        game_id = Some game_id_str;
        player_index = Some player_idx;
        unsubscribe_listener = Some unsubscribe;
        show_multiplayer_ui = false;  (* Hide the multiplayer UI once joined *)
        show_auth_ui = false;
        message = Printf.sprintf "Joined game as Player %d" (player_idx + 1);
      }

  | ToggleAuthUI ->
      { model with show_auth_ui = not model.show_auth_ui }

  | ToggleMultiplayerUI ->
      (* When toggling multiplayer UI, also show auth UI if not signed in *)
      let show_multiplayer = not model.show_multiplayer_ui in
      { model with 
        show_multiplayer_ui = show_multiplayer;
        show_auth_ui = show_multiplayer && Option.is_none model.current_user
      }

let component =
  (* Use state_machine0 to get schedule_event for async callbacks *)
  let%sub model, inject =
    Bonsai.state_machine0
      ~sexp_of_model:Model.sexp_of_t
      ~sexp_of_action:Action.sexp_of_t
      ~default_model:({
        game_state = None;
        selected_tiles = [];
        message = "Initializing...";
        game_mode = None;
        num_players = 2;
        last_drawn_tile_index = None;
        tiles_played_this_turn = false;
        rearrange_mode = false;
        staging_melds = [];
        dragging_tile = None;
        drag_over_meld = None;
        tiles_moved_from_hand = [];
        jokers_taken_from_board = [];
        current_user = None;
        game_id = None;
        firebase_initialized = false;
        unsubscribe_listener = None;
        email_input = "";
        password_input = "";
        player_index = None;
        join_game_id_input = "";
        show_auth_ui = false;
        show_multiplayer_ui = false;
        in_matchmaking = false;
        matchmaking_unsubscribe = None;
      } : Model.t)
      ~apply_action:(fun context model action ->
        let schedule_event action = 
          Bonsai.Apply_action_context.schedule_event context 
            (Bonsai.Apply_action_context.inject context action) 
        in
        apply_action ~schedule_event model action
      )
      ()
  in
  
  (* Initialize Firebase on component mount and set up auth listener *)
  let%sub () =
    let%sub callback =
      let%arr inject = inject
      and model = model in
      (* Initialize Firebase when component mounts if not already initialized *)
      let init_effect = 
        if not model.firebase_initialized then
          inject (Action.InitFirebase)
        else
          Vdom.Effect.Ignore
      in
      (* Set up auth state listener after a short delay to ensure Firebase is initialized *)
      let setup_effect = Vdom.Effect.of_sync_fun (fun () ->
        let _ = Js_of_ocaml.Js.Unsafe.meth_call Js_of_ocaml.Js.Unsafe.global "setTimeout"
          [| Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.wrap_callback (fun () ->
            if Firebase.is_initialized () then
              let _unsubscribe = Auth.on_auth_state_changed (fun user_opt ->
                (* Trigger action when auth state changes *)
                match user_opt with
                | Some user ->
                    pending_auth_user := Some user;
                    let _ = inject (Action.AuthStateChangedSignedIn user.uid) in ()
                | None ->
                    pending_auth_user := None;
                    let _ = inject (Action.AuthStateChangedSignedOut) in ())
              in
              ()
          )); Js_of_ocaml.Js.Unsafe.inject 100 |]
        in
        ()
      ) ()
      in
      Vdom.Effect.Many [init_effect; setup_effect]
    in
    Bonsai.Edge.lifecycle
      ~on_activate:callback
      ()
  in
  
  let%arr model = model
  and inject = inject in
  
  match model.game_state with
  | None ->
      (* Mode selection screen - Minimalistic Rummikub theme *)
      (* Rummikub colors: Red #dc3545, Blue #007bff, Black #1a1a1a, Orange #fd7e14 *)
      let container_style = "display: flex; flex-direction: column; align-items: center; \
                             padding: 2rem; background: #1a1a1a; min-height: 100vh;"
      in
      let game_container_style = "background: #fafafa; border-radius: 4px; padding: 2.5rem; \
                                   max-width: 500px; width: 100%; box-shadow: 0 4px 20px rgba(0,0,0,0.3);"
      in
      let title_style = "text-align: center; color: #1a1a1a; margin-bottom: 0.5rem; font-size: 2.5rem; \
                         font-weight: 300; letter-spacing: 0.3rem;"
      in
      let mode_selection_style = "display: flex; flex-direction: column; gap: 0.75rem;"
      in
      (* Each button uses a different Rummikub tile color *)
      let btn_red = "background: #dc3545; color: white; border: none; border-radius: 3px; \
                     padding: 1rem; font-size: 1rem; font-weight: 500; cursor: pointer; \
                     transition: opacity 0.2s ease; letter-spacing: 0.05rem;"
      in
      let btn_blue = "background: #007bff; color: white; border: none; border-radius: 3px; \
                      padding: 1rem; font-size: 1rem; font-weight: 500; cursor: pointer; \
                      transition: opacity 0.2s ease; letter-spacing: 0.05rem;"
      in
      let btn_black = "background: #1a1a1a; color: white; border: none; border-radius: 3px; \
                       padding: 1rem; font-size: 1rem; font-weight: 500; cursor: pointer; \
                       transition: opacity 0.2s ease; letter-spacing: 0.05rem;"
      in
      let btn_orange = "background: #fd7e14; color: white; border: none; border-radius: 3px; \
                        padding: 1rem; font-size: 1rem; font-weight: 500; cursor: pointer; \
                        transition: opacity 0.2s ease; letter-spacing: 0.05rem;"
      in
      Vdom.Node.div
        ~attrs:[style_string container_style]
        [
          Vdom.Node.div
            ~attrs:[style_string game_container_style]
            ([
              Vdom.Node.h1
                ~attrs:[style_string title_style]
                [Vdom.Node.text "RUMMIKUB"];
              Vdom.Node.p
                ~attrs:[style_string "text-align: center; color: #666; margin-bottom: 2rem; font-size: 0.9rem;"]
                [Vdom.Node.text "Select a game mode to begin"];
              Vdom.Node.div
                ~attrs:[style_string mode_selection_style]
                [
                  Vdom.Node.button
                    ~attrs:[
                      style_string btn_black;
                      Vdom.Attr.on_click (fun _ -> inject (SelectMode VsComputer));
                    ]
                    [Vdom.Node.text "vs Computer"];
                  Vdom.Node.button
                    ~attrs:[
                      style_string btn_red;
                      Vdom.Attr.on_click (fun _ -> inject (SelectMode PassAndPlay));
                    ]
                    [Vdom.Node.text "2 Players"];
                  Vdom.Node.button
                    ~attrs:[
                      style_string btn_blue;
                      Vdom.Attr.on_click (fun _ -> inject (SelectMode ThreePlayer));
                    ]
                    [Vdom.Node.text "3 Players"];
                  Vdom.Node.button
                    ~attrs:[
                      style_string btn_orange;
                      Vdom.Attr.on_click (fun _ -> inject (SelectMode FourPlayer));
                    ]
                    [Vdom.Node.text "4 Players"];
                  Vdom.Node.div ~attrs:[style_string "height: 1px; background: #ddd; margin: 0.5rem 0;"] [];
                  Vdom.Node.button
                    ~attrs:[
                      style_string (btn_black ^ " background: transparent; color: #1a1a1a; border: 2px solid #1a1a1a;");
                      Vdom.Attr.on_click (fun _ -> inject (Action.ToggleMultiplayerUI));
                    ]
                    [Vdom.Node.text "Online Multiplayer"];
                ];
            ]
            @
            (if model.show_auth_ui || (model.firebase_initialized && Option.is_none model.current_user) then
            [Vdom.Node.div
              ~attrs:[style_string "margin-top: 1.5rem; padding-top: 1.5rem; border-top: 1px solid #eee;"]
              [
                Vdom.Node.p
                  ~attrs:[style_string "margin: 0 0 1rem 0; text-align: center; color: #666; font-size: 0.85rem;"]
                  [Vdom.Node.text "Sign in to play online"];
                (match model.current_user with
                | Some user ->
                    Vdom.Node.div
                      ~attrs:[style_string "text-align: center;"]
                      [
                        Vdom.Node.p
                          ~attrs:[style_string "margin: 0 0 0.75rem 0; color: #333; font-size: 0.9rem;"]
                          [Vdom.Node.text (Printf.sprintf "Signed in: %s" 
                            (Option.value user.email ~default:(String.prefix user.uid 8)))];
                        Vdom.Node.button
                          ~attrs:[
                            style_string "background: transparent; color: #dc3545; border: 1px solid #dc3545; \
                                         border-radius: 3px; padding: 0.4rem 1rem; cursor: pointer; font-size: 0.85rem;";
                            Vdom.Attr.on_click (fun _ -> inject (Action.SignOut));
                          ]
                          [Vdom.Node.text "Sign Out"];
                      ]
                | None ->
                    Vdom.Node.div
                      ~attrs:[style_string "display: flex; flex-direction: column; gap: 0.5rem;"]
                      [
                        Vdom.Node.input
                          ~attrs:[
                            style_string "padding: 0.6rem; border: 1px solid #ddd; border-radius: 3px; \
                                         font-size: 0.9rem;";
                            Vdom.Attr.type_ "email";
                            Vdom.Attr.placeholder "Email";
                            Vdom.Attr.value model.email_input;
                            Vdom.Attr.on_input (fun _ s -> inject (Action.UpdateEmailInput s));
                          ]
                          ();
                        Vdom.Node.input
                          ~attrs:[
                            style_string "padding: 0.6rem; border: 1px solid #ddd; border-radius: 3px; \
                                         font-size: 0.9rem;";
                            Vdom.Attr.type_ "password";
                            Vdom.Attr.placeholder "Password";
                            Vdom.Attr.value model.password_input;
                            Vdom.Attr.on_input (fun _ s -> inject (Action.UpdatePasswordInput s));
                          ]
                          ();
                        Vdom.Node.div
                          ~attrs:[style_string "display: flex; gap: 0.5rem;"]
                          [
                            Vdom.Node.button
                              ~attrs:[
                                style_string "flex: 1; background: #1a1a1a; color: white; border: none; \
                                             border-radius: 3px; padding: 0.6rem; cursor: pointer; font-size: 0.85rem;";
                                Vdom.Attr.on_click (fun _ -> inject (Action.SignInWithEmail));
                              ]
                              [Vdom.Node.text "Sign In"];
                            Vdom.Node.button
                              ~attrs:[
                                style_string "flex: 1; background: transparent; color: #1a1a1a; \
                                             border: 1px solid #1a1a1a; border-radius: 3px; padding: 0.6rem; \
                                             cursor: pointer; font-size: 0.85rem;";
                                Vdom.Attr.on_click (fun _ -> inject (Action.CreateAccountWithEmail));
                              ]
                              [Vdom.Node.text "Create Account"];
                          ];
                        Vdom.Node.div ~attrs:[style_string "height: 1px; background: #eee; margin: 0.25rem 0;"] [];
                        Vdom.Node.button
                          ~attrs:[
                            style_string "background: #007bff; color: white; border: none; \
                                         border-radius: 3px; padding: 0.6rem; cursor: pointer; font-size: 0.85rem;";
                            Vdom.Attr.on_click (fun _ -> inject (Action.SignInAnonymously));
                          ]
                          [Vdom.Node.text "Quick Play as Guest"];
                      ]);
              ];
            ]
            else
              [])
            @
            (if model.show_multiplayer_ui && model.firebase_initialized && Option.is_some model.current_user then
            [Vdom.Node.div
              ~attrs:[style_string "margin-top: 1.5rem; padding-top: 1.5rem; border-top: 1px solid #eee;"]
              [
                Vdom.Node.p
                  ~attrs:[style_string "margin: 0 0 1rem 0; text-align: center; color: #1a1a1a; \
                                        font-size: 0.9rem; font-weight: 500;"]
                  [Vdom.Node.text "ONLINE MULTIPLAYER"];
                Vdom.Node.div
                  ~attrs:[style_string "display: flex; flex-direction: column; gap: 1rem;"]
                  [
                    (* Create Game Section *)
                    Vdom.Node.div
                      ~attrs:[style_string ""]
                      [
                        Vdom.Node.p
                          ~attrs:[style_string "margin: 0 0 0.5rem 0; color: #666; font-size: 0.8rem;"]
                          [Vdom.Node.text "Create game — share the code with friends"];
                        (* Player count selector using Rummikub colors *)
                        Vdom.Node.div
                          ~attrs:[style_string "display: flex; gap: 0.35rem; margin-bottom: 0.5rem;"]
                          [
                            Vdom.Node.button
                              ~attrs:[
                                style_string (if model.num_players = 2 
                                  then "flex: 1; padding: 0.5rem; border: none; \
                                        background: #dc3545; color: white; border-radius: 3px; \
                                        cursor: pointer; font-size: 0.85rem;"
                                  else "flex: 1; padding: 0.5rem; border: 1px solid #ddd; \
                                        background: white; color: #666; border-radius: 3px; \
                                        cursor: pointer; font-size: 0.85rem;");
                                Vdom.Attr.on_click (fun _ -> inject (Action.SetOnlinePlayerCount 2));
                              ]
                              [Vdom.Node.text "2P"];
                            Vdom.Node.button
                              ~attrs:[
                                style_string (if model.num_players = 3
                                  then "flex: 1; padding: 0.5rem; border: none; \
                                        background: #007bff; color: white; border-radius: 3px; \
                                        cursor: pointer; font-size: 0.85rem;"
                                  else "flex: 1; padding: 0.5rem; border: 1px solid #ddd; \
                                        background: white; color: #666; border-radius: 3px; \
                                        cursor: pointer; font-size: 0.85rem;");
                                Vdom.Attr.on_click (fun _ -> inject (Action.SetOnlinePlayerCount 3));
                              ]
                              [Vdom.Node.text "3P"];
                            Vdom.Node.button
                              ~attrs:[
                                style_string (if model.num_players = 4
                                  then "flex: 1; padding: 0.5rem; border: none; \
                                        background: #fd7e14; color: white; border-radius: 3px; \
                                        cursor: pointer; font-size: 0.85rem;"
                                  else "flex: 1; padding: 0.5rem; border: 1px solid #ddd; \
                                        background: white; color: #666; border-radius: 3px; \
                                        cursor: pointer; font-size: 0.85rem;");
                                Vdom.Attr.on_click (fun _ -> inject (Action.SetOnlinePlayerCount 4));
                              ]
                              [Vdom.Node.text "4P"];
                          ];
                        Vdom.Node.button
                          ~attrs:[
                            style_string "background: #1a1a1a; color: white; border: none; \
                                         border-radius: 3px; padding: 0.6rem; \
                                         cursor: pointer; font-size: 0.85rem; width: 100%;";
                            Vdom.Attr.on_click (fun _ -> inject (Action.CreateOnlineGame));
                          ]
                          [Vdom.Node.text (Printf.sprintf "Create %d-Player Game" model.num_players)];
                        (match model.game_id with
                        | Some gid ->
                            Vdom.Node.div
                              ~attrs:[style_string "margin-top: 0.75rem; padding: 0.6rem; \
                                                    background: #f5f5f5; border-radius: 3px;"]
                              [
                                Vdom.Node.p
                                  ~attrs:[style_string "margin: 0 0 0.25rem 0; font-size: 0.75rem; color: #666;"]
                                  [Vdom.Node.text "Game Code:"];
                                Vdom.Node.p
                                  ~attrs:[style_string "margin: 0; font-family: monospace; \
                                                        font-size: 0.95rem; color: #1a1a1a; \
                                                        word-break: break-all; font-weight: 500;"]
                                  [Vdom.Node.text gid];
                              ]
                        | None -> Vdom.Node.none);
                      ];
                    (* Quick Match Section - DISABLED FOR NOW
                    Vdom.Node.div
                      ~attrs:[style_string "padding: 1rem; background: white; border-radius: 8px; \
                                            border: 2px solid #ffc107;"]
                      [
                        Vdom.Node.h4
                          ~attrs:[style_string "margin-top: 0; color: #ff9800;"]
                          [Vdom.Node.text "⚡ Quick Match"];
                        Vdom.Node.p
                          ~attrs:[style_string "color: #666; font-size: 0.9rem;"]
                          [Vdom.Node.text (Printf.sprintf "Find %d random players for a %d-player game!" 
                            (model.num_players - 1) model.num_players)];
                        (if model.in_matchmaking then
                          Vdom.Node.div
                            ~attrs:[style_string "text-align: center;"]
                            [
                              Vdom.Node.p
                                ~attrs:[style_string "color: #ff9800; font-weight: bold; margin: 1rem 0;"]
                                [Vdom.Node.text (Printf.sprintf "🔍 Searching for %d-player game..." model.num_players)];
                              Vdom.Node.button
                                ~attrs:[
                                  style_string "background: #dc3545; color: white; border: none; \
                                               border-radius: 5px; padding: 0.75rem 1.5rem; \
                                               cursor: pointer; font-weight: bold;";
                                  Vdom.Attr.on_click (fun _ -> inject (Action.CancelQuickMatch));
                                ]
                                [Vdom.Node.text "Cancel Search"];
                            ]
                        else
                          Vdom.Node.button
                            ~attrs:[
                              style_string "background: #ff9800; color: white; border: none; \
                                           border-radius: 5px; padding: 0.75rem 1.5rem; \
                                           cursor: pointer; font-weight: bold; width: 100%; \
                                           font-size: 1.1rem;";
                              Vdom.Attr.on_click (fun _ -> inject (Action.QuickMatch));
                            ]
                            [Vdom.Node.text (Printf.sprintf "🎮 Find %d-Player Match" model.num_players)]);
                      ];
                    END Quick Match Section - DISABLED *)
                    (* Join Game Section *)
                    Vdom.Node.div
                      ~attrs:[style_string "margin-top: 0.5rem;"]
                      [
                        Vdom.Node.p
                          ~attrs:[style_string "margin: 0 0 0.5rem 0; color: #666; font-size: 0.8rem;"]
                          [Vdom.Node.text "Or join with a game code"];
                        Vdom.Node.div
                          ~attrs:[style_string "display: flex; gap: 0.35rem;"]
                          [
                            Vdom.Node.input
                              ~attrs:[
                                style_string "flex: 1; padding: 0.6rem; border: 1px solid #ddd; \
                                             border-radius: 3px; font-size: 0.9rem;";
                                Vdom.Attr.type_ "text";
                                Vdom.Attr.placeholder "Paste game code";
                                Vdom.Attr.value model.join_game_id_input;
                                Vdom.Attr.on_input (fun _ s -> inject (Action.UpdateJoinGameId s));
                              ]
                              ();
                            Vdom.Node.button
                              ~attrs:[
                                style_string "background: #1a1a1a; color: white; border: none; \
                                             border-radius: 3px; padding: 0.6rem 1rem; \
                                             cursor: pointer; font-size: 0.85rem;";
                                Vdom.Attr.on_click (fun _ -> inject (Action.JoinOnlineGame));
                              ]
                              [Vdom.Node.text "Join"];
                          ];
                      ];
                  ];
              ];
            ]
            else
              []));
        ]
  
  | Some state_any ->
      (* Cast state to ensure it's Rummikub.State.t *)
      let state = (Obj.magic state_any : Rummikub.State.t) in
      let is_game_over = Rummikub.Rules.is_game_over state in
      let winner = Rummikub.Rules.get_winner state in
      let current_player = state.players.(state.turn) in
      (* In multiplayer mode, check if it's this player's turn *)
      let is_my_turn = match model.player_index, model.game_id with
        | Some player_idx, Some _ -> state.turn = player_idx
        | _ -> true  (* Not in multiplayer or player_index not set - show controls *)
      in
      
      (* Minimalistic dark theme matching the menu *)
      let container_style = "display: flex; flex-direction: column; align-items: center; \
                             padding: 1.5rem; background: #1a1a1a; min-height: 100vh;"
      in
      let game_container_style = "background: #fafafa; border-radius: 4px; padding: 1.5rem; \
                                   max-width: 1200px; width: 100%; box-shadow: 0 4px 20px rgba(0,0,0,0.3);"
      in
      let title_style = "text-align: center; color: #1a1a1a; margin-bottom: 1rem; font-size: 2rem; \
                         font-weight: 300; letter-spacing: 0.2rem;"
      in
      
      (* Compact game info bar *)
      let game_info = Vdom.Node.div
        ~attrs:[style_string "display: flex; justify-content: space-between; align-items: center; \
                              margin-bottom: 1rem; padding: 0.75rem 1rem; background: #f5f5f5; \
                              border-radius: 4px; flex-wrap: wrap; gap: 0.5rem;"]
        [
          Vdom.Node.div
            ~attrs:[style_string "display: flex; gap: 1.5rem; align-items: center;"]
            [
              Vdom.Node.span
                ~attrs:[style_string "font-size: 0.85rem; color: #666;"]
                [Vdom.Node.text (Printf.sprintf "Turn: ");
                 Vdom.Node.span 
                   ~attrs:[style_string "font-weight: 500; color: #dc3545;"]
                   [Vdom.Node.text current_player.name]];
              Vdom.Node.span
                ~attrs:[style_string "font-size: 0.85rem; color: #666;"]
                [Vdom.Node.text (Printf.sprintf "Deck: %d" (List.length state.deck))];
              Vdom.Node.span
                ~attrs:[style_string (Printf.sprintf "font-size: 0.85rem; color: %s; font-weight: 500;"
                  (if is_game_over then "#dc3545" else "#28a745"))]
                [Vdom.Node.text (if is_game_over then "Game Over" else "In Progress")];
            ];
          Vdom.Node.div
            ~attrs:[style_string "font-size: 0.75rem; color: #999;"]
            [Vdom.Node.text "30+ pts first • Groups or runs • Jokers wild"];
        ] in
      
      let board = Vdom.Node.div
        ~attrs:[style_string "background: #2d2d2d; border-radius: 4px; padding: 1rem; \
                              margin-bottom: 1rem; min-height: 80px;"]
        [
          Vdom.Node.h3
            ~attrs:[style_string "text-align: left; color: #888; font-size: 0.75rem; \
                                  margin-bottom: 0.75rem; font-weight: 500; text-transform: uppercase; \
                                  letter-spacing: 0.1rem;"]
            [Vdom.Node.text (if model.rearrange_mode then "⚙ Rearrange Mode" else "TABLE")];
          (if model.rearrange_mode then
            Vdom.Node.div
              ~attrs:[]
              [
                Vdom.Node.p
                  ~attrs:[style_string "color: #aaa; margin-bottom: 10px; font-size: 0.85rem;"]
                  [Vdom.Node.text "Click tiles from board below or drag from hand to add to staging area"];
                render_staging_area ~staging_melds:model.staging_melds 
                  ~drag_over_meld:model.drag_over_meld ~inject;
                Vdom.Node.h4
                  ~attrs:[style_string "color: #888; margin-top: 1rem; margin-bottom: 0.5rem; \
                                        font-size: 0.75rem; text-transform: uppercase;"]
                  [Vdom.Node.text "Original Board (Click to add)"];
                render_board ~board:state.board ~rearrange_mode:true 
                  ~selected_board_tiles:[] ~inject;
              ]
          else
            render_board ~board:state.board ~rearrange_mode:false 
              ~selected_board_tiles:[] ~inject);
        ] in
      
      let players_style = match model.num_players with
        | 2 -> "display: grid; grid-template-columns: 1fr 1fr; gap: 0.75rem;"
        | 3 -> "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 0.75rem;"
        | _ -> "display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 0.75rem;"
      in
      
      let players = Vdom.Node.div
        ~attrs:[style_string players_style]
        (Array.to_list (Array.mapi state.players ~f:(fun idx player ->
          let is_current = state.turn = idx in
          let is_winner = Option.is_some winner && 
            String.equal (Option.value_exn winner).name player.name in
          (* Determine if this player's hand should be visible:
             - In multiplayer (game_id is Some): only show YOUR hand (player_index)
             - In pass-and-play/local: only show current player's hand *)
          let is_my_hand = match model.player_index, model.game_id with
            | Some player_idx, Some _ -> idx = player_idx  (* Multiplayer: only your hand *)
            | _ -> is_current  (* Local: current player's hand *)
          in
          let hide_this_player_tiles = not is_my_hand in
          (* In multiplayer, only allow interaction if it's this player's turn AND it's your hand *)
          let can_interact = match model.player_index, model.game_id with
            | Some player_idx, Some _ -> idx = player_idx && is_current
            | _ -> is_current  (* Not in multiplayer - allow interaction for current player *)
          in
          (* Create conditional inject that only works when can_interact *)
          let conditional_inject action = if can_interact then inject action else Vdom.Effect.Ignore in
          render_player 
            ~player 
            ~is_current 
            ~is_winner
            ~is_me:is_my_hand  (* Show "(You)" indicator for user's player *)
            ~selected_tiles:(if can_interact then model.selected_tiles else [])
            ~last_drawn_tile_index:(if can_interact then model.last_drawn_tile_index else None)
            ~inject:conditional_inject
            ~hide_tiles:hide_this_player_tiles
            ~rearrange_mode:model.rearrange_mode
            ~tiles_moved_from_hand:(if can_interact && model.rearrange_mode then model.tiles_moved_from_hand else [])
        ))) in
      
      (* Show game ID if in multiplayer mode - compact inline style *)
      let game_id_display = match model.game_id with
        | Some gid ->
            Vdom.Node.div
              ~attrs:[style_string "margin-bottom: 1rem; padding: 0.5rem 1rem; background: #1a1a1a; \
                                    border-radius: 4px; display: flex; align-items: center; \
                                    justify-content: space-between; flex-wrap: wrap; gap: 0.5rem;"]
              [
                Vdom.Node.span
                  ~attrs:[style_string "color: #28a745; font-size: 0.8rem; font-weight: 500;"]
                  [Vdom.Node.text "ONLINE"];
                Vdom.Node.span
                  ~attrs:[style_string "font-family: monospace; font-size: 0.85rem; \
                                        color: #fff; word-break: break-all;"]
                  [Vdom.Node.text gid];
                (match model.player_index with
                | Some idx ->
                    Vdom.Node.span
                      ~attrs:[style_string "font-size: 0.8rem; color: #007bff;"]
                      [Vdom.Node.text (Printf.sprintf "You: %s" state.players.(idx).State.name)]
                | None -> Vdom.Node.none);
              ]
        | None -> Vdom.Node.none
      in
      
      (* Show waiting message if it's not my turn in multiplayer *)
      let waiting_message = match model.player_index, model.game_id with
        | Some player_idx, Some _ when state.turn <> player_idx && not is_game_over ->
            Vdom.Node.div
              ~attrs:[style_string "margin: 0.75rem 0; padding: 0.75rem; background: #fd7e14; \
                                    border-radius: 4px; text-align: center;"]
              [
                Vdom.Node.p
                  ~attrs:[style_string "margin: 0; font-size: 0.95rem; font-weight: 500; color: white;"]
                  [Vdom.Node.text (Printf.sprintf "Waiting for %s..." current_player.name)];
              ]
        | _ -> Vdom.Node.none
      in
      
      (* Minimalistic button style *)
      let btn_style color = 
        Printf.sprintf "background: %s; color: white; border: none; border-radius: 3px; \
                        padding: 0.6rem 1.25rem; font-size: 0.9rem; font-weight: 500; \
                        cursor: pointer; transition: opacity 0.2s ease; letter-spacing: 0.03rem;" color
      in
      let controls = if not is_game_over && is_my_turn then
        if model.rearrange_mode then
          (* Rearrange mode controls *)
          Vdom.Node.div
            ~attrs:[style_string "display: flex; gap: 0.5rem; justify-content: center; \
                                  margin-top: 1rem; flex-wrap: wrap;"]
            [
              Vdom.Node.button
                ~attrs:(
                  let base_attrs = [
                    style_string (btn_style "#007bff");
                    Vdom.Attr.on_click (fun _ -> inject AddToNewMeld);
                  ] in
                  if List.is_empty model.selected_tiles then
                    Vdom.Attr.disabled :: base_attrs
                  else base_attrs
                )
                [Vdom.Node.text "Add to Meld"];
              Vdom.Node.button
                ~attrs:(
                  let base_attrs = [
                    style_string (btn_style "#28a745");
                    Vdom.Attr.on_click (fun _ -> inject SubmitRearrangement);
                  ] in
                  if List.is_empty model.staging_melds then
                    Vdom.Attr.disabled :: base_attrs
                  else base_attrs
                )
                [Vdom.Node.text "Submit"];
              Vdom.Node.button
                ~attrs:[
                  style_string (btn_style "#dc3545");
                  Vdom.Attr.on_click (fun _ -> inject CancelRearrangement);
                ]
                [Vdom.Node.text "Cancel"];
            ]
        else
          (* Normal mode controls *)
          Vdom.Node.div
            ~attrs:[style_string "display: flex; gap: 0.5rem; justify-content: center; \
                                  margin-top: 1rem; flex-wrap: wrap;"]
            [
              Vdom.Node.button
                ~attrs:(
                  let base_attrs = [
                    style_string (btn_style "#28a745");
                    Vdom.Attr.on_click (fun _ -> inject PlaySelected);
                  ] in
                  if List.is_empty model.selected_tiles then
                    Vdom.Attr.disabled :: base_attrs
                  else base_attrs
                )
                [Vdom.Node.text "Play"];
              (* Show Draw only if no tiles played this turn, End Turn only if tiles were played *)
              (if model.tiles_played_this_turn then
                Vdom.Node.button
                  ~attrs:[
                    style_string (btn_style "#1a1a1a");
                    Vdom.Attr.on_click (fun _ -> inject PassTurn);
                  ]
                  [Vdom.Node.text "End Turn"]
              else
                Vdom.Node.button
                  ~attrs:[
                    style_string (btn_style "#007bff");
                    Vdom.Attr.on_click (fun _ -> inject DrawTile);
                  ]
                  [Vdom.Node.text "Draw"]);
              Vdom.Node.button
                ~attrs:[
                  style_string (btn_style "#fd7e14");
                  Vdom.Attr.on_click (fun _ -> inject ToggleRearrangeMode);
                ]
                [Vdom.Node.text "Rearrange"];
            ]
      else
        Vdom.Node.div
          ~attrs:[style_string "display: flex; gap: 0.5rem; justify-content: center; \
                                margin-top: 1rem; flex-wrap: wrap;"]
          [
            Vdom.Node.button
              ~attrs:[
                style_string (btn_style "#1a1a1a");
                Vdom.Attr.on_click (fun _ -> inject NewGame);
              ]
              [Vdom.Node.text "New Game"];
          ] in
      
      let is_error_message = 
        String.is_prefix model.message ~prefix:"Error" || 
        String.is_prefix model.message ~prefix:"❌" ||
        String.is_substring model.message ~substring:"error" ||
        String.is_substring model.message ~substring:"failed"
      in
      let status_style = if is_error_message then
        "text-align: left; margin-top: 1rem; padding: 0.75rem; border-radius: 4px; \
         background: #dc3545; color: white; white-space: pre-wrap; font-size: 0.9rem;"
      else
        "text-align: center; margin-top: 1rem; padding: 0.75rem; border-radius: 4px; \
         background: #f5f5f5; color: #333; white-space: pre-wrap; font-size: 0.9rem;"
      in
      let status = Vdom.Node.div
        ~attrs:[style_string status_style]
        [Vdom.Node.text model.message] in
      
      let victory_message = if is_game_over && Option.is_some winner then
        Vdom.Node.div
          ~attrs:[style_string "text-align: center; margin-top: 1rem; padding: 1.5rem; \
                                background: #1a1a1a; color: white; border-radius: 4px;"]
          [
            Vdom.Node.span
              ~attrs:[style_string "font-size: 1.75rem; display: block; margin-bottom: 0.5rem;"]
              [Vdom.Node.text "🏆"];
            Vdom.Node.span
              ~attrs:[style_string "font-size: 1.1rem; font-weight: 500; letter-spacing: 0.1rem;"]
              [Vdom.Node.text (Printf.sprintf "%s WINS" 
                (String.uppercase (Option.value_exn winner).State.name))];
          ]
      else
        Vdom.Node.none in
      
      Vdom.Node.div
        ~attrs:[style_string container_style]
        [
          Vdom.Node.div
            ~attrs:[style_string game_container_style]
            [
              Vdom.Node.h1
                ~attrs:[style_string title_style]
                [Vdom.Node.text "RUMMIKUB"];
              game_id_display;
              game_info;
              board;
              players;
              waiting_message;
              controls;
              status;
              victory_message;
            ];
        ]

let app = component

