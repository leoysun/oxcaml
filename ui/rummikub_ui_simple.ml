open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Rummikub

(* Game mode type *)
type game_mode = VsComputer | PassAndPlay | ThreePlayer | FourPlayer [@@deriving sexp_of]

module Model = struct
  type drag_source = 
    | FromHand of int  (* tile index in hand *)
    | FromStagingMeld of int * int  (* (meld_index, tile_index) in staging area *)
  [@@deriving sexp_of]

  type t = {
    game_state : State.t option;
    selected_tiles : int list;  (* Indices of selected tiles from current player's hand *)
    message : string;
    game_mode : game_mode option;
    num_players : int;
    last_drawn_tile_index : int option;  (* Index of the most recently drawn tile *)
    rearrange_mode : bool;  (* Whether table manipulation mode is active *)
    staging_melds : Tile.tile list list;  (* Work-in-progress melds during rearrangement *)
    dragging_tile : drag_source option;  (* Currently dragged tile *)
    drag_over_meld : int option;  (* Meld index being hovered over *)
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
    | AddToNewMeld  (* Add selected tiles to a new meld in staging *)
    | StartDragFromHand of int  (* tile index in hand *)
    | StartDragFromStaging of int * int  (* (meld_index, tile_index) *)
    | DragOver of int option  (* meld_index option *)
    | DropOnMeld of int  (* meld_index to drop onto *)
    | DropOnNewMeld  (* create a new meld *)
    | EndDrag  (* cancelled drag *)
    | RemoveTileFromStaging of int * int  (* remove tile from staging meld *)
    | SubmitRearrangement
    | CancelRearrangement
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
  let meld_tiles = List.mapi meld ~f:(fun tile_idx tile ->
    let tile_text = tile_to_string tile in
    let color = tile_color tile in
    let style = Printf.sprintf
      "background: white; border: 2px solid %s; color: %s; border-radius: 8px; \
       padding: 0.5rem 0.625rem; font-weight: bold; font-size: 0.9rem; \
       min-width: 40px; text-align: center; margin: 0 2px; cursor: move; \
       transition: all 0.2s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
      color color
    in
    Vdom.Node.div
      ~attrs:[
        style_string style;
        Vdom.Attr.create "draggable" "true";
        Vdom.Attr.on_dragstart (fun _evt -> inject (Action.StartDragFromStaging (meld_index, tile_idx)));
        Vdom.Attr.on_dragend (fun _evt -> inject Action.EndDrag);
      ]
      [Vdom.Node.text tile_text]
  ) in
  
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
      Vdom.Attr.create "ondragover" "event.preventDefault(); return false;";
      Vdom.Attr.on_dragover (fun _evt -> inject (Action.DragOver (Some meld_index)));
      Vdom.Attr.on_dragleave (fun _evt -> inject (Action.DragOver None));
      Vdom.Attr.create "ondrop" "event.preventDefault(); return false;";
      Vdom.Attr.on_drop (fun _evt -> inject (Action.DropOnMeld meld_index));
    ]
    [
      Vdom.Node.div
        ~attrs:[style_string "font-size: 0.8rem; color: #666; margin-bottom: 5px; text-align: center;"]
        [Vdom.Node.text (Printf.sprintf "Meld (%d pts)" (Meld.meld_points meld)); meld_validation];
      Vdom.Node.div
        ~attrs:[style_string "display: flex; gap: 3px; flex-wrap: wrap;"]
        meld_tiles
    ]

let render_draggable_hand ~hand ~selected_tiles ~inject =
  let tiles = State.TileMultiset.to_list hand in
  if List.is_empty tiles then
    Vdom.Node.div
      ~attrs:[style_string "display: flex; justify-content: center; align-items: center; \
                            min-height: 50px; color: #28a745; font-weight: bold; \
                            font-size: 1.2rem; margin-top: 0.625rem;"]
      [Vdom.Node.text "🎉 EMPTY HAND! 🎉"]
  else
    Vdom.Node.div
      ~attrs:[style_string "display: flex; flex-wrap: wrap; gap: 5px; margin-top: 0.625rem;"]
      (List.mapi tiles ~f:(fun i tile ->
        let tile_text = tile_to_string tile in
        let color = tile_color tile in
        let selected = List.mem selected_tiles i ~equal:Int.equal in
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
            Vdom.Attr.on_dragstart (fun _evt -> inject (Action.StartDragFromHand i));
            Vdom.Attr.on_dragend (fun _evt -> inject Action.EndDrag);
            Vdom.Attr.on_click (fun _ -> inject (Action.ToggleTile i));
          ]
          [Vdom.Node.text tile_text]
      ))

let render_hand ~hand ~selected_tiles ~last_drawn_tile_index ~inject ~is_current ~hide_tiles =
  let tiles = State.TileMultiset.to_list hand in
  if List.is_empty tiles then
    Vdom.Node.div
      ~attrs:[style_string "display: flex; justify-content: center; align-items: center; \
                            min-height: 50px; color: #28a745; font-weight: bold; \
                            font-size: 1.2rem; margin-top: 0.625rem;"]
      [Vdom.Node.text "🎉 EMPTY HAND! 🎉"]
  else if hide_tiles && not is_current then
    Vdom.Node.div
      ~attrs:[style_string "display: flex; flex-wrap: wrap; gap: 5px; margin-top: 0.625rem;"]
      (List.init (List.length tiles) ~f:(fun _ ->
        Vdom.Node.span
          ~attrs:[style_string "background: #999; color: white; border: 2px solid #333; \
                                border-radius: 8px; padding: 0.5rem 0.625rem; font-weight: bold; \
                                min-width: 40px; text-align: center;"]
          [Vdom.Node.text "??"]
      ))
  else
    Vdom.Node.div
      ~attrs:[style_string "display: flex; flex-wrap: wrap; gap: 5px; margin-top: 0.625rem;"]
      (List.mapi tiles ~f:(fun i tile ->
        let selected = List.mem selected_tiles i ~equal:Int.equal in
        let newly_drawn = match last_drawn_tile_index with
          | Some idx -> idx = i
          | None -> false
        in
        render_tile ~tile ~selected ~newly_drawn ~inject ~action:(Action.ToggleTile i)
      ))

let render_player ~player ~is_current ~is_winner ~selected_tiles ~last_drawn_tile_index ~inject ~hide_tiles ~rearrange_mode =
  let bg_color = 
    if is_winner then "#d4edda"
    else if is_current then "#e3f2fd"
    else "#f8f9fa"
  in
  let border_color =
    if is_winner then "#28a745"
    else if is_current then "#667eea"
    else "#dee2e6"
  in
  let indicator_color =
    if is_winner then "#28a745"
    else if is_current then "#667eea"
    else "#6c757d"
  in
  let player_style = Printf.sprintf
    "background: %s; border-radius: 10px; padding: 1.25rem; border: 2px solid %s;"
    bg_color border_color
  in
  Vdom.Node.div
    ~attrs:[style_string player_style]
    [
      Vdom.Node.h3
        ~attrs:[style_string "color: #333; margin-bottom: 0.9375rem; display: flex; \
                              align-items: center; gap: 0.625rem;"]
        [
          Vdom.Node.span
            ~attrs:[style_string (Printf.sprintf "width: 12px; height: 12px; \
                                                   border-radius: 50%%; background: %s;"
                                                   indicator_color)]
            [];
          Vdom.Node.text (if is_winner then player.State.name ^ " - WINNER! 🏆"
                          else player.State.name);
        ];
      Vdom.Node.p
        ~attrs:[]
        [Vdom.Node.text (Printf.sprintf "Hand: %d tiles" 
          (List.length (State.TileMultiset.to_list player.State.hand)))];
      (if rearrange_mode && is_current then
        render_draggable_hand ~hand:player.State.hand ~selected_tiles ~inject
      else
        render_hand ~hand:player.State.hand ~selected_tiles ~last_drawn_tile_index ~inject ~is_current ~hide_tiles);
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
        Vdom.Attr.create "ondragover" "event.preventDefault(); return false;";
        Vdom.Attr.on_dragover (fun _evt -> inject (Action.DragOver (Some (List.length staging_melds))));
        Vdom.Attr.on_dragleave (fun _evt -> inject (Action.DragOver None));
        Vdom.Attr.create "ondrop" "event.preventDefault(); return false;";
        Vdom.Attr.on_drop (fun _evt -> inject Action.DropOnNewMeld);
      ]
      [Vdom.Node.text "+ Drop here for new meld"]
  in
  
  Vdom.Node.div ~attrs:[] (staging_display @ [new_meld_zone])

let render_board ~board ~rearrange_mode:_ ~selected_board_tiles:_ ~inject =
  if List.is_empty board then
    Vdom.Node.div
      ~attrs:[style_string "text-align: center; color: #6c757d; font-style: italic; padding: 2.5rem;"]
      [Vdom.Node.text "No tiles on the table yet"]
  else
    Vdom.Node.div ~attrs:[] (List.mapi board ~f:(fun meld_idx meld ->
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

  let find_best_play state player_idx =
    let player = state.State.players.(player_idx) in
    let tiles = State.TileMultiset.to_list player.State.hand in
    let groups = find_groups tiles in
    let runs = find_runs tiles in
    let all_melds = groups @ runs in
    
    if not player.State.met_initial_30 then
      let valid_initial = List.filter all_melds ~f:(fun meld ->
        Rules.initial_30_ok [meld]
      ) in
      match valid_initial with
      | meld :: _ -> Some [meld]
      | [] -> None
    else
      match all_melds with
      | meld :: _ -> Some [meld]
      | [] -> None
end

let apply_action (model : Model.t) (action : Action.t) : Model.t =
  match action with
  | SelectMode mode ->
      let num_players = match mode with
        | VsComputer | PassAndPlay -> 2
        | ThreePlayer -> 3
        | FourPlayer -> 4
      in
      { model with game_mode = Some mode; num_players; last_drawn_tile_index = None }
  
  | StartGame ->
      let mode = Option.value model.game_mode ~default:VsComputer in
      let rng = Stdlib.Random.State.make_self_init () in
      let num_players = model.num_players in
      let base_state = State.initial_state rng in
      
      let game_state =
        if num_players = 2 then
          match mode with
          | VsComputer ->
              { base_state with 
                players = [|
                  { (base_state.players.(0)) with name = "You" };
                  { (base_state.players.(1)) with name = "Computer" };
                |]
              }
          | _ ->
              { base_state with 
                players = [|
                  { (base_state.players.(0)) with name = "Player 1" };
                  { (base_state.players.(1)) with name = "Player 2" };
                |]
              }
        else
          (* Create more players by re-dealing from full deck *)
          let full_deck = State.shuffle rng (Tile.deck ()) in
          let rec deal_hands n deck acc =
            if n = 0 then (List.rev acc, deck)
            else
              let hand_tiles, remaining = List.split_n deck 14 in
              let player = {
                State.name = Printf.sprintf "Player %d" (List.length acc + 1);
                hand = State.TileMultiset.of_list hand_tiles;
                met_initial_30 = false;
              } in
              deal_hands (n - 1) remaining (player :: acc)
          in
          let players_list, remaining_deck = deal_hands num_players full_deck [] in
          { base_state with 
            players = Array.of_list players_list;
            deck = remaining_deck;
          }
      in
      { model with 
        game_state = Some game_state;
        selected_tiles = [];
        message = "Game started! Make your first move.";
        last_drawn_tile_index = None;
      }
  
  | ToggleTile idx ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if Rules.is_game_over state then model
          else
            let current_player = state.players.(state.turn) in
            let tiles = State.TileMultiset.to_list current_player.hand in
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
          if Rules.is_game_over state || List.is_empty model.selected_tiles then model
          else
            let current_player = state.players.(state.turn) in
            let tiles = State.TileMultiset.to_list current_player.hand in
            let selected_tiles_list = List.filter_map model.selected_tiles ~f:(fun i ->
              if i < List.length tiles then Some (List.nth_exn tiles i) else None
            ) in
            let meld = selected_tiles_list in
            match Rules.apply_play state [meld] with
            | Ok new_state ->
                let new_state = Rules.next_turn new_state in
                { model with 
                  game_state = Some new_state;
                  selected_tiles = [];
                  message = "Move played successfully!";
                  last_drawn_tile_index = None;
                }
            | Error error_msg ->
                { model with message = "Error: " ^ error_msg; last_drawn_tile_index = None }
      )
  
  | DrawTile ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if Rules.is_game_over state then model
          else
            let old_hand_size = List.length (State.TileMultiset.to_list state.players.(state.turn).hand) in
            match Rules.apply_draw state with
            | Ok new_state ->
                (* The newly drawn tile is at the end of the hand *)
                let new_hand_size = List.length (State.TileMultiset.to_list new_state.players.(state.turn).hand) in
                let drawn_tile_index = if new_hand_size > old_hand_size then Some (new_hand_size - 1) else None in
                let new_state = Rules.next_turn new_state in
                { model with 
                  game_state = Some new_state;
                  selected_tiles = [];
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
          if Rules.is_game_over state then model
          else
            let new_state = Rules.next_turn state in
            { model with 
              game_state = Some new_state;
              selected_tiles = [];
              message = "Passed turn";
              last_drawn_tile_index = None;
            }
      )
  
  | NewGame ->
      { model with 
        game_state = None;
        selected_tiles = [];
        message = "Select game mode";
        game_mode = None;
        last_drawn_tile_index = None;
      }
  
  | BotMove ->
      (match model.game_state, model.game_mode with
      | Some state, Some VsComputer ->
          if state.turn = 1 && not (Rules.is_game_over state) then
            match SimpleAI.find_best_play state 1 with
            | Some melds ->
                (match Rules.apply_play state melds with
                | Ok new_state ->
                    let new_state = Rules.next_turn new_state in
                    { model with 
                      game_state = Some new_state;
                      message = "Computer played a meld";
                      last_drawn_tile_index = None;
                    }
                | Error _ ->
                    match Rules.apply_draw state with
                    | Ok new_state ->
                        let new_state = Rules.next_turn new_state in
                        { model with 
                          game_state = Some new_state;
                          message = "Computer drew a tile";
                          last_drawn_tile_index = None;
                        }
                    | Error _ ->
                        let new_state = Rules.next_turn state in
                        { model with 
                          game_state = Some new_state;
                          message = "Computer passed";
                          last_drawn_tile_index = None;
                        }
                )
            | None ->
                match Rules.apply_draw state with
                | Ok new_state ->
                    let new_state = Rules.next_turn new_state in
                    { model with 
                      game_state = Some new_state;
                      message = "Computer drew a tile";
                      last_drawn_tile_index = None;
                    }
                | Error _ ->
                    let new_state = Rules.next_turn state in
                    { model with 
                      game_state = Some new_state;
                      message = "Computer passed";
                      last_drawn_tile_index = None;
                    }
          else model
      | _ -> model
      )
  
  | ToggleRearrangeMode ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if Rules.is_game_over state then model
          else
            let new_rearrange_mode = not model.rearrange_mode in
            { model with 
              rearrange_mode = new_rearrange_mode;
              selected_tiles = [];
              staging_melds = if new_rearrange_mode then state.board else [];
              dragging_tile = None;
              drag_over_meld = None;
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
            let hand_tiles = State.TileMultiset.to_list current_player.hand in
            
            (* Get selected tiles from hand *)
            let new_meld_tiles = List.filter_map model.selected_tiles ~f:(fun idx ->
              if idx < List.length hand_tiles then Some (List.nth_exn hand_tiles idx) else None
            ) in
            
            if List.is_empty new_meld_tiles then model
            else
              { model with
                staging_melds = model.staging_melds @ [new_meld_tiles];
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
          let hand_tiles = State.TileMultiset.to_list current_player.hand in
          
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
              (* Remove tile from source if from staging *)
              let new_staging = match drag_source with
                | Model.FromHand _ -> model.staging_melds (* Don't remove from hand *)
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
          let hand_tiles = State.TileMultiset.to_list current_player.hand in
          
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
  
  | SubmitRearrangement ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if not model.rearrange_mode then model
          else
            (* Flatten all tiles from original board and staging area *)
            let original_board_tiles = List.concat state.board in
            let staging_board_tiles = List.concat model.staging_melds in
            
            (* Find tiles in staging that weren't in original board (tiles from hand) *)
            let tiles_from_hand = 
              let rec find_new_tiles staging original acc =
                match staging with
                | [] -> List.rev acc
                | tile :: rest_staging ->
                    (* Try to find this tile in original *)
                    let rec find_and_remove t orig =
                      match orig with
                      | [] -> (None, [])
                      | h :: tl ->
                          if Stdlib.compare h t = 0 then (Some h, tl)
                          else
                            let (found, remaining) = find_and_remove t tl in
                            (found, h :: remaining)
                    in
                    let (found, remaining_original) = find_and_remove tile original in
                    match found with
                    | Some _ -> find_new_tiles rest_staging remaining_original acc
                    | None -> find_new_tiles rest_staging original (tile :: acc)
              in
              find_new_tiles staging_board_tiles original_board_tiles []
            in
            
            (* Submit using table manipulation function *)
            (match Rules.apply_play_with_table_manipulation state model.staging_melds tiles_from_hand with
            | Ok new_state ->
                let new_state = Rules.next_turn new_state in
                { model with 
                  game_state = Some new_state;
                  selected_tiles = [];
                  rearrange_mode = false;
                  staging_melds = [];
                  dragging_tile = None;
                  drag_over_meld = None;
                  message = "Table rearranged successfully!";
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
        message = "Rearrange mode cancelled";
        last_drawn_tile_index = None;
      }

let component =
  let%sub model_and_set_model = 
    Bonsai.state {
      Model.game_state = None;
      selected_tiles = [];
      message = "Select game mode";
      game_mode = None;
      num_players = 2;
      last_drawn_tile_index = None;
      rearrange_mode = false;
      staging_melds = [];
      dragging_tile = None;
      drag_over_meld = None;
    }
  in
  let%arr (model, set_model) = model_and_set_model in
  let inject action = set_model (apply_action model action) in
  
  match model.game_state with
  | None ->
      (* Mode selection screen *)
      let container_style = "display: flex; flex-direction: column; align-items: center; \
                             padding: 1.25rem; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); \
                             min-height: 100vh;"
      in
      let game_container_style = "background: white; border-radius: 15px; padding: 1.875rem; \
                                   max-width: 1200px; width: 100%;"
      in
      let title_style = "text-align: center; color: #333; margin-bottom: 1.875rem; font-size: 2.5rem;"
      in
      let mode_selection_style = "display: flex; flex-direction: column; gap: 1.25rem; \
                                   max-width: 600px; margin: auto;"
      in
      let button_style = "background: #667eea; color: white; border: none; border-radius: 8px; \
                          padding: 1.25rem; font-size: 1.2rem; font-weight: bold; cursor: pointer; \
                          transition: all 0.2s ease;"
      in
      Vdom.Node.div
        ~attrs:[style_string container_style]
        [
          Vdom.Node.div
            ~attrs:[style_string game_container_style]
            [
              Vdom.Node.h1
                ~attrs:[style_string title_style]
                [Vdom.Node.text "🀄 Rummikub"];
              Vdom.Node.div
                ~attrs:[style_string mode_selection_style]
                [
                  Vdom.Node.h2
                    ~attrs:[style_string "text-align: center; margin-bottom: 1.5rem;"]
                    [Vdom.Node.text "Select Game Mode"];
                  Vdom.Node.button
                    ~attrs:[
                      style_string button_style;
                      Vdom.Attr.on_click (fun _ -> 
                        Vdom.Effect.Many [inject (SelectMode VsComputer); inject StartGame]
                      );
                    ]
                    [Vdom.Node.text "🤖 Play vs Computer"];
                  Vdom.Node.button
                    ~attrs:[
                      style_string button_style;
                      Vdom.Attr.on_click (fun _ -> 
                        Vdom.Effect.Many [inject (SelectMode PassAndPlay); inject StartGame]
                      );
                    ]
                    [Vdom.Node.text "📱 Pass-and-Play (2 Players)"];
                  Vdom.Node.button
                    ~attrs:[
                      style_string button_style;
                      Vdom.Attr.on_click (fun _ -> 
                        Vdom.Effect.Many [inject (SelectMode ThreePlayer); inject StartGame]
                      );
                    ]
                    [Vdom.Node.text "👥 Pass-and-Play (3 Players)"];
                  Vdom.Node.button
                    ~attrs:[
                      style_string button_style;
                      Vdom.Attr.on_click (fun _ -> 
                        Vdom.Effect.Many [inject (SelectMode FourPlayer); inject StartGame]
                      );
                    ]
                    [Vdom.Node.text "👨‍👩‍👧‍👦 Pass-and-Play (4 Players)"];
                ];
            ];
        ]
  
  | Some state ->
      let is_game_over = Rules.is_game_over state in
      let winner = Rules.get_winner state in
      let current_player = state.players.(state.turn) in
      let hide_tiles = match model.game_mode with
        | Some VsComputer -> true
        | _ -> false
      in
      
      let container_style = "display: flex; flex-direction: column; align-items: center; \
                             padding: 1.25rem; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); \
                             min-height: 100vh;"
      in
      let game_container_style = "background: white; border-radius: 15px; padding: 1.875rem; \
                                   max-width: 1200px; width: 100%;"
      in
      let title_style = "text-align: center; color: #333; margin-bottom: 1.875rem; font-size: 2.5rem;"
      in
      
      let game_info = Vdom.Node.div
        ~attrs:[style_string "display: grid; grid-template-columns: 1fr 1fr; gap: 1.25rem; \
                              margin-bottom: 1.875rem; "]
        [
          Vdom.Node.div
            ~attrs:[style_string "background: #f8f9fa; padding: 1.25rem; border-radius: 10px; \
                                  border-left: 4px solid #667eea;"]
            [
              Vdom.Node.h3 ~attrs:[] [Vdom.Node.text "Game Status"];
              Vdom.Node.p ~attrs:[] [Vdom.Node.text (Printf.sprintf "Phase: %s" 
                (if is_game_over then "Game Over" else "In Progress"))];
              Vdom.Node.p ~attrs:[] [Vdom.Node.text (Printf.sprintf "Turn: %s" current_player.name)];
              Vdom.Node.p ~attrs:[] [Vdom.Node.text (Printf.sprintf "Deck: %d tiles remaining" 
                (List.length state.deck))];
            ];
          Vdom.Node.div
            ~attrs:[style_string "background: #f8f9fa; padding: 1.25rem; border-radius: 10px; \
                                  border-left: 4px solid #667eea;"]
            [
              Vdom.Node.h3 ~attrs:[] [Vdom.Node.text "Rules"];
              Vdom.Node.p ~attrs:[] [Vdom.Node.text "• First play must total 30+ points"];
              Vdom.Node.p ~attrs:[] [Vdom.Node.text "• Form groups (same rank) or runs (same color)"];
              Vdom.Node.p ~attrs:[] [Vdom.Node.text "• Use jokers as wild cards"];
            ];
        ] in
      
      let board = Vdom.Node.div
        ~attrs:[style_string "background: #e9ecef; border-radius: 10px; padding: 1.25rem; \
                              margin-bottom: 1.875rem; min-height: 100px; border: 2px dashed #6c757d;"]
        [
          Vdom.Node.h3
            ~attrs:[style_string "text-align: center; color: #6c757d; font-size: 1.2rem; \
                                  margin-bottom: 0.9375rem;"]
            [Vdom.Node.text (if model.rearrange_mode then "Rearrange Mode - Drag & Drop Tiles" else "Table")];
          (if model.rearrange_mode then
            render_staging_area ~staging_melds:model.staging_melds 
              ~drag_over_meld:model.drag_over_meld ~inject
          else
            render_board ~board:state.board ~rearrange_mode:false 
              ~selected_board_tiles:[] ~inject);
        ] in
      
      let players_style = match model.num_players with
        | 2 -> "display: grid; grid-template-columns: 1fr 1fr; gap: 1.25rem;"
        | 3 -> "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 1.25rem;"
        | _ -> "display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 1.25rem;"
      in
      
      let players = Vdom.Node.div
        ~attrs:[style_string players_style]
        (Array.to_list (Array.mapi state.players ~f:(fun idx player ->
          let is_current = state.turn = idx in
          let is_winner = Option.is_some winner && 
            String.equal (Option.value_exn winner).State.name player.State.name in
          render_player 
            ~player 
            ~is_current 
            ~is_winner
            ~selected_tiles:(if is_current then model.selected_tiles else [])
            ~last_drawn_tile_index:(if is_current then model.last_drawn_tile_index else None)
            ~inject
            ~hide_tiles
            ~rearrange_mode:model.rearrange_mode
        ))) in
      
      let button_style color = 
        Printf.sprintf "background: %s; color: white; border: none; border-radius: 8px; \
                        padding: 0.625rem 1.25rem; font-size: 1rem; font-weight: bold; \
                        cursor: pointer; transition: all 0.2s ease;" color
      in
      
      let controls = if not is_game_over then
        if model.rearrange_mode then
          (* Rearrange mode controls *)
          Vdom.Node.div
            ~attrs:[style_string "display: flex; gap: 0.625rem; justify-content: center; \
                                  margin-top: 1.25rem; flex-wrap: wrap;"]
            [
              Vdom.Node.button
                ~attrs:(
                  let base_attrs = [
                    style_string (button_style "#667eea");
                    Vdom.Attr.on_click (fun _ -> inject AddToNewMeld);
                  ] in
                  if List.is_empty model.selected_tiles then
                    Vdom.Attr.disabled :: base_attrs
                  else base_attrs
                )
                [Vdom.Node.text "Add to New Meld"];
              Vdom.Node.button
                ~attrs:(
                  let base_attrs = [
                    style_string (button_style "#28a745");
                    Vdom.Attr.on_click (fun _ -> inject SubmitRearrangement);
                  ] in
                  if List.is_empty model.staging_melds then
                    Vdom.Attr.disabled :: base_attrs
                  else base_attrs
                )
                [Vdom.Node.text "Submit Rearrangement"];
              Vdom.Node.button
                ~attrs:[
                  style_string (button_style "#dc3545");
                  Vdom.Attr.on_click (fun _ -> inject CancelRearrangement);
                ]
                [Vdom.Node.text "Cancel"];
            ]
        else
          (* Normal mode controls *)
          Vdom.Node.div
            ~attrs:[style_string "display: flex; gap: 0.625rem; justify-content: center; \
                                  margin-top: 1.25rem; flex-wrap: wrap;"]
            [
              Vdom.Node.button
                ~attrs:(
                  let base_attrs = [
                    style_string (button_style "#28a745");
                    Vdom.Attr.on_click (fun _ -> inject PlaySelected);
                  ] in
                  if List.is_empty model.selected_tiles then
                    Vdom.Attr.disabled :: base_attrs
                  else base_attrs
                )
                [Vdom.Node.text "Play Selected"];
              Vdom.Node.button
                ~attrs:[
                  style_string (button_style "#667eea");
                  Vdom.Attr.on_click (fun _ -> inject DrawTile);
                ]
                [Vdom.Node.text "Draw"];
              Vdom.Node.button
                ~attrs:[
                  style_string (button_style "#6c757d");
                  Vdom.Attr.on_click (fun _ -> inject PassTurn);
                ]
                [Vdom.Node.text "Pass"];
              Vdom.Node.button
                ~attrs:[
                  style_string (button_style "#fd7e14");
                  Vdom.Attr.on_click (fun _ -> inject ToggleRearrangeMode);
                ]
                [Vdom.Node.text "Rearrange Table"];
            ]
      else
        Vdom.Node.div
          ~attrs:[style_string "display: flex; gap: 0.625rem; justify-content: center; \
                                margin-top: 1.25rem; flex-wrap: wrap;"]
          [
            Vdom.Node.button
              ~attrs:[
                style_string (button_style "#667eea");
                Vdom.Attr.on_click (fun _ -> inject NewGame);
              ]
              [Vdom.Node.text "New Game"];
          ] in
      
      let status_style = if String.is_prefix model.message ~prefix:"Error" then
        "text-align: left; margin-top: 1.25rem; padding: 0.9375rem; border-radius: 8px; \
         border: 1px solid #ffeaa7; background: #fff3cd; color: #856404; white-space: pre-wrap;"
      else
        "text-align: center; margin-top: 1.25rem; padding: 0.9375rem; border-radius: 8px; \
         border: 1px solid #c3e6cb; background: #d4edda; color: #155724; white-space: pre-wrap;"
      in
      let status = Vdom.Node.div
        ~attrs:[style_string status_style]
        [Vdom.Node.text model.message] in
      
      let victory_message = if is_game_over && Option.is_some winner then
        Vdom.Node.div
          ~attrs:[style_string "text-align: center; margin-top: 1.25rem; padding: 1.25rem; \
                                background: linear-gradient(45deg, #28a745 0%, #20c997 100%); \
                                color: white; border-radius: 10px; font-size: 1.5rem; font-weight: bold;"]
          [
            Vdom.Node.text "🎊 CONGRATULATIONS! 🎊";
            Vdom.Node.br ();
            Vdom.Node.text (Printf.sprintf "%s wins the game!" (Option.value_exn winner).State.name);
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
                [Vdom.Node.text "🀄 Rummikub"];
              game_info;
              board;
              players;
              controls;
              status;
              victory_message;
            ];
        ]

let app = component

