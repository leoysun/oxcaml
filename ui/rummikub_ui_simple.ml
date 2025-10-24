open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Rummikub

(* Game mode type *)
type game_mode = VsComputer | PassAndPlay | ThreePlayer | FourPlayer [@@deriving sexp_of]

module Model = struct
  type t = {
    game_state : State.t option;
    selected_tiles : int list;
    message : string;
    game_mode : game_mode option;
    num_players : int;
    last_drawn_tile_index : int option;  (* Index of the most recently drawn tile *)
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

let render_meld meld =
  let meld_tiles = List.map meld ~f:(fun tile ->
    let tile_text = tile_to_string tile in
    let color = tile_color tile in
    let style = Printf.sprintf
      "background: white; border: 2px solid %s; color: %s; border-radius: 8px; \
       padding: 0.5rem 0.625rem; font-weight: bold; font-size: 0.9rem; \
       min-width: 40px; text-align: center; margin: 0 2px;"
      color color
    in
    Vdom.Node.span ~attrs:[style_string style] [Vdom.Node.text tile_text]
  ) in
  let meld_style = "display: inline-block; background: white; border: 2px solid #333; \
                    border-radius: 8px; padding: 0.625rem; margin: 5px;"
  in
  Vdom.Node.div
    ~attrs:[style_string meld_style]
    [
      Vdom.Node.div
        ~attrs:[style_string "font-size: 0.8rem; color: #666; margin-bottom: 5px; text-align: center;"]
        [Vdom.Node.text (Printf.sprintf "Meld (%d pts)" (Meld.meld_points meld))];
      Vdom.Node.div
        ~attrs:[style_string "display: flex; gap: 3px;"]
        meld_tiles
    ]

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

let render_player ~player ~is_current ~is_winner ~selected_tiles ~last_drawn_tile_index ~inject ~hide_tiles =
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
      render_hand ~hand:player.State.hand ~selected_tiles ~last_drawn_tile_index ~inject ~is_current ~hide_tiles;
    ]

let render_board board =
  if List.is_empty board then
    Vdom.Node.div
      ~attrs:[style_string "text-align: center; color: #6c757d; font-style: italic; padding: 2.5rem;"]
      [Vdom.Node.text "No tiles on the table yet"]
  else
    Vdom.Node.div ~attrs:[] (List.map board ~f:render_meld)

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

let component =
  let%sub model_and_set_model = 
    Bonsai.state {
      Model.game_state = None;
      selected_tiles = [];
      message = "Select game mode";
      game_mode = None;
      num_players = 2;
      last_drawn_tile_index = None;
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
            [Vdom.Node.text "Table"];
          render_board state.board;
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
        ))) in
      
      let button_style color = 
        Printf.sprintf "background: %s; color: white; border: none; border-radius: 8px; \
                        padding: 0.625rem 1.25rem; font-size: 1rem; font-weight: bold; \
                        cursor: pointer; transition: all 0.2s ease;" color
      in
      
      let controls = if not is_game_over then
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

