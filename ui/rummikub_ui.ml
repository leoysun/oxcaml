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
  }
  
  let equal _t1 _t2 = false  (* We don't need structural equality for state machine *)
  let sexp_of_t _ = Core.Sexp.Atom "model"  (* Minimal sexp for debugging *)
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

module Style = struct
  let container = 
    Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"flex"
    @> Css_gen.create ~field:"flex-direction" ~value:"column"
    @> Css_gen.create ~field:"align-items" ~value:"center"
    @> Css_gen.padding (`rem 1.25)
    @> Css_gen.create ~field:"background" ~value:"linear-gradient(135deg, #667eea 0%, #764ba2 100%)"
    @> Css_gen.create ~field:"min-height" ~value:"100vh"
  )

  let game_container = Css_gen.(
    background `white
    @> border_radius (`px 15)
    @> box_shadow ~x:(`px 0) ~y:(`px 10) ~blur:(`px 30) ~spread:(`px 0) ~color:(`rgba (0, 0, 0, 0.3))
    @> padding (`rem 1.875)
    @> max_width (`px 1200)
    @> width (`percent 100.)
  )

  let title = Css_gen.(
    text_align `center
    @> color (`hex "333")
    @> margin_bottom (`rem 1.875)
    @> font_size (`rem 2.5)
  )

  let mode_selection = Css_gen.(
    display `flex
    @> flex_direction `column
    @> gap (`rem 1.25)
    @> max_width (`px 600)
    @> margin `auto
  )

  let mode_button = Css_gen.(
    background (`hex "667eea")
    @> color `white
    @> border `none
    @> border_radius (`px 8)
    @> padding (`rem 1.25)
    @> font_size (`rem 1.2)
    @> font_weight `bold
    @> cursor `pointer
    @> transition "all 0.2s ease"
    @> create ~field:"hover" ~value:"transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,0.2);"
  )

  let game_info = Css_gen.(
    display `grid
    @> grid_template_columns [`fr 1; `fr 1]
    @> gap (`rem 1.25)
    @> margin_bottom (`rem 1.875)
    @> media_query "(max-width: 768px)" [grid_template_columns [`fr 1]]
  )

  let info_card = Css_gen.(
    background (`hex "f8f9fa")
    @> padding (`rem 1.25)
    @> border_radius (`px 10)
    @> border_left ~width:(`px 4) ~color:(`hex "667eea") ~style:`solid
  )

  let board = Css_gen.(
    background (`hex "e9ecef")
    @> border_radius (`px 10)
    @> padding (`rem 1.25)
    @> margin_bottom (`rem 1.875)
    @> min_height (`px 100)
    @> border ~width:(`px 2) ~color:(`hex "6c757d") ~style:`dashed
  )

  let board_title = Css_gen.(
    text_align `center
    @> color (`hex "6c757d")
    @> font_size (`rem 1.2)
    @> margin_bottom (`rem 0.9375)
  )

  let board_empty = Css_gen.(
    text_align `center
    @> color (`hex "6c757d")
    @> font_style `italic
    @> padding (`rem 2.5)
  )

  let players = Css_gen.(
    display `grid
    @> grid_template_columns [`fr 1; `fr 1]
    @> gap (`rem 1.25)
    @> media_query "(max-width: 768px)" [grid_template_columns [`fr 1]]
  )

  let players_three = Css_gen.(
    display `grid
    @> grid_template_columns [`fr 1; `fr 1; `fr 1]
    @> gap (`rem 1.25)
    @> media_query "(max-width: 968px)" [grid_template_columns [`fr 1; `fr 1]]
    @> media_query "(max-width: 768px)" [grid_template_columns [`fr 1]]
  )

  let players_four = Css_gen.(
    display `grid
    @> grid_template_columns [`fr 1; `fr 1; `fr 1; `fr 1]
    @> gap (`rem 1.25)
    @> media_query "(max-width: 1200px)" [grid_template_columns [`fr 1; `fr 1]]
    @> media_query "(max-width: 768px)" [grid_template_columns [`fr 1]]
  )

  let player = Css_gen.(
    background (`hex "f8f9fa")
    @> border_radius (`px 10)
    @> padding (`rem 1.25)
    @> border ~width:(`px 2) ~color:(`hex "dee2e6") ~style:`solid
  )

  let player_current = Css_gen.(
    background (`hex "e3f2fd")
    @> border_radius (`px 10)
    @> padding (`rem 1.25)
    @> border ~width:(`px 2) ~color:(`hex "667eea") ~style:`solid
  )

  let player_winner = Css_gen.(
    background (`hex "d4edda")
    @> border_radius (`px 10)
    @> padding (`rem 1.25)
    @> border ~width:(`px 2) ~color:(`hex "28a745") ~style:`solid
  )

  let player_title = Css_gen.(
    color (`hex "333")
    @> margin_bottom (`rem 0.9375)
    @> display `flex
    @> align_items `center
    @> gap (`rem 0.625)
  )

  let player_indicator = Css_gen.(
    width (`px 12)
    @> height (`px 12)
    @> border_radius (`percent 50.)
    @> background (`hex "667eea")
  )

  let player_indicator_inactive = Css_gen.(
    width (`px 12)
    @> height (`px 12)
    @> border_radius (`percent 50.)
    @> background (`hex "6c757d")
  )

  let player_indicator_winner = Css_gen.(
    width (`px 12)
    @> height (`px 12)
    @> border_radius (`percent 50.)
    @> background (`hex "28a745")
  )

  let hand = Css_gen.(
    display `flex
    @> flex_wrap `wrap
    @> gap (`px 5)
    @> margin_top (`rem 0.625)
  )

  let hand_empty = Css_gen.(
    display `flex
    @> flex_wrap `wrap
    @> gap (`px 5)
    @> margin_top (`rem 0.625)
    @> justify_content `center
    @> align_items `center
    @> min_height (`px 50)
    @> color (`hex "28a745")
    @> font_weight `bold
    @> font_size (`rem 1.2)
  )

  let tile = Css_gen.(
    background `white
    @> border ~width:(`px 2) ~color:(`hex "333") ~style:`solid
    @> border_radius (`px 8)
    @> padding ~top:(`rem 0.5) ~bottom:(`rem 0.5) ~left:(`rem 0.625) ~right:(`rem 0.625)
    @> font_weight `bold
    @> font_size (`rem 0.9)
    @> min_width (`px 40)
    @> text_align `center
    @> box_shadow ~x:(`px 0) ~y:(`px 2) ~blur:(`px 4) ~spread:(`px 0) ~color:(`rgba (0, 0, 0, 0.1))
    @> cursor `pointer
    @> transition "all 0.2s ease"
  )

  let tile_red = color (`hex "dc3545")
  let tile_blue = color (`hex "007bff")
  let tile_black = color (`hex "343a40")
  let tile_orange = color (`hex "fd7e14")
  let tile_joker = color (`hex "6f42c1") @> background (`hex "f8f9fa")

  let tile_selected = Css_gen.(
    transform (`scale 1.1)
    @> box_shadow ~x:(`px 0) ~y:(`px 4) ~blur:(`px 8) ~spread:(`px 0) ~color:(`rgba (0, 0, 0, 0.2))
  )

  let meld = Css_gen.(
    display `inline_block
    @> background `white
    @> border ~width:(`px 2) ~color:(`hex "333") ~style:`solid
    @> border_radius (`px 8)
    @> padding (`rem 0.625)
    @> margin (`px 5)
    @> box_shadow ~x:(`px 0) ~y:(`px 2) ~blur:(`px 4) ~spread:(`px 0) ~color:(`rgba (0, 0, 0, 0.1))
  )

  let meld_title = Css_gen.(
    font_size (`rem 0.8)
    @> color (`hex "666")
    @> margin_bottom (`px 5)
    @> text_align `center
  )

  let meld_tiles = Css_gen.(
    display `flex
    @> gap (`px 3)
  )

  let status = Css_gen.(
    text_align `center
    @> margin_top (`rem 1.25)
    @> padding (`rem 0.9375)
    @> border_radius (`px 8)
    @> border ~width:(`px 1) ~color:(`hex "c3e6cb") ~style:`solid
    @> background (`hex "d4edda")
    @> color (`hex "155724")
  )

  let status_warning = Css_gen.(
    text_align `center
    @> margin_top (`rem 1.25)
    @> padding (`rem 0.9375)
    @> border_radius (`px 8)
    @> border ~width:(`px 1) ~color:(`hex "ffeaa7") ~style:`solid
    @> background (`hex "fff3cd")
    @> color (`hex "856404")
  )

  let victory_message = Css_gen.(
    text_align `center
    @> margin_top (`rem 1.25)
    @> padding (`rem 1.25)
    @> background (linear_gradient ~direction:(`deg 45.) ~stops:[(0., `hex "28a745"); (100., `hex "20c997")])
    @> color `white
    @> border_radius (`px 10)
    @> font_size (`rem 1.5)
    @> font_weight `bold
  )

  let button = Css_gen.(
    background (`hex "667eea")
    @> color `white
    @> border `none
    @> border_radius (`px 8)
    @> padding ~top:(`rem 0.625) ~bottom:(`rem 0.625) ~left:(`rem 1.25) ~right:(`rem 1.25)
    @> font_size (`rem 1.)
    @> font_weight `bold
    @> cursor `pointer
    @> transition "all 0.2s ease"
  )

  let button_secondary = Css_gen.(
    background (`hex "6c757d")
    @> color `white
    @> border `none
    @> border_radius (`px 8)
    @> padding ~top:(`rem 0.625) ~bottom:(`rem 0.625) ~left:(`rem 1.25) ~right:(`rem 1.25)
    @> font_size (`rem 1.)
    @> font_weight `bold
    @> cursor `pointer
    @> transition "all 0.2s ease"
  )

  let button_success = Css_gen.(
    background (`hex "28a745")
    @> color `white
    @> border `none
    @> border_radius (`px 8)
    @> padding ~top:(`rem 0.625) ~bottom:(`rem 0.625) ~left:(`rem 1.25) ~right:(`rem 1.25)
    @> font_size (`rem 1.)
    @> font_weight `bold
    @> cursor `pointer
    @> transition "all 0.2s ease"
  )

  let controls = Css_gen.(
    display `flex
    @> gap (`rem 0.625)
    @> justify_content `center
    @> margin_top (`rem 1.25)
    @> flex_wrap `wrap
  )
end

let tile_to_string = function
  | Tile.Tile (Tile.Red, r) -> Printf.sprintf "R%02d" r
  | Tile.Tile (Tile.Blue, r) -> Printf.sprintf "B%02d" r
  | Tile.Tile (Tile.Black, r) -> Printf.sprintf "K%02d" r
  | Tile.Tile (Tile.Orange, r) -> Printf.sprintf "O%02d" r
  | Tile.Joker -> "Jk"

let tile_color_style = function
  | Tile.Tile (Tile.Red, _) -> Style.tile_red
  | Tile.Tile (Tile.Blue, _) -> Style.tile_blue
  | Tile.Tile (Tile.Black, _) -> Style.tile_black
  | Tile.Tile (Tile.Orange, _) -> Style.tile_orange
  | Tile.Joker -> Style.tile_joker

let render_tile ~tile ~selected ~on_click =
  let tile_text = tile_to_string tile in
  let color_style = tile_color_style tile in
  let base_style = Css_gen.(Style.tile @> color_style) in
  let style = if selected then Css_gen.(base_style @> Style.tile_selected) else base_style in
  Vdom.Node.button
    ~attrs:[
      Vdom.Attr.style style;
      Vdom.Attr.on_click (fun _ -> on_click);
    ]
    [Vdom.Node.text tile_text]

let render_meld meld =
  let meld_tile_nodes = List.map (fun tile ->
    let tile_text = tile_to_string tile in
    let color_style = tile_color_style tile in
    let style = Css_gen.(Style.tile @> color_style) in
    Vdom.Node.span
      ~attrs:[Vdom.Attr.style style]
      [Vdom.Node.text tile_text]
  ) meld in
  Vdom.Node.div
    ~attrs:[Vdom.Attr.style Style.meld]
    [
      Vdom.Node.div
        ~attrs:[Vdom.Attr.style Style.meld_title]
        [Vdom.Node.text (Printf.sprintf "Meld (%d pts)" (Meld.meld_points meld))];
      Vdom.Node.div
        ~attrs:[Vdom.Attr.style Style.meld_tiles]
        meld_tile_nodes
    ]

let render_hand ~hand ~selected_tiles ~on_tile_click ~is_current ~hide_tiles =
  let tiles = State.TileMultiset.to_list hand in
  if List.is_empty tiles then
    Vdom.Node.div
      ~attrs:[Vdom.Attr.style Style.hand_empty]
      [Vdom.Node.text "🎉 EMPTY HAND! 🎉"]
  else if hide_tiles && not is_current then
    Vdom.Node.div
      ~attrs:[Vdom.Attr.style Style.hand]
      (List.init (List.length tiles) (fun _ ->
        Vdom.Node.span
          ~attrs:[Vdom.Attr.style Css_gen.(Style.tile @> background (`hex "999") @> color `white)]
          [Vdom.Node.text "??"]
      ))
  else
    Vdom.Node.div
      ~attrs:[Vdom.Attr.style Style.hand]
      (List.mapi (fun i tile ->
        let selected = List.mem selected_tiles i ~equal:Int.equal in
        render_tile ~tile ~selected ~on_click:(fun () -> on_tile_click i)
      ) tiles)

let render_player ~player ~is_current ~is_winner ~selected_tiles ~on_tile_click ~hide_tiles =
  let player_style = 
    if is_winner then Style.player_winner
    else if is_current then Style.player_current
    else Style.player in
  let indicator_style = 
    if is_winner then Style.player_indicator_winner
    else if is_current then Style.player_indicator
    else Style.player_indicator_inactive in
  Vdom.Node.div
    ~attrs:[Vdom.Attr.style player_style]
    [
      Vdom.Node.h3
        ~attrs:[Vdom.Attr.style Style.player_title]
        [
          Vdom.Node.span
            ~attrs:[Vdom.Attr.style indicator_style]
            [];
          Vdom.Node.text (if is_winner then player.State.name ^ " - WINNER! 🏆"
                          else player.State.name);
        ];
      Vdom.Node.p
        []
        [Vdom.Node.text (Printf.sprintf "Hand: %d tiles" 
          (List.length (State.TileMultiset.to_list player.State.hand)))];
      render_hand ~hand:player.State.hand ~selected_tiles ~on_tile_click ~is_current ~hide_tiles;
    ]

let render_board board =
  if List.is_empty board then
    Vdom.Node.div
      ~attrs:[Vdom.Attr.style Style.board_empty]
      [Vdom.Node.text "No tiles on the table yet"]
  else
    Vdom.Node.div
      []
      (List.map render_meld board)

(* Simple AI: try to find valid melds in hand *)
module SimpleAI = struct
  let find_groups tiles =
    (* Group tiles by rank *)
    let by_rank = Hashtbl.create (module Int) in
    List.iter tiles ~f:(fun tile ->
      match tile with
      | Tile.Tile (_, rank) ->
          Hashtbl.add_multi by_rank ~key:rank ~data:tile
      | Tile.Joker -> ()
    );
    (* Find groups of 3+ with different colors *)
    Hashtbl.fold by_rank ~init:[] ~f:(fun ~key:_ ~data:group acc ->
      if List.length group >= 3 && Meld.is_meld group then
        group :: acc
      else acc
    )

  let find_runs tiles =
    (* Group tiles by color *)
    let by_color = Hashtbl.create (module Tile.Color) ~size:4 in
    List.iter tiles ~f:(fun tile ->
      match tile with
      | Tile.Tile (color, _) ->
          Hashtbl.add_multi by_color ~key:color ~data:tile
      | Tile.Joker -> ()
    );
    (* For each color, try to find runs of 3+ consecutive *)
    Hashtbl.fold by_color ~init:[] ~f:(fun ~key:_ ~data:color_tiles acc ->
      let sorted = List.sort color_tiles ~compare:Tile.compare_tile in
      let rec find_run current acc_runs = function
        | [] ->
            if List.length current >= 3 && Meld.is_meld current then
              current :: acc_runs
            else acc_runs
        | tile :: rest ->
            match tile, List.hd current with
            | Tile.Tile (_, r1), Some (Tile.Tile (_, r2)) when r1 = r2 + 1 ->
                find_run (tile :: current) acc_runs rest
            | Tile.Tile _, Some _ ->
                let new_acc = if List.length current >= 3 && Meld.is_meld current then
                  current :: acc_runs else acc_runs in
                find_run [tile] new_acc rest
            | Tile.Tile _, None ->
                find_run [tile] acc_runs rest
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
    
    (* Try to play melds that meet initial 30 requirement if needed *)
    if not player.State.met_initial_30 then
      let valid_initial = List.filter all_melds ~f:(fun melds ->
        Rules.initial_30_ok [melds]
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
      { model with game_mode = Some mode; num_players }
  
  | StartGame ->
      let mode = Option.value model.game_mode ~default:VsComputer in
      let rng = Random.State.make_self_init () in
      let num_players = model.num_players in
      let game_state = 
        let base_state = State.initial_state rng in
        if num_players = 2 then base_state
        else
          (* Add more players *)
          let deck_tiles = base_state.State.deck in
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
          let players_list, remaining_deck = deal_hands num_players (State.TileMultiset.to_list (List.fold base_state.State.players ~init:State.TileMultiset.empty ~f:(fun acc p -> List.fold (State.TileMultiset.to_list p.State.hand) ~init:acc ~f:State.TileMultiset.add)) @ deck_tiles) [] in
          { base_state with 
            players = Array.of_list players_list;
            deck = remaining_deck;
          }
      in
      let game_state = match mode with
        | VsComputer ->
            { game_state with 
              players = [|
                { (game_state.players.(0)) with name = "You" };
                { (game_state.players.(1)) with name = "Computer" };
              |]
            }
        | PassAndPlay ->
            { game_state with 
              players = [|
                { (game_state.players.(0)) with name = "Player 1" };
                { (game_state.players.(1)) with name = "Player 2" };
              |]
            }
        | ThreePlayer ->
            game_state
        | FourPlayer ->
            game_state
      in
      { model with 
        game_state = Some game_state;
        selected_tiles = [];
        message = "Game started! Make your first move.";
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
                }
            | Error error_msg ->
                { model with message = "Error: " ^ error_msg }
      )
  
  | DrawTile ->
      (match model.game_state with
      | None -> model
      | Some state ->
          if Rules.is_game_over state then model
          else
            match Rules.apply_draw state with
            | Ok new_state ->
                let new_state = Rules.next_turn new_state in
                { model with 
                  game_state = Some new_state;
                  selected_tiles = [];
                  message = "Drew a tile";
                }
            | Error error_msg ->
                { model with message = "Error: " ^ error_msg }
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
            }
      )
  
  | NewGame ->
      { model with 
        game_state = None;
        selected_tiles = [];
        message = "Select game mode";
        game_mode = None;
      }
  
  | BotMove ->
      (match model.game_state, model.game_mode with
      | Some state, Some VsComputer ->
          if state.turn = 1 && not (Rules.is_game_over state) then
            (* Computer's turn *)
            match SimpleAI.find_best_play state 1 with
            | Some melds ->
                (match Rules.apply_play state melds with
                | Ok new_state ->
                    let new_state = Rules.next_turn new_state in
                    { model with 
                      game_state = Some new_state;
                      message = "Computer played a meld";
                    }
                | Error _ ->
                    (* If can't play, draw *)
                    match Rules.apply_draw state with
                    | Ok new_state ->
                        let new_state = Rules.next_turn new_state in
                        { model with 
                          game_state = Some new_state;
                          message = "Computer drew a tile";
                        }
                    | Error _ ->
                        let new_state = Rules.next_turn state in
                        { model with 
                          game_state = Some new_state;
                          message = "Computer passed";
                        }
                )
            | None ->
                (* No valid play, try to draw *)
                match Rules.apply_draw state with
                | Ok new_state ->
                    let new_state = Rules.next_turn new_state in
                    { model with 
                      game_state = Some new_state;
                      message = "Computer drew a tile";
                    }
                | Error _ ->
                    let new_state = Rules.next_turn state in
                    { model with 
                      game_state = Some new_state;
                      message = "Computer passed";
                    }
          else model
      | _ -> model
      )

let component =
  let%sub model, inject =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module Action)
      ~default_model:{
        game_state = None;
        selected_tiles = [];
        message = "Select game mode";
        game_mode = None;
        num_players = 2;
      }
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        apply_action model action
      )
  in

  (* Trigger bot move when it's computer's turn *)
  let%sub () =
    let%sub callback =
      let%arr inject = inject
      and model = model in
      fun () ->
        match model.game_mode, model.game_state with
        | Some VsComputer, Some state ->
            if state.State.turn = 1 && not (Rules.is_game_over state) then
              inject BotMove
        | _ -> ()
    in
    Bonsai.Edge.lifecycle
      ~on_activate:callback
      ()
  in

  let%sub view =
    let%arr model = model
    and inject = inject in
    
    match model.game_state with
    | None ->
        (* Mode selection screen *)
        Vdom.Node.div
          ~attrs:[Vdom.Attr.style Style.container]
          [
            Vdom.Node.div
              ~attrs:[Vdom.Attr.style Style.game_container]
              [
                Vdom.Node.h1
                  ~attrs:[Vdom.Attr.style Style.title]
                  [Vdom.Node.text "🀄 Rummikub"];
                Vdom.Node.div
                  ~attrs:[Vdom.Attr.style Style.mode_selection]
                  [
                    Vdom.Node.h2
                      ~attrs:[Vdom.Attr.style Css_gen.(text_align `center @> margin_bottom (`rem 1.5))]
                      [Vdom.Node.text "Select Game Mode"];
                    Vdom.Node.button
                      ~attrs:[
                        Vdom.Attr.style Style.mode_button;
                        Vdom.Attr.on_click (fun _ -> 
                          inject (SelectMode VsComputer);
                          inject StartGame;
                        );
                      ]
                      [Vdom.Node.text "🤖 Play vs Computer"];
                    Vdom.Node.button
                      ~attrs:[
                        Vdom.Attr.style Style.mode_button;
                        Vdom.Attr.on_click (fun _ -> 
                          inject (SelectMode PassAndPlay);
                          inject StartGame;
                        );
                      ]
                      [Vdom.Node.text "📱 Pass-and-Play (2 Players)"];
                    Vdom.Node.button
                      ~attrs:[
                        Vdom.Attr.style Style.mode_button;
                        Vdom.Attr.on_click (fun _ -> 
                          inject (SelectMode ThreePlayer);
                          inject StartGame;
                        );
                      ]
                      [Vdom.Node.text "👥 Pass-and-Play (3 Players)"];
                    Vdom.Node.button
                      ~attrs:[
                        Vdom.Attr.style Style.mode_button;
                        Vdom.Attr.on_click (fun _ -> 
                          inject (SelectMode FourPlayer);
                          inject StartGame;
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
        
        let game_info = Vdom.Node.div
          ~attrs:[Vdom.Attr.style Style.game_info]
          [
            Vdom.Node.div
              ~attrs:[Vdom.Attr.style Style.info_card]
              [
                Vdom.Node.h3 [] [Vdom.Node.text "Game Status"];
                Vdom.Node.p [] [Vdom.Node.text (Printf.sprintf "Phase: %s" 
                  (if is_game_over then "Game Over" else "In Progress"))];
                Vdom.Node.p [] [Vdom.Node.text (Printf.sprintf "Turn: %s" current_player.name)];
                Vdom.Node.p [] [Vdom.Node.text (Printf.sprintf "Deck: %d tiles remaining" 
                  (List.length state.deck))];
              ];
            Vdom.Node.div
              ~attrs:[Vdom.Attr.style Style.info_card]
              [
                Vdom.Node.h3 [] [Vdom.Node.text "Rules"];
                Vdom.Node.p [] [Vdom.Node.text "• First play must total 30+ points"];
                Vdom.Node.p [] [Vdom.Node.text "• Form groups (same rank) or runs (same color)"];
                Vdom.Node.p [] [Vdom.Node.text "• Use jokers as wild cards"];
              ];
          ] in
        
        let board = Vdom.Node.div
          ~attrs:[Vdom.Attr.style Style.board]
          [
            Vdom.Node.h3
              ~attrs:[Vdom.Attr.style Style.board_title]
              [Vdom.Node.text "Table"];
            render_board state.board;
          ] in
        
        let players_style = match model.num_players with
          | 2 -> Style.players
          | 3 -> Style.players_three
          | _ -> Style.players_four
        in
        
        let players = Vdom.Node.div
          ~attrs:[Vdom.Attr.style players_style]
          (Array.to_list (Array.mapi state.players ~f:(fun idx player ->
            let is_current = state.turn = idx in
            let is_winner = Option.is_some winner && 
              String.equal (Option.value_exn winner).State.name player.State.name in
            render_player 
              ~player 
              ~is_current 
              ~is_winner
              ~selected_tiles:(if is_current then model.selected_tiles else [])
              ~on_tile_click:(fun i -> if is_current then inject (ToggleTile i) else ())
              ~hide_tiles
          ))) in
        
        let controls = if not is_game_over then
          Vdom.Node.div
            ~attrs:[Vdom.Attr.style Style.controls]
            [
              Vdom.Node.button
                ~attrs:[
                  Vdom.Attr.style Style.button_success;
                  Vdom.Attr.on_click (fun _ -> inject PlaySelected);
                  Vdom.Attr.disabled (List.is_empty model.selected_tiles);
                ]
                [Vdom.Node.text "Play Selected"];
              Vdom.Node.button
                ~attrs:[
                  Vdom.Attr.style Style.button;
                  Vdom.Attr.on_click (fun _ -> inject DrawTile);
                ]
                [Vdom.Node.text "Draw"];
              Vdom.Node.button
                ~attrs:[
                  Vdom.Attr.style Style.button_secondary;
                  Vdom.Attr.on_click (fun _ -> inject PassTurn);
                ]
                [Vdom.Node.text "Pass"];
            ]
        else
          Vdom.Node.div
            ~attrs:[Vdom.Attr.style Style.controls]
            [
              Vdom.Node.button
                ~attrs:[
                  Vdom.Attr.style Style.button;
                  Vdom.Attr.on_click (fun _ -> inject NewGame);
                ]
                [Vdom.Node.text "New Game"];
            ] in
        
        let status_style = if String.is_prefix model.message ~prefix:"Error" then
          Style.status_warning
        else
          Style.status in
        let status = Vdom.Node.div
          ~attrs:[Vdom.Attr.style status_style]
          [Vdom.Node.text model.message] in
        
        let victory_message = if is_game_over && Option.is_some winner then
          Vdom.Node.div
            ~attrs:[Vdom.Attr.style Style.victory_message]
            [
              Vdom.Node.text "🎊 CONGRATULATIONS! 🎊";
              Vdom.Node.br [];
              Vdom.Node.text (Printf.sprintf "%s wins the game!" (Option.value_exn winner).State.name);
            ]
        else
          Vdom.Node.none in
        
        Vdom.Node.div
          ~attrs:[Vdom.Attr.style Style.container]
          [
            Vdom.Node.div
              ~attrs:[Vdom.Attr.style Style.game_container]
              [
                Vdom.Node.h1
                  ~attrs:[Vdom.Attr.style Style.title]
                  [Vdom.Node.text "🀄 Rummikub"];
                game_info;
                board;
                players;
                controls;
                status;
                victory_message;
              ];
          ]
  in
  
  return view

let app = component
