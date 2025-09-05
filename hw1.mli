open! Core

type player_kind =
  | X
  | O

type cell_position =
  { row : int
  ; column : int
  }

type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind
  | Stalemate

type game_state =
  { board : (cell_position * player_kind) list
  ; rows : int
  ; columns : int
  ; winning_sequence_length : int
  ; decision : decision
  }

type move = cell_position

val initial_state : game_state
val move_at_0x0 : move
val state_after_move_at_0x0 : game_state
val before_terminal_state : game_state
val move_to_terminal_state : move
val terminal_state : game_state
