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

(*=
 | |
-----
 | |
-----
 | |
*)
let initial_state : game_state =
  { board = []
  ; rows = 3
  ; columns = 3
  ; winning_sequence_length = 3
  ; decision = In_progress { whose_turn = X }
  }
;;

let move_at_0x0 : move = { row = 0; column = 0 }

(*=
X| |
-----
 | |
-----
 | |
*)
let state_after_move_at_0x0 : game_state =
  { board = [ move_at_0x0, X ]
  ; rows = 3
  ; columns = 3
  ; winning_sequence_length = 3
  ; decision = In_progress { whose_turn = O }
  }
;;

(*=
 | |X
-----
O|O|X
-----
 | |
*)
let before_terminal_state : game_state =
  { board =
      [ { row = 0; column = 2 }, X
      ; { row = 1; column = 0 }, O
      ; { row = 1; column = 2 }, X
      ; { row = 1; column = 1 }, O
      ]
  ; rows = 3
  ; columns = 3
  ; winning_sequence_length = 3
  ; decision = In_progress { whose_turn = X }
  }
;;

let move_to_terminal_state : move = { row = 2; column = 2 }

(*=
 | |X
-----
O|O|X
-----
 | |X
*)
let terminal_state : game_state =
  { board =
      [ { row = 0; column = 2 }, X
      ; { row = 1; column = 0 }, O
      ; { row = 1; column = 2 }, X
      ; { row = 1; column = 1 }, O
      ; { row = 2; column = 2 }, X
      ]
  ; rows = 3
  ; columns = 3
  ; winning_sequence_length = 3
  ; decision = Winner X
  }
;;
