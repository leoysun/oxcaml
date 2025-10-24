open State

val board_valid : board -> bool

val apply_play : t -> meld list -> (t, string) result
val apply_draw : t -> (t, string) result
val next_turn  : t -> t

val make_move  : t -> move -> (t, string) result


let make_move st = function
  | Play melds ->
      begin match apply_play st melds with
      | Ok st'   -> Ok (next_turn st')
      | Error e  -> Error e
      end
  | Draw ->
      begin match apply_draw st with
      | Ok st'   -> Ok (next_turn st')
      | Error e  -> Error e
      end
  | Pass ->
      Ok (next_turn st)
