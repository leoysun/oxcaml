(* Firestore operations with real-time listeners *)
type unsubscribe = unit -> unit

(* Export State type directly to avoid module path issues *)
module State = State

val save_game_state : string -> State.t -> (unit -> unit) -> (string -> unit) -> unit
val load_game_state : string -> (State.t option -> unit) -> (string -> unit) -> unit
val listen_to_game_state : string -> (State.t -> unit) -> (string -> unit) -> unsubscribe
val create_game : State.t -> (string -> unit) -> (string -> unit) -> unit
val join_matchmaking_queue : string -> (string -> unit) -> (string -> unit) -> unsubscribe
val leave_matchmaking_queue : string -> (unit -> unit) -> (string -> unit) -> unit

