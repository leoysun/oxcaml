(* Firebase Authentication module *)

type user = {
  uid : string;
  email : string option;
  display_name : string option;
  photo_url : string option;
}

val current_user : unit -> user option
val sign_in_with_google : (user -> unit) -> (string -> unit) -> unit
val sign_in_with_facebook : (user -> unit) -> (string -> unit) -> unit
val sign_in_with_email : string -> string -> (user -> unit) -> (string -> unit) -> unit
val create_user_with_email : string -> string -> (user -> unit) -> (string -> unit) -> unit
val sign_in_anonymously : (user -> unit) -> (string -> unit) -> unit
val sign_out : (unit -> unit) -> (string -> unit) -> unit
val on_auth_state_changed : (user option -> unit) -> (unit -> unit)

