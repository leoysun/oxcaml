(* Firebase initialization and basic bindings *)

type firebase_app
type firestore
type auth

val init : string -> unit
val get_firestore : unit -> firestore Js_of_ocaml.Js.t
val get_auth : unit -> auth Js_of_ocaml.Js.t
val is_initialized : unit -> bool

