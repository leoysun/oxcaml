(* Firebase initialization and basic bindings *)
module Js = Js_of_ocaml.Js

type firebase_app
type firestore
type auth

let firebase_global : firebase_app Js.t option ref = ref None
let firestore_instance : firestore Js.t option ref = ref None
let auth_instance : auth Js.t option ref = ref None

let init (config : string) : unit =
  (* config is a JSON string with Firebase config *)
  let firebase = Js.Unsafe.get Js.Unsafe.global "firebase" in
  if Js.to_string (Js.typeof firebase) = "undefined" then
    failwith "Firebase SDK not loaded. Please include Firebase scripts in HTML."
  else
    let json = Js.Unsafe.get Js.Unsafe.global "JSON" in
    let config_obj = Js.Unsafe.meth_call json "parse" [|Js.Unsafe.inject (Js.string config)|] in
    let app = Js.Unsafe.meth_call firebase "initializeApp" [|config_obj|] in
    firebase_global := Some app;
    let fs = Js.Unsafe.meth_call firebase "firestore" [||] in
    firestore_instance := Some fs;
    let auth = Js.Unsafe.meth_call app "auth" [||] in
    auth_instance := Some auth

let get_firestore () : firestore Js.t =
  match !firestore_instance with
  | Some fs -> fs
  | None -> failwith "Firestore not initialized. Call Firebase.init first."

let get_auth () : auth Js.t =
  match !auth_instance with
  | Some auth -> auth
  | None -> failwith "Auth not initialized. Call Firebase.init first."

let is_initialized () : bool =
  !firestore_instance <> None && !auth_instance <> None

