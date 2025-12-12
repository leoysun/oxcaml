(* Firebase Authentication module *)
open Firebase
module Js = Js_of_ocaml.Js

type user = {
  uid : string;
  email : string option;
  display_name : string option;
  photo_url : string option;
}

let user_of_js (js_user : 'a Js.t) : user =
  {
    uid = Js.Unsafe.get js_user "uid" |> Js.to_string;
    email = (try Some (Js.Unsafe.get js_user "email" |> Js.to_string) with _ -> None);
    display_name = (try Some (Js.Unsafe.get js_user "displayName" |> Js.to_string) with _ -> None);
    photo_url = (try Some (Js.Unsafe.get js_user "photoURL" |> Js.to_string) with _ -> None);
  }

let current_user () : user option =
  let auth = get_auth () in
  let js_user = Js.Unsafe.meth_call auth "currentUser" [||] in
  if Js.to_string (Js.typeof js_user) = "undefined" || (try Js.to_bool (Js.Unsafe.get js_user "isNull") with _ -> true) then
    None
  else
    Some (user_of_js js_user)

(* Sign in with Google *)
let sign_in_with_google (on_success : user -> unit) (on_error : string -> unit) : unit =
  let auth = get_auth () in
  let firebase = Js.Unsafe.get Js.Unsafe.global "firebase" in
  let firebase_auth = Js.Unsafe.get firebase "auth" in
  let google_provider = Js.Unsafe.new_obj (Js.Unsafe.get firebase_auth "GoogleAuthProvider") [||] in
  let promise = Js.Unsafe.meth_call auth "signInWithPopup" [|google_provider|] in
  let success_cb = Js.wrap_callback (fun result ->
    let user = Js.Unsafe.get result "user" in
    on_success (user_of_js user)
  ) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  let then_fn = Js.Unsafe.get promise "then" in
  let catch_fn = Js.Unsafe.get promise "catch" in
  ignore (Js.Unsafe.fun_call then_fn [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.fun_call catch_fn [|Js.Unsafe.inject error_cb|])

(* Sign in with Facebook *)
let sign_in_with_facebook (on_success : user -> unit) (on_error : string -> unit) : unit =
  let auth = get_auth () in
  let firebase = Js.Unsafe.get Js.Unsafe.global "firebase" in
  let firebase_auth = Js.Unsafe.get firebase "auth" in
  let facebook_provider = Js.Unsafe.new_obj (Js.Unsafe.get firebase_auth "FacebookAuthProvider") [||] in
  let promise = Js.Unsafe.meth_call auth "signInWithPopup" [|facebook_provider|] in
  let success_cb = Js.wrap_callback (fun result ->
    let user = Js.Unsafe.get result "user" in
    on_success (user_of_js user)
  ) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  let then_fn = Js.Unsafe.get promise "then" in
  let catch_fn = Js.Unsafe.get promise "catch" in
  ignore (Js.Unsafe.fun_call then_fn [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.fun_call catch_fn [|Js.Unsafe.inject error_cb|])

(* Sign in with email and password *)
let sign_in_with_email (email : string) (password : string) (on_success : user -> unit) (on_error : string -> unit) : unit =
  let auth = get_auth () in
  let promise = Js.Unsafe.meth_call auth "signInWithEmailAndPassword" [|
    Js.Unsafe.inject (Js.string email);
    Js.Unsafe.inject (Js.string password)
  |] in
  let success_cb = Js.wrap_callback (fun js_user ->
    on_success (user_of_js js_user)
  ) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

(* Create account with email and password *)
let create_user_with_email (email : string) (password : string) (on_success : user -> unit) (on_error : string -> unit) : unit =
  let auth = get_auth () in
  let promise = Js.Unsafe.meth_call auth "createUserWithEmailAndPassword" [|
    Js.Unsafe.inject (Js.string email);
    Js.Unsafe.inject (Js.string password)
  |] in
  let success_cb = Js.wrap_callback (fun js_user ->
    on_success (user_of_js js_user)
  ) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

(* Sign in anonymously (guest account) *)
let sign_in_anonymously (on_success : user -> unit) (on_error : string -> unit) : unit =
  let auth = get_auth () in
  let promise = Js.Unsafe.meth_call auth "signInAnonymously" [||] in
  let success_cb = Js.wrap_callback (fun result ->
    let js_user = Js.Unsafe.get result "user" in
    on_success (user_of_js js_user)
  ) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  let then_fn = Js.Unsafe.get promise "then" in
  let catch_fn = Js.Unsafe.get promise "catch" in
  ignore (Js.Unsafe.fun_call then_fn [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.fun_call catch_fn [|Js.Unsafe.inject error_cb|])

(* Sign out *)
let sign_out (on_success : unit -> unit) (on_error : string -> unit) : unit =
  let auth = get_auth () in
  let promise = Js.Unsafe.meth_call auth "signOut" [||] in
  let success_cb = Js.wrap_callback (fun _ -> on_success ()) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

(* Listen to auth state changes *)
type auth_state_listener = user option -> unit

let on_auth_state_changed (callback : auth_state_listener) : (unit -> unit) =
  let auth = get_auth () in
  let unsubscribe = Js.Unsafe.meth_call auth "onAuthStateChanged" [|
    Js.Unsafe.inject (Js.wrap_callback (fun js_user ->
      if Js.to_string (Js.typeof js_user) = "undefined" || (try Js.to_bool (Js.Unsafe.get js_user "isNull") with _ -> true) then
        callback None
      else
        callback (Some (user_of_js js_user))
    ))
  |] in
  fun () -> ignore (Js.Unsafe.fun_call unsubscribe [||])

