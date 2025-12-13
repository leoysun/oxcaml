(* Firebase Authentication module *)
open Firebase
module Js = Js_of_ocaml.Js

type user = {
  uid : string;
  email : string option;
  display_name : string option;
  photo_url : string option;
}

(* Convert Firebase error codes to user-friendly messages *)
let friendly_error_message (err : 'a Js.t) : string =
  let code = try Js.Unsafe.get err "code" |> Js.to_string with _ -> "" in
  let message = try Js.Unsafe.get err "message" |> Js.to_string with _ -> "Unknown error" in
  match code with
  | "auth/email-already-in-use" ->
      "This email is already registered. Please sign in instead, or use a different email."
  | "auth/invalid-email" ->
      "Please enter a valid email address."
  | "auth/weak-password" ->
      "Password is too weak. Please use at least 6 characters."
  | "auth/user-not-found" ->
      "No account found with this email. Please check your email or create a new account."
  | "auth/wrong-password" ->
      "Incorrect password. Please try again."
  | "auth/invalid-credential" ->
      "Invalid email or password. Please check your credentials and try again."
  | "auth/too-many-requests" ->
      "Too many failed attempts. Please wait a moment before trying again."
  | "auth/user-disabled" ->
      "This account has been disabled. Please contact support."
  | "auth/network-request-failed" ->
      "Network error. Please check your internet connection and try again."
  | "auth/popup-closed-by-user" ->
      "Sign-in was cancelled. Please try again."
  | "auth/operation-not-allowed" ->
      "This sign-in method is not enabled. Please contact support."
  | "auth/requires-recent-login" ->
      "Please sign out and sign in again to continue."
  | "" -> message  (* No code, use the raw message *)
  | _ -> 
      (* For unknown codes, provide a cleaner message *)
      Printf.sprintf "Authentication failed: %s" message

let user_of_js (js_user : 'a Js.t) : user =
  (* Helper to check if a JS value is null or undefined *)
  let is_null_or_undefined v =
    let check_fn = Js.Unsafe.eval_string "(function(x) { return x === null || x === undefined; })" in
    Js.to_bool (Js.Unsafe.fun_call check_fn [|Js.Unsafe.inject v|])
  in
  let get_string field =
    let v = Js.Unsafe.get js_user field in
    if is_null_or_undefined v then ""
    else try Js.to_string v with _ -> ""
  in
  let get_string_opt field =
    let v = Js.Unsafe.get js_user field in
    if is_null_or_undefined v then None
    else 
      let s = try Js.to_string v with _ -> "" in
      if String.length s > 0 then Some s else None
  in
  {
    uid = get_string "uid";
    email = get_string_opt "email";
    display_name = get_string_opt "displayName";
    photo_url = get_string_opt "photoURL";
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
    on_error (friendly_error_message err)
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

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
    on_error (friendly_error_message err)
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

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
    on_error (friendly_error_message err)
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
    on_error (friendly_error_message err)
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
    on_error (friendly_error_message err)
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

(* Sign out *)
let sign_out (on_success : unit -> unit) (on_error : string -> unit) : unit =
  let auth = get_auth () in
  let promise = Js.Unsafe.meth_call auth "signOut" [||] in
  let success_cb = Js.wrap_callback (fun _ -> on_success ()) in
  let error_cb = Js.wrap_callback (fun err ->
    on_error (friendly_error_message err)
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

