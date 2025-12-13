(* Firestore operations with real-time listeners *)
open Firebase
open Game_sync
[@@@warning "-33"]  (* Suppress unused-open warning for State *)
open State
module Js = Js_of_ocaml.Js

(* Export State module for interface compatibility *)
module State = State

type unsubscribe = unit -> unit
type document_snapshot
[@@@warning "-34"]  (* Suppress unused-type warning for query_snapshot *)
type query_snapshot

let doc (db : firestore Js.t) (collection : string) (doc_id : string) : document_snapshot Js.t =
  let collection_ref = Js.Unsafe.meth_call db "collection" [|Js.Unsafe.inject (Js.string collection)|] in
  let doc_ref = Js.Unsafe.meth_call collection_ref "doc" [|Js.Unsafe.inject (Js.string doc_id)|] in
  Js.Unsafe.coerce doc_ref

let set_doc (doc_ref : document_snapshot Js.t) (data : 'a Js.t) (on_success : unit -> unit) (on_error : string -> unit) : unit =
  let promise = Js.Unsafe.meth_call doc_ref "set" [|data|] in
  let success_cb = Js.wrap_callback (fun _ -> on_success ()) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

let get_doc (doc_ref : document_snapshot Js.t) (on_success : document_snapshot Js.t -> unit) (on_error : string -> unit) : unit =
  let promise = Js.Unsafe.meth_call doc_ref "get" [||] in
  let success_cb = Js.wrap_callback on_success in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

[@@@warning "-32"]  (* Suppress unused-value warning *)
let update_doc (doc_ref : document_snapshot Js.t) (data : 'a Js.t) (on_success : unit -> unit) (on_error : string -> unit) : unit =
  let promise = Js.Unsafe.meth_call doc_ref "update" [|data|] in
  let success_cb = Js.wrap_callback (fun _ -> on_success ()) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

[@@@warning "-32"]  (* Suppress unused-value warning *)
let delete_doc (doc_ref : document_snapshot Js.t) (on_success : unit -> unit) (on_error : string -> unit) : unit =
  let promise = Js.Unsafe.meth_call doc_ref "delete" [||] in
  let success_cb = Js.wrap_callback (fun _ -> on_success ()) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

(* Real-time listener for a document *)
[@@@warning "-32"]  (* Suppress unused-value warning *)
let on_snapshot 
    (doc_ref : document_snapshot Js.t)
    (callback : document_snapshot Js.t -> unit)
    : unsubscribe =
  let unsubscribe_fn = Js.Unsafe.meth_call doc_ref "onSnapshot" [|
    Js.Unsafe.inject (Js.wrap_callback callback)
  |] in
  fun () -> ignore (Js.Unsafe.fun_call unsubscribe_fn [||])

(* Real-time listener with error handling *)
let on_snapshot_with_error
    (doc_ref : document_snapshot Js.t)
    (on_next : document_snapshot Js.t -> unit)
    (on_error : string -> unit)
    : unsubscribe =
  let unsubscribe_fn = Js.Unsafe.meth_call doc_ref "onSnapshot" [|
    Js.Unsafe.inject (Js.wrap_callback on_next);
    Js.Unsafe.inject (Js.wrap_callback (fun err ->
      let msg = Js.Unsafe.get err "message" |> Js.to_string in
      on_error msg
    ))
  |] in
  fun () -> ignore (Js.Unsafe.fun_call unsubscribe_fn [||])

(* Get data from a document snapshot *)
let snapshot_data (snapshot : document_snapshot Js.t) : 'a Js.t option =
  (* exists is a property in modern Firebase SDK, not a method *)
  let exists_val = Js.Unsafe.get snapshot "exists" in
  let exists_bool = 
    (* Handle both property (boolean) and method (function) cases *)
    if Js.to_string (Js.typeof exists_val) = "function" then
      Js.to_bool (Js.Unsafe.meth_call snapshot "exists" [||])
    else
      Js.to_bool exists_val
  in
  if exists_bool then
    Some (Js.Unsafe.meth_call snapshot "data" [||])
  else
    None

(* Game-specific Firestore operations *)
let save_game_state (game_id : string) (state : State.t) (on_success : unit -> unit) (on_error : string -> unit) : unit =
  let db = get_firestore () in
  let doc_ref = doc db "games" game_id in
  let data = state_to_firestore state in
  set_doc doc_ref data on_success on_error

let load_game_state (game_id : string) (on_success : State.t option -> unit) (on_error : string -> unit) : unit =
  let db = get_firestore () in
  let doc_ref = doc db "games" game_id in
  get_doc doc_ref
    (fun snapshot ->
      match snapshot_data snapshot with
      | Some data ->
          (match state_of_firestore data with
           | Some state -> on_success (Some state)
           | None -> on_success None)
      | None -> on_success None)
    on_error

let listen_to_game_state
    (game_id : string)
    (on_update : State.t -> unit)
    (on_error : string -> unit)
    : unsubscribe =
  let db = get_firestore () in
  let doc_ref = doc db "games" game_id in
  on_snapshot_with_error doc_ref
    (fun snapshot ->
      match snapshot_data snapshot with
      | Some data ->
          (match state_of_firestore data with
           | Some state -> on_update state
           | None -> on_error "Failed to parse game state")
      | None -> ())
    on_error

(* Create a new game and return its ID via callback *)
(* user_id is the creator's user ID, num_players is total players (2-4) *)
let create_game_with_user (state : State.t) (user_id : string) (num_players : int) (on_success : string -> unit) (on_error : string -> unit) : unit =
  let db = get_firestore () in
  let collection_ref = Js.Unsafe.meth_call db "collection" [|Js.Unsafe.inject (Js.string "games")|] in
  let doc_ref = Js.Unsafe.meth_call collection_ref "doc" [||] in
  let game_id = Js.Unsafe.get doc_ref "id" |> Js.to_string in
  let data = state_to_firestore state in
  (* Create player_ids array with correct number of slots - creator is player 0, others are empty *)
  let player_ids = Array.init num_players (fun i ->
    if i = 0 then Js.Unsafe.inject (Js.string user_id)
    else Js.Unsafe.inject Js.null
  ) in
  Js.Unsafe.set data (Js.string "player_ids") (Js.array player_ids);
  Js.Unsafe.set data (Js.string "num_players") (Js.number_of_float (Float.of_int num_players));
  Js.Unsafe.set data (Js.string "status") (Js.string "waiting");  (* waiting for other players *)
  set_doc doc_ref data
    (fun () -> on_success game_id)
    on_error

(* Legacy create_game without user tracking *)
let create_game (state : State.t) (on_success : string -> unit) (on_error : string -> unit) : unit =
  create_game_with_user state "unknown" 2 on_success on_error

(* Helper to check if a JS value is an empty slot (null, undefined, or not a string) *)
let is_empty_slot v =
  let check_fn = Js.Unsafe.eval_string "(function(x) { return x === null || x === undefined || typeof x !== 'string' || x === ''; })" in
  Js.to_bool (Js.Unsafe.fun_call check_fn [|Js.Unsafe.inject v|])

(* Join an existing game - finds first available player slot *)
let join_game (game_id : string) (user_id : string) (on_success : State.t -> int -> unit) (on_error : string -> unit) : unit =
  let db = get_firestore () in
  let doc_ref = doc db "games" game_id in
  get_doc doc_ref
    (fun snapshot ->
      match snapshot_data snapshot with
      | Some data ->
          let player_ids = try Some (Js.Unsafe.get data "player_ids") with _ -> None in
          let status = try Some (Js.Unsafe.get data "status" |> Js.to_string) with _ -> None in
          
          (match player_ids, status with
           | Some ids, Some s when String.equal s "waiting" ->
               let ids_array = Js.to_array ids in
               let num_players = Array.length ids_array in
               
               (* Find first empty slot *)
               let empty_slot = ref None in
               for i = 0 to num_players - 1 do
                 if Option.is_none !empty_slot && is_empty_slot (Array.get ids_array i) then
                   empty_slot := Some i
               done;
               
               (match !empty_slot with
                | Some slot_idx ->
                    (* Fill the empty slot with the user ID *)
                    Array.set ids_array slot_idx (Js.Unsafe.inject (Js.string user_id));
                    let new_ids = Js.array ids_array in
                    
                    (* Check if all slots are now filled *)
                    let all_filled = ref true in
                    for i = 0 to num_players - 1 do
                      if is_empty_slot (Array.get ids_array i) then
                        all_filled := false
                    done;
                    let new_status = if !all_filled then "playing" else "waiting" in
                    
                    (* Update the document *)
                    let update_data = Js.Unsafe.obj [||] in
                    Js.Unsafe.set update_data (Js.string "player_ids") new_ids;
                    Js.Unsafe.set update_data (Js.string "status") (Js.string new_status);
                    
                    let promise = Js.Unsafe.meth_call doc_ref "update" [|update_data|] in
                    let success_cb = Js.wrap_callback (fun _ ->
                      match state_of_firestore data with
                      | Some state -> on_success state slot_idx
                      | None -> on_error "Failed to parse game state"
                    ) in
                    let error_cb = Js.wrap_callback (fun err ->
                      let msg = Js.Unsafe.get err "message" |> Js.to_string in
                      on_error msg
                    ) in
                    ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
                    ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])
                | None ->
                    on_error "Game is full - no empty slots available")
           | Some _, Some s when String.equal s "playing" ->
               on_error "Game is already full"
           | None, _ ->
               (* Legacy game without player_ids - allow joining as player 1 *)
               (match state_of_firestore data with
                | Some state -> on_success state 1
                | None -> on_error "Failed to parse game state")
           | _, None ->
               on_error "Game not found or invalid status"
           | Some _, Some _ ->
               on_error "Game has an unexpected status")
      | None -> on_error "Game not found")
    on_error

(* Matchmaking: Add player to queue and try to match with another player *)
(* Returns unsubscribe function for the queue listener *)
let join_matchmaking_queue (user_id : string) (on_matched : string -> unit) (on_error : string -> unit) : (unit -> unit) =
  let db = get_firestore () in
  let queue_ref = Js.Unsafe.meth_call db "collection" [|Js.Unsafe.inject (Js.string "matchmaking")|] in
  
  (* Check if there's already a waiting player *)
  let query = Js.Unsafe.meth_call queue_ref "where" [|
    Js.Unsafe.inject (Js.string "status");
    Js.Unsafe.inject (Js.string "==");
    Js.Unsafe.inject (Js.string "waiting")
  |] in
  let query_limit = Js.Unsafe.meth_call query "limit" [|Js.Unsafe.inject (Js.number_of_float 1.0)|] in
  
  let promise = Js.Unsafe.meth_call query_limit "get" [||] in
  let success_cb = Js.wrap_callback (fun query_snapshot ->
    let docs = Js.Unsafe.meth_call query_snapshot "docs" [||] in
    let docs_array = Js.to_array docs in
    
    if Array.length docs_array > 0 then
      (* Found a waiting player - create a game with both players *)
      let waiting_doc = Array.get docs_array 0 in
      let waiting_data = Js.Unsafe.meth_call waiting_doc "data" [||] in
      let waiting_user_id = Js.Unsafe.get waiting_data "userId" |> Js.to_string in
      let waiting_doc_id = Js.Unsafe.get waiting_doc "id" |> Js.to_string in
      
      (* Create game with both players *)
      let rng = Stdlib.Random.State.make_self_init () in
      let initial_state = State.initial_state rng in
      let games_collection = Js.Unsafe.meth_call db "collection" [|Js.Unsafe.inject (Js.string "games")|] in
      let game_doc_ref = Js.Unsafe.meth_call games_collection "doc" [||] in
      let game_id = Js.Unsafe.get game_doc_ref "id" |> Js.to_string in
      
      (* Update player names to show user IDs *)
      let players = Array.copy initial_state.State.players in
      players.(0) <- { players.(0) with State.name = Printf.sprintf "Player %s" (String.sub waiting_user_id 0 (min 8 (String.length waiting_user_id))) };
      players.(1) <- { players.(1) with State.name = Printf.sprintf "Player %s" (String.sub user_id 0 (min 8 (String.length user_id))) };
      let game_state = { initial_state with State.players = players } in
      
      let game_data = state_to_firestore game_state in
      set_doc game_doc_ref game_data
        (fun () ->
          (* Update waiting player's queue doc with gameId and status=matched *)
          let waiting_doc_ref = Js.Unsafe.meth_call queue_ref "doc" [|Js.Unsafe.inject (Js.string waiting_doc_id)|] in
          let update_data = Js.Unsafe.obj [||] in
          Js.Unsafe.set update_data (Js.string "status") (Js.string "matched");
          Js.Unsafe.set update_data (Js.string "gameId") (Js.string game_id);
          update_doc waiting_doc_ref update_data
            (fun () ->
              (* Remove waiting player from queue after a short delay *)
              ignore (Js.Unsafe.meth_call (Js.Unsafe.get Js.Unsafe.global "setTimeout") "call" [|
                Js.Unsafe.inject (Js.wrap_callback (fun _ -> ignore (Js.Unsafe.meth_call waiting_doc_ref "delete" [||])));
                Js.Unsafe.inject (Js.number_of_float 1000.0)
              |]);
              (* Notify current player immediately *)
              on_matched game_id
            )
            on_error
        )
        on_error
    else
      (* No waiting player - add current player to queue *)
      let queue_doc_ref = Js.Unsafe.meth_call queue_ref "doc" [||] in
      let queue_data = Js.Unsafe.obj [||] in
      Js.Unsafe.set queue_data (Js.string "userId") (Js.string user_id);
      Js.Unsafe.set queue_data (Js.string "status") (Js.string "waiting");
      (* Use JavaScript Date.now() to get timestamp *)
      let date_global = Js.Unsafe.get Js.Unsafe.global "Date" in
      let timestamp = Js.Unsafe.meth_call date_global "now" [||] in
      Js.Unsafe.set queue_data (Js.string "timestamp") timestamp;
      set_doc queue_doc_ref queue_data
        (fun () ->
          (* Set up listener to wait for a match *)
          let unsubscribe = on_snapshot_with_error queue_doc_ref
            (fun snapshot ->
              match snapshot_data snapshot with
              | Some data ->
                  let status = try Some (Js.Unsafe.get data "status" |> Js.to_string) with _ -> None in
                  (match status with
                  | Some "matched" ->
                      let game_id = try Some (Js.Unsafe.get data "gameId" |> Js.to_string) with _ -> None in
                      (match game_id with
                      | Some gid -> 
                          (* Remove from queue *)
                          ignore (Js.Unsafe.meth_call queue_doc_ref "delete" [||]);
                          on_matched gid
                      | None -> ())
                  | _ -> ())
              | None -> ())
            on_error
          in
          (* Store unsubscribe in a ref so we can return it *)
          ignore unsubscribe
        )
        on_error
  ) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|]);
  
  (* Return unsubscribe function - for now return no-op, proper implementation would store the listener *)
  fun () -> ()

(* Leave matchmaking queue *)
let leave_matchmaking_queue (user_id : string) (on_success : unit -> unit) (on_error : string -> unit) : unit =
  let db = get_firestore () in
  let queue_ref = Js.Unsafe.meth_call db "collection" [|Js.Unsafe.inject (Js.string "matchmaking")|] in
  let query = Js.Unsafe.meth_call queue_ref "where" [|
    Js.Unsafe.inject (Js.string "userId");
    Js.Unsafe.inject (Js.string "==");
    Js.Unsafe.inject (Js.string user_id)
  |] in
  let promise = Js.Unsafe.meth_call query "get" [||] in
  let success_cb = Js.wrap_callback (fun query_snapshot ->
    let docs = Js.Unsafe.meth_call query_snapshot "docs" [||] in
    let docs_array = Js.to_array docs in
    Array.iter (fun doc ->
      let doc_ref = Js.Unsafe.meth_call queue_ref "doc" [|Js.Unsafe.inject (Js.Unsafe.get doc "id")|] in
      ignore (Js.Unsafe.meth_call doc_ref "delete" [||])
    ) docs_array;
    on_success ()
  ) in
  let error_cb = Js.wrap_callback (fun err ->
    let msg = Js.Unsafe.get err "message" |> Js.to_string in
    on_error msg
  ) in
  ignore (Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success_cb|]);
  ignore (Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error_cb|])

