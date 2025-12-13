open Tile

type meld = tile list

let is_group m =
  let joker_count = List.length (List.filter (function Joker -> true | _ -> false) m) in
  let n = List.length m in
  if n < 3 || n > 4 then false else
  let rec process_tiles rank_opt colors = function
    | [] -> 
        (match rank_opt with
         | None -> false
         | Some _ -> 
             let unique_colors = List.sort_uniq Stdlib.compare colors in
             List.length unique_colors + joker_count = n)
    | Joker::rest -> process_tiles rank_opt colors rest
    | Tile(c,r)::rest ->
        (match rank_opt with
         | None -> process_tiles (Some r) [c] rest
         | Some r0 when r = r0 -> process_tiles rank_opt (c::colors) rest
         | Some _ -> false)
  in
  process_tiles None [] m

let is_run m =
  let joker_count = List.length (List.filter (function Joker -> true | _ -> false) m) in
  if List.length m < 3 then false else
  let rec process_run color_opt ranks = function
    | [] ->
        (match color_opt with
         | None -> false
         | Some _ ->
             let sorted_ranks = List.sort Stdlib.compare ranks in
             let rec gaps acc = function
               | a::b::xs -> if a=b then max_int else gaps (acc + (b-a-1)) (b::xs)
               | _ -> acc
             in
             let g = gaps 0 sorted_ranks in
             if g = max_int then false  (* Duplicate ranks *)
             else
               (* Jokers can either fill gaps OR extend the run at the ends *)
               let remaining_jokers = joker_count - g in
               if remaining_jokers < 0 then false  (* Not enough jokers to fill gaps *)
               else
                 (match sorted_ranks with
                  | [] | [_] -> true  (* Single rank with jokers can always extend *)
                  | _ ->
                      let rmin = List.hd sorted_ranks in
                      let rmax = List.hd (List.rev sorted_ranks) in
                      (* Check if jokers can extend upward or downward without exceeding rank 13 *)
                      (* Try extending upward first (preferred), then downward if needed *)
                      let can_extend_up = remaining_jokers > 0 && (rmax + remaining_jokers <= 13) in
                      let can_extend_down = remaining_jokers > 0 && (rmin - remaining_jokers >= 1) in
                      (* Valid if: no extension needed (remaining_jokers = 0), or can extend up/down *)
                      (remaining_jokers = 0 || can_extend_up || can_extend_down)
                      && rmax <= 13  (* Base ranks must be valid *)
                      && rmin >= 1))  (* Base ranks must be valid *)
    | Joker::rest -> process_run color_opt ranks rest
    | Tile(c,r)::rest ->
        (match color_opt with
         | None -> process_run (Some c) [r] rest
         | Some c0 when c = c0 -> process_run color_opt (r::ranks) rest
         | Some _ -> false)
  in
  process_run None [] m

let is_meld m = is_group m || is_run m

(* Calculate meld points where jokers count as the tile they represent *)
let meld_points m =
  if is_run m then
    (* For runs, calculate the actual value jokers represent *)
    let non_joker_ranks = List.filter_map (function Tile(_,r) -> Some r | Joker -> None) m in
    let joker_count = List.length (List.filter (function Joker -> true | _ -> false) m) in
    if List.is_empty non_joker_ranks then
      (* All jokers - can't determine value, use approximation *)
      joker_count * 10
    else
      let sorted_ranks = List.sort Stdlib.compare non_joker_ranks in
      let base_points = List.fold_left (fun s r -> s + r) 0 non_joker_ranks in
      (* Calculate what ranks the jokers fill in the run *)
      let rec fill_gaps ranks jokers acc =
        match ranks, jokers with
        | _, 0 -> acc
        | [], j -> acc + (j * 10)  (* Can't determine, use approximation *)
        | [r], j ->
            (* Single rank with jokers - try to extend upward first, then downward *)
            let max_up = min j (13 - r) in
            let up_sum = if max_up > 0 then
              List.fold_left (fun s i -> s + (r + i)) 0 (List.init max_up (fun i -> i + 1))
            else 0 in
            let remaining = j - max_up in
            let down_sum = if remaining > 0 && r > 1 then
              let max_down = min remaining (r - 1) in
              List.fold_left (fun s i -> s + (r - i - 1)) 0 (List.init max_down (fun i -> i))
            else 0 in
            acc + up_sum + down_sum
        | r1::r2::rest, j ->
            let gap = r2 - r1 - 1 in
            if gap > 0 && j > 0 then
              (* Fill gap with jokers: r1+1, r1+2, ..., r2-1 *)
              let jokers_used = min gap j in
              let gap_sum = List.fold_left (fun s i -> s + (r1 + i + 1)) 0 (List.init jokers_used (fun i -> i)) in
              fill_gaps (r2::rest) (j - jokers_used) (acc + gap_sum)
            else if j > 0 then
              (* No gap, jokers extend the run *)
              let max_up = min j (13 - r2) in
              let up_sum = if max_up > 0 then
                List.fold_left (fun s i -> s + (r2 + i + 1)) 0 (List.init max_up (fun i -> i))
              else 0 in
              let remaining = j - max_up in
              let down_sum = if remaining > 0 && r1 > 1 then
                let max_down = min remaining (r1 - 1) in
                List.fold_left (fun s i -> s + (r1 - i - 1)) 0 (List.init max_down (fun i -> i))
              else 0 in
              fill_gaps (r2::rest) 0 (acc + up_sum + down_sum)
            else
              fill_gaps (r2::rest) 0 acc
      in
      let joker_points = fill_gaps sorted_ranks joker_count 0 in
      base_points + joker_points
  else if is_group m then
    (* For groups, jokers represent the same rank as the other tiles *)
    let non_joker_ranks = List.filter_map (function Tile(_,r) -> Some r | Joker -> None) m in
    if List.is_empty non_joker_ranks then
      (* All jokers - can't determine value, use approximation *)
      (List.length m) * 10
    else
      (* All tiles in a group have the same rank *)
      let rank = List.hd non_joker_ranks in
      let joker_count = List.length (List.filter (function Joker -> true | _ -> false) m) in
      (List.length non_joker_ranks * rank) + (joker_count * rank)
  else
    (* Not a valid meld, but return something *)
    List.fold_left
      (fun s -> function Tile(_,r)->s+r | Joker-> s+10)
      0 m

(* For the initial-30 rule, calculate joker values more accurately:
   - In runs: jokers represent the exact tile they fill in
   - Jokers can fill gaps in the middle, or extend on high/low ends
   - If joker can represent high or low end, choose high (but must be <= 13)
   - In groups: jokers represent the same rank as the other tiles
   - Uses the same logic as meld_points
*)
let meld_points_for_initial m =
  if is_run m then
    (* For runs, calculate the actual value jokers represent *)
    let non_joker_ranks = List.filter_map (function Tile(_,r) -> Some r | Joker -> None) m in
    let joker_count = List.length (List.filter (function Joker -> true | _ -> false) m) in
    if List.is_empty non_joker_ranks then
      (* All jokers - can't determine value, use approximation *)
      joker_count * 10
    else
      let sorted_ranks = List.sort Stdlib.compare non_joker_ranks in
      let base_points = List.fold_left (fun s r -> s + r) 0 non_joker_ranks in
      (* Calculate what ranks the jokers fill in the run *)
      let rec fill_gaps ranks jokers acc =
        match ranks, jokers with
        | _, 0 -> acc
        | [], j -> acc + (j * 10)  (* Can't determine, use approximation *)
        | [r], j ->
            (* Single rank with jokers - try to extend upward first, then downward *)
            (* Try upward: r+1, r+2, ... up to 13 *)
            let max_up = min j (13 - r) in
            let up_sum = if max_up > 0 then
              List.fold_left (fun s i -> s + (r + i)) 0 (List.init max_up (fun i -> i + 1))
            else 0 in
            (* Remaining jokers extend downward: r-1, r-2, ... down to 1 *)
            let remaining = j - max_up in
            let down_sum = if remaining > 0 && r > 1 then
              let max_down = min remaining (r - 1) in
              List.fold_left (fun s i -> s + (r - i - 1)) 0 (List.init max_down (fun i -> i))
            else 0 in
            acc + up_sum + down_sum
        | r1::r2::rest, j ->
            let gap = r2 - r1 - 1 in
            if gap > 0 && j > 0 then
              (* Fill gap with jokers: r1+1, r1+2, ..., r2-1 *)
              (* Note: gap must be <= j (already validated by is_run), so we can fill the entire gap *)
              (* Example: [10, 12] with 1 joker → gap=1, joker=11 (valid) *)
              (* Example: [10, 13] with 1 joker → gap=2, but only 1 joker (invalid, caught by is_run) *)
              let jokers_used = min gap j in
              let gap_sum = List.fold_left (fun s i -> s + (r1 + i + 1)) 0 (List.init jokers_used (fun i -> i)) in
              fill_gaps (r2::rest) (j - jokers_used) (acc + gap_sum)
            else if j > 0 then
              (* No gap, jokers extend the run *)
              (* Always prefer high end (upward) if valid (<=13), even if low end is also valid *)
              let max_up = min j (13 - r2) in
              let up_sum = if max_up > 0 then
                List.fold_left (fun s i -> s + (r2 + i + 1)) 0 (List.init max_up (fun i -> i))
              else 0 in
              (* Only use low end if we can't extend upward (e.g., already at rank 13) *)
              let remaining = j - max_up in
              let down_sum = if remaining > 0 && r1 > 1 then
                let max_down = min remaining (r1 - 1) in
                List.fold_left (fun s i -> s + (r1 - i - 1)) 0 (List.init max_down (fun i -> i))
              else 0 in
              fill_gaps (r2::rest) 0 (acc + up_sum + down_sum)
            else
              fill_gaps (r2::rest) 0 acc
      in
      let joker_points = fill_gaps sorted_ranks joker_count 0 in
      base_points + joker_points
  else if is_group m then
    (* For groups, jokers represent the same rank as the other tiles *)
    let non_joker_ranks = List.filter_map (function Tile(_,r) -> Some r | Joker -> None) m in
    if List.is_empty non_joker_ranks then
      (* All jokers - can't determine value, use approximation *)
      (List.length m) * 10
    else
      (* All tiles in a group have the same rank *)
      let rank = List.hd non_joker_ranks in
      let joker_count = List.length (List.filter (function Joker -> true | _ -> false) m) in
      (List.length non_joker_ranks * rank) + (joker_count * rank)
  else
    (* Not a valid meld, but return something *)
    List.fold_left
      (fun s -> function Tile(_,r)->s+r | Joker-> s+10)
      0 m
