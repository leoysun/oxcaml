open! Base
open Rummikub

let%expect_test "tile printing" =
  let open Tile in
  List.iter ~f:(fun t -> Stdio.print_endline (pp_tile t))
    [Tile(Red,5); Tile(Blue,13); Joker];
  [%expect {|
R05
B13
Jk |}]

let%expect_test "sample melds" =
  let open Tile in
  let open Meld in
  let g = [Tile(Red,5); Tile(Blue,5); Tile(Black,5)] in
  let r = [Tile(Red,3); Tile(Red,4); Tile(Red,5); Tile(Red,6)] in
  Stdio.printf "is_group=%b is_run=%b\n" (is_group g) (is_run g);
  Stdio.printf "is_group=%b is_run=%b\n" (is_group r) (is_run r);
  [%expect {|
is_group=true is_run=false
is_group=false is_run=true |}]
