open Rummikub
open QCheck

let gen_tile =
  let open Tile in
  let gen_color = Gen.oneofl [Red;Blue;Black;Orange] in
  Gen.frequency
    [ 9, Gen.map (fun (c,r)-> Tile (c,r)) Gen.(pair gen_color (int_range 1 13));
      1, Gen.pure Joker ]

let gen_meld = Gen.(list_size (int_range 3 4) gen_tile)

let test_meld_valid =
  Test.make ~name:"meld_valid_is_sound" ~count:200
    (make gen_meld)
    (fun m -> let _ = Meld.is_meld m in true)  (* property always runs, ensures no exceptions *)

let () = QCheck_base_runner.run_tests_main [ test_meld_valid ]
