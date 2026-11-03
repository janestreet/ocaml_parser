open! Base
open Expect_test_helpers_base

module Deriving_fields = struct
  type t = Ocaml_parser.Location.t =
    { loc_start : Ocaml_parser.Position.t
    ; loc_end : Ocaml_parser.Position.t
    ; loc_ghost : bool
    }
  [@@deriving fields ~getters]
end

include Deriving_fields

module T = struct
  type t = Ocaml_parser.Location.t
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of]
end

include T

type t = Ocaml_parser.Location.t =
  { loc_start : Ocaml_parser.Position.t
  ; loc_end : Ocaml_parser.Position.t
  ; loc_ghost : bool
  }

type comparator_witness = Ocaml_parser.Location.comparator_witness

let comparator = Ocaml_parser.Location.comparator

open struct
  (* test helpers *)

  let create_pos pos_fname pos_lnum column : Ppxlib.position =
    (* Assume every line has 10 characters in our imaginary files. *)
    let pos_bol = (pos_lnum - 1) * 10 in
    let pos_cnum = pos_bol + column in
    { pos_fname; pos_lnum; pos_bol; pos_cnum }
  ;;

  let create ?(ghost = false) file l1 c1 l2 c2 : t =
    { loc_ghost = ghost
    ; loc_start = create_pos file l1 c1
    ; loc_end = create_pos file l2 c2
    }
  ;;
end

let to_string = Ocaml_parser.Location.to_string

let%expect_test "to_string" =
  let test t = print_endline (to_string t) in
  test (create "file.ml" 1 1 1 1);
  [%expect {| File "file.ml", line 1, characters 1-1: |}];
  test (create "file.mli" 1 1 1 2);
  [%expect {| File "file.mli", line 1, characters 1-2: |}];
  test (create "file.sexp" 1 1 2 2);
  [%expect {| File "file.sexp", lines 1-2, characters 1-2: |}]
;;

let is_empty = Ocaml_parser.Location.is_empty
let length = Ocaml_parser.Location.length

let%expect_test "length" =
  let test t = print_s [%sexp (length t : int), (is_empty t : bool)] in
  test (create "file.ml" 1 1 1 1);
  [%expect {| (0 true) |}];
  test (create "file.mli" 1 1 1 2);
  [%expect {| (1 false) |}];
  test (create "file.sexp" 1 1 2 2);
  [%expect {| (11 false) |}]
;;

let contains = Ocaml_parser.Location.contains

let%expect_test "contains" =
  let test x y =
    let relationship =
      match contains x ~inner:y, contains y ~inner:x with
      | true, true -> "equals"
      | true, false -> "contains"
      | false, true -> "is contained by"
      | false, false -> "neither contains nor is contained by"
    in
    print_endline relationship
  in
  test (create "file.ml" 1 1 1 1) (create "file.ml" 1 1 1 1);
  [%expect {| equals |}];
  test (create "file.ml" 1 1 1 1) (create "other.ml" 1 1 1 1);
  [%expect {| neither contains nor is contained by |}];
  test (create "file.ml" 1 1 1 1) (create "file.ml" 1 1 2 2);
  [%expect {| is contained by |}];
  test (create "file.ml" 1 1 2 2) (create "file.ml" 1 1 1 1);
  [%expect {| contains |}];
  test (create "file.ml" 2 2 2 2) (create "file.ml" 1 1 2 2);
  [%expect {| is contained by |}];
  test (create "file.ml" 1 1 2 2) (create "file.ml" 2 2 2 2);
  [%expect {| contains |}];
  test (create "file.ml" 2 2 2 2) (create "file.ml" 1 1 3 3);
  [%expect {| is contained by |}];
  test (create "file.ml" 1 1 3 3) (create "file.ml" 2 2 2 2);
  [%expect {| contains |}];
  test (create "file.ml" 1 1 2 2) (create "file.ml" 2 2 3 3);
  [%expect {| neither contains nor is contained by |}]
;;

let overlap = Ocaml_parser.Location.overlap
let is_overlapping_or_adjacent = Ocaml_parser.Location.is_overlapping_or_adjacent
let is_overlapping_nonempty = Ocaml_parser.Location.is_overlapping_nonempty

let%expect_test "overlap" =
  let test x y =
    let overlap = overlap x y in
    let has_nonempty_overlap =
      match overlap with
      | None -> false
      | Some location -> not (Ocaml_parser.Location.is_empty location)
    in
    require_equal (module Bool) (Option.is_some overlap) (is_overlapping_or_adjacent x y);
    require_equal (module Bool) (Option.is_some overlap) (is_overlapping_or_adjacent y x);
    require_equal (module Bool) has_nonempty_overlap (is_overlapping_nonempty x y);
    require_equal (module Bool) has_nonempty_overlap (is_overlapping_nonempty y x);
    print_s [%sexp (overlap : Ocaml_parser.Location.t option)]
  in
  test (create "file.ml" 1 1 1 1) (create "file.ml" 1 1 1 1);
  [%expect {| ("File \"file.ml\", line 1, characters 1-1:") |}];
  test (create "file.ml" 1 1 1 1) (create "other.ml" 1 1 1 1);
  [%expect {| () |}];
  test (create "file.ml" 1 1 1 1) (create "file.ml" 1 1 2 2);
  [%expect {| ("File \"file.ml\", line 1, characters 1-1:") |}];
  test (create "file.ml" 1 1 1 1) (create "file.ml" 2 2 2 2);
  [%expect {| () |}];
  test (create "file.ml" 2 2 2 2) (create "file.ml" 1 1 3 3);
  [%expect {| ("File \"file.ml\", line 2, characters 2-2:") |}];
  test (create "file.ml" 1 1 3 3) (create "file.ml" 2 2 4 4);
  [%expect {| ("File \"file.ml\", lines 2-3, characters 2-3:") |}];
  test (create "file.ml" 4 4 6 6) (create "file.ml" 1 1 3 3);
  [%expect {| () |}]
;;

let containing = Ocaml_parser.Location.containing

let%expect_test "containing" =
  let test list =
    let result =
      let%map.Or_error t = containing list in
      [%sexp { t : t; ghost = (t.loc_ghost : bool) }]
    in
    print_s [%sexp (result : Sexp.t Or_error.t)]
  in
  test [ create "file.ml" 1 1 2 2 ];
  [%expect {| (Ok ((t "File \"file.ml\", lines 1-2, characters 1-2:") (ghost false))) |}];
  test [ create "file.ml" 1 1 3 3; create "file.ml" 2 2 4 4 ];
  [%expect {| (Ok ((t "File \"file.ml\", lines 1-4, characters 1-4:") (ghost false))) |}];
  test [ create "file.ml" 1 1 2 2; create "file.ml" 3 3 4 4 ];
  [%expect {| (Ok ((t "File \"file.ml\", lines 1-4, characters 1-4:") (ghost false))) |}];
  test [ create "file.ml" 1 1 2 2 ~ghost:true; create "file.ml" 3 3 4 4 ];
  [%expect {| (Ok ((t "File \"file.ml\", lines 1-4, characters 1-4:") (ghost true))) |}];
  test [ create "file.ml" 1 1 2 2; create "file.ml" 3 3 4 4 ~ghost:true ];
  [%expect {| (Ok ((t "File \"file.ml\", lines 1-4, characters 1-4:") (ghost true))) |}];
  test [ create "file.ml" 1 1 2 2; create "file.sexp" 3 3 4 4 ];
  [%expect {| (Error ("filename mismatch" file.ml file.sexp)) |}]
;;
