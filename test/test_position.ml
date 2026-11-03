open! Base
open Expect_test_helpers_base

module T = struct
  type t = Ocaml_parser.Position.t
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of]
end

include T

type t = Ocaml_parser.Position.t =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }

open struct
  (* test helpers *)

  let create pos_fname pos_lnum column =
    (* Assume every line has 10 characters in our imaginary files. *)
    let pos_bol = (pos_lnum - 1) * 10 in
    let pos_cnum = pos_bol + column in
    { pos_fname; pos_lnum; pos_bol; pos_cnum }
  ;;
end

include (Ocaml_parser.Position : Comparable.Infix with type t := t)

let to_string = Ocaml_parser.Position.to_string

let%expect_test "to_string" =
  let test t = print_endline (to_string t) in
  test (create "file.ml" 1 1);
  [%expect {| File "file.ml", line 1, characters 1-1: |}];
  test (create "file.mli" 1 2);
  [%expect {| File "file.mli", line 1, characters 2-2: |}];
  test (create "file.sexp" 2 2);
  [%expect {| File "file.sexp", line 2, characters 2-2: |}]
;;

let earliest = Ocaml_parser.Position.earliest
let latest = Ocaml_parser.Position.latest

let%expect_test "earliest, latest" =
  let test list =
    print_s [%sexp (earliest list : t Or_error.t)];
    print_s [%sexp (latest list : t Or_error.t)]
  in
  test [ create "file.ml" 1 1 ];
  [%expect
    {|
    (Ok "File \"file.ml\", line 1, characters 1-1:")
    (Ok "File \"file.ml\", line 1, characters 1-1:")
    |}];
  test [ create "file.ml" 1 1; create "file.ml" 2 2 ];
  [%expect
    {|
    (Ok "File \"file.ml\", line 1, characters 1-1:")
    (Ok "File \"file.ml\", line 2, characters 2-2:")
    |}];
  test [ create "file.ml" 2 1; create "file.ml" 1 2 ];
  [%expect
    {|
    (Ok "File \"file.ml\", line 1, characters 2-2:")
    (Ok "File \"file.ml\", line 2, characters 1-1:")
    |}];
  test [ create "file.ml" 1 1; create "file.sexp" 2 2 ];
  [%expect
    {|
    (Error ("filename mismatch" file.ml file.sexp))
    (Error ("filename mismatch" file.ml file.sexp))
    |}]
;;

let line_and_column = Ocaml_parser.Position.line_and_column

let%expect_test "to_string" =
  let test t = print_s [%sexp (line_and_column t : int * int)] in
  test (create "file.ml" 1 1);
  [%expect {| (1 1) |}];
  test (create "file.mli" 1 2);
  [%expect {| (1 2) |}];
  test (create "file.sexp" 2 2);
  [%expect {| (2 2) |}]
;;
