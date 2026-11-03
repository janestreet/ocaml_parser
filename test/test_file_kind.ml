open! Core

include struct
  type t = Ocaml_parser.File_kind.t
  [@@deriving compare ~localize, enumerate, equal ~localize, hash, sexp_of]
end

type t = Ocaml_parser.File_kind.t =
  | ML
  | MLI
  | MLT

let of_file_name = Ocaml_parser.File_kind.of_file_name

let%expect_test "of_file_name" =
  let test name = print_s [%sexp (of_file_name name : t option)] in
  test "file.ml";
  [%expect {| (ML) |}];
  test "file.mli";
  [%expect {| (MLI) |}];
  test "file.mlt";
  [%expect {| (MLT) |}];
  test "file_intf.ml";
  [%expect {| (ML) |}];
  test "file.sexp";
  [%expect {| () |}]
;;

let to_dotted_extension = Ocaml_parser.File_kind.to_dotted_extension

let%expect_test "to_dotted_extension" =
  all
  |> List.map ~f:(fun t -> t, to_dotted_extension t)
  |> [%sexp_of: (t * string) list]
  |> print_s;
  [%expect {| ((ML .ml) (MLI .mli) (MLT .mlt)) |}]
;;
