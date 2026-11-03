open! Core
open Ppxlib
open Expect_test_helpers_core

let parse_ml = Ocaml_parser.parse_ml

let%expect_test "parse_ml" =
  let go file_contents =
    parse_ml ~file_name:"file.ml" ~file_contents
    |> ok_exn
    |> Pprintast.structure Format.std_formatter
  in
  go "";
  [%expect {| |}];
  go "let x = 1";
  [%expect {| let x = 1 |}];
  go "module type S = sig type t end;; module M : S = struct let x = 1 end";
  [%expect
    {|
    module type S  = sig type t end
    module M : S = struct let x = 1 end
    |}];
  require_does_raise (fun () -> go "end cannot come before begin");
  [%expect
    {|
    (Lrgrep_syntax_error (
      "Syntax error (partial handler did not handle the case)"
      "File \"file.ml\", line 1, characters 0-3:"))
    |}];
  go "let x = #3.0";
  [%expect {| let x = #3.0 |}]
;;

let parse_mli = Ocaml_parser.parse_mli

let%expect_test "parse_mli" =
  let go file_contents =
    parse_mli ~file_name:"file.mli" ~file_contents
    |> ok_exn
    |> Pprintast.signature Format.std_formatter
  in
  go "";
  [%expect {| |}];
  go "val x : int";
  [%expect {| val x : int |}];
  go "module type S = sig type t end;; module M : sig include S val x : int end";
  [%expect
    {|
    module type S  = sig type t end
    module M : sig include S val x : int end
    |}];
  require_does_raise (fun () -> go "let x = 1");
  [%expect
    {|
    (Lrgrep_syntax_error (
      "Syntax error" "File \"file.mli\", line 1, characters 0-3:"))
    |}]
;;

let parse_mlt = Ocaml_parser.parse_mlt

let%expect_test "parse_mlt" =
  let go file_contents =
    parse_mlt ~file_name:"file.mlt" ~file_contents
    |> ok_exn
    |> List.iter ~f:(Pprintast.toplevel_phrase Format.std_formatter)
  in
  go "";
  [%expect {| |}];
  go {| #cd "/usr/local/home" |};
  [%expect {| #cd  "/usr/local/home" |}];
  go {| #cd "/usr/local/home";; 1 + 2;; print_endline "hello, world";; |};
  [%expect {| #cd  "/usr/local/home";;1 + 2;;print_endline "hello, world" |}];
  require_does_raise (fun () -> go "val x : 1");
  [%expect
    {|
    (Lrgrep_syntax_error (
      "Syntax error" "File \"file.mlt\", line 1, characters 8-9:"))
    |}]
;;

let parse_file = Ocaml_parser.parse_file

let%expect_test "parse_file" =
  let go file_kind file_contents =
    let file_name = "file" ^ Ocaml_parser.File_kind.to_dotted_extension file_kind in
    parse_file ~file_kind ~file_name ~file_contents
    |> ok_exn
    |> Ocaml_parser.File_contents.format Format.std_formatter
  in
  go ML "module type S = sig type t end;; module M : S = struct let x = 1 end";
  [%expect
    {|
    module type S  = sig type t end
    module M : S = struct let x = 1 end
    |}];
  go MLI "module type S = sig type t end;; module M : sig include S val x : int end";
  [%expect
    {|
    module type S  = sig type t end
    module M : sig include S val x : int end
    |}];
  go MLT {| #cd "/usr/local/home";; 1 + 2;; print_endline "hello, world";; |};
  [%expect
    {|
    #cd  "/usr/local/home"
    ;;1 + 2
    ;;print_endline "hello, world"
    |}];
  require_does_raise (fun () -> go ML "val x : 1");
  [%expect
    {|
    (Lrgrep_syntax_error (
      "Syntax error" "File \"file.ml\", line 1, characters 8-9:"))
    |}]
;;

let parse_longident = Ocaml_parser.parse_longident

let%expect_test "parse_longident" =
  let go string = parse_longident string |> ok_exn |> Longident.name |> print_string in
  go {| X |};
  [%expect {| X |}];
  go {| x |};
  [%expect {| x |}];
  go {| X.a |};
  [%expect {| X.a |}];
  go {| X.Y |};
  [%expect {| X.Y |}];
  go {| X.Y.a |};
  [%expect {| X.Y.a |}];
  go {| X.Y.z |};
  [%expect {| X.Y.z |}];
  require_does_raise (fun () -> go {| X(Y) |});
  [%expect
    {| (Lrgrep_syntax_error ("Syntax error" "File \"\", line 1, characters 6-6:")) |}];
  require_does_raise (fun () -> go {| X(Y)(Z) |});
  [%expect
    {| (Lrgrep_syntax_error ("Syntax error" "File \"\", line 1, characters 9-9:")) |}];
  go {| X(Y)(Z).A(B).t |};
  [%expect {| X(Y)(Z).A(B).t |}];
  require_does_raise (fun () -> go {| X Y |});
  [%expect
    {| (Lrgrep_syntax_error ("Syntax error" "File \"\", line 1, characters 3-4:")) |}]
;;

let parse_core_type = Ocaml_parser.parse_core_type

let%expect_test "parse_core_type" =
  let go string =
    parse_core_type string |> ok_exn |> Pprintast.core_type Format.std_formatter
  in
  go {| 'a list Or_error.t |};
  [%expect {| 'a list Or_error.t |}];
  require_does_raise (fun () -> go {| 'a list Or_error |});
  [%expect
    {| (Lrgrep_syntax_error ("Syntax error" "File \"\", line 1, characters 18-18:")) |}]
;;

let parse_expression = Ocaml_parser.parse_expression

let%expect_test "parse_expression" =
  let go string =
    parse_expression string |> ok_exn |> Pprintast.expression Format.std_formatter
  in
  go {| let x = 1 in x + 2 |};
  [%expect {| let x = 1 in x + 2 |}];
  require_does_raise (fun () -> go {| let x = 1 |});
  [%expect
    {|
    (Lrgrep_syntax_error (
      "Expecting `in' to continue let-binding at line 1, character 9"
      "File \"\", line 1, characters 9-10:"))
    |}]
;;

let parse_pattern = Ocaml_parser.parse_pattern

let%expect_test "parse_pattern" =
  let go string =
    parse_pattern string |> ok_exn |> Pprintast.pattern Format.std_formatter
  in
  go {| { x; y = (z, _) } as b |};
  [%expect {| { x; y = (z, _) } as b |}];
  require_does_raise (fun () -> go {| { x, y } |});
  [%expect
    {| (Lrgrep_syntax_error ("Syntax error" "File \"\", line 1, characters 4-5:")) |}]
;;

let parse_signature = Ocaml_parser.parse_signature

let%expect_test "parse_signature" =
  let go string =
    parse_signature string |> ok_exn |> Pprintast.signature Format.std_formatter
  in
  go {| type t val x : t list |};
  [%expect
    {|
    type t
    val x : t list
    |}];
  require_does_raise (fun () -> go {| let x = y |});
  [%expect
    {| (Lrgrep_syntax_error ("Syntax error" "File \"\", line 1, characters 1-4:")) |}]
;;

let parse_structure = Ocaml_parser.parse_structure

let%expect_test "parse_structure" =
  let go string =
    parse_structure string |> ok_exn |> Pprintast.structure Format.std_formatter
  in
  go {| type t = int let x = [y; z] |};
  [%expect
    {|
    type t = int
    let x = [y; z]
    |}];
  require_does_raise (fun () -> go {| type t := int |});
  [%expect
    {| (Lrgrep_syntax_error ("Syntax error" "File \"\", line 1, characters 8-10:")) |}]
;;

let file_tokens = Ocaml_parser.file_tokens

let%expect_test "file_tokens" =
  let go string =
    file_tokens ~file_name:"file" ~file_contents:string
    |> ok_exn
    |> List.map ~f:fst
    |> [%sexp_of: Ocaml_parser.Token.t list]
    |> print_s
  in
  go "";
  [%expect {| (EOF) |}];
  go "let x = 1\n;;";
  [%expect {| (LET (LIDENT x) EQUAL (INT (1 ())) EOL SEMISEMI EOF) |}];
  go ") end begin (";
  [%expect {| (RPAREN END BEGIN LPAREN EOF) |}];
  go "(* comment *); (** docstring *)";
  [%expect {| ((COMMENT _) SEMI (DOCSTRING _) EOF) |}];
  require_does_raise (fun () -> go {| " |});
  [%expect {| ("Lexer.Error(0, _)") |}]
;;

let string_tokens = Ocaml_parser.string_tokens

let%expect_test "string_tokens" =
  let go string =
    string_tokens string |> ok_exn |> [%sexp_of: Ocaml_parser.Token.t list] |> print_s
  in
  go "";
  [%expect {| () |}];
  go "let x = 1\n;;";
  [%expect {| (LET (LIDENT x) EQUAL (INT (1 ())) EOL SEMISEMI) |}];
  go ") end begin (";
  [%expect {| (RPAREN END BEGIN LPAREN) |}];
  go "(* comment *); (** docstring *)";
  [%expect {| ((COMMENT _) SEMI (DOCSTRING _)) |}];
  require_does_raise (fun () -> go {| " |});
  [%expect {| ("Lexer.Error(0, _)") |}]
;;
