open! Core
open Expect_test_helpers_core

module type S = Ocaml_parser.Ast.S

open struct
  let test (type a) (module M : S with type t = a) (t : a) =
    let module _ : S with type t = a = struct
      type t = M.t [@@deriving compare ~localize, equal ~localize, hash]

      let format = M.format

      let () =
        format Stdlib.Format.std_formatter t;
        Stdlib.Format.print_newline ();
        Stdlib.Format.print_flush ()
      ;;

      let to_string = M.to_string

      let () =
        let formatted =
          format Stdlib.Format.str_formatter t;
          Stdlib.Format.flush_str_formatter ()
        in
        require_equal (module String) (to_string t) formatted
      ;;

      let sexp_of_t = M.sexp_of_t

      let () =
        let sexp = sexp_of_t t in
        print_s sexp;
        require_equal
          (module Sexp)
          sexp
          [%sexp (String.split_lines (to_string t) : string list)]
      ;;
    end
    in
    ()
  ;;

  let loc = Ppxlib.Location.none
end

module Core_type = Ocaml_parser.Ast.Core_type

let%expect_test _ =
  test (module Core_type) [%type: (a, b) C.t];
  [%expect
    {|
    (a, b) C.t
    ("(a, b) C.t")
    |}]
;;

module Expression = Ocaml_parser.Ast.Expression

let%expect_test _ =
  test
    (module Expression)
    [%expr
      match x with
      | [] -> `nil
      | _ :: _ -> `cons];
  [%expect
    {|
    match x with | [] -> `nil | _::_ -> `cons
    ("match x with | [] -> `nil | _::_ -> `cons")
    |}]
;;

module Module_expr = Ocaml_parser.Ast.Module_expr

let%expect_test _ =
  let structure_item : Ppxlib.structure_item =
    [%stri
      module _ = struct
        include M

        let x = y
      end]
  in
  test
    (module Module_expr)
    (match structure_item with
     | [%stri module _ = [%m? module_expr]] -> module_expr
     | _ -> assert false);
  [%expect
    {|
    struct include M
           let x = y end
    ("struct include M" "       let x = y end")
    |}]
;;

module Module_type = Ocaml_parser.Ast.Module_type

let%expect_test _ =
  let signature_item : Ppxlib.signature_item =
    [%sigi:
      module _ : sig
        type t [@@attr a]

        include S with type t := t

        val f : unit
      end]
  in
  test
    (module Module_type)
    (match signature_item with
     | [%sigi: module _ : [%m? module_type]] -> module_type
     | _ -> assert false);
  [%expect
    {|
    sig type t[@@attr a] include S with type  t :=  t val f : unit end
    ("sig type t[@@attr a] include S with type  t :=  t val f : unit end")
    |}]
;;

module Pattern = Ocaml_parser.Ast.Pattern

let%expect_test _ =
  test (module Pattern) [%pat? { x = a; y = b; z = c }];
  [%expect
    {|
    { x = a; y = b; z = c }
    ("{ x = a; y = b; z = c }")
    |}]
;;

module Signature_item = Ocaml_parser.Ast.Signature_item

let%expect_test _ =
  test (module Signature_item) [%sigi: val x : int -> f:(unit -> unit) -> unit];
  [%expect
    {|
    val x : int -> f:(unit -> unit) -> unit
    ("val x : int -> f:(unit -> unit) -> unit")
    |}]
;;

module Structure_item = Ocaml_parser.Ast.Structure_item

let%expect_test _ =
  test
    (module Structure_item)
    [%stri
      module _ : sig end = struct
        let f x =
          x x;
          ()
        ;;
      end];
  [%expect
    {|
    module _ : sig  end = struct let f x = x x; () end
    ("module _ : sig  end = struct let f x = x x; () end ")
    |}]
;;

module Signature = Ocaml_parser.Ast.Signature

let%expect_test _ =
  test
    (module Signature)
    [%sig:
      type t = T.t [@@attr]

      include S with type t := t];
  [%expect
    {|
    type t = T.t[@@attr ]
    include S with type  t :=  t
    ("type t = T.t[@@attr ]" "include S with type  t :=  t")
    |}]
;;

module Structure = Ocaml_parser.Ast.Structure

let%expect_test _ =
  test
    (module Structure)
    [%str
      module M = struct end
      include (M : sig end)

      type t = M.t [@@attr foo]];
  [%expect
    {|
    module M = struct  end
    include (M : sig  end)
    type t = M.t[@@attr foo]
    ("module M = struct  end" "include (M : sig  end)" "type t = M.t[@@attr foo]")
    |}]
;;

module Toplevel_phrase = Ocaml_parser.Ast.Toplevel_phrase

let%expect_test _ =
  test
    (module Toplevel_phrase)
    (Ptop_def
       [%str
         let f x = x;;

         f 1]);
  [%expect
    {|
    let f x = x ;;f 1
    ("let f x = x ;;f 1")
    |}]
;;
