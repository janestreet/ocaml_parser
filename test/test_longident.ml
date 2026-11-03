open! Core
include Ppxlib.Longident

include (
struct
  type t = Ocaml_parser.Longident.t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp_of]

  type comparator_witness = Ocaml_parser.Longident.comparator_witness

  let comparator = Ocaml_parser.Longident.comparator
end :
  sig
    type t = Ocaml_parser.Longident.t
    [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp_of]

    include Comparator.S with type t := t
  end
  with type t := t
   and type comparator_witness = Ocaml_parser.Longident.comparator_witness)

module Sexp_of_constructors = Ocaml_parser.Longident.Sexp_of_constructors

let%expect_test "[sexp_of_t], [Sexp_of_constructors.sexp_of_t]" =
  let test (type_ : Ppxlib.Ast.core_type) =
    match type_.ptyp_desc with
    | Ptyp_constr ({ txt = lid; loc = _ }, []) ->
      print_s [%sexp (lid : t)];
      print_s [%sexp (lid : Sexp_of_constructors.t)]
    | _ ->
      let s = "Not a [Ptyp_constr (_, [])]." in
      raise_s [%message s ~_:(Ppxlib.Ast_traverse.sexp_of#core_type type_ : Sexp.t)]
  in
  let loc = Location.none in
  test [%type: t];
  [%expect
    {|
    t
    (Lident t)
    |}];
  test [%type: Foo.t];
  [%expect
    {|
    Foo.t
    (Ldot (Lident Foo) t)
    |}];
  test [%type: Foo.Bar.t];
  [%expect
    {|
    Foo.Bar.t
    (Ldot (Ldot (Lident Foo) Bar) t)
    |}];
  test [%type: Foo(Bar).t];
  [%expect
    {|
    "Foo(Bar).t"
    (Ldot (Lapply (Lident Foo) (Lident Bar)) t)
    |}];
  test [%type: Foo.Bar(Baz.Qux).t];
  [%expect
    {|
    "Foo.Bar(Baz.Qux).t"
    (Ldot (Lapply (Ldot (Lident Foo) Bar) (Ldot (Lident Baz) Qux)) t)
    |}];
  ignore ()
;;
