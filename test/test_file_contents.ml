open! Core
open Expect_test_helpers_core
open Ppxlib.Ast_builder.Default

open struct
  (* Helpers *)

  let loc = Ppxlib.Location.none
  let string_of_structure = Ppxlib.Pprintast.string_of_structure

  let string_of_directive directive =
    Ppxlib.Pprintast.toplevel_phrase Format.str_formatter (Ptop_dir directive);
    Format.flush_str_formatter ()
  ;;

  module Phrase = struct
    type t = Ppxlib.toplevel_phrase

    let sexp_of_t (t : t) =
      match t with
      | Ptop_def structure -> [%sexp Ptop_def (string_of_structure structure : string)]
      | Ptop_dir directive -> [%sexp Ptop_dir (string_of_directive directive : string)]
    ;;
  end
end

include (
  Ocaml_parser.File_contents :
  sig
    type t = Ocaml_parser.File_contents.t =
      | ML of Ppxlib.structure
      | MLI of Ppxlib.signature
      | MLT of Ppxlib.toplevel_phrase list
    [@@deriving compare ~localize, equal ~localize, hash]
  end)

let kind = Ocaml_parser.File_contents.kind

let%expect_test "kind" =
  let test t = print_s [%sexp (kind t : Ocaml_parser.File_kind.t)] in
  test (ML []);
  [%expect {| ML |}];
  test (MLI ([] |> Latest.signature ~loc));
  [%expect {| MLI |}];
  test (MLT []);
  [%expect {| MLT |}]
;;

let sexp_of_t = Ocaml_parser.File_contents.sexp_of_t
let to_string = Ocaml_parser.File_contents.to_string
let format = Ocaml_parser.File_contents.format

let%expect_test _ =
  let test t =
    let string = to_string t in
    print_endline string;
    let format_string =
      format Format.str_formatter t;
      Format.flush_str_formatter ()
    in
    require_equal (module String) string format_string;
    let sexp = sexp_of_t t in
    print_s sexp;
    require_equal
      (module Sexp)
      sexp
      [%sexp
        (kind t : Ocaml_parser.File_kind.t), (String.split_lines string : string list)]
  in
  test
    (ML
       [%str
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end = struct
           include Math.Make (Base.Int)

           let zero = Caml.int_zero
         end]);
  [%expect
    {|
    module type S  = sig type t = X.x end
    module M : sig include S val zero : t end =
      struct include (Math.Make)(Base.Int)
             let zero = Caml.int_zero end
    (ML (
      "module type S  = sig type t = X.x end"
      "module M : sig include S val zero : t end ="
      "  struct include (Math.Make)(Base.Int)"
      "         let zero = Caml.int_zero end "))
    |}];
  test
    (MLI
       [%sig:
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end]);
  [%expect
    {|
    module type S  = sig type t = X.x end
    module M : sig include S val zero : t end
    (MLI (
      "module type S  = sig type t = X.x end"
      "module M : sig include S val zero : t end"))
    |}];
  test
    (MLT
       [ Ptop_def
           [%str
             let f x = x
             let y = f x;;

             y]
       ; Ptop_dir
           { pdir_loc = loc
           ; pdir_name = { loc; txt = "directive" }
           ; pdir_arg = Some { pdira_loc = loc; pdira_desc = Pdir_ident (Lident "x") }
           }
       ; Ptop_def
           [%str
             let h () =
               g ();
               g ()
             ;;

             h ();
             h ()]
       ]);
  [%expect
    {|
    let f x = x
    let y = f x
    ;;y
    #directive  x
    let h () = g (); g ()
    ;;h (); h ()
    (MLT (
      "let f x = x"
      "let y = f x"
      ";;y"
      "#directive  x"
      "let h () = g (); g ()"
      ";;h (); h ()"))
    |}]
;;

type 'a iter = 'a Ocaml_parser.File_contents.iter

let iter = Ocaml_parser.File_contents.iter

let%expect_test "iter" =
  let test t =
    iter
      t
      (object
         inherit Ppxlib.Ast_traverse.iter
         method! longident id = print_endline (Ppxlib.Longident.name id)
      end)
  in
  test
    (ML
       [%str
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end = struct
           include Math.Make (Base.Int)

           let zero = Caml.int_zero
         end]);
  [%expect
    {|
    X.x
    Math.Make
    Base.Int
    Caml.int_zero
    S
    t
    |}];
  test
    (MLI
       [%sig:
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end]);
  [%expect
    {|
    X.x
    S
    t
    |}]
;;

type 'a map = 'a Ocaml_parser.File_contents.map

let map = Ocaml_parser.File_contents.map

let%expect_test "map" =
  let loc = Ppxlib.Location.none in
  let test t =
    map
      t
      (object
         inherit Ppxlib.Ast_traverse.map
         method! longident id = Ppxlib.Longident.parse (Ppxlib.Longident.name id ^ "_0")
      end)
    |> format Format.std_formatter
  in
  test
    (ML
       [%str
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end = struct
           include Math.Make (Base.Int)

           let zero = Caml.int_zero
         end]);
  [%expect
    {|
    module type S  = sig type t = X.x_0 end
    module M : sig include S_0 val zero : t_0 end =
      struct include (Math.Make_0)(Base.Int_0)
             let zero = Caml.int_zero_0 end
    |}];
  test
    (MLI
       [%sig:
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end]);
  [%expect
    {|
    module type S  = sig type t = X.x_0 end
    module M : sig include S_0 val zero : t_0 end
    |}]
;;

type ('a, 'b) lift = ('a, 'b) Ocaml_parser.File_contents.lift

let lift = Ocaml_parser.File_contents.lift

let%expect_test "lift" =
  let loc = Ppxlib.Location.none in
  let test t =
    lift
      t
      (object (self)
         inherit [string list] Ppxlib.Ast.lift
         method! location _ = []
         method bool _ = []
         method char _ = []
         method int _ = []
         method string s = [ s ]
         method list f l = List.concat_map ~f l
         method option f o = self#list f (Option.to_list o)
         method tuple l = self#list Fn.id l
         method record l = self#list snd l
         method constr _ l = self#tuple l
      end)
    |> [%sexp_of: string list]
    |> print_s
  in
  test
    (ML
       [%str
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end = struct
           include Math.Make (Base.Int)

           let zero = Caml.int_zero
         end]);
  [%expect {| (S t X x M Math Make Base Int zero Caml int_zero S zero t) |}];
  test
    (MLI
       [%sig:
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end]);
  [%expect {| (S t X x M S zero t) |}]
;;

module Monadic_t = Ocaml_parser.File_contents.Monadic_t
module Monadic = Ocaml_parser.File_contents.Monadic

let%expect_test "Monadic" =
  let module With_names = struct
    type 'a t = 'a * string list

    include Monad.Make (struct
        type nonrec 'a t = 'a t

        let return a = a, []

        let bind a ~f =
          let b = f (fst a) in
          fst b, snd a @ snd b
        ;;

        let map = `Define_using_bind
      end)
  end
  in
  let module Monadic_map = Monadic (With_names) in
  let module Monadic_traversals = Refactor_traversal.Traversals (With_names) in
  let loc = Ppxlib.Location.none in
  let test t =
    Monadic_map.map
      t
      (object
         inherit Monadic_traversals.map
         method! string string = String.map string ~f:(Fn.const '_'), [ string ]

         (* do not bother traversing source file names *)
         method! location loc = loc, []
      end)
    |> [%sexp_of: t * string list]
    |> print_s
  in
  test
    (ML
       [%str
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end = struct
           include Math.Make (Base.Int)

           let zero = Caml.int_zero
         end]);
  [%expect
    {|
    ((ML (
       "module type _  = sig type _ = _._ end"
       "module _ : sig include _ val ____ : _ end ="
       "  struct include (____.____)(____.___)"
       "         let ____ = ____.________ end "))
     (S t X x M Math Make Base Int zero Caml int_zero S zero t))
    |}];
  test
    (MLI
       [%sig:
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end]);
  [%expect
    {|
    ((MLI (
       "module type _  = sig type _ = _._ end"
       "module _ : sig include _ val ____ : _ end"))
     (S t X x M S zero t))
    |}]
;;

module Monadic2_t = Ocaml_parser.File_contents.Monadic2_t
module Monadic2 = Ocaml_parser.File_contents.Monadic2

let%expect_test "Monadic2" =
  let module Monadic2_map = Monadic2 (Sequence.Generator) in
  let module Monadic2_traversals = Refactor_traversal.Map2 (Sequence.Generator) in
  let loc = Ppxlib.Location.none in
  let test t =
    Monadic2_map.map
      t
      (object
         inherit [Ppxlib.Longident.t] Monadic2_traversals.map

         method! longident id =
           Sequence.Generator.map (Sequence.Generator.yield id) ~f:(fun () -> id)
      end)
    |> Sequence.Generator.ignore_m
    |> Sequence.Generator.run
    |> Sequence.map ~f:Ppxlib.Longident.name
    |> [%sexp_of: string Sequence.t]
    |> print_s
  in
  test
    (ML
       [%str
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end = struct
           include Math.Make (Base.Int)

           let zero = Caml.int_zero
         end]);
  [%expect {| (X.x Math.Make Base.Int Caml.int_zero S t) |}];
  test
    (MLI
       [%sig:
         module type S = sig
           type t = X.x
         end

         module M : sig
           include S

           val zero : t
         end]);
  [%expect {| (X.x S t) |}]
;;

let mlt_combine_structures = Ocaml_parser.File_contents.mlt_combine_structures
let mlt_separate_structures = Ocaml_parser.File_contents.mlt_separate_structures

let toplevel_phrases_combine_structures =
  Ocaml_parser.File_contents.toplevel_phrases_combine_structures
;;

let toplevel_phrases_separate_structures =
  Ocaml_parser.File_contents.toplevel_phrases_separate_structures
;;

let%expect_test "mlt_separate_structures" =
  let test file_contents =
    let raw = ok_exn (Ocaml_parser.parse_mlt ~file_name:"file.mlt" ~file_contents) in
    let combined = toplevel_phrases_combine_structures raw in
    let separated = toplevel_phrases_separate_structures raw in
    print_s
      [%sexp { raw : Phrase.t list; combined : Phrase.t list; separated : Phrase.t list }]
  in
  test
    "let x = 1;; let y = 2;; #use \"helpers.ml\";; #load \"deps.ml\";; type t = int;; \
     module M = struct end";
  [%expect
    {|
    ((raw (
       (Ptop_def "let x = 1")
       (Ptop_def "let y = 2")
       (Ptop_dir "#use  \"helpers.ml\"")
       (Ptop_dir "#load  \"deps.ml\"")
       (Ptop_def "type t = int")
       (Ptop_def "module M = struct  end")))
     (combined (
       (Ptop_def "let x = 1\nlet y = 2")
       (Ptop_dir "#use  \"helpers.ml\"")
       (Ptop_dir "#load  \"deps.ml\"")
       (Ptop_def "type t = int\nmodule M = struct  end")))
     (separated (
       (Ptop_def "let x = 1")
       (Ptop_def "let y = 2")
       (Ptop_dir "#use  \"helpers.ml\"")
       (Ptop_dir "#load  \"deps.ml\"")
       (Ptop_def "type t = int")
       (Ptop_def "module M = struct  end"))))
    |}]
;;
