open! Base
include Ast_intf

module type Basic = sig
  type t

  val format : Stdlib.Format.formatter -> t -> unit
end

module Make (T : Basic) : S with type t = T.t = struct
  type t = T.t

  (* polymorphic compare/hash work fine for AST types *)
  let%template compare = (Poly.compare [@mode m]) [@@mode m = (local, global)]
  let%template equal = (Poly.equal [@mode m]) [@@mode m = (local, global)]
  let hash = Hashtbl.hash
  let hash_fold_t state t = hash_fold_int state (hash t)

  (* derive string and sexp formats from the formatter *)
  let format = T.format

  let to_string t =
    (* allocate a fresh buffer, avoid having to think about prior effects *)
    let buffer = Buffer.create 80 in
    let formatter = Stdlib.Format.formatter_of_buffer buffer in
    format formatter t;
    Stdlib.Format.pp_print_flush formatter ();
    Buffer.contents buffer
  ;;

  let sexp_of_t t = [%sexp_of: string list] (String.split_lines (to_string t))
end

module Core_type = Make (struct
    type t = Ppxlib.core_type

    let format = Ppxlib.Pprintast.core_type
  end)

module Expression = Make (struct
    type t = Ppxlib.expression

    let format = Ppxlib.Pprintast.expression
  end)

module Module_expr = Make (struct
    type t = Ppxlib.module_expr

    let format = Ppxlib.Pprintast.module_expr
  end)

module Module_type = Make (struct
    type t = Ppxlib.module_type

    let format = Ppxlib.Pprintast.module_type
  end)

module Pattern = Make (struct
    type t = Ppxlib.pattern

    let format = Ppxlib.Pprintast.pattern
  end)

module Signature = Make (struct
    type t = Ppxlib.signature

    let format = Ppxlib.Pprintast.signature
  end)

module Signature_item = Make (struct
    type t = Ppxlib.signature_item

    let format = Ppxlib.Pprintast.signature_item
  end)

module Structure = Make (struct
    type t = Ppxlib.structure

    let format = Ppxlib.Pprintast.structure
  end)

module Structure_item = Make (struct
    type t = Ppxlib.structure_item

    let format = Ppxlib.Pprintast.structure_item
  end)

module Toplevel_phrase = Make (struct
    type t = Ppxlib.toplevel_phrase

    let format = Ppxlib.Pprintast.toplevel_phrase
  end)
