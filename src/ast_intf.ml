open! Base

module type S = sig
  (** [sexp_of_t] is defined using [to_string] and [String.split_lines]. *)
  type t [@@deriving compare ~localize, equal ~localize, hash, sexp_of]

  (** [to_string] is defined using [format]. *)
  val to_string : t -> string

  (** [format] is taken from [Ppxlib.Pprintast]. *)
  val format : Stdlib.Format.formatter -> t -> unit
end

module type Ast = sig
  module type S = S

  module Core_type : S with type t = Ppxlib.core_type
  module Expression : S with type t = Ppxlib.expression
  module Module_expr : S with type t = Ppxlib.module_expr
  module Module_type : S with type t = Ppxlib.module_type
  module Pattern : S with type t = Ppxlib.pattern
  module Signature : S with type t = Ppxlib.signature
  module Signature_item : S with type t = Ppxlib.signature_item
  module Structure : S with type t = Ppxlib.structure
  module Structure_item : S with type t = Ppxlib.structure_item
  module Toplevel_phrase : S with type t = Ppxlib.toplevel_phrase
end
