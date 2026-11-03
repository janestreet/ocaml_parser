open! Base

type t = Ppxlib.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving compare ~localize, equal ~localize, hash, sexp_of]

include Comparable.Infix with type t := t

val to_string : t -> string
val earliest : t Nonempty_list.t -> t Or_error.t
val latest : t Nonempty_list.t -> t Or_error.t
val line_and_column : t -> int * int @@ portable
