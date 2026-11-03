open! Base
open! Bin_prot.Std
include Ppxlib.Longident

type t = Ppxlib.Longident.t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t
[@@deriving bin_io, hash]

include (
  Poly :
    sig
      type nonrec t = t [@@deriving compare ~localize, equal ~localize]
    end
    with type t := t)

include (val Comparator.make ~compare ~sexp_of_t)

module Sexp_of_constructors = struct
  type t = Ppxlib.Longident.t =
    | Lident of string
    | Ldot of t * string
    | Lapply of t * t
  [@@deriving sexp_of]
end
