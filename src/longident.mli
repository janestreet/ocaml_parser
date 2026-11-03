(** A wrapper of [Longident] that adds sexp serializations. *)

open! Base

type t = Ppxlib.Longident.t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t
[@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp_of]

include Comparator.S with type t := t

(** [sexp_of_t t] returns [Atom (name t)] *)

include module type of Ppxlib.Longident with type t := t

module Sexp_of_constructors : sig
  (** The usual sexp serialization showing the constructors and their arguments.

      Less legible, but useful for people not familiar with OCaml AST. *)
  type nonrec t = t [@@deriving sexp_of]
end
