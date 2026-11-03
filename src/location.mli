open! Base

(* File locations as used internally by the OCaml compiler. *)
type t = Ppxlib.location =
  { loc_start : Position.t
  ; loc_end : Position.t
  ; loc_ghost : bool
  }
[@@deriving compare ~localize, equal ~localize, fields ~getters, hash, sexp_of ~portable]

include Comparator.S with type t := t

val to_string : t -> string

(** Number of characters from [loc_start] to [loc_end]. *)
val length : t -> int

(** Equivalent to [length t = 0]. *)
val is_empty : t -> bool

(** Reports whether the first argument contains (or equals) [inner]. *)
val contains : t -> inner:t -> bool

(** Returns the largest [inner] such that [contains t1 ~inner] and [contains t2 ~inner].
    May return an empty [inner], either if one or both of [t1] and [t2] are empty or if
    [t1] and [t2] are only adjacent at their edges.

    Returns [None] if [t1] and [t2] are disjoint or refer to different files, in which
    case there is no such [t]. *)
val overlap : t -> t -> t option

(** Reports whether [overlap] would return [Some _]. *)
val is_overlapping_or_adjacent : t -> t -> bool

(** Reports whether [overlap] would return [Some t] where [not (is_empty t)]. *)
val is_overlapping_nonempty : t -> t -> bool

(** [containing list] is the smallest [t] such that [contains t ~inner] for every [inner]
    in [list]. *)
val containing : t Nonempty_list.t -> t Or_error.t
