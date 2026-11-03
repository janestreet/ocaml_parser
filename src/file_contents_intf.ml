(** Represents the contents of an arbitrary OCaml source file. See also [File_kind]. *)

open! Base

module Types = struct
  type 'a iter =
    < structure : Ppxlib.structure -> unit
    ; signature : Ppxlib.signature -> unit
    ; toplevel_phrase : Ppxlib.toplevel_phrase -> unit
    ; .. >
    as
    'a

  type 'a map =
    < structure : Ppxlib.structure -> Ppxlib.structure
    ; signature : Ppxlib.signature -> Ppxlib.signature
    ; toplevel_phrase : Ppxlib.toplevel_phrase -> Ppxlib.toplevel_phrase
    ; .. >
    as
    'a

  type ('a, 'b) lift =
    < structure : Ppxlib.structure -> 'a
    ; signature : Ppxlib.signature -> 'a
    ; toplevel_phrase : Ppxlib.toplevel_phrase -> 'a
    ; list : 'b. ('b -> 'a) -> 'b list -> 'a
    ; .. >
    as
    'b

  module Monadic_t (M : T1) = struct
    type 'a map =
      < structure : Ppxlib.structure -> Ppxlib.structure M.t
      ; signature : Ppxlib.signature -> Ppxlib.signature M.t
      ; toplevel_phrase : Ppxlib.toplevel_phrase -> Ppxlib.toplevel_phrase M.t
      ; .. >
      as
      'a
  end

  module Monadic2_t (M : T2) = struct
    type ('a, 'b) map =
      < structure : Ppxlib.structure -> (Ppxlib.structure, 'a) M.t
      ; signature : Ppxlib.signature -> (Ppxlib.signature, 'a) M.t
      ; toplevel_phrase : Ppxlib.toplevel_phrase -> (Ppxlib.toplevel_phrase, 'a) M.t
      ; .. >
      as
      'b
  end
end

module type File_contents = sig
  include module type of struct
    include Types
  end

  type t =
    | ML of Ppxlib.structure
    | MLI of Ppxlib.signature
    | MLT of Ppxlib.toplevel_phrase list
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of]

  val to_string : t -> string
  val format : Formatter.t -> t -> unit
  val kind : t -> File_kind.t

  (** Combines all consecutive [Ptop_def structure] phrases. Useful so that structure
      traversal can process adjacent items. *)
  val toplevel_phrases_combine_structures
    :  Ppxlib.toplevel_phrase list
    -> Ppxlib.toplevel_phrase list

  (** Separates [Ptop_def structure] phrases to one structure item each. Useful so that
      serialization can inject newlines. *)
  val toplevel_phrases_separate_structures
    :  Ppxlib.toplevel_phrase list
    -> Ppxlib.toplevel_phrase list

  (** Applies [toplevel_phrases_combine_structures] if applicable. *)
  val mlt_combine_structures : t -> t

  (** Applies [toplevel_phrases_separate_structures] if applicable. *)
  val mlt_separate_structures : t -> t

  val iter : t -> _ iter -> unit
  val map : t -> _ map -> t
  val lift : t -> ('a, _) lift -> 'a

  module Monadic (M : Monad.S) : sig
    val map : t -> _ Monadic_t(M).map -> t M.t
  end

  module Monadic2 (M : Monad.S2) : sig
    val map : t -> ('a, _) Monadic2_t(M).map -> (t, 'a) M.t
  end
end
