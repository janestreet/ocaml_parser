(** Represents the different kinds of OCaml files: .ml contains a structure, .mli contains
    a signature, and .mlt contains top-level phrases. See also [File_contents]. *)

open! Base

type t =
  | ML
  | MLI
  | MLT
[@@deriving compare ~localize, enumerate, equal ~localize, hash, sexp_of]

(** Produces the kind of an OCaml file, or [None] if the file does not have an OCaml
    extension. *)
val of_file_name : string -> t option

(** Produces the dotted extension, e.g. ".ml", for a given kind. *)
val to_dotted_extension : t -> string
