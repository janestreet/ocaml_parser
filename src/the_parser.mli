(** Parsing functions for OCaml source. *)

open! Base

(** Parses contents as if they were a ".ml" file. The filename is not checked; it is used
    only for source locations. *)
val parse_ml : file_name:string -> file_contents:string -> Ppxlib.structure Or_error.t

(** Parses contents as if they were a ".mli" file. The filename is not checked; it is used
    only for source locations. *)
val parse_mli : file_name:string -> file_contents:string -> Ppxlib.signature Or_error.t

(** Parses contents as if they were a ".mlt" file. The filename is not checked; it is used
    only for source locations. *)
val parse_mlt
  :  file_name:string
  -> file_contents:string
  -> Ppxlib.toplevel_phrase list Or_error.t

(** Parses contents as if they were the given kind of file. The filename is not checked;
    it is used only for source locations. *)
val parse_file
  :  file_name:string
  -> file_contents:string
  -> file_kind:File_kind.t
  -> File_contents.t Or_error.t

(** Parses a long identifier if possible. *)
val parse_longident : string -> Ppxlib.longident Or_error.t

(** Parses a type if possible. Uses dummy locations. *)
val parse_core_type : string -> Ppxlib.core_type Or_error.t

(** Parses an expression if possible. Uses dummy locations. *)
val parse_expression : string -> Ppxlib.expression Or_error.t

(** Parses a pattern if possible. Uses dummy locations. *)
val parse_pattern : string -> Ppxlib.pattern Or_error.t

(** Parses a structure if possible. Uses dummy locations. *)
val parse_structure : string -> Ppxlib.structure Or_error.t

(** Parses a signature if possible. Uses dummy locations. *)
val parse_signature : string -> Ppxlib.signature Or_error.t

(** Parses the tokens of a source file. *)
val file_tokens
  :  file_name:string
  -> file_contents:string
  -> (Token.t * Ppxlib.location) list Or_error.t

(** Parses the tokens of a string. Produces no locations. *)
val string_tokens : string -> Token.t list Or_error.t
