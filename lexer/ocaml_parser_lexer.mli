(** The tokenizing part of [Ocaml_parser], split out so that ppxes can depend on it
    without pulling in [Base]. It depends only on compiler-libs.
    [Ocaml_parser.file_tokens] and [Ocaml_parser.string_tokens] wrap these in [Or_error]. *)

val file_lexbuf : file_name:string -> file_contents:string -> Lexing.lexbuf
val string_lexbuf : string -> Lexing.lexbuf

(** Tokenizes a source file. Raises on lexing errors. *)
val file_tokens_exn
  :  file_name:string
  -> file_contents:string
  -> (Parser.token * Location.t) list

(** Tokenizes a string, without locations or the trailing [EOF]. Raises on lexing errors. *)
val string_tokens_exn : string -> Parser.token list
