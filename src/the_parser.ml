open! Base

exception Lrgrep_syntax_error of string * Location.t

let () =
  Sexplib.Conv.Exn_converter.add [%extension_constructor Lrgrep_syntax_error] (function
    | Lrgrep_syntax_error (message, loc) ->
      [%sexp Lrgrep_syntax_error ((message : string), (loc : Location.t))]
    | _ -> assert false)
;;

module Frontend = struct
  module Syntax_error = struct
    type t =
      { msg : string
      ; loc : Location.t
      }
  end

  let no_improvement (_ : Lexing.lexbuf) : (unit, Syntax_error.t) Result.t = Ok ()
  let implementation = no_improvement
  let interface = no_improvement
  let use_file = no_improvement
  let parse_any_longident = no_improvement
  let parse_core_type = no_improvement
  let parse_expression = no_improvement
  let parse_pattern = no_improvement
end

(* Improve the raw parser exn with [Lrgrep] if possible. It only parses whole .ml or .mli
   files, not .mlt files or other grammars (expression, pattern, etc.). *)
let parse lexbuf_f raw_f lrgrep_f =
  try raw_f (lexbuf_f ()) with
  | raw_parser_exn ->
    (match lrgrep_f (lexbuf_f ()) with
     | Ok () -> raise raw_parser_exn
     | Error ({ msg; loc } : Frontend.Syntax_error.t) ->
       raise (Lrgrep_syntax_error (msg, loc)))
;;

let parse_ml_exn ~file_name ~file_contents =
  parse
    (fun () -> Ocaml_parser_lexer.file_lexbuf ~file_name ~file_contents)
    Ppxlib.Parse.implementation
    Frontend.implementation
;;

let parse_mli_exn ~file_name ~file_contents =
  parse
    (fun () -> Ocaml_parser_lexer.file_lexbuf ~file_name ~file_contents)
    Ppxlib.Parse.interface
    Frontend.interface
;;

let parse_mlt_exn ~file_name ~file_contents =
  parse
    (fun () -> Ocaml_parser_lexer.file_lexbuf ~file_name ~file_contents)
    Ppxlib.Parse.use_file
    Frontend.use_file
;;

let parse_exn ~file_name ~file_contents ~file_kind =
  match (file_kind : File_kind.t) with
  | ML -> File_contents.ML (parse_ml_exn ~file_name ~file_contents)
  | MLI -> File_contents.MLI (parse_mli_exn ~file_name ~file_contents)
  | MLT -> File_contents.MLT (parse_mlt_exn ~file_name ~file_contents)
;;

let parse_longident_exn string =
  parse
    (fun () -> Ocaml_parser_lexer.string_lexbuf string)
    Parse.longident
    Frontend.parse_any_longident
;;

let parse_core_type_exn string =
  parse
    (fun () -> Ocaml_parser_lexer.string_lexbuf string)
    Ppxlib.Parse.core_type
    Frontend.parse_core_type
;;

let parse_expression_exn string =
  parse
    (fun () -> Ocaml_parser_lexer.string_lexbuf string)
    Ppxlib.Parse.expression
    Frontend.parse_expression
;;

let parse_pattern_exn string =
  parse
    (fun () -> Ocaml_parser_lexer.string_lexbuf string)
    Ppxlib.Parse.pattern
    Frontend.parse_pattern
;;

let parse_signature_exn string =
  parse
    (fun () -> Ocaml_parser_lexer.string_lexbuf string)
    Ppxlib.Parse.interface
    Frontend.interface
;;

let parse_structure_exn string =
  parse
    (fun () -> Ocaml_parser_lexer.string_lexbuf string)
    Ppxlib.Parse.implementation
    Frontend.implementation
;;

let try_with_syntax_error f =
  match Ocaml_common.Warnings.without_warnings f with
  | x -> Ok x
  | exception
      (( Lexer.Error _
       | Syntaxerr.Error _
       | Stdlib.Parsing.Parse_error
       | Ocaml_common.Language_extension.Error.Error _
       | Lrgrep_syntax_error _ ) as exn) -> Error (Error.of_exn exn)
;;

let file_tokens ~file_name ~file_contents =
  try_with_syntax_error (fun () ->
    Ocaml_parser_lexer.file_tokens_exn ~file_name ~file_contents)
;;

let string_tokens string =
  try_with_syntax_error (fun () -> Ocaml_parser_lexer.string_tokens_exn string)
;;

let parse_ml ~file_name ~file_contents =
  try_with_syntax_error (fun () -> parse_ml_exn ~file_name ~file_contents)
;;

let parse_mlt ~file_name ~file_contents =
  try_with_syntax_error (fun () -> parse_mlt_exn ~file_name ~file_contents)
;;

let parse_mli ~file_name ~file_contents =
  try_with_syntax_error (fun () -> parse_mli_exn ~file_name ~file_contents)
;;

let parse_file ~file_name ~file_contents ~file_kind =
  try_with_syntax_error (fun () -> parse_exn ~file_name ~file_contents ~file_kind)
;;

let parse_longident string =
  try_with_syntax_error (fun () ->
    Ppxlib_jane.Longident.to_parsetree (parse_longident_exn string))
;;

let parse_core_type string = try_with_syntax_error (fun () -> parse_core_type_exn string)

let parse_expression string =
  try_with_syntax_error (fun () -> parse_expression_exn string)
;;

let parse_pattern string = try_with_syntax_error (fun () -> parse_pattern_exn string)
let parse_signature string = try_with_syntax_error (fun () -> parse_signature_exn string)
let parse_structure string = try_with_syntax_error (fun () -> parse_structure_exn string)
