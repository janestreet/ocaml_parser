let file_lexbuf ~file_name ~file_contents =
  Lexer.init ();
  let lexbuf = Lexing.from_string file_contents ~with_positions:true in
  Lexing.set_filename lexbuf file_name;
  lexbuf
;;

let string_lexbuf string =
  Lexer.init ();
  Lexing.from_string string ~with_positions:true
;;

let rec tokens_of lexbuf acc =
  match (Lexer.token_with_comments lexbuf : Parser.token) with
  | EOF as tok ->
    let loc = Location.curr lexbuf in
    List.rev ((tok, loc) :: acc)
  | tok ->
    let loc =
      match tok with
      | COMMENT (_, loc) ->
        (* [Location.curr] doesn't work properly for comments *)
        loc
      | DOCSTRING ds -> Docstrings.docstring_loc ds
      | _ -> Location.curr lexbuf
    in
    tokens_of lexbuf ((tok, loc) :: acc)
;;

let file_tokens_exn ~file_name ~file_contents =
  let lexbuf = file_lexbuf ~file_name ~file_contents in
  tokens_of lexbuf []
;;

let string_tokens_exn string =
  let lexbuf = string_lexbuf string in
  tokens_of lexbuf []
  |> List.map fst
  |> List.filter (function
    (* EOF is synthetic when parsing only part of a file, so drop it *)
    | (EOF : Parser.token) -> false
    | _ -> true)
;;
