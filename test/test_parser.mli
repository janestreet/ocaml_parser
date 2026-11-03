include
  module type of Ocaml_parser
  with module Ast := Ocaml_parser.Ast
   and module File_kind := Ocaml_parser.File_kind
   and module File_contents := Ocaml_parser.File_contents
   and module Location := Ocaml_parser.Location
   and module Longident := Ocaml_parser.Longident
   and module Position := Ocaml_parser.Position
   and module Token := Ocaml_parser.Token
