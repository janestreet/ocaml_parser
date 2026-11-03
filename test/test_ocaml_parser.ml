open! Core

include (
struct
  module Ast = Test_ast
  module File_contents = Test_file_contents
  module File_kind = Test_file_kind
  module Location = Test_location
  module Longident = Test_longident
  module Position = Test_position
  module Token = Test_token
  include Test_parser
end :
  module type of Ocaml_parser [@ocaml.remove_aliases])
