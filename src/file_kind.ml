open! Base

type t =
  | ML
  | MLI
  | MLT
[@@deriving compare ~localize, enumerate, equal ~localize, hash, sexp_of]

let to_dotted_extension = function
  | ML -> ".ml"
  | MLI -> ".mli"
  | MLT -> ".mlt"
;;

let matches t ~file_name = String.is_suffix file_name ~suffix:(to_dotted_extension t)
let of_file_name file_name = List.find all ~f:(matches ~file_name)
