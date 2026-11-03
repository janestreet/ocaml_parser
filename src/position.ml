open! Base

type t = Ppxlib.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving compare ~localize, equal ~localize, hash]

include Comparable.Infix (struct
    type nonrec t = t [@@deriving compare ~localize]
  end)

let to_string t =
  Ppxlib.Location.print
    Stdlib.Format.str_formatter
    { loc_start = t; loc_end = t; loc_ghost = true };
  Stdlib.Format.flush_str_formatter ()
;;

let sexp_of_t t = Sexp.Atom (to_string t)

let preferred (first :: rest : _ Nonempty_list.t) ~is_better_than =
  List.fold_result ~init:first rest ~f:(fun x y ->
    match String.equal x.pos_fname y.pos_fname with
    | false ->
      Or_error.error_s
        [%sexp "filename mismatch", (x.pos_fname : string), (y.pos_fname : string)]
    | true -> if is_better_than y x then Ok y else Ok x)
;;

let earliest = preferred ~is_better_than:( < )
let latest = preferred ~is_better_than:( > )
let line_and_column t = t.pos_lnum, t.pos_cnum - t.pos_bol
