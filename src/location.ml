open! Base

type t = Ppxlib.location =
  { loc_start : Position.t
  ; loc_end : Position.t
  ; loc_ghost : bool
  }
[@@deriving compare ~localize, equal ~localize, fields ~getters, hash]

let (to_string @ portable) t =
  let line1, column1 = Position.line_and_column t.loc_start in
  let line2, column2 = Position.line_and_column t.loc_end in
  Printf.sprintf
    "File %S, %s, characters %d-%d:"
    t.loc_start.pos_fname
    (if line1 = line2
     then Printf.sprintf "line %d" line1
     else Printf.sprintf "lines %d-%d" line1 line2)
    column1
    column2
;;

let sexp_of_t t = Sexp.Atom (to_string t)
let length t = t.loc_end.pos_cnum - t.loc_start.pos_cnum
let is_empty t = Int.equal (length t) 0

let contains t ~inner =
  String.equal t.loc_start.pos_fname t.loc_end.pos_fname
  && Position.( <= ) t.loc_start inner.loc_start
  && Position.( >= ) t.loc_end inner.loc_end
;;

let overlap t1 t2 =
  if String.equal t1.loc_start.pos_fname t2.loc_start.pos_fname
  then (
    let loc_start = Comparable.max Position.compare t1.loc_start t2.loc_start in
    let loc_end = Comparable.min Position.compare t1.loc_end t2.loc_end in
    if Position.( <= ) loc_start loc_end
    then Some { loc_start; loc_end; loc_ghost = true }
    else None)
  else None
;;

let is_overlapping_or_adjacent t1 t2 = Option.is_some (overlap t1 t2)

let is_overlapping_nonempty t1 t2 =
  match overlap t1 t2 with
  | None -> false
  | Some t -> not (is_empty t)
;;

let containing list =
  match (list : _ Nonempty_list.t) with
  | [ loc ] -> Ok loc
  | _ :: _ :: _ ->
    let%bind.Or_error loc_start =
      Position.earliest (Nonempty_list.map list ~f:loc_start)
    in
    let%bind.Or_error loc_end = Position.latest (Nonempty_list.map list ~f:loc_end) in
    let loc_ghost = Nonempty_list.exists list ~f:loc_ghost in
    Ok { loc_start; loc_end; loc_ghost }
;;

include (val Comparator.make ~compare ~sexp_of_t)
