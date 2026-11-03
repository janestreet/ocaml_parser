open! Base
include File_contents_intf.Types

type t =
  | ML of Ast.Structure.t
  | MLI of Ast.Signature.t
  | MLT of Ast.Toplevel_phrase.t list
[@@deriving compare ~localize, equal ~localize, hash]

let kind t : File_kind.t =
  match t with
  | ML _ -> ML
  | MLI _ -> MLI
  | MLT _ -> MLT
;;

let iter t obj =
  match t with
  | ML structure -> obj#structure structure
  | MLI signature -> obj#signature signature
  | MLT phrases -> List.iter phrases ~f:obj#toplevel_phrase
;;

let map t obj =
  match t with
  | ML structure -> ML (obj#structure structure)
  | MLI signature -> MLI (obj#signature signature)
  | MLT phrases -> MLT (List.map phrases ~f:obj#toplevel_phrase)
;;

let lift t (obj : _ lift) =
  match t with
  | ML structure -> obj#structure structure
  | MLI signature -> obj#signature signature
  | MLT phrases -> obj#list obj#toplevel_phrase phrases
;;

module Monadic2 (M : Monad.S2) = struct
  let map t obj =
    match t with
    | ML structure -> M.map (obj#structure structure) ~f:(fun structure -> ML structure)
    | MLI signature -> M.map (obj#signature signature) ~f:(fun signature -> MLI signature)
    | MLT phrases ->
      let rec loop phrases ~acc =
        match phrases with
        | [] -> M.return (List.rev acc)
        | head :: tail ->
          let%bind.M head = obj#toplevel_phrase head in
          loop tail ~acc:(head :: acc)
      in
      M.map (loop phrases ~acc:[]) ~f:(fun phrases -> MLT phrases)
  ;;
end

module Monadic (M : Monad.S) = struct
  include Monadic2 (struct
      include M

      type ('a, _) t = 'a M.t
    end)
end

let toplevel_phrases_combine_structures phrases =
  List.fold_right phrases ~init:[] ~f:(fun phrase phrases ->
    match (phrase : Ppxlib.toplevel_phrase) with
    (* keep directives *)
    | Ptop_dir _ -> phrase :: phrases
    (* discard empty structures *)
    | Ptop_def [] -> phrases
    (* try to combine non-empty structures *)
    | Ptop_def (_ :: _ as items) ->
      (match phrases with
       (* combine if followed by a (non-empty) structure *)
       | Ptop_def other_items :: other_phrases ->
         Ptop_def (items @ other_items) :: other_phrases
       (* otherwise add to the list *)
       | [] | Ptop_dir _ :: _ -> phrase :: phrases))
;;

let mlt_combine_structures t =
  match t with
  | ML _ | MLI _ -> t
  | MLT phrases -> MLT (toplevel_phrases_combine_structures phrases)
;;

let toplevel_phrases_separate_structures phrases =
  List.fold_right phrases ~init:[] ~f:(fun phrase phrases ->
    match (phrase : Ppxlib.toplevel_phrase) with
    (* keep directives *)
    | Ptop_dir _ -> phrase :: phrases
    (* keep individual structure items *)
    | Ptop_def items ->
      List.fold_right items ~init:phrases ~f:(fun item phrases ->
        Ppxlib.Ptop_def [ item ] :: phrases))
;;

let mlt_separate_structures t =
  match t with
  | ML _ | MLI _ -> t
  | MLT phrases -> MLT (toplevel_phrases_separate_structures phrases)
;;

let format formatter t =
  match mlt_separate_structures t with
  | ML structure -> Ast.Structure.format formatter structure
  | MLI signature -> Ast.Signature.format formatter signature
  | MLT phrases ->
    Stdlib.Format.pp_open_vbox formatter 0;
    List.iteri phrases ~f:(fun i phrase ->
      if i > 0 then Stdlib.Format.pp_print_break formatter 1 0;
      Ast.Toplevel_phrase.format formatter phrase);
    Stdlib.Format.pp_close_box formatter ()
;;

let to_string t =
  match mlt_separate_structures t with
  | ML structure -> Ast.Structure.to_string structure
  | MLI signature -> Ast.Signature.to_string signature
  | MLT phrases ->
    List.map phrases ~f:Ast.Toplevel_phrase.to_string |> String.concat ~sep:"\n"
;;

let sexp_of_t t =
  [%sexp (kind t : File_kind.t), (String.split_lines (to_string t) : string list)]
;;
