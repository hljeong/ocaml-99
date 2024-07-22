(* last two elements of a list *)

let rec last_two l =
  match l with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t
