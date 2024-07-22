(* tail of a list *)

let rec last l =
  match l with [] -> None | h :: [] -> Some h | _ :: t -> last t
