(* drop every nth element from a list *)

let drop l n =
  let rec aux acc idx = function
    | [] -> acc
    | h :: t -> if idx = n then aux acc 1 t else aux (h :: acc) (idx + 1) t
  in
  List.rev (aux [] 1 l)
