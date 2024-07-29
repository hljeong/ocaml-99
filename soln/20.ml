(* remove the kth element from a list *)

let remove_at k l =
  let rec aux acc idx = function
    | [] -> acc
    | h :: t ->
        if idx = k then aux acc (idx + 1) t else aux (h :: acc) (idx + 1) t
  in
  List.rev (aux [] 0 l)
