(* extract a slice from a list *)

let slice l i k =
  let rec aux acc idx = function
    | [] -> List.rev acc
    | h :: t ->
        if i <= idx && idx <= k then aux (h :: acc) (idx + 1) t
        else aux acc (idx + 1) t
  in
  aux [] 0 l
