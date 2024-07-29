(* insert an element at a given position into a list *)

let insert_at e i l =
  let rec aux acc idx = function
    | [] -> if idx <= i then e :: acc else acc
    | h :: t ->
        if idx == i then aux (h :: e :: acc) (idx + 1) t
        else aux (h :: acc) (idx + 1) t
  in
  List.rev (aux [] 0 l)
