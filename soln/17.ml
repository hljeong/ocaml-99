(* split a list into two parts; the length of the first part is given *)

let split l n =
  let rec aux acc idx = function
    | [] -> (List.rev acc, [])
    | h :: t ->
        if idx = n then (List.rev acc, h :: t) else aux (h :: acc) (idx + 1) t
  in
  aux [] 0 l
