(* rotate a list n places to the left *)

let rotate l n =
  let len = List.length l in
  let n' = ((n mod len) + len) mod len in
  let rec aux acc idx = function
    | [] -> (List.rev acc, [])
    | h :: t ->
        if idx = n' then (List.rev acc, h :: t) else aux (h :: acc) (idx + 1) t
  in
  let lf, rg = aux [] 0 l in
  rg @ lf
