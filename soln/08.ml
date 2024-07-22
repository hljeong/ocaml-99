(* eliminate duplicates *)

(* for some reason they really want it tail-recursive... *)
(* ok why is their solution not tail-recursive on this one??? *)
let compress l =
  let rec aux acc = function
    | [] -> acc
    | [ e ] -> e :: acc
    | x :: (y :: _ as t) -> if x = y then aux acc t else aux (x :: acc) t
  in
  List.rev (aux [] l)
