(* run-length encoding *)

(* how is this beginner again?? *)
let encode l =
  let rec aux acc run = function
    | [] -> []
    | [ e ] -> (1 + run, e) :: acc
    | x :: (y :: _ as t) ->
        if x = y then aux acc (1 + run) t else aux ((1 + run, x) :: acc) 0 t
  in
  List.rev (aux [] 0 l)
