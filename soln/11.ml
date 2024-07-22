(* modified run-length encoding *)

type 'a rle = One of 'a | Many of int * 'a

let encode l =
  let rec aux acc run = function
    | [] -> []
    | [ e ] -> if run = 0 then One e :: acc else Many (1 + run, e) :: acc
    | x :: (y :: _ as t) ->
        if x = y then aux acc (1 + run) t
        else if run = 0 then aux (One x :: acc) 0 t
        else aux (Many (1 + run, x) :: acc) 0 t
  in
  List.rev (aux [] 0 l)
