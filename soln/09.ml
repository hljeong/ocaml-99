(* pack consecutive duplicates *)

let pack l =
  let rec aux acc cur = function
    | [] -> []
    | [ e ] -> (e :: cur) :: acc
    | x :: (y :: _ as t) ->
        if x = y then aux acc (x :: cur) t else aux ((x :: cur) :: acc) [] t
  in
  List.rev (aux [] [] l)
