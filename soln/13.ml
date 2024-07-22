(* run-length encoding of a list (direct solution) *)

type 'a rle = One of 'a | Many of int * 'a

(* but why? *)
let encode l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> (
        match acc with
        | [] -> aux [ One h ] t
        | One x :: t_ ->
            if h = x then aux (Many (2, x) :: t_) t else aux (One h :: acc) t
        | Many (n, x) :: t_ ->
            if h = x then aux (Many (n + 1, x) :: t_) t
            else aux (One h :: acc) t)
  in
  List.rev (aux [] l)
