(* decode a run-length encoded list *)

type 'a rle = One of 'a | Many of int * 'a

let decode l =
  let rec rep acc x = function 0 -> acc | n -> rep (x :: acc) x (n - 1) in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (rep acc x n) t
  in
  List.rev (aux [] l)
