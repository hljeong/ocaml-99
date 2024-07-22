(* replicate the elements of a list a given number of times *)

let replicate l n =
  let rec prepend acc x = function
    | 0 -> acc
    (* maybe shadowing is not great... *)
    | n -> prepend (x :: acc) x (n - 1)
  in
  let rec aux acc = function [] -> acc | h :: t -> aux (prepend acc h n) t in
  List.rev (aux [] l)
