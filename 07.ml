(* flatten a list *)

type 'a node = One of 'a | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One x :: t -> x :: flatten t
  | Many x :: t -> flatten x @ flatten t
