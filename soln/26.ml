(* generate the combinations of k distinct objects chosen from the n elements of a list *)

(* too hard to make this tail-recursive... *)
let rec extract n l =
  if n = 0 then [ [] ]
  else
    match l with
    | [] -> []
    | h :: t ->
        let rec prepend e = function
          | [] -> []
          | h' :: t' -> (e :: h') :: prepend e t'
        in
        prepend h (extract (n - 1) t) @ extract n t
