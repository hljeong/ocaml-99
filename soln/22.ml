(* create a list containing all integers within a given range *)

let rec range l r =
  let rec aux acc cur =
    if cur = l then l :: acc else aux (cur :: acc) (cur - 1)
  in
  if l <= r then aux [] r else List.rev (range r l)
