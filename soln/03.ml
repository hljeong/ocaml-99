(* nth element of a list *)

let rec nth l n =
  match l with [] -> None | h :: t -> if n = 0 then Some h else nth t (n - 1)
