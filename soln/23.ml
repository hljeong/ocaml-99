(* extract a given number of randomly selected elements from a list *)

let rand_select l n =
  Random.init 0;
  let len = List.length l in
  let rec extract acc i = function
    | [] -> raise Not_found
    | h :: t ->
        if i = 0 then (h, List.rev acc @ t) else extract (h :: acc) (i - 1) t
  in
  let rec aux acc n' len' pool =
    if n' = 0 then List.rev acc
    else
      let e, rest = extract [] (Random.int len') pool in
      aux (e :: acc) (n' - 1) (len' - 1) rest
  in
  aux [] n len l
