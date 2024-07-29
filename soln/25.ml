(* generate a random permutation of the elements of a list *)

let permutation l =
  let lotto_select n m =
    let rec gen_range acc = function
      | 0 -> 0 :: acc
      | cur -> gen_range (cur :: acc) (cur - 1)
    in
    let rand_select l n =
      Random.init 0;
      let len = List.length l in
      let rec extract acc i = function
        | [] -> raise Not_found
        | h :: t ->
            if i = 0 then (h, List.rev acc @ t)
            else extract (h :: acc) (i - 1) t
      in
      let rec aux acc n' len' pool =
        if n' = 0 then List.rev acc
        else
          let e, rest = extract [] (Random.int len') pool in
          aux (e :: acc) (n' - 1) (len' - 1) rest
      in
      aux [] n len l
    in
    rand_select (gen_range [] (m - 1)) n
  in
  let rec at i = function
    | [] -> raise Not_found
    | h :: t -> if i = 0 then h else at (i - 1) t
  in
  let rec select acc = function
    | [] -> List.rev acc
    | h :: t -> select (at h l :: acc) t
  in
  let len = List.length l in
  select [] (lotto_select len len)
