(* palindrome *)

let pal l =
  let rec rev = function [] -> [] | h :: t -> rev t @ [ h ] in
  l = rev l
