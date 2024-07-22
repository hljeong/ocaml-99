(* reverse a list *)

let rev l =
  let rec rev_ acc = function [] -> acc | h :: t -> rev_ (h :: acc) t in
  rev_ [] l
