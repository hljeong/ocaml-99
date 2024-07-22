(* length of a list *)

let len l =
  let rec len_ acc = function [] -> acc | _ :: t -> len_ (acc + 1) t in
  len_ 0 l
