(** float bigarray *)

open Bigarray
type t = (float, float64_elt, c_layout) Array1.t

let create n =
  let a = Array1.create float64 c_layout n in
  Array1.fill a 0.0;
  a

let len a =
  Array1.dim a
