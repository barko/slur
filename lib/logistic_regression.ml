type model = Linear_t.model

let phi t =
  if t > 0. then
    let e = exp (-. t) in
    1. /. (1. +. e)
  else
    let e = exp t in
    e /. (1. +. e)

let log_phi t =
  if t > 0. then
    let e = exp (-. t) in
    -.log (1. +. e)
  else
    let e = exp t in
    t -. log (1. +. e)

let bool_to_1 = function
  | true  ->    1.0
  | false -> (-.1.0)


open Linear_t

let infer { c; fa } x =
  let _, sum = List.fold_left2 (
    fun (j, sum) { w; mean; std } xj ->
      j + 1, sum +. w *. (xj -. mean) /. std
  ) (0, c) fa x in
  phi sum

let num_features { fa; _ } =
  List.length fa




