let trapezoid ~x1 ~x2 ~y1 ~y2 =
  let base = abs_float (x1 -. x2) in
  let height = 0.5 *. (y1 +. y2) in
  base *. height

let auc p_y_array =
  Array.sort (
    fun (p1, _) (p2, _) ->
      Stdlib.compare p2 p1
  ) p_y_array;

  let fp, tp, fp_prev, tp_prev, a, _ = Array.fold_left (
    fun (fp, tp, fp_prev, tp_prev, a, p_prev) (p_i, y_i) ->
      let a, p_prev, fp_prev, tp_prev =
        if p_i = p_prev then
          a, p_prev, fp_prev, tp_prev
        else
          let a = a +. trapezoid ~x1:fp ~x2:fp_prev ~y1:tp ~y2:tp_prev in
          let p_prev = p_i in
          let fp_prev = fp in
          let tp_prev = tp in
          a, p_prev, fp_prev, tp_prev
      in
      let fp, tp =
        if y_i then
          fp, tp +. 1.
        else
          fp +. 1., tp
      in
      fp, tp, fp_prev, tp_prev, a, p_prev

  ) (0., 0., 0., 0., 0., neg_infinity) p_y_array in

  let a = a +. trapezoid ~x1:fp ~x2:fp_prev ~y1:tp ~y2:tp_prev in
  let a = a /. (fp *. tp) in
  a

