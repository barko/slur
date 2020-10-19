open Logistic_regression

let dot1z ~w ~x z_stats =
  let sum = ref w.{0} in
  List.iteri (
    fun j xj ->
      let mean, std = z_stats.(j) in
      let zj = (xj -. mean) /. std in
      sum := !sum +. zj *. w.{j + 1}
  ) x;
  !sum


type z = {
  histograms : Bentov.histogram array;
  mutable n : int;
  mutable n_true : int
}

let num_features { histograms; _ } =
  Array.length histograms

let zscore ~num_features ~num_bins =
  if num_features < 1 || num_bins < 2 then
    Error `InvalidArgument
  else
    let histograms = Array.init num_features (fun _ -> Bentov.create num_bins) in
    Ok { histograms; n = 0; n_true = 0 }

let add_z z yy xx =
  if List.length xx = num_features z then (
    z.n <- z.n + 1;
    if yy then
      z.n_true <- z.n_true + 1;
    List.iteri (
      fun j xj ->
        z.histograms.(j) <- Bentov.add xj z.histograms.(j)
    ) xx;
    Ok ()
  )
  else (Error `BadLength)

type train_eval = {
  (* number of features, including mean term *)
  m : int;

  (* number of observations in the training set *)
  n : int;
  nf : float;

  (* learned coefficients *)
  w : FBA.t;

  (* mean, standard deviation of each feature *)
  z_stats : (float * float) array;

  alpha : float;
  (* learning rate *)

  lambda : float;
  (* regularization rate *)

  g : FBA.t;
  (* derivatives *)

  mutable loss : float;

  mutable k : int;
}

type t = train_eval

let train ~learning_rate ~lambda z =
  if 0.0 >= learning_rate || learning_rate >= 1.0 then
    Error `InvalidArgument
  else
    let m = Array.length z.histograms in
    let z_stats = Array.map Bentov.mean_stdev z.histograms in
    (* identify uniform features, if any *)
    let uniform_features = Util.array_indexes_where (fun (_, std) -> std = 0.0) z_stats in
    match uniform_features with
    | _ :: _ -> (* not empty: at least one uniform feature *)
      Error (`UniformFeatures uniform_features)
    | [] ->
      let g = FBA.create (m + 1) in
      let w = FBA.create (m + 1) in

      (* initialize mean term *)
      let n_false = z.n - z.n_true in
      if z.n_true = 0 || n_false = 0 then
        Error `UniformTarget
      else
    (*
    let c =
      if n_false = 0 then
        failwith "target is uniform";
      0.5 *. (log (float z.n_true /. float n_false))
    in
    *)
        Ok {
          m = m; g; w; n = z.n;
          nf = float z.n;
          alpha = learning_rate;
          loss = 0.0;
          z_stats;
          lambda;
          k = 0;
        }

let grad t y x =
  let {w; g; lambda; nf; z_stats; _ } = t in
  let d = dot1z ~w ~x z_stats in
  let y = bool_to_1 y in
  let p = (phi (y *. d)) -. 1.0 in
  g.{0} <- y *. p +. lambda *. w.{0} /. nf;
  Util.list_iteri 1 (
    fun j xj ->
      let mean, std = z_stats.(j-1) in
      let zj = (xj -. mean) /. std in
      g.{j} <- y *. zj *. p +. lambda *. w.{j} /. nf;
  ) x

let add_t t y x =
  let { w; m; g; alpha; _ } = t in
  if List.length x = m then (
    grad t y x;
    for j = 0 to m do
      w.{j} <- w.{j} -. alpha *. g.{j}
    done;
    Ok ()
  )
  else
    Error `BadLength


type e = train_eval

let eval_loss t =
  t.loss <- 0.0;
  t.k <- 0;
  t

let add_e e y x =
  if List.length x = e.m then (
    let { k; loss; w; z_stats; _ } = e in
    let d = dot1z ~w ~x z_stats in
    let y = bool_to_1 y in
    e.loss <- loss +. log_phi (y *. d);
    e.k <- k + 1;
    Ok ()
  )
  else
    Error `BadLength

let loss e =
  let { m; w; lambda; _ } = e in
  let norm_squared_w =
    let sum = ref 0. in
    for j = 0 to m do
      sum := !sum +. w.{j} *. w.{j}
    done;
    !sum
  in
  let result = -. e.loss /. (float e.k) +. 0.5 *. lambda *. norm_squared_w in
  result, e

let model (e:e) =
  let { m; w; z_stats; _ } = e in
  let open Linear_t in
  let fa = Array.init m (
    fun j ->
      let mean, std = z_stats.(j) in
      { w = w.{j + 1}; mean; std }
  ) |> Array.to_list in
  let c = w.{0} in
  { fa; c }




