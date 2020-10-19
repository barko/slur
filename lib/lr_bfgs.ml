open Bigarray
type vec = Lbfgs.F.vec
open Logistic_regression

let dot1z ~w ~x z_stats =
  let sum = ref w.{1} in
  List.iteri (
    fun j xj ->
      let mean, std = z_stats.(j) in
      let zj = (xj -. mean) /. std in
      sum := !sum +. zj *. w.{j + 2}
  ) x;
  !sum

let norm_squared m w =
  let sum = ref 0. in
  for j = 1 to m do
    Printf.printf "%d %e %e\n" j w.{j} (w.{j} *. w.{j});
    sum := !sum +. w.{j} *. w.{j}
  done;
  !sum

type observation = bool * float list

let list_init =
  let rec loop i n f accu =
    if i = n then
      List.rev accu
    else
      loop (i + 1) n f ((f i) :: accu)
  in
  fun n f ->
    loop 0 n f []

(* [m] here is the number of features *)
let z_stats ~m ~n_bins feature_seq =
  let hists = list_init m (fun _ -> Bentov.create n_bins) in
  let n_true, hists = Seq.fold_left (
    fun (n_true, hists) (y, x) ->
      let hists = List.map2 Bentov.add x hists in
      let n_true = if y then n_true + 1 else n_true in
      n_true, hists
  ) (0, hists) feature_seq in
  (* could use this later to initialize mean term *)
  ignore n_true;
  Array.of_list (List.map Bentov.mean_stdev hists)


type t = {
  m : int;
  (* number of parameters to be learned, including mean term. The
     number of features is [m-1]. *)

  n_bins : int;
  (* number of bins with which to estimate each feature's distribution
  *)

  training_set : unit -> observation Seq.t;
  (* return an observation sequence *)

  lambda : float;
  (* regularization parameter *)

  logging : bool;
  (* should we enable logging? *)

  max_iters : int;
  (* after how many major iterations should we stop? *)

}

let f_df t z_stats (w: vec) (g: vec) =
  let { m; training_set; _ } = t in

  (* initialize g, loss, n *)
  for j = 1 to m do
    g.{j} <- 0.0
  done;

  let n, loss = Seq.fold_left (
    fun (n, loss) (y, x) ->
      let y = bool_to_1 y in
      let z = dot1z ~w ~x z_stats in
      let yz = y *. z in
      let p = phi yz in
      let loss = loss +. log_phi yz in
      let p1 = p -. 1.0 in
      g.{1} <- g.{1} +. y *. p1;
      Util.list_iteri 2 (
        fun j xj ->
          let mean, std = z_stats.(j-2) in
          let zj = (xj -. mean) /. std in
          g.{j} <- g.{j} +. y *. zj *. p1;
      ) x;
      let n = n + 1 in
      n, loss
  ) (0, 0.0) (training_set ()) in

  let n = float n in
  for j = 1 to m do
    g.{j} <- g.{j} /. n +. t.lambda *. w.{j}
  done;

  let norm_squared_w = norm_squared m w in
  Printf.printf "term1=%+e term2=%+e\n"  (-. loss/. n)
    (0.5 *. t.lambda *. norm_squared_w);
  (-. loss) /. n +. 0.5 *. t.lambda *. norm_squared_w

let fit t =
  let {m; n_bins; training_set; logging; lambda; max_iters } = t in
  if m < 1 || n_bins < 2 || lambda <= 0.0 || max_iters < 1 then
    raise (Invalid_argument "fit");

  let w = Array1.create float64 fortran_layout m in
  let z_stats = z_stats ~m:(m-1) ~n_bins (training_set ()) in
  (* identify uniform features, if any *)
  let uniform_features = Util.array_indexes_where (fun (_, std) -> std = 0.0) z_stats in
  match uniform_features with
  | _ :: _ ->
    Error uniform_features

  | [] ->
    let print =
      match logging with
      | true -> Lbfgs.Full
      | false -> Lbfgs.No
    in
    (* ~pgtol:1e-6 ~factr:1e-30 *)
    let minimum_loss = Lbfgs.F.min (f_df t z_stats) w
        ~print ~nsteps:max_iters in
    let open Linear_t in
    let c = w.{1} in
    let fa = Array.init (m-1) (
      fun j ->
        let mean, std = z_stats.(j) in
        { w = w.{j + 2}; mean; std }
    ) |> Array.to_list in
    Ok (minimum_loss, { fa; c })

