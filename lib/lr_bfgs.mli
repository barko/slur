type observation = bool * float list

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

val fit : t -> (float * Logistic_regression.model, int list) result

