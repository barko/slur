type z

val zscore : num_features:int -> num_bins:int -> (z, [`InvalidArgument]) result
val add_z : z -> bool -> float list -> (unit, [`BadLength]) result

type t

val train : learning_rate:float -> lambda:float -> z ->
  (t, [`InvalidArgument | `UniformTarget | `UniformFeatures of int list ]) result
val add_t : t -> bool -> float list -> (unit, [`BadLength]) result

val model : t -> Logistic_regression.model

type e
val eval_loss : t -> e
val add_e : e -> bool -> float list -> (unit, [`BadLength]) result
val loss : e -> float * t


