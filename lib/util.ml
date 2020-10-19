let list_iteri =
  let rec loop i f = function
    | [] -> ()
    | h :: t ->
      f i h;
      loop (i + 1) f t
  in
  fun start f list ->
    loop start f list

let array_indexes_where f a =
  let _, indexes = Array.fold_left (
    fun (j, indexes) x ->
      let indexes =
        if f x then
          j :: indexes
        else
          indexes
      in
      j + 1, indexes
  ) (0, []) a in
  indexes


