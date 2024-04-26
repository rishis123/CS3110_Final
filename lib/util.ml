let non_empty_or_none = function
  | "" -> None
  | str -> Some str

let min lst =
  let min_opt =
    List.fold_left
      (fun acc_opt x ->
        match acc_opt with
        | None -> Some x
        | Some acc -> Some (min acc x))
      None lst
  in
  match min_opt with
  | Some m -> m
  | None -> failwith "No minimum exists for an empty list"

let uncurry f x y = f (x, y)