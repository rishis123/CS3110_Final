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

let fuzzy_equal x y = 
  String.trim x = String.trim y

let rec zip l1 l2 =
  match (l1, l2) with
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip t1 t2
  | [], [] -> []
  | _ -> failwith "different sizes"