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
let fuzzy_equal x y = String.trim x = String.trim y

let zip (s1 : 'a Seq.t) (s2 : 'b Seq.t) : ('a * 'b) Seq.t =
  let open Seq in
  match (s1 (), s2 ()) with
  | Cons (h1, t1), Cons (h2, t2) -> fun () -> Cons ((h1, h2), zip t1 t2)
  | Nil, Nil -> fun () -> Nil
  | _ -> failwith "different sizes"

let compare_by sel x y = compare (sel x) (sel y)

let sorted_by_below_threshold sel thresh seq =
  let seq_with_select = Seq.map sel seq |> zip seq in
  let arr_with_select_above_threshold =
    seq_with_select
    |> Seq.filter (fun (_, select_val) -> select_val <= thresh)
    |> Array.of_seq
  in
  let compare_by_select = compare_by snd in
  Array.fast_sort compare_by_select arr_with_select_above_threshold;
  arr_with_select_above_threshold
  |> Array.to_seq
  |> Seq.map (fun (elem, _) -> elem)
