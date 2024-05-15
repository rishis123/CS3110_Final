type 'a t = {
  mutable underlying : 'a option array;
  mutable length : int;
}
(** The type of an ordered multiset.
    AF: the multiset { underlying=[Some x_0; ... Some x_n; None; None]; length=n } represents
      the set containing the elements {x_0; ... x_n} (in that order).
    RI: set.length = the number of Some elements in set.underlying. 
    Also, all Somes come before all Nones in set *)

let make size = { underlying = Array.make size None; length = 0 }

let maybe_resize arr =
  let underlying_size = Array.length arr.underlying in
  if arr.length = underlying_size then begin
    (* resize *)
    let old_arr = arr.underlying in
    let new_arr = Array.make (2 * underlying_size) None in
    Array.blit old_arr 0 new_arr 0 underlying_size;
    arr.underlying <- new_arr
  end

let insert x arr =
  maybe_resize arr;
  arr.underlying.(arr.length) <- Some x;
  arr.length <- arr.length + 1

let find_opt pred arr =
  arr.underlying
  |> Array.find_opt (function
       | Some node -> pred node
       | None -> false)
  |> Option.join

let fold_left f init arr =
  Array.fold_left
    (fun acc x_opt ->
      match x_opt with
      | Some x -> f acc x
      | None -> acc)
    init arr.underlying
