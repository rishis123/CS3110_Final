module HashedIntPair = struct
  type t = int * int

  let equal (i1, i2) (i1', i2') = i1 = i1' && i2 = i2'
  let hash = Hashtbl.hash
end

module Memo = Memoize.Make (HashedIntPair)

let min_edit_distance (gap_penalty : float) (match_cost : char -> char -> float)
    (str1 : string) (str2 : string) : float =
  (* [opt i j] denotes the min edit distance between the first i chars of str1
     and the first j chars of str2 *)
  let opt =
    Util.uncurry
    @@ Memo.memoize ~n:10 (fun opt (i, j) ->
           let opt = Util.uncurry opt in
           if i = 0 && j = 0 then 0.
             (* This base case is necessary in case gap_penalty =
                Float.infinity, as Float.infinity * 0. = Float.nan *)
           else if j = 0 then gap_penalty *. float_of_int i
           else if i = 0 then gap_penalty *. float_of_int j
           else
             let match_cost =
               let match_ij = match_cost str1.[i - 1] str2.[j - 1] in
               match_ij +. opt (i - 1) (j - 1)
             in
             let skip_i_cost = gap_penalty +. opt (i - 1) j in
             let skip_j_cost = gap_penalty +. opt i (j - 1) in
             Util.min [ match_cost; skip_i_cost; skip_j_cost ])
  in
  let m = String.length str1 in
  let n = String.length str2 in
  opt m n

let min_edit_distance_unit_cost =
  min_edit_distance 1.0 (fun x y -> if x = y then 0. else 1.)

let min_edit_distance_unit_cost_case_insensitive =
  min_edit_distance 1.0 (fun x y ->
      if Char.lowercase_ascii x = Char.lowercase_ascii y then 0. else 1.)
