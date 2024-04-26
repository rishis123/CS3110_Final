val min_edit_distance :
  float -> (char -> char -> float) -> string -> string -> float
(** [min_edit_distance gap_penalty match_cost str1 str2] calculates the
    "minimum edit distance" between [str1] and [str2]; that is, the number of
    operations required to transform one of the strings into another.
    Implementation is a memoized recursive implementation of that in Klienberg
    and Tardoš. *)
