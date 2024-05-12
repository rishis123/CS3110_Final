val min_edit_distance :
  float -> (char -> char -> float) -> string -> string -> float
(** [min_edit_distance gap_penalty match_cost str1 str2] calculates the "minimum
    edit distance" between [str1] and [str2]; that is, the number of operations
    required to transform one of the strings into another. Implementation is a
    memoized recursive implementation of that in Klienberg and Tardoš. *)

val min_edit_distance_unit_cost : string -> string -> float
(** [min_edit_distance_unit_cost str1 str2] returns the minimum edit distance
    between [str1] and [str2] with unit weights for all differences. *)

val min_edit_distance_unit_cost_case_insensitive : string -> string -> float
(** [min_edit_distance_unit_cost_case_insensitive str1 str2] returns the minimum edit distance
    between [str1] and [str2] with unit weights for all differences, ignoring case. *)

