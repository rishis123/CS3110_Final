val gen_password_val : unit -> string
(** Generates a password of length [length_choice], with the user's choice of
    special characters (#, $, and &) or not. *)

val generate_password_without_special : int -> char list -> char list
(** [generate_password_without_special index acc] is a random list of characters
    starting with [acc], with an additional length of [index] containing
    characters a-z, A-Z, and 0-9 (no special characters). *)

val generate_password_with_special : int -> char list -> char list
(** [generate_password_with_special index acc] is a random list of characters
    starting with [acc], with an additional length of [index] containing
    characters a-z, A-Z, #, $, & (special characters), and 0-9. *)
