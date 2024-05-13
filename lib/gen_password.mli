val gen_password_val : unit -> string
(** Generates a password of length [length_choice], with the user's choice of
    special characters (#, $, and &) or not. *)

val generate_password_without_special : int -> string
(** [generate_password_without_special length] is a random password of length
    [length] that may include characters a-z, A-Z, and 0-9 (no special
    characters). *)

val generate_password_with_special : int -> string
(** [generate_password_with_special length] is a random password of length
    [length] that may include characters a-z, A-Z, #, $, & (special characters),
    and 0-9. *)
