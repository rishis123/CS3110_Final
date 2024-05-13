val generate_password_without_special : int -> string
(** [generate_password_without_special length] is a random password of length
    [length] that may include characters a-z, A-Z, and 0-9 (no special
    characters). *)

val generate_password_with_special : int -> string
(** [generate_password_with_special length] is a random password of length
    [length] that may include characters a-z, A-Z, #, $, & (special characters),
    and 0-9. *)
