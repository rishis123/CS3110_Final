open Types

val import : string -> encryptable list
(** [import path] returns all of the secrets stored in plaintext in the file at
    [path] *)

val export : encryptable list -> string -> unit
(** [export secrets path] writes all of [secrets] in plaintext to [path] *)
