val autocomplete : string -> Types.encryptable list
(** [autocomplete seek_word] is the list representing every value in the saved
    passwords/login file with a name sharing at least 3 characters in common
    with [seek_word]. *)

val check_strength : string -> bool
(** [check_strength password_entry] is [true] if any overlap exists between
    [password_entry] and the most common passwords. *)

val compare_words : string -> string -> bool
(** [compare_words str1 str2] is [true] if [str1] and [str2] have at least 3
    characters in common, and [false] otherwise. Note that this is not
    case-sensitive, and doesn't include non-alphanumerics in names. *)

val common_passwords : string list
val check_vulnerabilities : unit -> string list
(* Checks every saved password or login, checking which are secure and which
   aren't. *)
