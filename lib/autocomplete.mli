val autocomplete : string -> Types.encryptable list
(** [autocomplete seek_word] is the list representing every value in the saved
    passwords/login file with a name sharing at least 3 characters in common
    with [seek_word]. *)

val compare_words : string -> string -> bool
(** [compare_words str1 str2] is [true] if [str1] and [str2] have at least 3
    characters in common, and [false] otherwise. Note that this is not
    case-sensitive, and doesn't include non-alphanumerics in names. *)