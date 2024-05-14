val autocomplete : string -> Types.encryptable list
(** [autocomplete seek_word] is the list representing every value in the saved
    passwords/login file with a name sharing at least 3 characters in common
    with [seek_word]. *)
