type t
(** The type representing tries, supporing the alphabet consisting of
   alphanumeric characters, space, and the following special characters: *, ?,
   &, %, ^, !, @, ., _, -, #, ~. Note that this trie cannot contain the empty string. *)

val make : unit -> t
(** [make ()] creates an empty trie. *)

val insert : string -> t -> unit
(** [insert str trie] inserts [str] into [trie]. *)

val mem : string -> t -> bool
(** [mem str trie] checks if [trie] contains [str]. *)

val of_list : string list -> t
(** [of_list lst] is the trie consisting of all strings in [lst]. *)

val of_file : string -> t
(** [of_file path] is the trie consisting of all lines in the file at [path]. *)

val to_list : t -> string list
(** [to_list trie] is the list of strings contained in [trie]. *)
