val delete_recursive : string -> unit
(** [delete_recursive dir_path] deletes the directory at [dir_path] along with
    all of its subdirectories. Precondition: the directory at [dir_path] must
    not be mutated during execution of this function. *)

val is_empty : string -> bool
(** [is_empty dir_path] returns true if the directory at [dir_path] is empty *)

val contents_recursive : string -> string list
(** [contents_recursive dir_path] returns all files in the directory at
    [dir_path] as well as all files in any subdirectory. Precondition: the
    directory at [dir_path] must not be mutated during execution of this
    function. *)
