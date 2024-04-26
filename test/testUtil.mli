val delete_recursive : string -> unit
(** [delete_recursive dir_path] deletes the directory at [dir_path] along with
    all of its subdirectories. Precondition: the directory at [dir_path] must
    not be mutated during execution of this function. *)

val is_empty : string -> bool
(** [is_empty dir_path] returns true if the directory at [dir_path] is empty *)

val contents_recursive : string -> include_dots:bool -> string list
(** [contents_recursive dir_path] returns the paths of all files in the
    directory at [dir_path] as well as the paths of all files in any
    subdirectory. Precondition: the directory at [dir_path] must not be mutated
    during execution of this function. *)

val run_timeout : int -> (unit -> unit) -> bool
(** [run_timeout timeout f] runs [f], stopping execution after [timeout]
    seconds. Returns true if execution completed within the timeout and false if
    it was halted. *)

val zip : 'a Seq.t -> 'b Seq.t -> ('a * 'b) Seq.t
(** [zip s1 s2] returns a sequence of tuples, with the ith tuple containing the
    ith element of s1 and the ith element of s2 *)
