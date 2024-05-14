type t

val make : unit -> t
val insert : string -> t -> unit
val to_string : t -> string
val of_list : string list -> t
val of_file : string -> t
val mem : string -> t -> bool
val to_list_list : ?str_start_idx:int -> t -> char list list
val to_list : t -> string list
