type t

val make : unit -> t
val insert : string -> t -> unit
val mem : string -> t -> bool
val of_list : string list -> t
val of_file : string -> t
val to_list : t -> string list
