module type Directories = sig
  val masterpwd_file_path : string
  val encrypted_file_path : string
  val permission : int
end

module type Persistence = sig
  val read_master_password_hash : unit -> Types.master_password_hash
  val write_unencryptable : Types.unencryptable -> unit
  val read_all_encryptable : unit -> Types.encryptable list
  val write_encryptable : Types.encryptable -> unit
  val delete_encryptable_by_name : string -> unit

  val set_file_perms : unit -> unit
  (** [set_file_perms ()] sets the data files to read and write allowed only for
      the owner. *)
end

module Make (_ : Directories) : Persistence
module Default : Persistence
