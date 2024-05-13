module type Directories = sig
  (** [Directories] defines the filepaths for the master password and
      encryptables, as well as the permission used to handle them. *)

  val masterpwd_file_path : string
  (** [masterpwd_file_path] is the filename for the file that stores master
      password in hashed form *)

  val encrypted_file_path : string
  (** [encrypted_file_path] is the filename for the file that stores all
      encrypted data. Invariant: this data matches the schema in
      schemas/encryptables-schema.json. *)

  val permission : int
  (** [permission] is the Unix permission code for the encrypted files. *)
end

module type Persistence = sig
  (** [Persistence] handles persistent storage of master password and
      encryptables, including reading, writing, and deletion. *)

  val read_master_password_hash : unit -> Types.master_password_hash
  (** [read_master_password_hash ()] reads the hashed master password. Requires
      that the hash is in the first line of [masterpwd_file_path]. *)

  val write_unencryptable : Types.unencryptable -> unit
  (** [write_unencryptable master_value] writes the unencryptable information
      (i.e. master password) [master_value] to memory for first time. Requires
      that [master_value] is in hashed form. *)

  val read_all_encryptable : unit -> Types.encryptable list
  (** [read_all_encryptable ()] reads all passwords and logins from
      [encrypted_file_path] and decrypts them. *)

  val write_encryptable : Types.encryptable -> unit
  (** [write_encryptable encryptable] encrypts [encryptable] and writes the
      information to [encrypted_file_path]. *)

  val delete_encryptable_by_name : string -> unit
  (** [delete_encryptable_by_name encrypt_val_name] removes the corresponding
      password/login entry with the name [encrypt_val_name] from
      [encrypted_file_path]. *)

  val set_file_perms : unit -> unit
  (** [set_file_perms ()] sets the data files to read and write allowed only for
      the owner. *)
end

(** [Make Dirs] builds a module to handle persistent storage. *)
module Make (_ : Directories) : Persistence

module Default : Persistence
(** [Default] handles persistent storage using the default settings. *)
