val set_key : string -> unit
val encrypt : Types.encryptable -> string
val decrypt_password : string -> Types.encryptable
val decrypt_login : string -> Types.encryptable
val salt_hash : string -> Types.unencryptable
