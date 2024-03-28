let unencryptable_to_string (Types.MasterPasswordHash h) = h

let encryptable_to_string = function
  | Types.Password { name = _; password = p } -> p
