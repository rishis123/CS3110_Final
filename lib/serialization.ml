let unencryptable_to_string (Types.MasterPasswordHash h) = h

let encryptable_to_string = function
  | Types.Password { name = n; password = p } -> n ^ "," ^ p
