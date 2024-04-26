let unencryptable_to_string (Types.MasterPasswordHash h) = h

let encryptable_to_string = function
  | Types.Password { name = n; password = p } ->
      "name = " ^ n ^ ", password = " ^ p
  | Types.Login { name = n; username = u; password = p; url } -> (
      match url with
      | None -> "name = " ^ n ^ ", username = " ^ u ^ ", password = " ^ p
      | Some url_val ->
          "name = "
          ^ n
          ^ ", username = "
          ^ u
          ^ ", password = "
          ^ p
          ^ ", url = "
          ^ url_val)
