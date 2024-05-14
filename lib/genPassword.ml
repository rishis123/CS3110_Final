let generate_password_without_special length =
  String.init length (fun _ ->
      match Random.int 4 with
      | 0 -> Char.chr (97 + Random.int 26) (* lowercase letters (a-z) *)
      | 1 -> Char.chr (65 + Random.int 26) (* uppercase letters (A-Z) *)
      | _ -> Char.chr (48 + Random.int 10)
      (* digits (0-9) *))

let generate_password_with_special length =
  String.init length (fun _ ->
      match Random.int 4 with
      | 0 -> Char.chr (97 + Random.int 26) (* lowercase letters (a-z) *)
      | 1 -> Char.chr (65 + Random.int 26) (* uppercase letters (A-Z) *)
      | 2 -> Char.chr (48 + Random.int 10) (* digits (0-9) *)
      | _ -> Char.chr (Array.get [| 35; 36; 38 |] (Random.int 3))
      (* symbols (#, $, &) *))
