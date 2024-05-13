(* let rec generate_password_without_special_old index acc = if index <= 0 then
   acc else let range_choice = Random.int 3 in let char_val = match range_choice
   with | 0 -> Char.chr (97 + Random.int 26) (* lowercase letters (a-z) *) | 1
   -> Char.chr (65 + Random.int 26) (* uppercase letters (A-Z) *) | _ ->
   Char.chr (48 + Random.int 10) (* digits (0-9) *) in
   generate_password_without_special_old (index - 1) (char_val :: acc) *)

let generate_password_without_special length =
  String.make length
    (match Random.int 4 with
    | 0 -> Char.chr (97 + Random.int 26) (* lowercase letters (a-z) *)
    | 1 -> Char.chr (65 + Random.int 26) (* uppercase letters (A-Z) *)
    | _ -> Char.chr (48 + Random.int 10) (* digits (0-9) *))

(* let rec generate_password_with_special_old index acc = if index <= 0 then acc
   else let range_choice = Random.int 4 in let char_val = match range_choice
   with | 0 -> Char.chr (97 + Random.int 26) (* lowercase letters (a-z) *) | 1
   -> Char.chr (65 + Random.int 26) (* uppercase letters (A-Z) *) | 2 ->
   Char.chr (48 + Random.int 10) (* digits (0-9) *) | _ -> Char.chr (Array.get
   [| 35; 36; 38 |] (Random.int 3)) (* symbols (#, $, &) *) in
   generate_password_with_special_old (index - 1) (char_val :: acc) *)

let generate_password_with_special length =
  String.make length
    (match Random.int 4 with
    | 0 -> Char.chr (97 + Random.int 26) (* lowercase letters (a-z) *)
    | 1 -> Char.chr (65 + Random.int 26) (* uppercase letters (A-Z) *)
    | 2 -> Char.chr (48 + Random.int 10) (* digits (0-9) *)
    | _ ->
        Char.chr (Array.get [| 35; 36; 38 |] (Random.int 3))
        (* symbols (#, $, &) *))

let gen_password_val () =
  print_endline "Choose password length:";
  let length_choice = int_of_string (read_line ()) in

  print_endline "Allow special characters?";
  print_endline "1. Yes";
  print_endline "2. No";
  let special_choice = int_of_string (read_line ()) in

  (* printing string representation of the returned char list*)
  match (length_choice, special_choice) with
  | len, 1 -> generate_password_with_special len
  | len, 2 -> generate_password_without_special len
  | _ -> "Invalid response"
