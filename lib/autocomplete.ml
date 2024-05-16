module Persistence = Persistence.Default

(** Function to remove all non-alphanumeric characters from the word [str], and
    make it lowercase. *)
let clean_up str =
  let normalized = String.lowercase_ascii str in
  let is_alphanumeric c =
    match c with
    | 'a' .. 'z' | '0' .. '9' -> true
    | _ -> false
  in
  let rec clean_chars index =
    if index >= String.length normalized then ""
    else if is_alphanumeric normalized.[index] then
      String.make 1 normalized.[index] ^ clean_chars (index + 1)
    else clean_chars (index + 1)
  in
  clean_chars 0

let get_substrings str =
  let len = String.length str in
  let rec generate_substrings i =
    if i + 3 > len then [] else String.sub str i 3 :: generate_substrings (i + 1)
  in
  generate_substrings 0

let check_substring sub_str str =
  if String.length sub_str < 3 then false
  else if String.length sub_str > String.length str then false
  else
    let len = String.length sub_str in
    let rec compare_chars i =
      if i >= len then true
      else if sub_str.[i] <> str.[i] then false
      else compare_chars (i + 1)
    in
    compare_chars 0 && len >= 3

let rec iterate substrings str =
  match substrings with
  | [] -> false
  | sub_str :: rest ->
      if check_substring sub_str str then true else iterate rest str

let compare_words str1 str2 =
  let new_str1 = clean_up str1 in
  let substrings = get_substrings new_str1 in
  let new_str2 = clean_up str2 in
  iterate substrings new_str2

let autocomplete seek_word =
  let encryptable_list = Persistence.read_all_encryptable () in
  let filtered_list =
    List.filter
      (fun encryptable ->
        match encryptable with
        | Types.Password pwd -> compare_words pwd.name seek_word
        | Types.Login login -> compare_words login.name seek_word)
      encryptable_list
  in
  filtered_list
