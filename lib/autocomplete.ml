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

let compare_words str1 str2 =
  let str1 = clean_up str1 in
  let str2 = clean_up str2 in
  let len1 = String.length str1 in
  let len2 = String.length str2 in
  let rec check_overlap i1 i2 count =
    if i1 >= len1 || i2 >= len2 then count >= 3
    else if str1.[i1] = str2.[i2] then
      check_overlap (i1 + 1) (i2 + 1) (count + 1)
    else check_overlap (i1 + 1) 0 0
  in
  let rec iterate i1 =
    if i1 >= len1 then false
    else
      let rec iterate_inner i2 =
        if i2 >= len2 then iterate (i1 + 1)
        else if check_overlap i1 i2 0 then true
        else iterate_inner (i2 + 1)
      in
      iterate_inner 0
  in
  iterate 0

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