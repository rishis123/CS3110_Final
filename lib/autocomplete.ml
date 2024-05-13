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

(** Function that returns true if [str1] and [str2] have at least 3 characters
    in common. Note that this is not case-sensitive, and doesn't include
    non-alphanumerics in names.*)
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

(** Returns Type.encryptable list representing every value in the saved
    passwords/login file with a name sharing at least 3 characters in common
    with [seek_word]*)
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

let common_passwords =
  [
    "123456";
    "password";
    "12345678";
    "qwerty";
    "123456789";
    "12345";
    "1234";
    "111111";
    "1234567";
    "dragon";
    "123123";
    "baseball";
    "abc123";
    "football";
    "monkey";
    "letmein";
    "696969";
    "shadow";
    "master";
    "666666";
    "qwertyuiop";
    "123321";
    "mustang";
    "1234567890";
    "michael";
    "654321";
    "pussy";
    "superman";
    "1qaz2wsx";
    "7777777";
    "fuckyou";
    "121212";
    "000000";
    "qazwsx";
    "123qwe";
    "killer";
    "trustno1";
    "jordan";
    "jennifer";
    "zxcvbnm";
    "asdfgh";
    "hunter";
    "buster";
    "soccer";
    "harley";
    "batman";
    "andrew";
    "tigger";
    "sunshine";
    "iloveyou";
    "fuckme";
    "2000";
    "charlie";
    "robert";
    "thomas";
    "hockey";
    "ranger";
    "daniel";
    "starwars";
    "klaster";
    "112233";
    "george";
    "asshole";
    "computer";
    "michelle";
    "jessica";
    "pepper";
    "1111";
    "zxcvbn";
    "555555";
    "11111111";
    "131313";
    "freedom";
    "777777";
    "pass";
    "fuck";
    "maggie";
    "159753";
    "aaaaaa";
    "ginger";
    "princess";
    "joshua";
    "cheese";
    "amanda";
    "summer";
    "love";
    "ashley";
    "6969";
    "nicole";
    "chelsea";
    "biteme";
    "matthew";
    "access";
    "yankees";
    "987654321";
    "dallas";
    "austin";
    "thunder";
    "taylor";
    "matrix";
  ]

(* returns true if any overlap exists between the passed in word and the common
   passwords*)
let check_strength password_entry =
  List.exists (compare_words password_entry) common_passwords

(* Checks every saved password or login, checking which are secure and which
   aren't *)
let check_vulnerabilities () =
  let pwd_list = Persistence.read_all_encryptable () in

  let get_only_passwords (pwd : Types.encryptable) =
    match pwd with
    | Types.Login l -> l.password
    | Types.Password p -> p.password
  in
  let get_only_names (pwd : Types.encryptable) : string =
    match pwd with
    | Types.Login l -> l.name
    | Types.Password p -> Types.string_of_master_password_hash p.name
  in
  let string_pwd_list = List.map get_only_passwords pwd_list in
  let len = List.length string_pwd_list in
  let vulnerable = ref [] in

  (* we just want to modify this one ref rather than return a new list for each
     iteration of the loop*)
  for i = 0 to len - 1 do
    let password_entry = List.nth string_pwd_list i in
    if check_strength password_entry then
      (* if the password is vulnerable, add the corresponding name to the
         vulnerable list*)
      vulnerable := get_only_names (List.nth pwd_list i) :: !vulnerable
  done;
  !vulnerable
