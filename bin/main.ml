open FinalProject

let login_procedure pwd =
  if FinalProject.MasterPassword.check_master pwd then
    let () = FinalProject.Encrypt.set_key pwd in
    true
  else false

let quit_procedure () =
  print_endline "Exited the program";
  exit 0

(** [set_file_perms] sets the data files to read and write allowed only for the
    owner. *)
let set_file_perms () =
  Unix.chmod "data/masterpwd" 0o600;
  Unix.chmod "data/pwd" 0o600

let help_msg =
  "Here are the available commands:\n\
   login: Log into the password manager. Must be logged in to use other \
   commands.\n\
   add: Add a new password.\n\
   list: List saved passwords.\n\
   findsing: Autocompletes given name and lists relevant password or login \
   information \n\
   gen_password: Generates password with choice\n\
  \   setpwd: Change the master password.\n\
   import: Import passwords from an existing file.\n\
   export: Export passwords form an existing file. Warning: Exporting \
   passwords will export decrypted data to a file.\n\
   quit: Quit the program."

let get_hidden_input () =
  let settings = Unix.tcgetattr Unix.stdin in
  settings.c_echo <- false;
  Unix.tcsetattr Unix.stdin Unix.TCSANOW settings;
  let input = read_line () in
  settings.c_echo <- true;
  Unix.tcsetattr Unix.stdin Unix.TCSANOW settings;
  input

let rec generate_password_with_special index acc =
  if index <= 0 then acc
  else
    let range_choice = Random.int 4 in
    let char_val =
      match range_choice with
      | 0 -> Char.chr (97 + Random.int 26) (* lowercase letters (a-z) *)
      | 1 -> Char.chr (65 + Random.int 26) (* uppercase letters (A-Z) *)
      | 2 -> Char.chr (48 + Random.int 10) (* digits (0-9) *)
      | _ -> Char.chr (Array.get [| 35; 36; 38 |] (Random.int 3))
      (* symbols (#, $, &) *)
    in
    generate_password_with_special (index - 1) (char_val :: acc)

let rec generate_password_without_special index acc =
  if index <= 0 then acc
  else
    let range_choice = Random.int 3 in
    let char_val =
      match range_choice with
      | 0 -> Char.chr (97 + Random.int 26) (* lowercase letters (a-z) *)
      | 1 -> Char.chr (65 + Random.int 26) (* uppercase letters (A-Z) *)
      | _ -> Char.chr (48 + Random.int 10)
      (* digits (0-9) *)
    in
    generate_password_without_special (index - 1) (char_val :: acc)

let gen_password_val () =
  print_endline "Choose password length:";
  let length_choice = int_of_string (read_line ()) in

  print_endline "Allow special characters?";
  print_endline "1. Yes";
  print_endline "2. No";
  let special_choice = int_of_string (read_line ()) in

  (* printing string representation of the returned char list*)
  match (length_choice, special_choice) with
  | len, 1 ->
      print_endline
        (String.concat ""
           (List.map Char.escaped (generate_password_with_special len [])))
  | len, 2 ->
      print_endline
        (String.concat ""
           (List.map Char.escaped (generate_password_without_special len [])))
  | _ -> print_endline "Invalid response."

let rec logged_in_loop () =
  print_endline "Type a command:";
  let input = read_line () in
  match input with
  | "quit" -> quit_procedure ()
  | "help" ->
      print_endline help_msg;
      logged_in_loop ()
  | "list" ->
      let pwd_list = FinalProject.Persistence.read_all_encryptable () in
      List.iter
        (fun x ->
          print_endline (FinalProject.Serialization.encryptable_to_string x))
        pwd_list;
      logged_in_loop ()
  | "findsing" ->
      let desired = read_line () in
      let autocomplete : Types.encryptable list =
        FinalProject.Persistence.autocomplete desired
      in
      List.iter
        (fun x ->
          print_endline (FinalProject.Serialization.encryptable_to_string x))
        autocomplete;
      logged_in_loop ()
  | "gen_password" ->
      gen_password_val ();
      logged_in_loop ()
  | "add" ->
      print_endline "What would you like to name this password?";
      let name = read_line () in
      print_endline "Enter the password:";
      let password = get_hidden_input () in
      let encryptable = FinalProject.Types.Password { name; password } in
      FinalProject.Persistence.write_encryptable encryptable;
      logged_in_loop ()
  | "setpwd" ->
      print_endline "Type a new password: ";
      let newpwd = get_hidden_input () in
      (* Salt & Hash -> Convert ot MasterPasswordHash type*)
      let master_pwd = FinalProject.Encrypt.salt_hash newpwd in

      let () = FinalProject.Persistence.write_unencryptable master_pwd in
      print_endline ("The password input was :" ^ newpwd)
  | "export" ->
      print_endline "Type the path to which you would like to export: ";
      let path = read_line () in
      let secrets = Persistence.read_all_encryptable () in
      PasswordImport.export secrets path;
      Printf.printf "Passwords successfully exported to %s\n%!" path;
      logged_in_loop ()
  | "import" ->
      print_endline "Type the path to the passwords you would like to import: ";
      let path = read_line () in
      let new_secrets = PasswordImport.import path in
      new_secrets |> List.iter Persistence.write_encryptable;
      Printf.printf "Passwords successfully imported from %s\n%!" path;
      logged_in_loop ()
  | _ ->
      print_endline "That is not a valid command.";
      logged_in_loop ()

let rec main_loop () =
  set_file_perms ();
  print_endline "Type a command:";
  let input = read_line () in
  match input with
  | "quit" -> quit_procedure ()
  | "help" ->
      print_endline help_msg;
      main_loop ()
  | "login" -> begin
      print_endline "Type your master password:";
      let pwd = get_hidden_input () in
      if login_procedure pwd then begin
        print_endline "Logged in!";
        logged_in_loop ()
      end
      else
        let () = print_endline "The password does not match" in
        main_loop ()
    end
  | x -> begin
      if
        x = "add"
        || x = "list"
        || x = "findsing"
        || x = "gen_password"
        || x = "setpwd"
        || x = "import"
        || x = "export"
      then
        let () =
          print_endline
            "Must log in before using these commands -- try 'login' to log in \
             or 'help'"
        in
        main_loop ()
      else print_endline "That is not a valid command.";
      main_loop ()
    end

let _ = main_loop ()
