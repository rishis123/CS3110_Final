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
   setpwd: Change the master password.\n\
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
  | "add" ->
      print_endline "Type a new password:";
      let pwd = get_hidden_input () in
      let encryptable =
        FinalProject.Types.Password { name = ""; password = pwd }
      in
      FinalProject.Persistence.write_encryptable encryptable;
      logged_in_loop ()
  | "setpwd" ->
      print_endline "Type a new password: ";
      let newpwd = get_hidden_input () in
      (* Salt & Hash -> Convert ot MasterPasswordHash type*)
      let master_pwd = FinalProject.Encrypt.salt_hash newpwd in

      let () = FinalProject.Persistence.write_unencryptable master_pwd in
      print_endline ("The password input was :" ^ newpwd);
  | "export" ->
      print_endline "Type the path to which you would like to export: ";
      let path = read_line () in
      let secrets = Persistence.read_all_encryptable () in
      PasswordImport.export secrets path;
      Printf.printf "Passwords successfully exported to %s\n%!" path
  | "import" ->
      print_endline "Type the path to the passwords you would like to import: ";
      let path = read_line () in
      let new_secrets = PasswordImport.import path in
      new_secrets |> List.iter Persistence.write_encryptable;
      Printf.printf "Passwords successfully imported from %s\n%!" path
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
  | _ ->
      print_endline "That is not a valid command.";
      main_loop ()

let _ = main_loop ()
