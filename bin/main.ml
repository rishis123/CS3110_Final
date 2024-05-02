open FinalProject

let login_procedure pwd =
  if FinalProject.MasterPassword.check_master pwd then
    let () = Encrypt.set_key pwd in
    true
  else false

let quit_procedure () =
  print_endline "Exited the program";
  exit 0

let help_msg =
  "Here are the available commands:\n\n\
   add: Add a new password.\n\
   list: List saved passwords.\n\
   findsing: Autocompletes given name and lists relevant password or login \
   information \n\
   check_strength: Checks if your password is at risk or not.\n\
   gen_password: Generates password with choice\n\
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

(** Asynchronous timer for [n] seconds -- helps maintain security.*)
let timer n =
  let rec countdown remaining =
    if remaining <= 0 then (
      print_endline "Time's up!";
      (* to notify user why program has quit.*)
      quit_procedure ())
    else (
      Unix.sleep 1;
      countdown (remaining - 1))
  in
  let _ = Thread.create countdown n in
  ()

let rec logged_in_loop () =
  print_endline "Type a command. You have 5 minutes:";
  let () = timer 300 in
  let input = read_line () in
  match input with
  | "quit" -> quit_procedure ()
  | "help" ->
      print_endline help_msg;
      logged_in_loop ()
  | "list" ->
      let pwd_list = Persistence.read_all_encryptable () in
      List.iter
        (fun x -> print_endline (Types.string_of_encryptable x))
        pwd_list;
      logged_in_loop ()
  | "findsing" ->
      print_endline "Enter what you think the name of your password or login is";
      let desired = read_line () in
      let autocomplete : Types.encryptable list =
        Autocomplete.autocomplete desired
      in
      List.iter
        (fun x -> print_endline (Types.string_of_encryptable x))
        autocomplete;
      logged_in_loop ()
  | "gen_password" ->
      let () = print_endline (Gen_password.gen_password_val ()) in
      logged_in_loop ()
  | "add" ->
      print_endline
        "Usage: add [pwd|login].\n\
         Run add pwd if you would like to add a password, and add login if you \
         would like to add a login";
      logged_in_loop ()
  | "add pwd" ->
      print_endline "What would you like to name this password?";
      let name = read_line () in
      print_endline "Enter the password:";
      let password = get_hidden_input () in
      let encryptable = Types.Password { name; password } in
      Persistence.write_encryptable encryptable;
      logged_in_loop ()
  | "add login" ->
      print_endline "What would you like to name this login?";
      let name = read_line () in
      print_endline "Enter the username:";
      let username = read_line () in
      print_endline "Enter the password:";
      let password = get_hidden_input () in
      print_endline "Enter the url, or press enter to skip:";
      let url = get_hidden_input () |> Util.non_empty_or_none in
      let encryptable = Types.Login { name; username; password; url } in
      Persistence.write_encryptable encryptable;
      logged_in_loop ()
  | "setpwd" ->
      print_endline "Type a new password: ";
      let newpwd = get_hidden_input () in
      (* Salt & Hash -> Convert ot MasterPasswordHash type*)
      let master_pwd = Encrypt.salt_hash newpwd in

      let () = Persistence.write_unencryptable master_pwd in
      print_endline ("The password input was :" ^ newpwd)
  | "check_strength" ->
      print_endline "Enter your existing password.";
      let existing = read_line () in
      if Autocomplete.check_strength existing then
        print_endline
          "Your password is a security risk -- try one of our randomly \
           generated passwords by calling gen_password"
      else print_endline "Your password is fine!";
      logged_in_loop ()
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
  Persistence.set_file_perms ();
  print_endline "Type a command -- quit, help, or login:";
  let input = read_line () in
  match input with
  | "quit" -> quit_procedure ()
  | "help" ->
      print_endline "Must login before accessing other functionalities";
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
        || x = "check_strength"
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
