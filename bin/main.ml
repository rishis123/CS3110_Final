(* @author Rohen Giralt (rmg296), Sophia Song, Rishi Shah (ss3579), Lakshmi
   Meghana Kesanapalli (lk496), Zhijia Ye (zy424) *)

open FinalProject
module Persistence = Persistence.Default

let quit_procedure () =
  print_endline "Exited the program";
  exit 0

let help_msg =
  "Here are the available commands:\n\n\
   add: Add a new login or password.\n\
   delete: Delete a login or password.\n\
   list: List saved passwords.\n\
   findsing: Autocompletes given name and lists relevant password or login \
   information \n\
   search: Searches for all relevant passwords or logins \n\
   check_strength: Checks if your password is at risk or not.\n\
   health_check: Checks saved passwords for health or duplicates \n\
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

let help_procedure () = print_endline help_msg

let list_procedure () =
  let pwd_list = Persistence.read_all_encryptable () in
  match List.length pwd_list with
  | 0 -> print_endline "No saved logins."
  | _ ->
      List.iter
        (fun x -> print_endline (Types.string_of_encryptable x))
        pwd_list

let findsing_procedure () =
  print_endline "Enter what you think the name of your password or login is";
  let desired = read_line () in
  let autocomplete : Types.encryptable list =
    Autocomplete.autocomplete desired
  in
  if List.length autocomplete = 0 then print_endline "No matches"
  else
    List.iter
      (fun x -> print_endline (Types.string_of_encryptable x))
      autocomplete

let search_procedure () =
  print_endline "Enter search query:";
  let desired = read_line () in
  let name_distance_per_len enc =
    let name = Types.name_of_encryptable enc in
    EditDistance.min_edit_distance_unit_cost desired name
    /. float_of_int (String.length name)
  in
  let most_relevant =
    Persistence.read_all_encryptable ()
    |> List.to_seq
    |> Util.sorted_by_below_threshold name_distance_per_len 0.6
    |> List.of_seq
  in
  if List.length most_relevant > 0 then begin
    print_endline "Here are the most relevant matches:";
    List.iter
      (fun x -> print_endline (Types.string_of_encryptable x))
      most_relevant
  end
  else print_endline "No matches found."

let gen_password_procedure () =
  print_endline "Choose password length:";
  let length_choice = int_of_string (read_line ()) in

  print_endline "Allow special characters?";
  print_endline "1. Yes";
  print_endline "2. No";
  let special_choice = int_of_string (read_line ()) in

  (* printing string representation of the returned char list*)
  match (length_choice, special_choice) with
  | len, 1 -> print_endline (GenPassword.generate_password_with_special len)
  | len, 2 -> print_endline (GenPassword.generate_password_without_special len)
  | _ -> print_endline "Invalid response"

let add_procedure () =
  print_endline
    "Usage: add [pwd|login].\n\
     Run add pwd if you would like to\n\
     add a password, and add login if you would like to add a login"

let add_password_procedure () =
  print_endline "What would you like to name this password?";
  let name = read_line () in
  print_endline "Enter the password:";
  let password = get_hidden_input () in
  let encryptable = Types.Password { name; password } in
  Persistence.write_encryptable encryptable

let add_login_procedure () =
  print_endline "What would you like to name this login?";
  let name = read_line () in
  print_endline "Enter the username:";
  let username = read_line () in
  print_endline "Enter the password:";
  let password = get_hidden_input () in
  print_endline "Enter the url, or press enter to skip:";
  let url = get_hidden_input () |> Util.non_empty_or_none in
  let encryptable = Types.Login { name; username; password; url } in
  Persistence.write_encryptable encryptable

let delete_procedure () =
  print_endline
    "What is the name of the login or password you would like to delete?";
  let name = read_line () in
  Persistence.delete_encryptable_by_name name;
  Printf.printf "Successfully deleted the login %s" name

let set_pwd_procedure () =
  print_endline "Type a new password: ";
  let newpwd = get_hidden_input () in
  (* Salt & Hash -> Convert ot MasterPasswordHash type*)
  MasterPassword.string_to_salted_hash newpwd |> Persistence.write_unencryptable;
  print_endline ("The password input was :" ^ newpwd)

module StrengthCheck = StrengthCheck.Make (struct
  let common_passwords_path = "data/10-million-combos-2.txt"
end)

let check_strength_procedure () =
  print_endline "Enter your existing password.";
  let existing = get_hidden_input () in
  if not (StrengthCheck.is_initialized ()) then begin
    print_endline "Loading weak passwords list...";
    print_endline "Please wait a few moments."
  end;
  if%lwt StrengthCheck.is_weak existing then
    Lwt.return
      (print_endline
         "Your password is a security risk -- try one of our randomly \
          generated passwords by calling gen_password!")
  else Lwt.return (print_endline "Your password is fine!")

let health_check_procedure () =
  let open Batteries in
  let open Lwt in
  if not (StrengthCheck.is_initialized ()) then begin
    print_endline "Loading weak passwords list...";
    print_endline "Please wait a few moments."
  end;
  let%lwt weak_encryptables =
    Persistence.read_all_encryptable ()
    |> Lwt_list.filter_p (StrengthCheck.is_weak % Types.password_of_encryptable)
  in
  let output_printer enc =
    Printf.printf "Your password for %s (%s) is not secure\n"
      (Types.name_of_encryptable enc)
      (Types.password_of_encryptable enc)
  in
  match List.length weak_encryptables with
  | 0 ->
      print_endline "No weak logins or passwords found!";
      return_unit
  | _ -> begin
      List.iter output_printer weak_encryptables;
      print_endline
        "Try randomly generating a password with the gen_password command!";
      return_unit
    end

let export_procedure () =
  print_endline "Type the path to which you would like to export: ";
  let path = read_line () in
  let secrets = Persistence.read_all_encryptable () in
  PasswordImport.export secrets path;
  Printf.printf "Passwords successfully exported to %s\n%!" path

let import_procedure () =
  print_endline "Type the path to the passwords you would like to import: ";
  let path = read_line () in
  let new_secrets = PasswordImport.import path in
  new_secrets |> List.iter Persistence.write_encryptable;
  Printf.printf "Passwords successfully imported from %s\n%!" path

let synchronous_logged_in_actions =
  [
    ("quit", quit_procedure);
    ("help", help_procedure);
    ("list", list_procedure);
    ("findsing", findsing_procedure);
    ("search", search_procedure);
    ("gen_password", gen_password_procedure);
    ("add", add_procedure);
    ("add pwd", add_password_procedure);
    ("add login", add_login_procedure);
    ("delete", delete_procedure);
    ("setpwd", set_pwd_procedure);
    ("export", export_procedure);
    ("import", import_procedure);
  ]

let async_logged_in_actions =
  [
    ("check_strength", check_strength_procedure);
    ("health_check", health_check_procedure);
  ]

let unrecognized_input_procedure input =
  print_endline "That is not a valid command.";
  let input_distance = EditDistance.min_edit_distance_unit_cost input in
  let async_commands = async_logged_in_actions |> List.to_seq |> Seq.map fst in
  let sync_commands =
    synchronous_logged_in_actions |> List.to_seq |> Seq.map fst
  in
  let commands = Seq.append sync_commands async_commands in
  let closest_commands =
    Util.sorted_by_below_threshold input_distance 3. commands |> List.of_seq
  in
  match List.length closest_commands with
  | 0 -> ()
  | 1 -> Printf.printf "Maybe you meant %s?\n" (List.hd closest_commands)
  | _ ->
      Printf.printf "Maybe you meant one of these: %s?\n"
        (String.concat ", " closest_commands)

let logged_in_loop =
  let open PromptCommands in
  prompt_commands ~synchronous_commands_to_actions:synchronous_logged_in_actions
    ~async_commands_to_actions:async_logged_in_actions
    ~default:unrecognized_input_procedure
    ~timeout_handler:(TimeoutHandler.make 300. quit_procedure)
    ~prompt_message:
      "Type a command (you will be logged out after five minutes of \
       inactivity):"

let try_login pwd =
  if FinalProject.MasterPassword.check_master_pwd pwd then
    let () = Encrypt.set_key pwd in
    true
  else false

let login_procedure () =
  print_endline "Type your master password:";
  let pwd = get_hidden_input () in
  if try_login pwd then begin
    print_endline "Logged in!\n";
    Lwt_main.run (logged_in_loop ())
  end
  else print_endline "The password does not match."

let main_incorrect_input_procedure input =
  if
    input = "add"
    || input = "list"
    || input = "findsing"
    || input = "gen_password"
    || input = "setpwd"
    || input = "import"
    || input = "export"
    || input = "check_strength"
    || input = "health_check"
    || input = "search"
  then
    print_endline
      "Must log in before using these commands -- try 'login' to log in or \
       'help'"
  else print_endline "That is not a valid command."

let main_loop () =
  Persistence.set_file_perms ();
  StrengthCheck.init_async ();
  let open PromptCommands in
  prompt_commands
    ~synchronous_commands_to_actions:
      [
        ("quit", quit_procedure);
        ( "help",
          fun () ->
            print_endline "Must login before accessing other functionalities" );
        ("login", login_procedure);
      ]
    ~default:main_incorrect_input_procedure
    ~prompt_message:"Type a command -- quit, help, or login:" ()

let _ = main_loop ()
