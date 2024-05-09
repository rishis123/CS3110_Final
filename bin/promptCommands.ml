(** Asynchronous timer for [n] seconds -- helps maintain security.*)
let timer n on_timeout =
  let rec countdown remaining =
    if remaining <= 0 then (
      print_endline "Time's up!";
      (* to notify user why program has quit. *)
      on_timeout ())
    else (
      Unix.sleep 1;
      countdown (remaining - 1))
  in
  let _ = Thread.create countdown n in
  ()

let rec prompt_commands commands_to_actions ~on_timeout =
  print_endline "Type a command. You have 5 minutes:";
  let () = timer 300 on_timeout in
  let input = read_line () in
  let action_opt =
    commands_to_actions
    |> List.find_opt (fun (command, _) ->
           FinalProject.Util.fuzzy_equal command input)
    |> Option.map (fun (_, action) -> action)
  in
  let handle_invalid_input () =
    print_endline "I don't recognize that command."
  in
  let action =
    match action_opt with
    | Some a -> a
    | None -> handle_invalid_input
  in
  action ();
  prompt_commands commands_to_actions ~on_timeout
