let prompt_commands_no_timeout commands_to_actions
    ?(default = fun () -> print_endline "I don't recognize that command.") () =
  print_endline
    "Type a command (you will be logged out after five minutes of inactivity):";
  let input = read_line () in
  let action_opt =
    commands_to_actions
    |> List.find_opt (fun (command, _) ->
           FinalProject.Util.fuzzy_equal command input)
    |> Option.map (fun (_, action) -> action)
  in
  let action =
    match action_opt with
    | Some a -> a
    | None -> default
  in
  action ()

let rec prompt_commands commands_to_actions ?default ~on_timeout () =
  let open Lwt in
  let prompt_promise =
    let%lwt () =
      Lwt_preemptive.detach
        (prompt_commands_no_timeout commands_to_actions ?default)
        ()
    in
    return_true
  in
  let timeout_promise =
    let%lwt () = Lwt_unix.sleep 300. in
    return_false
  in
  let%lwt success = pick [ prompt_promise; timeout_promise ] in
  if success then prompt_commands commands_to_actions ?default ~on_timeout ()
  else begin
    print_endline "Time's up!\nYou have been logged out due to inactivity.";
    return (on_timeout ())
  end
