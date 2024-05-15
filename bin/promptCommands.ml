let action_matching_input_opt commands_to_actions input =
  commands_to_actions
  |> List.find_opt (fun (command, _) ->
         FinalProject.Util.fuzzy_equal command input)
  |> Option.map (fun (_, action) -> action)

let action_or_default ~(default : string -> unit) action_opt input =
  match action_opt with
  | Some a -> a
  | None -> fun () -> Lwt.return (default input)

let prompt_command_no_timeout commands_to_actions
    ?(default = fun _ -> print_endline "I don't recognize that command.")
    ~prompt_message () =
  let open Lwt in
  print_endline prompt_message;
  Printf.printf "> %!";
  let input = read_line () in
  let action_opt = action_matching_input_opt commands_to_actions input in
  let action = action_or_default ~default action_opt input in
  let%lwt () = action () in
  print_endline "";
  return_unit

let rec prompt_commands_no_timeout commands_to_actions ?default ~prompt_message
    () =
  let%lwt () =
    prompt_command_no_timeout commands_to_actions ?default ~prompt_message ()
  in
  prompt_commands_no_timeout commands_to_actions ?default ~prompt_message ()

open TimeoutHandler

let timeout_promise timeout_handler =
  let%lwt () = Lwt_unix.sleep (timeout_s timeout_handler) in
  Lwt.return_false

let prompt_and_run_action commands_to_actions ?default ~prompt_message () =
  let%lwt action_promise =
    Lwt_preemptive.detach
      (prompt_command_no_timeout commands_to_actions ?default ~prompt_message)
      ()
  in
  let%lwt () = action_promise in
  Lwt.return_true

let rec prompt_commands_with_timeout commands_to_actions ?default
    ~timeout_handler ~prompt_message () =
  let open Lwt in
  let prompt_promise =
    prompt_and_run_action commands_to_actions ?default ~prompt_message ()
  in
  let%lwt success = pick [ prompt_promise; timeout_promise timeout_handler ] in
  if success then
    prompt_commands_with_timeout commands_to_actions ?default ~timeout_handler
      ~prompt_message ()
  else begin
    print_endline "Time's up!\nYou have been logged out due to inactivity.";
    return (on_timeout timeout_handler ())
  end

let prompt_commands ?(synchronous_commands_to_actions = [])
    ?(async_commands_to_actions = []) ?default ?timeout_handler =
  let wrap_action_in_Lwt =
    List.map (fun (name, action) -> (name, fun () -> Lwt.return (action ())))
  in
  let commands_to_actions =
    async_commands_to_actions
    @ wrap_action_in_Lwt synchronous_commands_to_actions
  in
  match timeout_handler with
  | Some timeout_handler ->
      prompt_commands_with_timeout commands_to_actions ?default ~timeout_handler
  | None -> prompt_commands_no_timeout commands_to_actions ?default
