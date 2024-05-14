let prompt_command_no_timeout
    (commands_to_actions : (string * (unit -> unit Lwt.t)) list)
    ?(default = fun _ -> print_endline "I don't recognize that command.")
    ~prompt_message () =
  let open Lwt in
  print_endline prompt_message;
  Printf.printf "> %!";
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
    | None -> fun () -> return (default input)
  in
  let%lwt () = action () in
  print_endline "";
  return_unit

let rec prompt_commands_no_timeout commands_to_actions ?default ~prompt_message
    () =
  let%lwt () =
    prompt_command_no_timeout commands_to_actions ?default ~prompt_message ()
  in
  prompt_commands_no_timeout commands_to_actions ?default ~prompt_message ()

module TimeoutHandler = struct
  type 'a t = {
    timeout_s : float;
    on_timeout : unit -> 'a;
  }

  let make timeout_s on_timeout = { timeout_s; on_timeout }
  let timeout_s th = th.timeout_s
  let on_timeout th = th.on_timeout
end

open TimeoutHandler

let rec prompt_commands_with_timeout commands_to_actions ?default
    ~timeout_handler ~prompt_message () =
  let open Lwt in
  let prompt_promise =
    let%lwt action_promise =
      Lwt_preemptive.detach
        (prompt_command_no_timeout commands_to_actions ?default ~prompt_message)
        ()
    in
    let%lwt () = action_promise in
    return_true
  in
  let timeout_promise =
    let%lwt () = Lwt_unix.sleep (timeout_s timeout_handler) in
    return_false
  in
  let%lwt success = pick [ prompt_promise; timeout_promise ] in
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
