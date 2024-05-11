let prompt_command_no_timeout commands_to_actions
    ?(default = fun _ -> print_endline "I don't recognize that command.")
    ~prompt_message () =
  print_endline prompt_message;
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
    | None -> fun () -> default input
  in
  action ()

let rec prompt_commands_no_timeout commands_to_actions ?default ~prompt_message
    () =
  let%lwt () =
    Lwt.return
      (prompt_command_no_timeout commands_to_actions ?default ~prompt_message ())
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
    let%lwt () =
      Lwt_preemptive.detach
        (prompt_command_no_timeout commands_to_actions ?default ~prompt_message)
        ()
    in
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

let prompt_commands commands_to_actions ?default ?timeout_handler =
  match timeout_handler with
  | Some timeout_handler ->
      prompt_commands_with_timeout commands_to_actions ?default ~timeout_handler
  | None -> prompt_commands_no_timeout commands_to_actions ?default
