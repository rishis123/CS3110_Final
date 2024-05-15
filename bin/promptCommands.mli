val prompt_commands :
  ?synchronous_commands_to_actions:(string * (unit -> unit)) list ->
  ?async_commands_to_actions:(string * (unit -> unit Lwt.t)) list ->
  ?default:(string -> unit) ->
  ?timeout_handler:'a TimeoutHandler.t ->
  prompt_message:string ->
  unit ->
  'a Lwt.t
(** [prompt_commands ?synchronous_commands_to_actions ?async_commands_to_actions ?default ~timeout_handler ~prompt_message]
    prompts the user with message, awaiting the commands in a loop, running the
    specified action whenever the user enters the given command. Commands that
    run synchronously should be passed as [synchronous_commands_to_actions];
    commands that run asynchronously should be passed as
    [async_commands_to_actions] If the user enters an unrecognized command,
    default () is run. If [timeout_handler] is provided, its timeout is used. *)
