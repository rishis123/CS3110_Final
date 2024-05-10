val prompt_commands :
  (string * (unit -> unit)) list ->
  ?default:(unit -> unit) ->
  on_timeout:(unit -> 'a) ->
  unit ->
  'a Lwt.t
(** [prompt_commands commands_to_actions ?default ~on_timeout ()] prompts the
    user with the following commands in a loop, running the specified action
    whenever the user enters the given command. If the user enters an
    unrecognized command, default () is run. Five minutes after their last
    command, if the user is inactive, on_timeout () is run. *)
