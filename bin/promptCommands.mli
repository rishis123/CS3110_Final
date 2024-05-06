val prompt_commands :
  (string * (unit -> unit)) list -> on_timeout:(unit -> 'a) -> 'b
(** [prompt_commands commands_to_actions ~on_timeout] prompts the user with the
    following commands in a loop, running the specified action whenever the user
    enters the given command. Five minutes after their last command, if the user
    is inactive, on_timeout () is run. *)
