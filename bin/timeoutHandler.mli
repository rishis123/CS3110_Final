type 'a t
(** Represents a timeout duration and the function to run on timeout *)

val make : float -> (unit -> 'a) -> 'a t
(** [make timeout_s on_timeout] creates a [TimeoutHandler.t] that represents a
    timeout of [timeout_s] seconds, running [on_timeout] after the timeout.
    [timeout_s] must be greater than zero *)

val timeout_s : 'a t -> float
(** [timeout_s handler] returns the timeout (in seconds) represented by
    [handler] *)

val on_timeout : 'a t -> unit -> 'a
(** [on_timeout handler] returns the function that should be called when
    [handler]'s timeout expires *)
