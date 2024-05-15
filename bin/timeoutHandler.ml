type 'a t = {
  timeout_s : float;
  on_timeout : unit -> 'a;
}
(** The type of a timeout handler. AF: the represented timeout occurs after
    timeout_s seconds, and after the timeout, on_timeout is called. RI:
    timeout_s > 0*)

let make timeout_s on_timeout =
  assert (timeout_s > 0.);
  { timeout_s; on_timeout }

let timeout_s th = th.timeout_s
let on_timeout th = th.on_timeout
