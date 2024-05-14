let common_passwords_opt = ref None

let init_async_return () =
  let pwds_promise =
    Lwt_preemptive.detach Trie.of_file "data/xato-net-10-million-passwords.txt"
  in
  common_passwords_opt := Some pwds_promise;
  pwds_promise

let init_async () = init_async_return () |> ignore

let is_weak password_str =
  let%lwt common_passwords =
    match !common_passwords_opt with
    | Some pass_promise -> pass_promise
    | None -> init_async_return ()
  in
  Lwt.return (Trie.mem password_str common_passwords)
