open FinalProject.Types

let string_with_range lower upper =
  QCheck.string_of_size (QCheck.Gen.int_range lower upper)

let password_arb =
  let open QCheck in
  map
    (fun (name, password) -> { name; password })
    (pair (string_with_range 10 30) (string_with_range 20 50))

let login_arb =
  let open QCheck in
  map
    (fun (name, username, password, url) -> { name; username; password; url })
    (quad (string_with_range 10 30) (string_with_range 10 30)
       (string_with_range 20 50)
       (option (string_with_range 50 100)))

let rec delete_recursive dir_path =
  (* Implementation adapted from https://stackoverflow.com/a/56344603/13160488,
     accessed 2024-04-20 *)
  if Sys.file_exists dir_path then
    match Sys.is_directory dir_path with
    | true ->
        Sys.readdir dir_path
        |> Array.iter (fun name ->
               delete_recursive (Filename.concat dir_path name));
        Unix.rmdir dir_path
    | false -> Sys.remove dir_path

let is_empty dir = Array.length (Sys.readdir dir) = 0
(* Implementation from
   https://gist.github.com/lindig/be55f453026c65e761f4e7012f8ab9b5, accessed
   2024-04-20 *)

let contents_recursive dir ~include_dots =
  (* Implementation adapted from
     https://gist.github.com/lindig/be55f453026c65e761f4e7012f8ab9b5, accessed
     2024-04-20 *)
  let open BatPervasives in
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f
        |> Array.to_list
        |> (if include_dots then Fun.id
            else List.filter (not % String.starts_with ~prefix:"."))
        |> List.map (Filename.concat f)
        |> List.append fs
        |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [ dir ]

exception Timeout

let run_timeout timeout f =
  try
    (* Implementation adapted from
       https://discuss.ocaml.org/t/computation-with-time-constraint/5548/9,
       accessed 2024-04-24 *)
    let _ =
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
    in
    ignore (Unix.alarm timeout);
    try
      f ();
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm Sys.Signal_default;
      true
    with e ->
      ignore (Unix.alarm 0);
      raise e
  with Timeout ->
    Sys.set_signal Sys.sigalrm Sys.Signal_default;
    false

let conf_use_sequential_runner () =
  OUnitCore.run_test_tt_main_conf :=
    fun ?(preset = []) ?argv extra_specs ->
      try
        let run_sequential_arg = [| "--"; "-runner"; "sequential" |] in
        match argv with
        | Some argv ->
            OUnitConf.load
              ~argv:(Array.append argv run_sequential_arg)
              ~preset:(OUnitChooser.preset (OUnitRunner.preset preset))
              extra_specs
        | None ->
            OUnitConf.load ~argv:run_sequential_arg
              ~preset:(OUnitChooser.preset (OUnitRunner.preset preset))
              extra_specs
      with Stdlib.Arg.Bad bad_message ->
        print_endline bad_message;
        raise (Stdlib.Arg.Bad bad_message)

let equals_ignoring_duplicates lst1 lst2 =
  lst1 |> List.for_all (fun x -> List.mem x lst2)
  && lst2 |> List.for_all (fun x -> List.mem x lst2)
