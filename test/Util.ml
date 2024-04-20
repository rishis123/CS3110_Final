let rec rmrf path =
  if Sys.file_exists path then
    match Sys.is_directory path with
    | true ->
        Sys.readdir path
        |> Array.iter (fun name -> rmrf (Filename.concat path name));
        Unix.rmdir path
    | false -> Sys.remove path
