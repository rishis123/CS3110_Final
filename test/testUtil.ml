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
