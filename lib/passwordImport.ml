open Types

type import_exn =
  | UnsupportedHeaders
  | NonRectangularInput

exception ImportExn of import_exn

let proprietary_export_header =
  [ "type"; "name"; "username"; "password"; "url" ]

module Parser = struct
  type t = string list -> encryptable

  let parse_proprietary_csv : t = function
    | [ "Login"; name; username; password; url ] ->
        let url = Util.non_empty_or_none url in
        Login { name; username; password; url }
    | [ "Password"; name; ""; password; "" ] -> Password { name; password }
    | _ -> raise (ImportExn NonRectangularInput)

  let parse_chrome_csv : t = function
    | [ name; url; username; password; note ] ->
        (* ignored parameter retained for clarity; also, may be used in the
           future *)
        ignore note;
        let url = Util.non_empty_or_none url in
        Login { name; username; password; url }
    | _ -> raise (ImportExn NonRectangularInput)

  let parse_safari_csv : t = function
    | [ name; url; username; password; notes; otpAuth ] ->
        (* ignored parameters retained for clarity; also, may be used in the
           future *)
        ignore notes;
        ignore otpAuth;
        let url = Util.non_empty_or_none url in
        Login { name; username; password; url }
    | _ -> raise (ImportExn NonRectangularInput)

  let of_header : string list -> t = function
    | [ "name"; "url"; "username"; "password"; "note" ] -> parse_chrome_csv
    | [ "Title"; "URL"; "Username"; "Password"; "Notes"; "OTPAuth" ] ->
        parse_safari_csv
    | header when header = [ "type"; "name"; "username"; "password"; "url" ] ->
        parse_proprietary_csv
    | _ -> raise (ImportExn UnsupportedHeaders)
end

let import path =
  let open Csv in
  let csv = In_channel.open_text path |> of_channel ~has_header:true in
  let header = Rows.header csv in
  let parse = Parser.of_header header in
  fold_left ~f:(fun acc row -> parse row :: acc) ~init:[] csv |> List.rev

let row_of_encryptable =
  let assert_len row =
    assert (List.length row = List.length proprietary_export_header);
    row
  in
  function
  | Login l ->
      begin
        match l.url with
        | Some url -> [ "Login"; l.name; l.username; l.password; url ]
        | None -> [ "Login"; l.name; l.username; l.password; "" ]
      end
      |> assert_len
  | Password p -> [ "Password"; p.name; ""; p.password; "" ] |> assert_len

type export_exn = FileExists

exception ExportExn of export_exn

let export secrets path =
  if Sys.file_exists path then raise @@ ExportExn FileExists;
  let csv =
    secrets
    |> List.map (fun secret -> row_of_encryptable secret |> Array.of_list)
    |> Array.of_list
    |> Array.append @@ [| Array.of_list proprietary_export_header |]
    |> Csv.of_array
  in
  Csv.save path csv;
  assert (Sys.file_exists path)
