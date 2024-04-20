open Types

type import_exn =
  | UnsupportedHeaders
  | NonRectangularInput

exception ImportExn of import_exn

module Parser = struct
  type t = string list -> encryptable

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
    | _ -> raise (ImportExn UnsupportedHeaders)
end

let import path =
  let open Csv in
  let csv = In_channel.open_text path |> of_channel ~has_header:true in
  let header = Rows.header csv in
  let parse = Parser.of_header header in
  fold_left ~f:(fun acc row -> parse row :: acc) ~init:[] csv |> List.rev

let export _ _ = failwith "Not implemented"
