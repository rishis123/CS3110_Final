open OUnit2
open FinalProject

module StrengthCheck = StrengthCheck.Make (struct
  let common_passwords_path = "data/test-common-passwords.txt"
end)

let pick_10_in_common () =
  let lst_of_passwords =
    BatFile.lines_of "data/test-common-passwords.txt" |> BatList.of_enum
  in
  (* list of 100 most commmon passwords*)
  let len = List.length lst_of_passwords in
  (* needed in picking a random element each time *)
  let output_lst = ref [] in

  for _ = 0 to 9 do
    let random_index = Random.int len in
    let password = List.nth lst_of_passwords random_index in
    output_lst := password :: !output_lst
  done;
  !output_lst

let filter_weak (pwd_list : Types.encryptable list) =
  let open Lwt in
  let string_pwd_list = List.map Types.password_of_encryptable pwd_list in
  let%lwt weak_pwd_list =
    string_pwd_list |> Lwt_list.filter_p StrengthCheck.is_weak
  in
  weak_pwd_list
  |> Lwt_list.mapi_p (fun i _ -> return (Types.name_of_encryptable (List.nth pwd_list i)))

let tests =
  [
    "Test is_weak"
    >:: OUnitLwt.lwt_wrapper (fun _ ->
            let ten_passwords = pick_10_in_common () in
            let%lwt all_valid =
              ten_passwords |> Lwt_list.for_all_p StrengthCheck.is_weak
            in
            assert_bool "Check strength finds no overlap between overlappers"
              all_valid;
            Lwt.return_unit)
  ]

let strength_check_test_suite = "strength check test suite" >::: tests
