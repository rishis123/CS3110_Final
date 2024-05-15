open OUnit2
open FinalProject.MasterPassword
open FinalProject.Types
open TestUtil

let unencryptable_to_string = function
  | MasterPasswordHash h -> Bcrypt.string_of_hash h

let randomized_tests =
  let open QCheck in
  [
    Test.make
      ~name:
        "salted hash does not contain original string as substring (for \
         reasonably long strings)" ~count:200 (string_with_range 10 10000)
      (fun str ->
        BatString.exists
          (str |> string_to_salted_hash |> unencryptable_to_string)
          str
        |> not);
    Test.make
      ~name:
        "salting (and hashing) the same string twice creates different outputs"
      ~count:200 (string_with_range 10 10000) (fun str ->
        str
        |> string_to_salted_hash
        |> unencryptable_to_string
        <> (str |> string_to_salted_hash |> unencryptable_to_string));
    Test.make
      ~name:
        "salting (and hashing) two different passwords creates different \
         outputs"
      ~count:200
      (pair (string_with_range 10 10000) (string_with_range 10 10000))
      (fun (str1, str2) ->
        str1
        |> string_to_salted_hash
        |> unencryptable_to_string
        <> (str2 |> string_to_salted_hash |> unencryptable_to_string));
  ]

let master_password_test_suite =
  "master password test suite"
  >::: QCheck_runner.to_ounit2_test_list randomized_tests
