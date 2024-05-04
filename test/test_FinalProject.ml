open OUnit2

let test_suites =
  [
    Test_GenPassword.gen_password_suite;
    Test_PasswordImport.password_import_suite;
    Test_Util.util_suite;
    Test_Memoize.memoize_suite;
    Test_EditDistance.edit_distance_suite;
  ]

let () = test_suites |> List.iter run_test_tt_main
