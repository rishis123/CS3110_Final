open OUnit2

let test_suites = [ Test_PasswordImport.password_import_suite; Test_Util.util_suite ]
let () = test_suites |> List.iter run_test_tt_main
