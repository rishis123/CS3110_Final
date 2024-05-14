open OUnit2

let parallel_test_suites =
  [
    Test_GenPassword.gen_password_suite;
    Test_Autocomplete.autocomplete_suite;
    Test_Util.util_suite;
    Test_Memoize.memoize_suite;
    Test_EditDistance.edit_distance_suite;
    Test_Serialization.serialization_suite;
    Test_StrengthCheck.strength_check_test_suite;
    Test_Trie.trie_test_suite;
  ]

let sequential_test_suites =
  [
    Test_Encrypt.encrypt_test_suite;
    Test_PasswordImport.password_import_suite;
    Test_Persistence.persistence_suite;
  ]

let () =
  parallel_test_suites |> List.iter run_test_tt_main;
  TestUtil.conf_use_sequential_runner ();
  sequential_test_suites |> List.iter run_test_tt_main
