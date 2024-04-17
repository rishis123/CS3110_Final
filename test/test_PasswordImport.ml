open OUnit2
open FinalProject

let tests =
  [
    ("unit" >:: fun _ -> ());
  ]

let password_import_suite = "password import test suite" >::: tests
