open FinalProject.Util
open OUnit2

let deterministic_tests =
  [
    ( "non_empty_or_none [empty string] = None" >:: fun _ ->
      non_empty_or_none "" |> assert_equal None );
    ( "non_empty_or_none s = Some s if s blank" >:: fun _ ->
      non_empty_or_none "  " |> assert_equal @@ Some "  " );
  ]

let random_tests =
  let open QCheck in
  [
    Test.make ~name:"non_empty_or_none s = Some s if len s <> 0 for random s"
      ~count:100_000 string (fun str ->
        String.length str
        > 0
        ==> begin
              match non_empty_or_none str with
              | Some s -> s = str
              | None -> false
            end);
  ]

let util_suite =
  "util test suite"
  >::: deterministic_tests @ QCheck_runner.to_ounit2_test_list random_tests
