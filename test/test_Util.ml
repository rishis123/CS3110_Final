open FinalProject.Util
open OUnit2

let deterministic_tests =
  [
    ( "non_empty_or_none [empty string] = None" >:: fun _ ->
      non_empty_or_none "" |> assert_equal None );
    ( "non_empty_or_none s = Some s if s blank" >:: fun _ ->
      non_empty_or_none "  " |> assert_equal @@ Some "  " );
    ( "banana contains ana" >:: fun _ ->
      substring "banana" "ana" |> assert_equal true );
    ( "banana does not contain nanan" >:: fun _ ->
      substring "banana" "nanan" |> assert_equal false );
    ( "keysmash substring" >:: fun _ ->
      substring "]q-4tqopyhio[erphun4589qrewknqe" "4589qrewk"
      |> assert_equal true );
    ( "keysmash non-substring" >:: fun _ ->
      substring "2tu92-5 [o35yu-294igj2]itj[5h33" "ylk54jh3r]"
      |> assert_equal false );
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
    Test.make ~name:"A string is a substring of itself." ~count:200 string
      (fun str -> substring str str = true);
    Test.make ~name:"The empty string is a substring of any string." ~count:200
      string (fun str -> substring str "" = true);
    Test.make
      ~name:
        "If s2 is strictly longer than s1, then s2 cannot be a substring of s1."
      ~count:200
      (pair small_string (string_of_size (Gen.int_range 11 10000)))
      (fun (s1, s2) -> substring s1 s2 = false);
    Test.make ~name:"If s2 is a prefix of s1, then s2 is a substring of s1."
      ~count:200 (pair string string) (fun (s1, s2) ->
        substring (s2 ^ s1) s2 = true);
    Test.make ~name:"If s2 is a suffix of s1, then s2 is a substring of s1."
      ~count:200 (pair string string) (fun (s1, s2) ->
        substring (s1 ^ s2) s2 = true);
  ]

let util_suite =
  "util test suite"
  >::: deterministic_tests @ QCheck_runner.to_ounit2_test_list random_tests
