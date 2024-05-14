open OUnit2
open QCheck
open FinalProject.Trie

let trie_char_code_gen =
  let alphanum =
    let open Gen in
    [ int_range 65 90; int_range 97 122; int_range 48 57 ]
  in
  let special_chars =
    "!@#$%^&*()?.-_~ "
    |> String.to_seq
    |> Seq.map int_of_char
    |> Seq.map Gen.return
    |> List.of_seq
  in
  alphanum @ special_chars |> Gen.oneof

let trie_char_gen =
  let open Gen in
  trie_char_code_gen >|= char_of_int

let trie_string = string_gen trie_char_gen

let empty_trie =
  let open Gen in
  QCheck.make (return FinalProject.Trie.make >|= fun f -> f ())

let lowercase = List.map String.lowercase_ascii

let randomized_tests =
  [
    Test.make ~name:"insert x; mem x = true for all x" ~count:500
      (tup2 trie_string empty_trie) (fun (x, trie) ->
        assume (x <> "");
        insert x trie;
        mem x trie);
    Test.make ~name:"insert x; insert y; mem x = true for all x, y" ~count:500
      (tup3 trie_string trie_string empty_trie) (fun (x, y, trie) ->
        assume (x <> "" && y <> "");
        insert x trie;
        insert y trie;
        mem x trie);
    Test.make ~name:"insert x; insert y; mem y = true for all x, y" ~count:500
      (tup3 trie_string trie_string empty_trie) (fun (x, y, trie) ->
        assume (x <> "" && y <> "");
        insert x trie;
        insert y trie;
        mem y trie);
    Test.make ~name:"insert x; insert xy; mem x = true for all x, y" ~count:500
      (tup3 trie_string trie_string empty_trie) (fun (x, y, trie) ->
        assume (x <> "" && y <> "");
        insert x trie;
        insert (x ^ y) trie;
        mem x trie);
    Test.make ~name:"insert x; insert xy; mem xy = true for all x, y" ~count:500
      (tup3 trie_string trie_string empty_trie) (fun (x, y, trie) ->
        assume (x <> "" && y <> "");
        insert x trie;
        insert (x ^ y) trie;
        mem (x ^ y) trie);
    Test.make ~name:"insert x; insert y; to_list = [x; y] for all x, y, x<>y"
      ~count:500 (tup3 trie_string trie_string empty_trie) (fun (x, y, trie) ->
        assume (x <> "" && y <> "" && x <> y);
        insert x trie;
        insert y trie;
        let ( = ) = TestUtil.equals_ignoring_duplicates in
        lowercase (to_list trie) = lowercase [ x; y ]);
    Test.make ~name:"of_list [x; y] |> to_list = [x; y] for all x, y, x<>y"
      ~count:500 (tup2 trie_string trie_string) (fun (x, y) ->
        assume (x <> "" && y <> "" && x <> y);
        let trie = of_list [ x; y ] in
        let ( = ) = TestUtil.equals_ignoring_duplicates in
        lowercase (to_list trie) = lowercase [ x; y ]);
  ]

let trie_test_suite =
  "trie test suite" >::: QCheck_runner.to_ounit2_test_list randomized_tests
