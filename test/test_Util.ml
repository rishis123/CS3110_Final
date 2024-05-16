open FinalProject.Util
open OUnit2

let deterministic_tests =
  [
    ( "non_empty_or_none [empty string] = None" >:: fun _ ->
      non_empty_or_none "" |> assert_equal None );
    ( "non_empty_or_none s = Some s if s blank" >:: fun _ ->
      non_empty_or_none "  " |> assert_equal @@ Some "  " );
    ( "sorted_by_below_threshold works on reasonable input" >:: fun _ ->
      let result =
        sorted_by_below_threshold Fun.id 4 ([ 1; 4; 5; 2; 3; 9 ] |> List.to_seq)
      in
      assert (result |> List.of_seq = [ 1; 2; 3; 4 ]) );
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
    Test.make ~name:"zip [x1..xn] [y1..yn] = [(x1, _)..(xn, _)]" ~count:100
      small_nat (fun size ->
        let list_gen = Gen.list_size (Gen.return size) Gen.int in
        let s1 = Gen.generate1 list_gen |> List.to_seq in
        let s2 = Gen.generate1 list_gen |> List.to_seq in
        let zipped_list = zip s1 s2 in
        let all_equal = ref true in
        Seq.iter2
          (fun s1_x (zip_x, _) -> all_equal := !all_equal && s1_x = zip_x)
          s1 zipped_list;
        !all_equal);
    Test.make ~name:"zip [x1..xn] [y1..yn] = [(_, y1)..(_, yn)]" ~count:100
      small_nat (fun size ->
        let list_gen = Gen.list_size (Gen.return size) Gen.int in
        let s1 = Gen.generate1 list_gen |> List.to_seq in
        let s2 = Gen.generate1 list_gen |> List.to_seq in
        let zipped_list = zip s1 s2 in
        let all_equal = ref true in
        Seq.iter2
          (fun s2_y (_, zip_y) -> all_equal := !all_equal && s2_y = zip_y)
          s2 zipped_list;
        !all_equal);
    Test.make ~name:"fuzzy_equal x x = true for all x" ~count:1000 string
      (fun str -> fuzzy_equal str str);
    Test.make ~name:"fuzzy_equal x y = fuzzy_equal y x for all x, y" ~count:1000
      (tup2 string string) (fun (str1, str2) ->
        fuzzy_equal str1 str2 = fuzzy_equal str2 str1);
    Test.make ~name:"fuzzy_equal ' '^x^' ' x = true for all x" ~count:1000
      string (fun str -> fuzzy_equal (" " ^ str ^ " ") str);
    Test.make ~name:"fuzzy_equal x ' '^x^' '= true for all x" ~count:1000 string
      (fun str -> fuzzy_equal (" " ^ str ^ " ") str);
    Test.make ~name:"fuzzy_equal xy x = false for all nonblank x, y" ~count:1000
      (tup2 string string) (fun (str1, str2) ->
        assume (String.trim str1 <> "" && String.trim str2 <> "");
        fuzzy_equal (str1 ^ str2) str1 = false);
    Test.make ~name:"fuzzy_equal x xy = false for all nonblank x, y" ~count:1000
      (tup2 string string) (fun (str1, str2) ->
        assume (String.trim str1 <> "" && String.trim str2 <> "");
        fuzzy_equal str1 (str1 ^ str2) = false);
    Test.make ~name:"compare_by sel x x = 0 for all x, y, sel" ~count:1000
      (tup2 int (fun1 Observable.int int))
      (fun (x, sel) -> compare_by (Fn.apply sel) x x = 0);
  ]

let util_suite =
  "util test suite"
  >::: deterministic_tests @ QCheck_runner.to_ounit2_test_list random_tests
