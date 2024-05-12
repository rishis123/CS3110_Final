open OUnit2
open FinalProject.EditDistance

let deterministic_tests =
  [
    ( "edit_distance hi hello = 4" >:: fun _ ->
      let edit_distance =
        min_edit_distance 1. (fun x y -> if x = y then 0. else 1.) "hi" "hello"
      in
      assert_equal 4. edit_distance );
    ( "edit_distance hello hepllo = 1" >:: fun _ ->
      let edit_distance =
        min_edit_distance 1.
          (fun x y -> if x = y then 0. else 1.)
          "hello" "hepllo"
      in
      assert_equal 1. edit_distance );
    ( "edit_distance a λ = 1" >:: fun _ ->
      let edit_distance =
        min_edit_distance 1. (fun x y -> if x = y then 0. else 1.) "a" ""
      in
      assert_equal 1. edit_distance );
  ]

let randomized_tests =
  let open QCheck in
  [
    Test.make ~name:"edit_distance x x = 0. if match_cost x x = 0. for random x"
      ~count:200 (tup3 small_string float float)
      (fun (str, gap_penalty, mismatch_cost) ->
        let precondition = gap_penalty >= 0. && mismatch_cost >= 0. in
        precondition
        ==>
        let edit_distance =
          min_edit_distance gap_penalty
            (fun x y -> if x = y then 0. else mismatch_cost)
            str str
        in
        edit_distance = 0.);
    Test.make
      ~name:
        "edit_distance _ _ = 0. if len x = len y and match_cost _ _ = 0. for \
         random x" ~count:200 (tup3 small_string small_string float)
      (fun (str1, str2, gap_penalty) ->
        let precondition =
          gap_penalty >= 0. && String.length str1 = String.length str2
        in
        precondition
        ==>
        let edit_distance =
          min_edit_distance gap_penalty (Fun.const @@ Fun.const 0.) str1 str2
        in
        edit_distance = 0.);
    Test.make
      ~name:
        "edit_distance x y = [# of mismatched indices] * mismatch_cost if \
         match_cost x x = 0 and len x = len y and gap_cost = infinity for \
         random x" ~count:200 (tup3 small_string small_string float)
      (fun (str1, str2, mismatch_cost) ->
        let precondition = String.length str1 = String.length str2 in
        assume precondition;
        let expected_cost =
          FinalProject.Util.zip (String.to_seq str1) (String.to_seq str2)
          |> Seq.filter (fun (c1, c2) -> c1 <> c2)
          |> Seq.length
          |> float_of_int
          |> ( *. ) mismatch_cost
        in
        let edit_distance =
          min_edit_distance Float.infinity
            (fun x y -> if x = y then 0. else mismatch_cost)
            str1 str2
        in
        abs_float (edit_distance -. expected_cost) < 0.001);
    Test.make ~name:"edit_distance x λ = (len x) * gap_cost" ~count:200
      (tup3 small_string float float) (fun (str, match_cost, gap_cost) ->
        let edit_distance =
          min_edit_distance gap_cost (Fun.const @@ Fun.const match_cost) str ""
        in
        edit_distance = float_of_int (String.length str) *. gap_cost);
    Test.make ~name:"edit_distance λ x = (len x) * gap_cost" ~count:200
      (tup3 small_string float float) (fun (str, match_cost, gap_cost) ->
        let edit_distance =
          min_edit_distance gap_cost (Fun.const @@ Fun.const match_cost) "" str
        in
        edit_distance = float_of_int (String.length str) *. gap_cost);
    Test.make ~name:"edit_distance x y = edit_distance y x" ~count:200
      (tup4 small_string small_string float float)
      (fun (str1, str2, match_cost, gap_cost) ->
        let edit_distance1 =
          min_edit_distance gap_cost
            (Fun.const @@ Fun.const match_cost)
            str1 str2
        in
        let edit_distance2 =
          min_edit_distance gap_cost
            (Fun.const @@ Fun.const match_cost)
            str2 str1
        in
        edit_distance1 = edit_distance2);
  ]

let edit_distance_suite =
  "edit distance test suite"
  >::: deterministic_tests @ QCheck_runner.to_ounit2_test_list randomized_tests
