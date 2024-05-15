open QCheck
open OUnit2
open FinalProject.OrderedMultiset

let empty_set =
  let open Gen in
  QCheck.make (return make >|= fun f -> f 2)

let randomized_tests =
  [
    Test.make ~name:"insert x; find_opt (( = ) x) = Some x" ~count:1000
      (tup2 int empty_set) (fun (x, set) ->
        insert x set;
        find_opt (( = ) x) set = Some x);
    Test.make
      ~name:
        "[x1..xn] |> iter (insert); [x1..xn] |> iter (x -> find_opt (( = ) x) \
         = Some x)"
      ~count:100
      (tup2 (list int) empty_set)
      (fun (xs, set) ->
        xs |> List.iter (Fun.flip insert set);
        xs |> List.for_all (fun x -> find_opt (( = ) x) set = Some x));
    Test.make
      ~name:
        "insert [x1..xn]; fold_left f init set = List.fold_left f init [x1..xn]"
      ~count:500
      (tup4 (list int) (fun2 Observable.int Observable.int int) int empty_set)
      (fun (xs, f, init, set) ->
        xs |> List.iter (Fun.flip insert set);
        let set_fold = fold_left (Fn.apply f) init set in
        let list_fold = List.fold_left (Fn.apply f) init xs in
        set_fold = list_fold);
    Test.make ~name:"inserting many does not fail" ~count:100
      (tup2 (list_of_size Gen.big_nat int) empty_set)
      (fun (xs, set) ->
        xs |> List.iter (Fun.flip insert set);
        true (* This test only fails if insert throws *));
  ]

let ordered_multiset_tests =
  "ordered multiset tests"
  >::: QCheck_runner.to_ounit2_test_list randomized_tests
