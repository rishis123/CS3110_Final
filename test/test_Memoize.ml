open OUnit2
open FinalProject.Memoize
module IntMemo = Make (Int)

let rec fib x = if x = 0 || x = 1 then 1 else fib (x - 1) + fib (x - 2)

let fib_memo : int -> int =
  IntMemo.memoize (fun fib_m x ->
      if x = 0 || x = 1 then 1 else fib_m (x - 1) + fib_m (x - 2))

let fib_memo_preset_cap : int -> int =
  IntMemo.memoize (fun fib_m x ->
      if x = 0 || x = 1 then 1 else fib_m (x - 1) + fib_m (x - 2))

let tests =
  [
    ( "memoized fib = fib for all n" >:: fun _ ->
      for i = 0 to 35 do
        (* past ~35, fib i takes too long *)
        assert_equal (fib i) (fib_memo i)
      done );
    ( "memoized with preset capacity fib = fib for all n" >:: fun _ ->
      for i = 0 to 35 do
        (* past ~35, fib i takes too long *)
        assert_equal (fib i) (fib_memo_preset_cap i)
      done );
    ( "memoized fib n = memoized fib (n - 1) + memoized fib (n - 2) for all n"
    >:: fun _ ->
      for i = 2 to 100_000 do
        assert_equal (fib_memo i) (fib_memo (i - 1) + fib_memo (i - 2))
      done );
    ( "memoized with preset capacity fib n = fib_prst (n - 1) + fib_prst (n - \
       2) for all n"
    >:: fun _ ->
      for i = 2 to 100_000 do
        assert_equal (fib_memo_preset_cap i)
          (fib_memo_preset_cap (i - 1) + fib_memo_preset_cap (i - 2))
      done );
    ( "large arguments of memoized fib terminate in reasonable time" >:: fun _ ->
      let fib1000 () = fib_memo 1000 |> ignore in
      let ran_fast = TestUtil.run_timeout 1 fib1000 in
      assert_equal true ran_fast );
  ]

let memoize_suite = "memoization test suite" >::: tests
