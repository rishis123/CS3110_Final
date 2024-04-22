let test_dir_idx_atomic = Atomic.make 1

let with_test_dir f ctxt =
  let test_dir_idx = Atomic.fetch_and_add test_dir_idx_atomic 1 in
  let test_dir_name = "test" ^ string_of_int test_dir_idx in
  TestUtil.delete_recursive test_dir_name;
  begin
    (* Need to delimit this try block so that the ; operator works *)
    try Sys.mkdir test_dir_name 0o700
    with Sys_error message ->
      if not (message |> String.ends_with ~suffix:"File exists") then
        raise (Sys_error message)
  end;
  f ctxt test_dir_name
