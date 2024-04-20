open OUnit2
open! FinalProject

let assert_files_equal path1 path2 =
  let open Core in
  let contents1 = In_channel.read_all path1 in
  let contents2 = In_channel.read_all path2 in
  assert_equal contents1 contents2

let with_setup f ctxt =
  TestUtil.delete_recursive "test";
  (try Sys.mkdir "test" 0o700
   with Sys_error message ->
     if not (message |> String.ends_with ~suffix:"File exists") then
       raise (Sys_error message));
  f ctxt

let with_import_files f ctxt =
  let open Core in
  TestUtil.contents_recursive "data" |> List.iter ~f:(f ctxt)

let dummy_encryptable_list =
  let open Types in
  [
    Login { name = "login1"; username = "user1"; password = "pwd1"; url = None };
    Password { name = "password1"; password = "password1" };
    Login
      (* Intentional duplicate of above *)
      { name = "login1"; username = "user1"; password = "pwd1"; url = None };
    Login
      {
        name = "login2";
        username = "user2@example.com";
        password =
          "mysupersecurepassword1234#$%^&*()!#^¡™£¢∞§¶•ªºß√∫˜µ≤ˆ¥†®´∂√∫˜µπ¬…†®´∑œå≈†";
        url = None;
      };
    Login
      {
        name = "Login #3";
        username = "u s e r ¢∞§¶•ªº \\ asdf :-)";
        password = "cccccbhugivihtcefjgletbdrcuuinbbihblfhnvkdnt";
        url = Some "domain.example.com";
      };
    Password { name = "My Password!!"; password = "super duper secregt!!!" };
    Password { name = "my password!!"; password = "my password!!" };
  ]

let tests =
  [
    "export |> import is identity"
    >:: with_setup (fun _ ->
            let export_path = "test/export.csv" in
            PasswordImport.export dummy_encryptable_list export_path;
            let reimported = PasswordImport.import export_path in
            assert_equal dummy_encryptable_list reimported);
    "import |> export is identity"
    >:: with_setup
        @@ with_import_files (fun _ import_path ->
               let imported = PasswordImport.import import_path in
               let export_path = "test/Chrome Passwords.csv" in
               PasswordImport.export imported export_path;
               assert_files_equal import_path export_path);
  ]

let password_import_suite = "password import test suite" >::: tests
