open Bogue
module W = Widget
module L = Layout
module T = Trigger
open FinalProject

(* let text_size = 18 *)
let controller = W.empty ~w:0 ~h:0 ()

(** [trasition_home_view layout] changes the view to the one shown when the user
    logs in. *)
let transition_home_view layout =
  let label = W.label ~size:40 "You are signed in!" in
  let add_btn =
    W.button ~border_radius:10
      ~action:(fun _ -> print_endline "add btn pressed")
      "Add a password"
  in
  let list_btn =
    W.button ~border_radius:10
      ~action:(fun _ -> print_endline "list btn pressed")
      "Show passwords"
  in
  let gen_btn =
    W.button ~border_radius:10
      ~action:(fun _ -> print_endline "gen btn pressed")
      "Generate a password"
  in
  let set_pwd_btn =
    W.button ~border_radius:10
      ~action:(fun _ -> print_endline "set btn pressed")
      "Set the master password"
  in
  let import_btn =
    W.button ~border_radius:10
      ~action:(fun _ -> print_endline "import btn pressed")
      "Import passwords"
  in
  let export_btn =
    W.button ~border_radius:10
      ~action:(fun _ -> print_endline "export btn pressed")
      "Export passwords"
  in
  let new_layout =
    L.tower
      [
        L.resident ~w:400 ~h:200 label;
        L.resident ~w:400 add_btn;
        L.resident ~w:400 list_btn;
        L.resident ~w:400 gen_btn;
        L.resident ~w:400 set_pwd_btn;
        L.resident ~w:400 import_btn;
        L.resident ~w:400 export_btn;
      ]
  in
  L.set_rooms layout [ new_layout ];
  Sync.push (fun () -> L.fit_content ~sep:0 layout)

(** [login_view] is the view shown before the user logs in *)
let login_view =
  let input =
    W.text_input ~size:30 ~max_size:200 ~prompt:"Enter your name" ()
  in
  let label = W.label ~size:40 "Hello! Please log in" in
  let login_btn =
    W.button ~border_radius:10
      ~action:(fun _ ->
        let pwd = W.get_text input in
        if MasterPassword.check_master pwd then
          let () = Encrypt.set_key pwd in
          Update.push controller
        else W.set_text label "Password incorrect")
      "Login"
  in
  let layout =
    L.tower
      [
        L.resident ~w:400 input;
        L.resident ~w:400 login_btn;
        L.resident ~w:400 ~h:200 label;
      ]
  in
  let c =
    W.connect_main controller controller
      (fun _ _ _ -> transition_home_view layout)
      [ Trigger.update ]
  in
  Bogue.of_layout ~connections:[ c ] layout

let main () =
  let board = login_view in
  Bogue.run board

let _ =
  main ();
  Draw.quit ()
