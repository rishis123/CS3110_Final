open Bogue
module W = Widget
module L = Layout
module T = Trigger
open FinalProject

let btn_font_size = 18
let btn_border_radius = 10
let window_width = 400
let controller = W.empty ~w:0 ~h:0 ()

let create_btn s f =
  W.button
    ~label:(Label.create ~size:btn_font_size s)
    ~border_radius:btn_border_radius
    ~action:(fun _ -> f ())
    ""

(** [trasition_home_view layout] changes the view to the one shown when the user
    logs in. *)
let transition_home_view layout =
  let label = W.label ~size:40 "You are signed in!" in
  let new_layout =
    L.tower
      [
        L.resident ~w:window_width ~h:200 label;
        L.resident ~w:window_width
          (create_btn "Add a password" (fun () ->
               print_endline "add btn pressed"));
        L.resident ~w:window_width
          (create_btn "List passwords" (fun () ->
               print_endline "list btn pressed"));
        L.resident ~w:window_width
          (create_btn "Generate a password" (fun () ->
               print_endline "gen btn pressed"));
        L.resident ~w:window_width
          (create_btn "Set the master password" (fun () ->
               print_endline "setpwd btn pressed"));
        L.resident ~w:window_width
          (create_btn "Import passwords" (fun () ->
               print_endline "import btn pressed"));
        L.resident ~w:window_width
          (create_btn "Export a password" (fun () ->
               print_endline "Export btn pressed"));
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
    create_btn "Login" (fun () ->
        let pwd = W.get_text input in
        if MasterPassword.check_master pwd then
          let () = Encrypt.set_key pwd in
          Update.push controller
        else W.set_text label "Password incorrect")
  in
  let layout =
    L.tower
      [
        L.resident ~w:window_width input;
        L.resident ~w:window_width login_btn;
        L.resident ~w:window_width ~h:200 label;
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
