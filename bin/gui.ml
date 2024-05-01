open Bogue
module W = Widget
module L = Layout
module T = Trigger
open FinalProject

let btn_font_size = 18
let btn_border_radius = 10
let window_width = 400
let login_signal = W.empty ~w:0 ~h:0 ()
let add_signal = W.empty ~w:0 ~h:0 ()

let create_connection signal layout new_view =
  W.connect_main signal signal
    (fun _ _ _ ->
      L.set_rooms layout [ new_view ];
      Sync.push (fun () -> L.fit_content ~sep:0 layout))
    [ Trigger.update ]

let create_btn s f =
  W.button
    ~label:(Label.create ~size:btn_font_size s)
    ~border_radius:btn_border_radius
    ~action:(fun _ -> f ())
    ""

let add_view =
  let name_input =
    W.text_input ~size:30 ~max_size:200 ~prompt:"Enter password name" ()
  in
  let pwd_input =
    W.text_input ~size:30 ~max_size:200 ~prompt:"Enter password" ()
  in
  let add_btn =
    create_btn "Add password" (fun () -> print_endline "add password pressed")
  in
  L.tower
    [
      L.resident ~w:window_width name_input;
      L.resident ~w:window_width pwd_input;
      L.resident ~w:window_width ~h:200 add_btn;
    ]

let home_view =
  let label = W.label ~size:40 "You are signed in!" in
  let add_btn =
    create_btn "Add a password" (fun () -> Update.push add_signal)
  in
  L.tower
    [
      L.resident ~w:window_width ~h:200 label;
      L.resident ~w:window_width add_btn;
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
          Update.push login_signal
        else W.set_text label "Password incorrect")
  in
  L.tower
    [
      L.resident ~w:window_width input;
      L.resident ~w:window_width login_btn;
      L.resident ~w:window_width ~h:200 label;
    ]

let connections =
  [
    create_connection login_signal login_view home_view;
    create_connection add_signal home_view add_view;
  ]

let main () =
  let board = Bogue.of_layout ~connections login_view in
  Bogue.run board

let _ =
  main ();
  Draw.quit ()
