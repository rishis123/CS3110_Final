open Bogue
module W = Widget
module L = Layout
module T = Trigger
open FinalProject

(** Constants controlling visual aspects of the gui such as font size and
    colors. *)
let btn_font_size = 18

let btn_border_radius = 10
let window_width = 400
let label_text_size = 18

(** Buttons that change views need "signal" widgets, which are empty widgets
    used to recieve Update.push signals to trigger view changes. *)
let login_signal = W.empty ~w:0 ~h:0 ()

let add_signal = W.empty ~w:0 ~h:0 ()
let add_complete_signal = W.empty ~w:0 ~h:0 ()
let back_home_signal = W.empty ~w:0 ~h:0 ()

(** [create_btn s f] creates a button with label text [s] that does [f] when
    clicked. Note: This function does not create a connection, which is
    necessary if the button changes the view. *)
let create_btn s f =
  W.button
    ~label:(Label.create ~size:btn_font_size s)
    ~border_radius:btn_border_radius
    ~action:(fun _ -> f ())
    ""

(** [action_complete_view msg] is a view with the message [msg] and a back
    button which trasitions to [home_view]. Displayed after an action is
    completed. *)
let action_complete_view msg =
  let label = W.label ~size:label_text_size msg in
  let back_btn =
    create_btn "Back to home" (fun () -> Update.push back_home_signal)
  in
  L.tower
    [ L.resident ~w:window_width label; L.resident ~w:window_width back_btn ]

(** [add_view] is the view shown when the user adds a new password. *)
let add_view =
  let label = W.label ~size:label_text_size "Enter information" in
  let name_input =
    W.text_input ~size:30 ~max_size:200 ~prompt:"Enter password name" ()
  in
  let pwd_input =
    W.text_input ~size:30 ~max_size:200 ~prompt:"Enter password" ()
  in
  let add_btn =
    create_btn "Add password" (fun () ->
        let name = W.get_text name_input in
        let pwd = W.get_text pwd_input in
        if String.length name > 0 && String.length pwd > 0 then (
          let encryptable =
            FinalProject.Types.Password { name; password = pwd }
          in
          FinalProject.Persistence.write_encryptable encryptable;
          Update.push add_complete_signal)
        else W.set_text label "Fields must not be empty")
  in
  L.tower
    [
      L.resident ~w:window_width name_input;
      L.resident ~w:window_width pwd_input;
      L.resident ~w:window_width add_btn;
      L.resident ~w:window_width ~h:200 label;
    ]

(** [home_view] is the view shown right after the user logs in. *)
let home_view =
  let label = W.label ~size:label_text_size "You are signed in!" in
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
  let label = W.label ~size:label_text_size "Hello! Please log in" in
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

let master_layout = L.tower [ login_view ]

(** [create_connection signal new_view] creates a connection such that when
    [Update.push signal] is called, the view changes to [newview]. *)
let create_connection signal new_view =
  W.connect_main signal signal
    (fun _ _ _ ->
      L.set_rooms master_layout [ new_view ];
      Sync.push (fun () -> L.fit_content ~sep:0 new_view))
    [ Trigger.update ]

(** [connections] is the list of connections for events. A connection indicates
    what should happen when a widget receives [Update.push]. *)
let connections =
  [
    create_connection login_signal home_view;
    create_connection add_signal add_view;
    create_connection add_complete_signal
      (action_complete_view "Password added");
    create_connection back_home_signal home_view;
  ]

(** [main ()] is Bogue's main loop. It will display this board until the window
    is closed. *)
let main () =
  let board = Bogue.of_layout ~connections master_layout in
  Bogue.run board

(** Run the main loop and exit the program gracefully if the window is closed. *)
let _ =
  main ();
  Draw.quit ()
