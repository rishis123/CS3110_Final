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
let label_height = 200
let text_input_size = 30
let text_input_max_size = 200

(** Buttons that change views need "signal" widgets, which are empty widgets
    used to recieve Update.push signals to trigger view changes. *)
let login_signal = W.empty ~w:0 ~h:0 ()

let add_signal = W.empty ~w:0 ~h:0 ()
let add_complete_signal = W.empty ~w:0 ~h:0 ()
let back_home_signal = W.empty ~w:0 ~h:0 ()
let list_signal = W.empty ~w:0 ~h:0 ()
let master_pwd_change_complete_signal = W.empty ~w:0 ~h:0 ()
let master_pwd_change_signal = W.empty ~w:0 ~h:0 ()

(** [create_btn s f] creates a button with label text [s] that does [f] when
    clicked. Note: This function does not create a connection, which is
    necessary if the button changes the view. *)
let create_btn s f =
  W.button
    ~label:(Label.create ~size:btn_font_size s)
    ~border_radius:btn_border_radius
    ~action:(fun _ -> f ())
    ""

(** [create_text_input s] creates a text input with prompt text [s]. *)
let create_text_input s =
  W.text_input ~size:text_input_size ~max_size:text_input_max_size ~prompt:s ()

(** [back_btn] is a button that when pressed, returns to the home view. *)
let back_btn =
  create_btn "Back to home" (fun () -> Update.push back_home_signal)

(** [action_complete_view msg] is a view with the message [msg] and a back
    button which trasitions to [home_view]. Displayed after an action is
    completed. *)
let action_complete_view msg =
  let label = W.label ~size:label_text_size msg in
  L.tower
    [ L.resident ~w:window_width label; L.resident ~w:window_width back_btn ]

(** [add_view] is the view shown when the user adds a new password. *)
let add_view =
  let label =
    W.rich_text ~size:label_text_size ~h:label_height
      Text_display.(
        page
          [
            para "";
            para
              "The name and password fields must be filled. If the url field \
               is filled, the username field must be filled. ";
          ])
  in
  let name_input = create_text_input "Enter name" in
  let pwd_input = create_text_input "Enter password" in
  let username_input = create_text_input "Enter username" in
  let url_input = create_text_input "Enter url" in
  let add_btn =
    create_btn "Add password" (fun () ->
        let name = W.get_text name_input in
        let username = W.get_text username_input in
        let pwd = W.get_text pwd_input in
        let url = W.get_text url_input in
        let not_empty s = String.length s > 0 in
        let erase_all_input () =
          W.set_text name_input "";
          W.set_text username_input "";
          W.set_text pwd_input "";
          W.set_text url_input ""
        in
        if
          not_empty name && not_empty username && not_empty pwd && not_empty url
        then (
          Persistence.write_encryptable
            (Types.Login { name; username; password = pwd; url = Some url });
          erase_all_input ();
          Update.push add_complete_signal)
        else if
          not_empty name
          && not_empty username
          && not_empty pwd
          && not (not_empty url)
        then (
          Persistence.write_encryptable
            (Types.Login { name; username; password = pwd; url = None });
          erase_all_input ();
          Update.push add_complete_signal)
        else if
          not_empty name
          && not_empty pwd
          && (not (not_empty username))
          && not (not_empty url)
        then (
          let encryptable = Types.Password { name; password = pwd } in
          Persistence.write_encryptable encryptable;
          erase_all_input ();
          Update.push add_complete_signal)
        else ignore ())
  in
  let cancel_btn =
    create_btn "Cancel" (fun () ->
        W.set_text name_input "";
        W.set_text pwd_input "";
        Update.push back_home_signal)
  in
  L.tower
    [
      L.resident ~w:window_width name_input;
      L.resident ~w:window_width username_input;
      L.resident ~w:window_width pwd_input;
      L.resident ~w:window_width url_input;
      L.resident ~w:window_width add_btn;
      L.resident ~w:window_width cancel_btn;
      L.resident ~w:window_width ~h:label_height label;
    ]

(** [set_master_pwd_view] is the view shown when the uset sets the master
    password. *)
let set_master_pwd_view =
  let pwd_input = create_text_input "Enter new password" in
  let set_btn =
    create_btn "Set master password" (fun () ->
        let newpwd = W.get_text pwd_input in
        (* Salt & Hash -> Convert ot MasterPasswordHash type*)
        let master_pwd = Encrypt.salt_hash newpwd in
        Persistence.write_unencryptable master_pwd;
        W.set_text pwd_input "";
        Update.push master_pwd_change_complete_signal)
  in
  let label =
    W.label ~size:label_text_size "Please enter the new master password"
  in
  let cancel_btn =
    create_btn "Cancel" (fun () -> Update.push back_home_signal)
  in
  L.tower
    [
      L.resident ~w:window_width pwd_input;
      L.resident ~w:window_width set_btn;
      L.resident ~w:window_width cancel_btn;
      L.resident ~w:window_width ~h:label_height label;
    ]

(** [list_view] is the view shown when the user lists all passwords. *)
let list_view =
  let pwd_list = Persistence.read_all_encryptable () in
  let h = 50 in
  let create_login_label = function
    | Types.Password p ->
        L.tower
          [
            L.resident ~w:window_width
              (W.rich_text ~size:label_text_size ~h
                 Text_display.(page [ bold (para "Name: "); para p.name ]));
            L.resident ~w:window_width
              (W.rich_text ~size:label_text_size ~h
                 Text_display.(
                   page [ bold (para "Password: "); para p.password ]));
          ]
    | Types.Login l -> (
        let fields =
          [
            L.resident ~w:window_width
              (W.rich_text ~size:label_text_size ~h
                 Text_display.(page [ bold (para "Name: "); para l.name ]));
            L.resident ~w:window_width
              (W.rich_text ~size:label_text_size ~h
                 Text_display.(
                   page [ bold (para "Username: "); para l.username ]));
            L.resident ~w:window_width
              (W.rich_text ~size:label_text_size ~h
                 Text_display.(
                   page [ bold (para "Password: "); para l.password ]));
          ]
        in
        match l.url with
        | None -> L.tower fields
        | Some url ->
            L.tower
              (fields
              @ [
                  L.resident ~w:window_width
                    (W.rich_text ~size:label_text_size ~h
                       Text_display.(page [ bold (para "Url: "); para url ]));
                ]))
  in
  let label_list = List.map create_login_label pwd_list in
  let scrollpane = L.make_clip ~scrollbar:true ~h:500 (L.tower label_list) in
  L.tower [ scrollpane; L.resident ~w:window_width back_btn ]

(** [home_view] is the view shown right after the user logs in. *)
let home_view =
  let label = W.label ~size:label_text_size "You are signed in!" in
  let add_btn =
    create_btn "Add a password" (fun () -> Update.push add_signal)
  in
  let list_btn = create_btn "List logins" (fun () -> Update.push list_signal) in
  L.tower
    [
      L.resident ~w:window_width ~h:label_height label;
      L.resident ~w:window_width add_btn;
      L.resident ~w:window_width list_btn;
      L.resident ~w:window_width
        (create_btn "Generate a password" (fun () ->
             print_endline "gen btn pressed"));
      L.resident ~w:window_width
        (create_btn "Set the master password" (fun () ->
             Update.push master_pwd_change_signal));
      L.resident ~w:window_width
        (create_btn "Import passwords" (fun () ->
             print_endline "import btn pressed"));
      L.resident ~w:window_width
        (create_btn "Export a password" (fun () ->
             print_endline "Export btn pressed"));
    ]

(** [login_view] is the view shown before the user logs in *)
let login_view =
  let input = create_text_input "Enter master password" in
  let label = W.label ~size:label_text_size "Hello! Please log in." in
  let login_btn =
    create_btn "Login" (fun () ->
        let pwd = W.get_text input in
        W.set_text input "";
        if MasterPassword.check_master pwd then
          let () = Encrypt.set_key pwd in
          Update.push login_signal
        else W.set_text label "Password incorrect")
  in
  L.tower
    [
      L.resident ~w:window_width input;
      L.resident ~w:window_width login_btn;
      L.resident ~w:window_width ~h:label_height label;
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
    create_connection list_signal list_view;
    create_connection master_pwd_change_complete_signal
      (action_complete_view "Master password set");
    create_connection master_pwd_change_signal set_master_pwd_view;
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
