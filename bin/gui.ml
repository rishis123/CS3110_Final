(* @author Rohen Giralt (rmg296), Sophia Song, Rishi Shah (ss3579), Lakshmi
   Meghana Kesanapalli (lk496), Zhijia Ye (zy424) *)

open Bogue
module W = Widget
module L = Layout
module T = Trigger
open FinalProject
module Persistence = Persistence.Default

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
let gen_pwd_signal = W.empty ~w:0 ~h:0 ()
let import_signal = W.empty ~w:0 ~h:0 ()
let import_complete_signal = W.empty ~w:0 ~h:0 ()
let export_signal = W.empty ~w:0 ~h:0 ()
let export_complete_signal = W.empty ~w:0 ~h:0 ()
let delete_signal = W.empty ~w:0 ~h:0 ()
let delete_complete_signal = W.empty ~w:0 ~h:0 ()

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

(** [export_view] is the view shown when exporting passwords to a file. *)
let export_view =
  let path_input = create_text_input "Enter file path" in
  let export_btn =
    create_btn "Export" (fun () ->
        let path = W.get_text path_input in
        let secrets = Persistence.read_all_encryptable () in
        PasswordImport.export secrets path;
        W.set_text path_input "";
        Update.push export_complete_signal)
  in
  let cancel_btn =
    create_btn "Cancel" (fun () ->
        W.set_text path_input "";
        Update.push back_home_signal)
  in
  L.tower
    [
      L.resident ~w:window_width path_input;
      L.resident ~w:window_width export_btn;
      L.resident ~w:window_width cancel_btn;
    ]

(** [import_view] is the view shown when importing passwords from a file. *)
let import_view =
  let path_input = create_text_input "Enter file path" in
  let import_btn =
    create_btn "Import" (fun () ->
        let path = W.get_text path_input in
        let new_secrets = PasswordImport.import path in
        new_secrets |> List.iter Persistence.write_encryptable;
        W.set_text path_input "";
        Update.push import_complete_signal)
  in
  let cancel_btn =
    create_btn "Cancel" (fun () ->
        W.set_text path_input "";
        Update.push back_home_signal)
  in
  L.tower
    [
      L.resident ~w:window_width path_input;
      L.resident ~w:window_width import_btn;
      L.resident ~w:window_width cancel_btn;
    ]

(** [!special_char_on] is whether to include special characters in the generated
    password. *)
let special_char_on = ref true

(** [pwd_length] is the length of the password to generate. *)
let pwd_length = ref 12

(** [get_pwd ()] generates a password with [!pwd_length] and includes special
    characters if [!special_char_on] is true. *)
let get_pwd () =
  if !special_char_on then
    GenPassword.generate_password_with_special !pwd_length
  else GenPassword.generate_password_without_special !pwd_length

(** [gen_pwd_view] is the view shown when the user generates a new password *)
let gen_pwd_view =
  let length_label = W.label ~size:label_text_size "Password length: " in
  let length_input = create_text_input "      " in
  let pwd_label = W.label ~size:label_text_size (get_pwd ()) in
  let special_char_btn =
    W.button ~kind:Button.Switch
      ~label_on:(Label.create ~size:label_text_size "Special charaters: On")
      ~label_off:(Label.create ~size:label_text_size "Special character: Off")
      ~action:(fun x ->
        if x then special_char_on := true else special_char_on := false)
      ~state:!special_char_on ""
  in
  let gen_again_btn =
    create_btn "Generate again" (fun () ->
        let new_len =
          match int_of_string_opt (W.get_text length_input) with
          | None -> 12
          | Some x -> if x <= 0 || x > 40 then 12 else x
        in
        W.set_text length_input (string_of_int new_len);
        pwd_length := new_len;
        W.set_text pwd_label (get_pwd ()))
  in
  L.tower
    [
      L.resident ~w:window_width pwd_label;
      L.resident ~w:window_width special_char_btn;
      L.flat
        [
          L.resident ~w:(window_width / 2) length_label;
          L.resident ~w:(window_width / 2) length_input;
        ];
      L.resident ~w:window_width gen_again_btn;
      L.resident ~w:window_width back_btn;
    ]

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
  let not_empty s = String.length s > 0 in
  let is_empty s = s = String.empty in
  let write_and_update encryptable =
    Persistence.write_encryptable encryptable;
    W.set_text name_input "";
    W.set_text username_input "";
    W.set_text pwd_input "";
    W.set_text url_input "";
    Update.push add_complete_signal
  in
  let add_btn =
    create_btn "Add password" (fun () ->
        let name = W.get_text name_input in
        let username = W.get_text username_input in
        let pwd = W.get_text pwd_input in
        let url = W.get_text url_input in
        if
          not_empty name && not_empty username && not_empty pwd && not_empty url
        then
          write_and_update
            (Types.Login { name; username; password = pwd; url = Some url })
        else if
          not_empty name && not_empty username && not_empty pwd && is_empty url
        then
          write_and_update
            (Types.Login { name; username; password = pwd; url = None })
        else if
          not_empty name && not_empty pwd && is_empty username && is_empty url
        then write_and_update (Types.Password { name; password = pwd })
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
        MasterPassword.string_to_salted_hash newpwd
        |> Persistence.write_unencryptable;
        W.set_text pwd_input "";
        Update.push master_pwd_change_complete_signal)
  in
  let label =
    W.rich_text ~size:label_text_size ~h:label_height
      Text_display.(
        page [ para ""; para "Delete a password or login based on its name." ])
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

(** [!list_view] is the view shown when the user lists all passwords. *)
let list_view = ref (L.empty ~w:window_width ~h:600 ())

(** [create_login_label_resident name data] creates a resident layout for a
    field of a login's data, with [name] as the name of the field and [data] as
    the login's information. *)
let create_login_label_resident name data =
  let h = 50 in
  L.resident ~w:window_width
    (W.rich_text ~size:label_text_size ~h
       Text_display.(page [ bold (para name); para data ]))

(** [create_login_label data] creates a resident layout with all of a login's
    data, to be displayed in [list_view]. *)
let create_login_label data =
  match data with
  | Types.Password p ->
      L.tower
        [
          create_login_label_resident "Name: " p.name;
          create_login_label_resident "Password: " p.password;
        ]
  | Types.Login l -> (
      let fields =
        [
          create_login_label_resident "Name: " l.name;
          create_login_label_resident "Username: " l.username;
          create_login_label_resident "Password: " l.password;
        ]
      in
      match l.url with
      | None -> L.tower fields
      | Some url ->
          L.tower (fields @ [ create_login_label_resident "Url: " url ]))

(** [update_list_view ()] updates [list_view] so that changes made to the logins
    are displayed. In other words, list view updates if changes are made to the
    data. *)
let update_list_view () =
  let new_list_view =
    let pwd_list = Persistence.read_all_encryptable () in
    let label_list = List.map create_login_label pwd_list in
    let scrollpane = L.make_clip ~scrollbar:true ~h:500 (L.tower label_list) in
    L.tower [ scrollpane; L.resident ~w:window_width back_btn ]
  in
  list_view := new_list_view

(** [delete_view] is the view shown when a login is deleted. *)
let delete_view =
  let input = create_text_input "Enter name" in
  let delete_btn =
    create_btn "Delete" (fun () ->
        let name = W.get_text input in
        W.set_text input "";
        Persistence.delete_encryptable_by_name name;
        Update.push delete_complete_signal)
  in
  let label =
    W.label ~size:label_text_size "Delete a password or login based on its name"
  in
  let cancel_btn =
    create_btn "Cancel" (fun () ->
        W.set_text input "";
        Update.push back_home_signal)
  in
  L.tower
    [
      L.resident ~w:window_width input;
      L.resident ~w:window_width delete_btn;
      L.resident ~w:window_width cancel_btn;
      L.resident ~w:window_width ~h:label_height label;
    ]

(** [home_view] is the view shown right after the user logs in. *)
let home_view =
  let label = W.label ~size:label_text_size "You are signed in!" in
  let add_btn =
    create_btn "Add a password" (fun () -> Update.push add_signal)
  in
  let delete_btn =
    create_btn "Delete a password" (fun () -> Update.push delete_signal)
  in
  let list_btn = create_btn "List logins" (fun () -> Update.push list_signal) in
  let gen_pwd_btn =
    create_btn "Generate a password" (fun () -> Update.push gen_pwd_signal)
  in
  let set_master_pwd_btn =
    create_btn "Set the master password" (fun () ->
        Update.push master_pwd_change_signal)
  in
  let import_btn =
    create_btn "Import passwords" (fun () -> Update.push import_signal)
  in
  let export_btn =
    create_btn "Export passwords" (fun () -> Update.push export_signal)
  in
  L.tower
    [
      L.resident ~w:window_width ~h:label_height label;
      L.resident ~w:window_width add_btn;
      L.resident ~w:window_width delete_btn;
      L.resident ~w:window_width list_btn;
      L.resident ~w:window_width gen_pwd_btn;
      L.resident ~w:window_width set_master_pwd_btn;
      L.resident ~w:window_width import_btn;
      L.resident ~w:window_width export_btn;
    ]

(** [login_view] is the view shown before the user logs in *)
let login_view =
  let input = create_text_input "Enter master password" in
  let label = W.label ~size:label_text_size "Hello! Please log in." in
  let login_btn =
    create_btn "Login" (fun () ->
        let pwd = W.get_text input in
        W.set_text input "";
        if MasterPassword.check_master_pwd pwd then
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

(** [master_layout] is the layout which is displayed in the window*)
let master_layout = L.tower [ login_view ]

(** [create_connection signal new_view] creates a connection such that when
    [Update.push signal] is called, the view changes to [newview]. *)
let create_connection signal new_view =
  W.connect_main signal signal
    (fun _ _ _ ->
      L.set_rooms master_layout [ new_view ];
      Sync.push (fun () -> L.fit_content ~sep:0 master_layout))
    [ Trigger.update ]

let create_mutation_connection signal view_ref update_fun =
  W.connect_main signal signal
    (fun _ _ _ ->
      update_fun ();
      L.set_rooms master_layout [ !view_ref ];
      Sync.push (fun () -> L.fit_content ~sep:0 master_layout))
    [ Trigger.update ]

(** [connections] is the list of connections for events. A connection indicates
    what should happen when a widget receives [Update.push]. *)
let connections =
  (* This connection not only changes the view but also causes the listview to
     update with new changes made since the last time the listview was shown. *)
  let list_view_connection =
    create_mutation_connection list_signal list_view update_list_view
  in
  let gen_pwd_view_connection = create_connection gen_pwd_signal gen_pwd_view in
  [
    create_connection login_signal home_view;
    create_connection add_signal add_view;
    create_connection add_complete_signal
      (action_complete_view "Password added");
    create_connection back_home_signal home_view;
    list_view_connection;
    create_connection master_pwd_change_complete_signal
      (action_complete_view "Master password set");
    create_connection master_pwd_change_signal set_master_pwd_view;
    gen_pwd_view_connection;
    create_connection import_signal import_view;
    create_connection import_complete_signal
      (action_complete_view "Import complete");
    create_connection export_signal export_view;
    create_connection export_complete_signal
      (action_complete_view "Export complete");
    create_connection delete_signal delete_view;
    create_connection delete_complete_signal
      (action_complete_view "Deletion complete");
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
