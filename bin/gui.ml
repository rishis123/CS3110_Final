open Bogue

let () =
  Widget.label "Hello world"
  |> Layout.resident ~w:300 ~h:150
  |> Bogue.of_layout
  |> Bogue.run
