(*
   Created by Martin Jambon and placed in the Public Domain on June 1, 2019.
   Print a tree or a DAG as tree, similarly to the 'tree' command.
   Sample output:
root
├── Mr. Poopypants
│   ├── something something
│   │   └── hello
│   │       ├── world
│   │       └── you
│   └── Ms. Poopypants
└── hello
    ├── world
    └── you
*)
[@@@ocamlformat "wrap-comments=false"]

val to_buffer :
  ?line_prefix:string ->
  get_name:('a -> string) ->
  get_children:('a -> 'a list) ->
  Buffer.t ->
  'a ->
  unit

val to_string :
  ?line_prefix:string ->
  get_name:('a -> string) ->
  get_children:('a -> 'a list) ->
  'a ->
  string
