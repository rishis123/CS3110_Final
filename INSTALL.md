This document gives instructions to install the Final Projectors password manager.

First, OPAM must be installed.
Consult the following link if OPAM has not been installed. (It is not necessary to install or set up VS Code).
https://cs3110.github.io/textbook/chapters/preface/install.html#create-an-opam-switch

Next, install the dependency using OPAM.
Install Batteries using the following commands in a terminal:
$ opam update
$ opam upgrade
$ opam install batteries

Then, you can build and run the project with the following commands:
$ dune build
$ dune exec bin/main.exe