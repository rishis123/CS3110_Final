This document gives instructions to install the Final Projectors password manager.

First, OPAM must be installed.
Consult the following link if OPAM has not been installed. (It is not necessary to install or set up VS Code).
https://cs3110.github.io/textbook/chapters/preface/install.html#create-an-opam-switch

Next, install the dependencies using OPAM.
Install the required dependencies using the following commands in a terminal:
```sh
$ opam update
$ opam upgrade
$ opam install batteries ppx_inline_test core csv bogue
```

Note: If using WSL, WSL 2 on Windows 10 or higher is required. WSL may need to be updated. See Microsoft's guide for more details: https://learn.microsoft.com/en-us/windows/wsl/tutorials/gui-apps

Then, you can build and run the project with the following commands. Make sure you have changed directory to be in the project's root directory (the project root is the folder containing this `INSTALL.md` file). Run main.exe for the terminal interface. Run gui.exe for the gui interface.
```sh
$ dune build
$ dune exec bin/main.exe
$ dune exec bin/gui.exe
```