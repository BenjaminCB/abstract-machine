# Abstract-machine
A Haskell implementation of an abstract machine that find the locals for a small subset of jsx.

# Development and running
- Install [nix](https://nixos.org/download.html)
- Enable [flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) (append `--experimental-features 'nix-command flakes'` to your nix commands to enable flakes for that command only)
- Run `nix develop` in project root
- Run `just run <path-to-abstract-machine-root-dir> <path-to-entry-point>` in project root

`<path-to-abstract-machine-root-dir>` is the path to the directory on your machine that you want to use as the root directory in the abstract machine. For first time users this should be the programs directory in this repo.

`<path-to-entry-point>` is the path to the file that you want to run relative to the root directory of the abstract machine.

As an example i can turn the `programs` directory into the abstract machine root directory and run the `program2.jsx` file, by running the command `just run ./programs /program2.jsx`.
