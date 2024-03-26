default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
    treefmt

build:
    cabal build abstract-machine

# Run ghcid -- auto-recompile and run `main` function
test:
    ghcid -c "cabal repl exe:abstract-machine-test" --warnings -T :main

run *ARGS: build
    cabal run abstract-machine {{ARGS}}
