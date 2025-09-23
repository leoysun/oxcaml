# TicTacToe using OCaml

To make a dev-environment, press the green "Code" button, then select "+" next to "Codespaces".  A new Codespace will open.  It currently takes 20-40 minutes to initialize; please be patient.

Afterwards you should have a full OPAM environment with the OCaml compiler and dune on the path.  VSCode will have the OCaml Platform plugin together with the LSP server and merlin, the editor assistant.

## Building the OCaml project
To format the files:
```shell
eval $(opam env --switch 4.14.0) && dune fmt
```

To build and run tests continously:
```shell
eval $(opam env --switch 4.14.0) && dune build @runtest --watch
```

To promote/update expect-tests:
```shell
eval $(opam env --switch 4.14.0) && dune promote
```

To update the javascript:
```shell
cp _build/default/ui/tictactoe_ui.bc.js generated_js/
```
Then commit the changes and surf to your github.io to see you site:
https://yoav-zibin.github.io/oxcaml/


