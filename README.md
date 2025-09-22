# TicTacToe using OCaml

To make a dev-environment, press the green "Code" button, then select "+" next to "Codespaces".  A new Codespace will open.  It currently takes 20-40 minutes to initialize; please be patient.

Once initialized you need to run the following commands:
```shell
opam init -a --disable-sandboxing --yes --bare && \
        opam update -a && \
        opam switch create 4.14.0 --yes  && \
        eval $(opam env --switch 4.14.0) && \
        opam install --yes  ocamlformat merlin async ocaml-lsp-server utop core_unix \
                async async_extra async_js async_kernel async_rpc_kernel async_rpc_websocket \
                base core core_kernel ocaml-embed-file \
                ppx_jane virtual_dom cohttp cohttp-async uri \
                bonsai js_of_ocaml js_of_ocaml-ppx
```

Afterwards you should have a full OPAM environment with the OCaml compiler and dune on the path.  VSCode will have the OCaml Platform plugin together with the LSP server and merlin, the editor assistant.

## Building the OCaml project
To format the files:
```shell
dune fmt
```

To build and run tests continously:
```shell
dune build @runtest --watch
```

To promote/update expect-tests:
```shell
dune promote
```

To update the javascript:
```shell
cp _build/default/ui/tictactoe_ui.bc.js generated_js/
```
Then commit the changes and surf to your github.io to see you site:
https://yoav-zibin.github.io/oxcaml/


