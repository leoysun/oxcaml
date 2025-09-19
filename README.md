# TicTacToe using OxCaml

To make a dev-environment, press the green "Code" button, then select "+" next to "Codespaces".  A new Codespace will open.  It currently takes 20-40 minutes to initialize; please be patient.

Once initialized you need to run the following commands:
```shell
$ eval $(opam env --switch 5.2.0+ox) && 
        opam install -j 1  --yes --locked ocamlformat && 
        opam install -j 1 --yes --locked merlin && 
        opam install -j 1  --yes --locked async && 
        opam install -j 1  --yes --locked ocaml-lsp-server && 
        opam install --yes --locked utop && 
        opam install --yes --locked parallel && 
        opam install --yes --locked core_unix && 
        opam install --yes --locked \
                async async_extra async_js async_kernel async_rpc_kernel async_rpc_websocket \
                base core core_kernel ocaml-embed-file \
                ppx_jane virtual_dom cohttp cohttp-async uri \
                bonsai bonsai_web js_of_ocaml js_of_ocaml-ppx
```

Afterwards you should have a full OPAM environment with the OxCaml compiler and dune on the path.  VSCode will have the OCaml Platform plugin together with the LSP server and merlin, the editor assistant.

## Building the OxCaml project
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


