# A playground for OxCaml

**(Disclaimer: currently in alpha)**

To make a playground, press the green "Code" button, then select "+" next to "Codespaces".  A new Codespace will open.  It currently takes maybe 20 or 30 minutes to initialize; please be patient.  We'll work on improving this startup time shortly.
You can click the link in the status popup in the bottom-right of the window to see current progress (although there are no spinners).

Once initialized you should have a full OPAM environment with the OxCaml compiler and dune on the path.  VSCode will have the OCaml Platform plugin together with the LSP server and merlin, the editor assistant.

## Building your first OxCaml project

```shell
$ eval $(opam env --switch 5.2.0+ox) && \
        opam install --yes utop && \
        opam install --yes parallel && \
        opam install --yes core_unix && \
        opam install --yes \
                async async_extra async_js async_kernel async_rpc_kernel async_rpc_websocket \
                base core core_kernel ocaml-embed-file \
                ppx_jane virtual_dom cohttp cohttp-async uri \
                bonsai bonsai_web js_of_ocaml js_of_ocaml-ppx 

cd example
opam exec -- dune fmt
dune build --watch
dune build --watch --terminal-persistence=clear-on-rebuild-and-flush-history

```
