# A playground for OxCaml

**(Disclaimer: currently in alpha)**

To make a playground, press the green "Code" button, then select "+" next to "Codespaces".  A new Codespace will open.  It currently takes maybe 20 or 30 minutes to initialize; please be patient.  We'll work on improving this startup time shortly.
You can click the link in the status popup in the bottom-right of the window to see current progress (although there are no spinners).

Once initialized you should have a full OPAM environment with the OxCaml compiler and dune on the path.  VSCode will have the OCaml Platform plugin together with the LSP server and merlin, the editor assistant.

## Building your first OxCaml project

```shell
$ opam init -a --disable-sandboxing --yes --bare && \
        opam update -a && \
        opam switch create 5.2.0+ox --yes \
        --repos "ox=git+https://github.com/oxcaml/opam-repository.git,default" && \
        eval $(opam env --switch 5.2.0+ox) && \
        opam install --yes ocamlformat && \
        opam install --yes merlin && \
        opam install --yes ocaml-lsp-server && \
        opam install --yes utop && \
        opam install --yes parallel && \
        opam install --yes core_unix

opam install --yes \
    async async_extra async_js async_kernel async_rpc_kernel async_rpc_websocket \
    base core core_kernel ocaml-embed-file \
    ppx_jane virtual_dom cohttp cohttp-async uri \
    bonsai bonsai_web js_of_ocaml js_of_ocaml-ppx 

opam exec -- dune fmt
dune build --watch
dune build --watch --terminal-persistence=clear-on-rebuild-and-flush-history

$ cd parallel-example/filter
$ dune build filter.exe
$ ../_build/default/filter/filter.exe
$ mogrify -format jpg filtered-ox.pgm
```

Then you can open `filtered-ox.pgm` directly from the sidebar on the left (expand the `parallel-example` directory in the tree view).  Behold the *filtered ox*.
