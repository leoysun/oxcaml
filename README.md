# TicTacToe using OCaml

To make a dev-environment, press the green "Code" button, then select "+" next to "Codespaces".  A new Codespace will open.  It currently takes 20-40 minutes to initialize; please be patient.

Once initialized you need to run the following commands:
```shell
opam init -a --disable-sandboxing --yes --bare && \
        opam update -a && \
        opam switch create 4.14.0 --yes  && \
        eval $(opam env --switch 4.14.0) && \
        opam install --yes  ocamlformat merlin ocaml-lsp-server bonsai
```

Afterwards you should have a full OPAM environment with the OCaml compiler and dune on the path.  VSCode will have the OCaml Platform plugin together with the LSP server and merlin, the editor assistant.

## Building the OCaml project
Make sure you're using the right opam switch:
```shell
eval $(opam env --switch 4.14.0)
```

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



TODOs as of 10/31am: 
- make the tile DISAPPEAR FROM PLAYER INVENTORY when it is added to a meld during tile rearrangement

- Make Jokers moveable between melds, and do proper revalidation when this occurs

- Allow player to TAKE JOKER INTO INVENTORY if they are able to replace it in the meld to keep the meld valid, or if the joker was not necessary to keep the meld valid in the first place. OBVIOUSLY do not save this action if the meld is invalid upon saving rearrangement.

- DO NOT END TURN after any action is played; player must pass or draw to end turn

- do not allow REARRANGING to bypass the 30-point first-move rule

