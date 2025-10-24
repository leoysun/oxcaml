.PHONY: build test run sim web
build: ; dune build
test:  ; dune runtest
run:   ; dune exec src/cli_main.exe
sim:   ; dune exec sim/simulate.exe
web:   ; dune build bonsai_app
