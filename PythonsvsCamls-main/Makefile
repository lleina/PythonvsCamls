.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f 3110FinalProject.zip
	zip -r 3110FinalProject.zip . -x@exclude.lst

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean:
	dune clean
	rm -f 3110FinalProject.zip

docs:
	dune build @doc

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

playgui:
	OCAMLRUNPARAM=b dune exec gui/gui.exe