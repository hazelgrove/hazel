pkgs :=js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml,react,reactiveData,camomile
source :=Semantics.re $(wildcard *.rei?) $(wildcard *.ml[ly])
build_opts :=-use-menhir -use-ocamlfind -cflag -g -lflag -g

.PHONY : all clean debug run_tests

all: www/hazel.js run_tests
debug: Hazel.byte
	js_of_ocaml +weak.js --debuginfo -o www/hazel.js Hazel.byte
www/hazel.js: Hazel.byte
	js_of_ocaml +weak.js -o www/hazel.js Hazel.byte
Hazel.byte: $(source)
	scripts/refmt.sh
	rebuild $(build_opts) -X tests -pkgs $(pkgs) Hazel.byte
Semantics.re: Semantics.v
	scripts/extract-coq.sh
tests/RunTests.byte: $(source) $(wildcard tests/*.rei?) $(shell find tests/test_data)
	scripts/refmt.sh
	rebuild $(build_opts) -X tests/test_data -pkgs oUnit tests/RunTests.byte
run_tests: tests/RunTests.byte
	RunTests.byte
clean:
	rm -rf _build
	rm -f *.glob *.vo *.byte www/hazel.js Semantics.re
	rm -f tests/*.byte

