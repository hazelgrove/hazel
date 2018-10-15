pkgs :=js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml,react,reactiveData,camomile
source :=Semantics.ml $(wildcard *.rei?) $(wildcard *.ml[ly]) $(wildcard serialization/*.rei?) $(wildcard serialization/*.ml[ly])
build_opts :=-use-menhir -use-ocamlfind -cflag -g -lflag -g -I serialization

.PHONY : all clean debug run_tests

all: www/hazel.js run_tests
debug: Hazel.byte
	js_of_ocaml --debuginfo -o www/hazel.js Hazel.byte
www/hazel.js: Hazel.byte
	js_of_ocaml -o www/hazel.js Hazel.byte
Hazel.byte: $(source)
	scripts/refmt.sh
	rebuild $(build_opts) -X tests -pkgs $(pkgs) Hazel.byte
Semantics.ml: Semantics.v
	scripts/extract-coq.sh
tests/RunTests.byte: $(source) $(wildcard tests/*.rei?) $(shell find tests/test_data)
	scripts/refmt.sh
	rebuild $(build_opts) -X tests/test_data -pkgs oUnit,str tests/RunTests.byte
run_tests: tests/RunTests.byte
	./RunTests.byte
clean:
	rm -rf _build
	rm -f *.glob *.vo *.byte www/hazel.js Semantics.ml
	rm -f tests/*.byte

