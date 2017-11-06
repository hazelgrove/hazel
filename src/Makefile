pkgs :=compiler-libs,reason,js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml,react,reactiveData,camomile
reasons :=hazel.re semantics.re chrome.re action_palette.re view.re general_util.re js_util.re model.re pretty.re transpile.re transpile.rei hazelPrelude.re

.PHONY : all clean run_tests

all: www/hazel.js run_tests
www/hazel.js: hazel.byte
	js_of_ocaml +weak.js -o www/hazel.js hazel.byte
hazel.byte: $(reasons)
	scripts/refmt.sh
	rebuild -use-ocamlfind -pkgs $(pkgs) hazel.byte
semantics.re: semantics.v
	scripts/extract-coq.sh
tests/runTests.byte: tests/runTests.re tests/transpileTest.re $(reasons)
	cd tests && ../scripts/refmt.sh
	rebuild -use-ocamlfind -pkgs $(pkgs),oUnit,str tests/runTests.byte
run_tests: tests/runTests.byte
	runTests.byte
clean:
	rm -rf _build
	rm -f *.glob *.vo *.byte www/hazel.js semantics.re
	rm -f tests/*.byte

