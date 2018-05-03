all: www/hazel.js
debug: Hazel.byte
	js_of_ocaml +weak.js --debuginfo -o www/hazel.js Hazel.byte
www/hazel.js: Hazel.byte
	js_of_ocaml +weak.js -o www/hazel.js Hazel.byte
Hazel.byte: \
	Hazel.re \
	Semantics.re \
	Action.re \
	Chrome.re \
	CursorInspector.re \
	ContextInspector.re \
  ActionPanel.re \
	View.re \
	GeneralUtil.re \
	JSUtil.re \
	HTMLUtil.re \
	LangUtil.re \
	Model.re \
	Pretty.re \
	Associator.re \
	PanelUtils.re \
	SkelExprLexer.mll \
	SkelExprParser.mly \
	SkelTypLexer.mll \
	SkelTypParser.mly \
	Transpile.re \
	Transpile.rei \
	HZLex.mll \
	HZParse.mly
	scripts/refmt.sh
	rebuild -use-menhir -use-ocamlfind -cflag -g -lflag -g -pkgs js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml,react,reactiveData,camomile Hazel.byte
Semantics.re: Semantics.v
	scripts/extract-coq.sh
clean:
	rm -rf _build
	rm -f *.glob *.vo *.byte www/hazel.js Semantics.re
