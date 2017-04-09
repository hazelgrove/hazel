for f in `find . -name '*.ml'` ; \
do ( \
ocp-indent -i $f; \
); \
done

ocamlbuild -use-ocamlfind \
  -pkgs js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml,react,reactiveData,camomile \
   hazel.byte;

js_of_ocaml +weak.js -o www/hazel.js hazel.byte

