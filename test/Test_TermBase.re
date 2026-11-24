open Alcotest;
open Haz3lcore;
open Language;

let parse = (src: string): Exp.t =>
  Parser.to_term(src, ~root=Exp) |> Option.value_exn;

let describe = (x: Any.t): string =>
  switch (x) {
  | Exp(e) => "Exp " ++ Exp.show_cls(Exp.cls_of_term(e.term))
  | Pat(p) => "Pat " ++ Pat.show_cls(Pat.cls_of_term(p.term))
  | Typ(t) => "Typ " ++ Typ.show_cls(Typ.cls_of_term(t.term))
  | TPat(tp) => "TPat " ++ TPat.show_cls(TPat.cls_of_term(tp.term))
  | Rul(_) => "Rul"
  | Mod(m) => "Mod " ++ Mod.show_cls(Mod.cls_of_term(m.term))
  | Sig(s) => "Sig " ++ Sig.show_cls(Sig.cls_of_term(s.term))
  | MPat(mp) => "MPat " ++ MPat.show_cls(MPat.cls_of_term(mp.term))
  | Drv(_) => "Drv"
  | Any () => "Any"
  };

let check_descriptions = (name, expected, actual) =>
  check(list(string), name, expected, List.map(~f=describe, actual));

let children_tests = [
  test_case("Children of a let, in source order", `Quick, () =>
    check_descriptions(
      "children",
      ["Pat Variable binding", "Exp Number literal", "Exp Addition"],
      Any.children(Exp(parse("let x = 1 in x + 2"))),
    )
  ),
  test_case("A leaf has no children", `Quick, () =>
    check_descriptions("children", [], Any.children(Exp(parse("x"))))
  ),
  test_case("Subterms are listed parents first", `Quick, () =>
    check_descriptions(
      "subterms",
      [
        "Exp Function literal",
        "Pat Parenthesized pattern",
        "Pat Annotation",
        "Pat Variable binding",
        "Typ Base type",
        "Exp Variable reference",
      ],
      Any.subterms(Exp(parse("fun (x : Int) -> x"))),
    )
  ),
];

/* A random expression, with fresh ids like a parsed one. */
let of_menhir = exp =>
  Grammar.map_exp_annotation(
    _ => IdTagged.IdTag.fresh(),
    MenhirParser.Conversion.Exp.of_menhir_ast(exp),
  );

/* map_term and Any.fold are built on the same generated primitives, so they
   must reach the same nodes. */
let count_map_term_visits = (e: Exp.t): int => {
  let visits = ref(0);
  let visit = (continue, x) => {
    incr(visits);
    continue(x);
  };
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=visit,
      ~f_pat=visit,
      ~f_typ=visit,
      ~f_tpat=visit,
      ~f_rul=visit,
      ~f_mod=visit,
      ~f_sig=visit,
      ~f_mpat=visit,
      e,
    );
  visits^;
};

let qcheck_fold_agrees_with_map_term =
  QCheck.Test.make(
    ~name="Any.fold reaches the nodes map_term does",
    ~count=100,
    MenhirParser.AST.arb_exp(7),
    exp => {
      let e = of_menhir(exp);
      Any.fold(Exp(e), ~init=0, ~f=(n, _) => n + 1)
      == count_map_term_visits(e);
    },
  );

let qcheck_map_term_continue_is_identity =
  QCheck.Test.make(
    ~name="map_term without callbacks rebuilds the same term",
    ~count=100,
    MenhirParser.AST.arb_exp(7),
    exp => {
      let e = of_menhir(exp);
      Grammar.equal_exp_t(Poly.equal, Exp.map_term(e), e);
    },
  );

let tests = (
  "TermBase",
  children_tests
  @ [
    QCheck_alcotest.to_alcotest(qcheck_fold_agrees_with_map_term),
    QCheck_alcotest.to_alcotest(qcheck_map_term_continue_is_identity),
  ],
);
