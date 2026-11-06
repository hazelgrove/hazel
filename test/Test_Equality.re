open Alcotest;

/* Every shipped documentation slide, parsed the way the editor loads it
   (FastParse + MakeTerm, so projector wrappers are present). Slides that
   need the typing-parser fallback are skipped. */
let doc_slide_terms: Lazy.t(list((string, Language.Exp.t))) =
  lazy(
    snd(Lazy.force(Web.Init.startup).documentation)
    |> List.filter_map(((name, slide: Web.CellEditor.Model.persistent)) =>
         Haz3lcore.FastParse.of_text(
           ~materialize=Haz3lcore.Triggers.invoked_projector,
           ~collect_refractors=true,
           ~root=Exp,
           String.trim(slide.editor.zipper.backup_text),
         )
         |> Option.map(seg => (name, Haz3lcore.MakeTerm.go(seg).term))
       )
  );

open Language;

let reflexive_on_doc_slides = (eq_name, eq: (Exp.t, Exp.t) => bool, ()) => {
  let terms = Lazy.force(doc_slide_terms);
  check(bool, "corpus is non-empty", true, terms != []);
  List.iter(
    ((name, e)) =>
      check(
        bool,
        eq_name ++ ".exp(e, e) for slide " ++ name,
        true,
        eq(e, e),
      ),
    terms,
  );
};

open IdTagged.FreshGrammar;

/* module M = { let (a=x, b=y) = (a=1, b=2) } in M.x
   (built per call so each copy has its own ids). */
let module_with_labeled_let = () =>
  Exp.module_exp(
    MPat.var("M"),
    Exp.module_([
      Mod.mod_let(
        Pat.tuple([
          Pat.tup_label(Pat.label("a"), Pat.var("x")),
          Pat.tup_label(Pat.label("b"), Pat.var("y")),
        ]),
        Exp.tuple([
          Exp.tup_label(Exp.label("a"), Exp.int(1)),
          Exp.tup_label(Exp.label("b"), Exp.int(2)),
        ]),
      ),
    ]),
    Exp.dot(Exp.var("M"), Exp.label("x")),
  );

let tests = (
  "Equality",
  [
    test_case(
      "let alpha equivalence",
      `Quick,
      () => {
        let x1 = Exp.let_(Pat.var("x"), Exp.int(1), Exp.var("x"));
        let x2 = Exp.let_(Pat.var("x'"), Exp.int(1), Exp.var("x'"));
        check(
          bool,
          "let x = 1 in x === let x' = 1 in x'",
          true,
          Equality.semantic.exp(x1, x2),
        );
      },
    ),
    test_case(
      "forall type inequality",
      `Quick,
      () => {
        let forall_string =
          Exp.forall(
            Pat.asc(Pat.var("x"), Typ.string()),
            Exp.bin_op(
              Operators.Poly(Operators.Equals),
              Exp.var("x"),
              Exp.var("x"),
            ),
          );
        let forall_int =
          Exp.forall(
            Pat.asc(Pat.var("x"), Typ.int()),
            Exp.bin_op(
              Operators.Poly(Operators.Equals),
              Exp.var("x"),
              Exp.var("x"),
            ),
          );
        check(
          bool,
          "forall x : String -> x == x !== forall x : Int -> x == x",
          false,
          Equality.semantic.exp(forall_string, forall_int),
        );
      },
    ),
    test_case(
      "module item alpha equivalence (pat bindings are alpha-renamed)",
      `Quick,
      () => {
        let m1 =
          Exp.module_([
            Mod.mod_let(Pat.var("x"), Exp.int(1)),
            Mod.mod_let(Pat.var("y"), Exp.int(2)),
          ]);
        let m2 =
          Exp.module_([
            Mod.mod_let(Pat.var("a"), Exp.int(1)),
            Mod.mod_let(Pat.var("b"), Exp.int(2)),
          ]);
        /* ModLet pattern names become labels, so different names
           means different modules — no alpha-renaming. */
        check(
          bool,
          "{let x=1; let y=2} !== {let a=1; let b=2}",
          false,
          Equality.semantic.exp(m1, m2),
        );
      },
    ),
    test_case(
      "module structural equality",
      `Quick,
      () => {
        let m1 = Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]);
        let m2 = Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]);
        check(
          bool,
          "{let x=1} === {let x=1}",
          true,
          Equality.semantic.exp(m1, m2),
        );
      },
    ),
    test_case(
      "module keyword - MPat uses literal name comparison",
      `Quick,
      () => {
        let e1 =
          Exp.module_exp(
            MPat.var("M"),
            Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
            Exp.dot(Exp.var("M"), Exp.label("x")),
          );
        let e2 =
          Exp.module_exp(
            MPat.var("N"),
            Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
            Exp.dot(Exp.var("N"), Exp.label("x")),
          );
        /* MPat supports alpha-equivalence: M and N are just binders,
           so module M = ... in M.x === module N = ... in N.x */
        check(
          bool,
          "module M = {let x=1} in M.x === module N = {let x=1} in N.x",
          true,
          Equality.semantic.exp(e1, e2),
        );
      },
    ),
    test_case(
      "module keyword structural equality",
      `Quick,
      () => {
        let e1 =
          Exp.module_exp(
            MPat.var("M"),
            Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
            Exp.dot(Exp.var("M"), Exp.label("x")),
          );
        let e2 =
          Exp.module_exp(
            MPat.var("M"),
            Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
            Exp.dot(Exp.var("M"), Exp.label("x")),
          );
        check(
          bool,
          "module M = {let x=1} in M.x === module M = {let x=1} in M.x",
          true,
          Equality.semantic.exp(e1, e2),
        );
      },
    ),
    test_case(
      "module let with labeled tuple pattern is reflexive",
      `Quick,
      () => {
        let e = module_with_labeled_let();
        check(
          bool,
          "syntactic: e === e",
          true,
          Equality.syntactic.exp(e, e),
        );
        check(bool, "semantic: e === e", true, Equality.semantic.exp(e, e));
        check(
          bool,
          "semantic: e === copy of e",
          true,
          Equality.semantic.exp(e, module_with_labeled_let()),
        );
      },
    ),
    test_case(
      "module let names stay literal under a labeled tuple pattern",
      `Quick,
      () => {
        let m = (x, y) =>
          Exp.module_([
            Mod.mod_let(
              Pat.tuple([
                Pat.tup_label(Pat.label("a"), Pat.var(x)),
                Pat.tup_label(Pat.label("b"), Pat.var(y)),
              ]),
              Exp.var("p"),
            ),
          ]);
        check(
          bool,
          "{let (a=x, b=y) = p} !== {let (a=x, b=z) = p}",
          false,
          Equality.semantic.exp(m("x", "y"), m("x", "z")),
        );
      },
    ),
    test_case(
      "module let honours ignore_ascriptions",
      `Quick,
      () => {
        let plain = Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]);
        let ascribed =
          Exp.module_([
            Mod.mod_let(Pat.asc(Pat.var("x"), Typ.int()), Exp.int(1)),
          ]);
        check(
          bool,
          "{let x : Int = 1} === {let x = 1} ignoring ascriptions",
          true,
          Equality.ignoring_ascriptions.exp(ascribed, plain),
        );
      },
    ),
    test_case(
      "syntactic equality is reflexive on the doc slides",
      `Quick,
      reflexive_on_doc_slides("syntactic", Equality.syntactic.exp),
    ),
    test_case(
      "semantic equality is reflexive on the doc slides",
      `Quick,
      reflexive_on_doc_slides("semantic", Equality.semantic.exp),
    ),
  ],
);
