open Alcotest;
open Language;

module Fresh = IdTagged.FreshGrammar;

let exp_eq =
  testable(Fmt.using(Exp.show, Fmt.string), Equality.syntactic.exp);
let pat_eq =
  testable(Fmt.using(Pat.show, Fmt.string), Equality.syntactic.pat);
let typ_eq =
  testable(Fmt.using(Typ.show, Fmt.string), Equality.syntactic.typ);

let tests = (
  "Canonicalize",
  [
    test_case("BuiltinFun → Var", `Quick, () => {
      check(
        exp_eq,
        "int_of_string",
        Fresh.Exp.var("int_of_string"),
        Canonicalize.exp(Fresh.Exp.builtin_fun("int_of_string")),
      )
    }),
    test_case(
      "Nat/SInt atoms → Int",
      `Quick,
      () => {
        check(
          exp_eq,
          "nat 8",
          Fresh.Exp.int(8),
          Canonicalize.exp(Fresh.Exp.nat(Bigint.of_int(8))),
        );
        check(
          exp_eq,
          "sint 8",
          Fresh.Exp.int(8),
          Canonicalize.exp(Fresh.Exp.sint(8)),
        );
      },
    ),
    /* Print/parse can change a float's binary representation, so canonical
       form is whatever Atom.to_literal round-trips to. */
    test_case(
      "floats normalize to their printed form",
      `Quick,
      () => {
        let f = 1.72128496811e-05;
        let printed = float_of_string(Language.Atom.to_literal(Float(f)));
        check(
          exp_eq,
          "float through to_literal",
          Canonicalize.exp(Fresh.Exp.float(printed)),
          Canonicalize.exp(Fresh.Exp.float(f)),
        );
      },
    ),
    test_case("DynamicErrorHole unwraps", `Quick, () => {
      check(
        exp_eq,
        "hole around 1",
        Fresh.Exp.int(1),
        Canonicalize.exp(
          Fresh.Exp.dynamic_error_hole(
            Fresh.Exp.int(1),
            InvalidOperationError.DivideByZero,
          ),
        ),
      )
    }),
    test_case("ExplicitNonlabel exp → Deferral", `Quick, () => {
      check(
        exp_eq,
        "_",
        Fresh.Exp.deferral(OutsideAp),
        Canonicalize.exp(Fresh.Exp.explicit_non_label()),
      )
    }),
    test_case("ExplicitNonlabel pat → Wild", `Quick, () => {
      check(
        pat_eq,
        "_",
        Fresh.Pat.wild(),
        Canonicalize.pat(Fresh.Pat.explicit_non_label()),
      )
    }),
    test_case(
      "Ap with deferral → DeferredAp",
      `Quick,
      () => {
        let f = Fresh.Exp.var("f");
        let d = Fresh.Exp.deferral(OutsideAp);
        check(
          exp_eq,
          "f(_)",
          Fresh.Exp.deferred_ap(f, [Fresh.Exp.deferral(InAp)]),
          Canonicalize.exp(Fresh.Exp.ap(Forward, f, d)),
        );
      },
    ),
    test_case("singleton unlabeled tuple → (_=e)", `Quick, () => {
      check(
        exp_eq,
        "(x,)",
        Fresh.Exp.tuple([
          Fresh.Exp.tup_label(
            Fresh.Exp.explicit_non_label(),
            Fresh.Exp.var("x"),
          ),
        ]),
        Canonicalize.exp(Fresh.Exp.tuple([Fresh.Exp.var("x")])),
      )
    }),
    test_case("Fun name is stripped", `Quick, () => {
      check(
        exp_eq,
        "fun x -> x",
        Fresh.Exp.fn(Fresh.Pat.var("x"), Fresh.Exp.var("x"), None, None),
        Canonicalize.exp(
          Fresh.Exp.fn(
            Fresh.Pat.var("x"),
            Fresh.Exp.var("x"),
            None,
            Some("f"),
          ),
        ),
      )
    }),
    test_case(
      "unlabeled field `_=e` keeps ExplicitNonlabel",
      `Quick,
      () => {
        let field =
          Fresh.Exp.tup_label(
            Fresh.Exp.explicit_non_label(),
            Fresh.Exp.int(1),
          );
        check(
          exp_eq,
          "(_=1)",
          Fresh.Exp.tuple([field]),
          Canonicalize.exp(field),
        );
      },
    ),
    /* A singleton unlabeled tuple becomes `(_=q)`, and as a tuple field it
       keeps its parens, as the printer does: `((_=q), _)`. */
    test_case(
      "singleton unlabeled tuple in field position stays a tuple",
      `Quick,
      () => {
        let p =
          Fresh.Pat.tuple([
            Fresh.Pat.tuple([Fresh.Pat.var("q")]),
            Fresh.Pat.wild(),
          ]);
        check(
          pat_eq,
          "((_=q), _)",
          Fresh.Pat.tuple([
            Fresh.Pat.tuple([
              Fresh.Pat.tup_label(
                Fresh.Pat.explicit_non_label(),
                Fresh.Pat.var("q"),
              ),
            ]),
            Fresh.Pat.wild(),
          ]),
          Canonicalize.pat(p),
        );
      },
    ),
    /* A singleton labeled tuple the term already had is not flattened into its
       parent: print+parse keeps `((h=_), _)` nested. Only the wrap added for a
       bare `lab=e` is undone. */
    test_case(
      "nested singleton labeled tuple survives in field position",
      `Quick,
      () => {
        let inner =
          Fresh.Pat.tuple([
            Fresh.Pat.tup_label(Fresh.Pat.label("h"), Fresh.Pat.wild()),
          ]);
        let p = Fresh.Pat.tuple([inner, Fresh.Pat.wild()]);
        check(pat_eq, "((h=_), _)", p, Canonicalize.pat(p));
      },
    ),
    test_case(
      "bare TupLabel → Tuple([TupLabel])",
      `Quick,
      () => {
        let tl =
          Fresh.Exp.tup_label(Fresh.Exp.label("a"), Fresh.Exp.int(1));
        check(exp_eq, "a=1", Fresh.Exp.tuple([tl]), Canonicalize.exp(tl));
        check(
          exp_eq,
          "(a=1, b=2) stays a tuple of fields",
          Fresh.Exp.tuple([
            Fresh.Exp.tup_label(Fresh.Exp.label("a"), Fresh.Exp.int(1)),
            Fresh.Exp.tup_label(Fresh.Exp.label("b"), Fresh.Exp.int(2)),
          ]),
          Canonicalize.exp(
            Fresh.Exp.tuple([
              Fresh.Exp.tup_label(Fresh.Exp.label("a"), Fresh.Exp.int(1)),
              Fresh.Exp.tup_label(Fresh.Exp.label("b"), Fresh.Exp.int(2)),
            ]),
          ),
        );
      },
    ),
    test_case(
      "unlabeled pat field `_=p` keeps ExplicitNonlabel",
      `Quick,
      () => {
        let field =
          Fresh.Pat.tup_label(
            Fresh.Pat.explicit_non_label(),
            Fresh.Pat.var("x"),
          );
        check(
          pat_eq,
          "(_=x)",
          Fresh.Pat.tuple([field]),
          Canonicalize.pat(field),
        );
      },
    ),
    test_case(
      "bare typ TupLabel → Prod",
      `Quick,
      () => {
        let tl = Fresh.Typ.tup_label(Fresh.Typ.label("a"), Fresh.Typ.int());
        check(typ_eq, "a=Int", Fresh.Typ.prod([tl]), Canonicalize.typ(tl));
      },
    ),
    test_case(
      "Dot of TupLabel → MultiHole field",
      `Quick,
      () => {
        let tl =
          Fresh.Exp.tup_label(Fresh.Exp.label("a"), Fresh.Exp.tuple([]));
        /* The field reparses as the parenthesized singleton tuple. */
        let expected =
          Fresh.Exp.dot(
            Fresh.Exp.deferral(OutsideAp),
            Fresh.Exp.multi_hole([Exp(Fresh.Exp.tuple([tl]))]),
          );
        check(
          exp_eq,
          "_ . (a=()) from bare TupLabel",
          expected,
          Canonicalize.exp(
            Fresh.Exp.dot(Fresh.Exp.deferral(OutsideAp), tl),
          ),
        );
        check(
          exp_eq,
          "_ . (a=()) from Conversion MultiHole",
          expected,
          Canonicalize.exp(
            Fresh.Exp.dot(
              Fresh.Exp.deferral(OutsideAp),
              Fresh.Exp.multi_hole([Exp(tl)]),
            ),
          ),
        );
      },
    ),
    test_case("Dot of Var → Label", `Quick, () => {
      check(
        exp_eq,
        "m.x",
        Fresh.Exp.dot(Fresh.Exp.var("m"), Fresh.Exp.label("x")),
        Canonicalize.exp(
          Fresh.Exp.dot(Fresh.Exp.var("m"), Fresh.Exp.var("x")),
        ),
      )
    }),
    test_case(
      "BadEntry(Var) sum term → Variant",
      `Quick,
      () => {
        let bad = ConstructorMap.BadEntry(Fresh.Typ.var("a"));
        let variant =
          ConstructorMap.Variant("a", ConstructorMap.empty_variant_ann, None);
        check(
          typ_eq,
          "+ a",
          Fresh.Typ.sum([variant]),
          Canonicalize.typ(Fresh.Typ.sum([bad])),
        );
      },
    ),
    /* Its arguments are not tuple fields to wrap: `f(a=1, _)` keeps a bare
       labeled argument, as both parsers read it. */
    test_case(
      "A labeled argument to a partial application stays bare", `Quick, () =>
      switch (Haz3lcore.Parser.to_term("f(a=1, _)", ~root=Exp)) {
      | Some(e) =>
        switch (Canonicalize.exp(e).term) {
        | DeferredAp(_, [{term: TupLabel(_), _}, _]) => ()
        | _ =>
          fail(
            "expected f(a=1, _), got "
            ++ Language.Exp.show(Canonicalize.exp(e)),
          )
        }
      | None => fail("Failed to parse")
      }
    ),
    /* A wrap must not give two nodes one id: a property looking nodes up
       by id would see only one of them. */
    test_case("Wrapping a field keeps ids distinct", `Quick, () =>
      List.iter(
        ~f=
          ((name, e)) => {
            let ids = ref([]);
            let _ =
              Grammar.map_exp_annotation(
                (a: IdTagged.IdTag.t) => {
                  ids := a.ids @ ids^;
                  a;
                },
                Canonicalize.exp(e),
              );
            check(
              int,
              name,
              List.length(List.dedup_and_sort(~compare=Id.compare, ids^)),
              List.length(ids^),
            );
          },
        Fresh.Exp.[
          ("(1)", tuple([int(1)])),
          ("a=1", tup_label(label("a"), int(1))),
          ("(a=1, 2)", tuple([tup_label(label("a"), int(1)), int(2)])),
          ("x.(1)", dot(var("x"), int(1))),
        ],
      )
    ),
  ],
);
