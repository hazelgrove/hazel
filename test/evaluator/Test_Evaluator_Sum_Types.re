open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.SumTypes",
  [
    test_case("Ascribed constructor", `Quick, () => {
      evaluation_test(
        {|A :(+A +B +C)|},
        constructor(
          "A",
          Some(
            Some(
              Typ.(
                sum([
                  Variant("A", ConstructorMap.empty_variant_ann, None),
                  Variant("B", ConstructorMap.empty_variant_ann, None),
                  Variant("C", ConstructorMap.empty_variant_ann, None),
                ])
              ),
            ),
          ),
        ),
        elaborate(parse_exp({|A :(+A +B +C)|})),
      )
    }),
    test_case(
      "Constructors can pass through consistent ascriptions", `Quick, () => {
      evaluation_test(
        {|A : (+A +B) : (+A + ?)|},
        constructor(
          "A",
          Some(
            Some(
              Typ.(
                sum([
                  Variant("A", ConstructorMap.empty_variant_ann, None),
                  Variant("B", ConstructorMap.empty_variant_ann, None),
                ])
              ),
            ),
          ),
        ),
        elaborate(parse_exp({|A : (+A +B) : (+A + ?)|})),
      )
    }),
    test_case(
      "Constructors don't pass through inconsistent ascriptions", `Quick, () => {
      evaluation_test(
        {|A : (+A +B) : (+A +C)|},
        asc(
          constructor(
            "A",
            Some(
              Some(
                Typ.(
                  sum([
                    Variant("A", ConstructorMap.empty_variant_ann, None),
                    Variant("B", ConstructorMap.empty_variant_ann, None),
                  ])
                ),
              ),
            ),
          ),
          Typ.(
            sum([
              Variant("A", ConstructorMap.empty_variant_ann, None),
              Variant("C", ConstructorMap.empty_variant_ann, None),
            ])
          ),
        ),
        elaborate(parse_exp({|A : (+A +B) : (+A +C)|})),
      )
    }),
    test_case("Invalid constructor match", `Quick, () => {
      evaluation_test(
        "let T = 1 in ?",
        let_(
          Pat.(
            constructor(
              "T",
              Some(
                Some(
                  Typ.sum([
                    Variant("T", ConstructorMap.empty_variant_ann, None),
                    BadEntry(Typ.unknown(Internal)),
                  ]),
                ),
              ),
            )
          ),
          int(1),
          empty_hole(),
        ),
        elaborate(
          let_(Pat.(constructor("T", Some(None))), int(1), empty_hole()),
        ),
      )
    }),
    test_case(
      "Historical unboxing failures",
      `Quick,
      () => {
        evaluation_test(
          "Indet when unboxing constructor with payload without payload",
          let_(
            Pat.(
              asc(
                constructor(
                  "B",
                  Some(
                    Some(
                      Typ.(
                        arrow(
                          unknown(Hole(EmptyHole)),
                          sum([
                            Variant(
                              "B",
                              ConstructorMap.empty_variant_ann,
                              Some(unknown(Hole(EmptyHole))),
                            ),
                          ]),
                        )
                      ),
                    ),
                  ),
                ),
                Typ.(
                  sum([
                    Variant(
                      "B",
                      ConstructorMap.empty_variant_ann,
                      Some(unknown(Hole(EmptyHole))),
                    ),
                  ])
                ),
              )
            ),
            empty_hole(),
            empty_hole(),
          ),
          elaborate(parse_exp("let B : (+B(?)) = ? in ?")),
        );
        evaluation_test(
          "Indet when unboxing constructor as list",
          let_(
            Pat.list_lit([]),
            constructor("On", Some(Some(Typ.(list(unknown(SynSwitch)))))), // This type on the constructor can't be right
            empty_hole(),
          ),
          elaborate(parse_exp("type g = + On in let [] = On in")),
        );
        evaluation_test(
          "Indet when unboxing constructor as cons",
          let_(
            Pat.(cons(wild(), list_lit([]))),
            constructor("B", Some(Some(Typ.(list(unknown(SynSwitch)))))), // This type on the constructor can't be right
            empty_hole(),
          ),
          elaborate(parse_exp("let (_:: []) = type y = + B in B in ?")),
        );
        evaluation_test(
          "Indet when unboxing constructor as bool",
          if_(
            constructor("Baz", Some(Some(Typ.bool()))),
            bool(false),
            constructor("Qux", Some(None)),
          ),
          elaborate(
            parse_exp("type y = + Baz(Float) in if Baz then false else Qux"),
          ),
        );
        evaluation_test(
          "Indet when unboxing constructor as tuple",
          let_(
            Pat.tuple([]),
            constructor("Qux", Some(Some(Typ.(prod([]))))),
            empty_hole(),
          ),
          elaborate(parse_exp("let () = type x = + Qux in Qux in ?")),
        );
        evaluation_test(
          "Indet when unboxing constructor as typfun",
          typ_ap(
            constructor(
              "B",
              Some(
                Some(Typ.(poly(TPat.empty_hole(), unknown(SynSwitch)))),
              ),
            ),
            Typ.unknown(Hole(EmptyHole)),
          ),
          elaborate(
            parse_exp("type y = + B in case true | a => B end @<?>"),
          ),
        );
        evaluation_test(
          "Indet when unboxing constructor as float",
          let_(
            Pat.(
              constructor(
                "A",
                Some(
                  Some(
                    Typ.(
                      arrow(
                        float(),
                        sum([
                          Variant(
                            "A",
                            ConstructorMap.empty_variant_ann,
                            Some(float()),
                          ),
                        ]),
                      )
                    ),
                  ),
                ),
              )
            ),
            var("a"),
            int(0),
          ),
          elaborate(parse_exp("type x = + A(Float) in let A = a in 0")),
        );
        evaluation_test(
          {|Indet when unboxing constructor as string|},
          bin_op(
            String(Concat),
            string(""),
            constructor("A", Some(Some(Typ.string()))),
          ),
          elaborate(parse_exp({|type y = + A in ""++A|})),
        );
        evaluation_test(
          "Indet when unboxing constructor as int",
          un_op(Int(Minus), constructor("A", Some(Some(Typ.int())))),
          elaborate(parse_exp("type y = + A in -A")),
        );
      },
    ),
    /* Builtin constructor type compactness tests.
     *
     * The elaborator intentionally keeps constructor type annotations for
     * builtin sum types (HTML, Attr, Sub, Cmd) in compact Var form rather
     * than expanded Rec form. For example, the Text constructor's type is
     * stored as Arrow(String, Var("HTML")) rather than
     * Arrow(String, Rec("HTML", Sum(47 constructors...))).
     *
     * This compactness is critical for post-eval statics performance:
     * it enables the O(1) Var-Var fast path in Typ.meet, avoiding
     * expensive O(n^2) structural comparison of large sum types.
     * Without it, post-eval statics on full HTML apps takes ~2s instead
     * of ~4ms (a 500x difference).
     *
     * The elaborator's compact_builtin_recs function handles this by
     * replacing Rec("HTML",...) back to Var("HTML") when the Rec came
     * from a meet result that expanded the Var. Ascriptions.re resolves
     * these Var references lazily via weak_head_normalize when it needs
     * the structural form during evaluation.
     *
     * These tests guard against regressions: if constructor annotations
     * regress to expanded Rec form, post-eval statics performance will
     * degrade dramatically on programs using builtin sum types.
     */
    test_case(
      "Builtin constructor annotation uses Var, not Rec (no payload)",
      `Quick,
      () => {
        /* Br is a no-payload HTML constructor. Its elaborated type
           annotation should be Var("HTML"), not Rec("HTML", Sum(...)). */
        let elaborated = elaborate(parse_exp("Br"));
        let has_compact_annotation =
          switch (elaborated.term) {
          | Constructor(_, Some(Some(ty))) =>
            switch (Language.Typ.term_of(ty)) {
            | Var("HTML") => true
            | _ => false
            }
          | _ => false
          };
        Alcotest.(check(bool, "Br annotation is Var(\"HTML\")", true, has_compact_annotation));
      },
    ),
    test_case(
      "Builtin constructor annotation uses Var, not Rec (with payload)",
      `Quick,
      () => {
        /* Text("hello") is an HTML constructor with a String payload.
           Its elaborated type annotation should be Arrow(String, Var("HTML")),
           not Arrow(String, Rec("HTML", Sum(...))). */
        let elaborated = elaborate(parse_exp({|Text("hello")|}));
        /* The elaborated form is Ap(Forward, Constructor("Text", ...), "hello").
           We need to find the Constructor node inside the Ap. */
        let ctr_type =
          switch (elaborated.term) {
          | Ap(_, {term: Constructor(_, Some(Some(ty))), _}, _) => Some(ty)
          | Constructor(_, Some(Some(ty))) => Some(ty)
          | _ => None
          };
        let has_compact_annotation =
          switch (ctr_type) {
          | Some(ty) =>
            switch (Language.Typ.term_of(ty)) {
            | Arrow(_, ret) =>
              switch (Language.Typ.term_of(ret)) {
              | Var("HTML") => true
              | _ => false
              }
            | _ => false
            }
          | None => false
          };
        Alcotest.(check(bool, "Text annotation return type is Var(\"HTML\")", true, has_compact_annotation));
      },
    ),
    test_case(
      "Builtin constructor compactness survives evaluation",
      `Quick,
      () => {
        /* After evaluation, the constructor annotation should still use
           Var form. This tests the full elaborate→evaluate pipeline. */
        let evaluated =
          evaluate(elaborate(parse_exp("Br")));
        let has_compact_annotation =
          switch (evaluated.term) {
          | Constructor(_, Some(Some(ty))) =>
            switch (Language.Typ.term_of(ty)) {
            | Var("HTML") => true
            | _ => false
            }
          | _ => false
          };
        Alcotest.(check(bool, "Br annotation is Var(\"HTML\") after eval", true, has_compact_annotation));
      },
    ),
  ],
);
