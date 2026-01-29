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
            constructor("B", Some(Some(Typ.bool()))),
            bool(false),
            constructor("A", Some(None)),
          ),
          elaborate(
            parse_exp("type y = + B(Float) in if B then false else A"),
          ),
        );
        evaluation_test(
          "Indet when unboxing constructor as tuple",
          let_(
            Pat.tuple([]),
            constructor("A", Some(Some(Typ.(prod([]))))),
            empty_hole(),
          ),
          elaborate(parse_exp("let () = type x = + A in A in ?")),
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
  ],
);
