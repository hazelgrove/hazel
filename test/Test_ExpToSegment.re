open Alcotest;
open Haz3lcore;
open Language;
open Base;
open EditingPrelude;

let exp_to_segment_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_filters: true,
  show_unknown_as_hole: true,
};

let exp_to_segment =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings);

let equivalent_to_make_term = (serialized: string) => {
  switch (Parser.to_term(serialized), Parser.to_segment(serialized)) {
  | (Some(exp), Some(seg)) =>
    check(
      string,
      "Make term text equivalent: " ++ serialized,
      serialized,
      print_seg(seg),
    );
    print_seg(exp_to_segment(exp)) |> print_endline;
    check(
      segment,
      "Make term segments equivalent: " ++ serialized,
      seg,
      exp_to_segment(exp),
    );
  | _ => Alcotest.fail("Failed to parse term")
  };
};

let type_equivalent_to_make_term = (type_serialized: string) => {
  let expr_serialized = "1:" ++ type_serialized;
  equivalent_to_make_term(expr_serialized);
};

module TempGrammar =
  Grammar.Factory({
    type t = IdTagged.IdTag.t;
    let default_value: unit => IdTagged.IdTag.t =
      () => {
        ids: [Id.invalid],
        secondary: IdTagged.IdTag.empty_secondary,
      };
  });
let tests = (
  "ExpToSegment",
  [
    test_case(
      "Literals",
      `Quick,
      () => {
        open TempGrammar.Exp;
        check(
          segment,
          "Integer",
          [
            Tile({
              id: Id.invalid,
              label: ["1"],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
          ],
          exp_to_segment(int(1)),
        );
        check(
          segment,
          "String",
          [
            Tile({
              id: Id.invalid,
              label: ["\"hello\""],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
          ],
          exp_to_segment(string("hello")),
        );
      },
    ),
    test_case(
      "Negative ints",
      `Quick,
      () => {
        let _ = Alcotest.skip(); // TODO: Fix negative ints
        equivalent_to_make_term("-1");
      },
    ),
    test_case(
      "Empty Ids on ExpToSegment constructor",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        open Exp;
        let segment =
          exp_to_segment(
            let_(
              Pat.(
                asc(list_lit([]), Typ.(sum([Variant("Jg", ConstructorMap.empty_variant_ann, None)])))
              ),
              empty_hole(),
              empty_hole(),
            ),
          );
        let serialized = print_seg(segment);

        check(
          Alcotest.string,
          "ascribed sum type constructor in pattern",
          "let []:(+ Jg) = ? in ?",
          serialized,
        );
      },
    ),
    test_case(
      "Tuple",
      `Quick,
      () => {
        open TempGrammar.Exp;
        check(
          segment,
          "Unit",
          [
            Tile({
              id: Id.invalid,
              label: ["()"],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
          ],
          exp_to_segment(tuple([])),
        );
        check(
          option(segment),
          "2-ary",
          Parser.to_segment("(1, 2)"),
          Some(exp_to_segment(tuple([int(1), int(2)]))),
        );
      },
    ),
    test_case(
      "Basic Labeled Tuples",
      `Quick,
      () => {
        open TempGrammar.Exp;

        check(
          option(segment),
          "Singleton Labeled",
          Parser.to_segment("(x=1)"),
          Some(exp_to_segment(tuple([tup_label(label("x"), int(1))]))),
        );
        equivalent_to_make_term({|(x=1, y=2)|});
      },
    ),
    test_case("Labels in types with single quotes", `Quick, () => {
      equivalent_to_make_term({|type t = (``=Int, ab=String) in 7|})
    }),
    test_case("Labels in  patterns with single quotes", `Quick, () => {
      equivalent_to_make_term({|fun (``=a, ab=_) -> 3|})
    }),
    test_case("Function call with label arguments", `Quick, () => {
      equivalent_to_make_term({|omit_labels((a=1), `a`)|})
    }),
    test_case("Doc page labeled tuple example", `Quick, () => {
      equivalent_to_make_term(
        {|let labeled_tuple = (a=1, b=2.000000, c=true) in let prj_a = labeled_tuple.a in prj_a|},
      )
    }),
    test_case(
      "Match statement",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        open Exp;
        let segment =
          exp_to_segment(
            match(
              var("x"),
              [
                (Pat.(constructor("A", None)), int(1)),
                (Pat.(constructor("B", None)), int(2)),
              ],
            ),
          );
        let serialized = print_seg(segment);

        check(
          Alcotest.string,
          "Match statement",
          "case x | A => 1| B => 2 end",
          serialized,
        );
      },
    ),
    test_case(
      "Deferred application",
      `Quick,
      () => {
        let segment =
          IdTagged.FreshGrammar.Exp.(
            exp_to_segment(
              deferred_ap(
                var("string_sub"),
                [string("hello"), int(1), deferral(InAp)],
              ),
            )
          );
        let serialized = print_seg(segment);

        check(
          string,
          "deferral in application",
          {|string_sub("hello", 1, _)|},
          serialized,
        );
      },
    ),
    test_case(
      "Test",
      `Quick,
      () => {
        let segment =
          IdTagged.FreshGrammar.Exp.(exp_to_segment(test(bool(true))));
        let serialized = print_seg(segment);

        check(string, "Test of true", {|test true end|}, serialized);
      },
    ),
    test_case(
      "Filter",
      `Quick,
      () => {
        let segment =
          exp_to_segment(
            IdTagged.FreshGrammar.Exp.(
              filter(
                Filter({
                  pat: int(1),
                  act: (Step, One),
                }),
                int(2),
              )
            ),
          );
        let serialized = print_seg(segment);

        check(string, "Pause", serialized, {|pause 1 in 2|});
      },
    ),
    test_case(
      "Right associativity",
      `Quick,
      () => {
        check(
          string,
          "No parens",
          print_seg(
            exp_to_segment(
              IdTagged.FreshGrammar.Exp.(
                bin_op(
                  Int(Power),
                  int(2),
                  bin_op(Int(Power), int(3), int(4)),
                )
              ),
            ),
          ),
          {|2 ** 3 ** 4|},
        );
        check(
          string,
          "Parens",
          print_seg(
            exp_to_segment(
              IdTagged.FreshGrammar.Exp.(
                bin_op(
                  Int(Power),
                  bin_op(Int(Power), int(2), int(3)),
                  int(4),
                )
              ),
            ),
          ),
          {|(2 ** 3) ** 4|},
        );
        check(
          string,
          "Arrow types",
          print_seg(
            exp_to_segment(
              IdTagged.FreshGrammar.(
                Exp.ty_alias(
                  TPat.(var("x")),
                  Typ.(arrow(arrow(int(), bool()), var("x"))),
                  Exp.(int(1)),
                )
              ),
            ),
          ),
          {|type x = (Int -> Bool) -> x in 1|},
        );
      },
    ),
    test_case("Unit type", `Quick, () => {
      check(
        string,
        "Unit type",
        "()",
        print_seg(
          ExpToSegment.typ_to_segment(
            ~settings=exp_to_segment_settings,
            IdTagged.FreshGrammar.Typ.prod([]),
          ),
        ),
      )
    }),
    test_case("Function call", `Quick, () => {
      equivalent_to_make_term("a(1, 2)")
    }),
    test_case("Unit pattern", `Quick, () => {
      check(
        string,
        "Unit pattern",
        "()",
        print_seg(
          ExpToSegment.any_to_segment(
            ~settings=exp_to_segment_settings,
            Pat(IdTagged.FreshGrammar.Pat.tuple([])),
          ),
        ),
      )
    }),
    test_case("Dot operator on float", `Quick, () => {
      check(
        string,
        "",
        {|1.230000 . 4.560000|},
        print_seg(
          exp_to_segment(
            IdTagged.FreshGrammar.Exp.(dot(float(1.23), float(4.56))),
          ),
        ),
      )
    }),
    test_case("ProdProjection - basic product type", `Quick, () => {
      type_equivalent_to_make_term("(a=Int, b=String).a")
    }),
    test_case("ProdProjection - empty label", `Quick, () => {
      type_equivalent_to_make_term("(``=Int).``")
    }),
    test_case("ProdProjection - label with spaces", `Quick, () => {
      type_equivalent_to_make_term(
        "(`label with spaces`=Int).`label with spaces`",
      )
    }),
    test_case("ProdProjection - type variable", `Quick, () => {
      type_equivalent_to_make_term("t.a")
    }),
    test_case("ProdExtension - product types", `Quick, () => {
      type_equivalent_to_make_term("(a=Int) ... (b=String)")
    }),
    test_case("ProdExtension - type variables", `Quick, () => {
      type_equivalent_to_make_term("t ... u")
    }),
    test_case("ProdExtension - with special labels", `Quick, () => {
      type_equivalent_to_make_term(
        "(``=Int) ... (`label with spaces`=String)",
      )
    }),
    test_case("Singleton unlabeled tuple", `Quick, () =>
      check(
        string,
        "Singleton unlabeled tuple",
        "(_=1)",
        print_seg(
          exp_to_segment(IdTagged.FreshGrammar.Exp.(tuple([int(1)]))),
        ),
      )
    ),
    test_case("Singleton unlabeled tuple type", `Quick, () =>
      check(
        string,
        "Singleton unlabeled tuple type",
        "(_=Int)",
        print_seg(
          ExpToSegment.typ_to_segment(
            ~settings=exp_to_segment_settings,
            IdTagged.FreshGrammar.Typ.(prod([int()])),
          ),
        ),
      )
    ),
  ],
);

/* Round-trip tests: Segment → Term → Segment
   These tests verify that secondary (whitespace/comments) is preserved
   when converting between segments and terms using PreserveExact mode.
   See plans/secondary-in-terms-v2.md for design details. */

let exp_to_segment_roundtrip_settings: ExpToSegment.Settings.t = {
  secondary: PreserveExact,
  inline: true, /* ignored when secondary = PreserveExact */
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_filters: true,
  show_unknown_as_hole: true,
};

let exp_to_segment_roundtrip =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_roundtrip_settings);

/* Test that a string round-trips through segment → term → segment */
let roundtrip_test = (name: string, input: string) =>
  test_case(name, `Quick, () => {
    switch (Parser.to_term(input), Parser.to_segment(input)) {
    | (Some(term), Some(seg)) =>
      let seg' = exp_to_segment_roundtrip(term);
      let input' = print_seg(seg');
      check(string, {|Round-trip text|}, input, input');
      check(segment, {|Round-trip segments|}, seg, seg');
    | _ => Alcotest.fail({|Failed to parse|})
    }
  });

let roundtrip_tests = (
  "Secondary Round-Trip",
  [
    /* Simple atoms */
    roundtrip_test({|Integer literal|}, {|42|}),
    roundtrip_test({|Negative int|}, {|-42|}),
    roundtrip_test({|Variable|}, {|x|}),
    roundtrip_test({|String literal|}, {|"hello"|}),
    roundtrip_test({|Float literal|}, {|3.140000|}),
    roundtrip_test({|Boolean literal|}, {|true|}),
    /* Binary operations */
    roundtrip_test({|Binary op: standard spacing|}, {|1 + 2|}),
    roundtrip_test({|Binary op: no spaces|}, {|1+2|}),
    roundtrip_test({|Binary op: extra spaces|}, {|1  +  2|}),
    /* Chained binary operations - tests selective collection */
    roundtrip_test({|Binary op: chained standard|}, {|1 + 2 + 3|}),
    roundtrip_test({|Binary op: chained compact|}, {|1+2+3|}),
    roundtrip_test({|Binary op: chained mixed|}, {|1 +2+ 3|}),
    roundtrip_test({|Binary op: chained 4 terms|}, {|1 + 2 + 3 + 4|}),
    /* Prefix operators */
    roundtrip_test({|Prefix: negation|}, {|-x|}),
    roundtrip_test({|Prefix: negation with space|}, {|- x|}),
    roundtrip_test({|Prefix: not|}, {|!x|}),
    roundtrip_test({|Prefix: not with space|}, {|! x|}),
    /* Mixed prefix and binary */
    roundtrip_test({|Mixed: prefix then binary|}, {|-x + y|}),
    roundtrip_test({|Mixed: binary then prefix|}, {|x + -y|}),
    roundtrip_test({|Mixed: not and binary|}, {|!x && y|}),
    roundtrip_test({|Mixed: complex prefix/binary|}, {|a + !b * -c|}),
    /* Let expressions */
    roundtrip_test({|Let: standard|}, {|let x = 1 in x|}),
    roundtrip_test({|Let: compact|}, {|let x=1 in x|}),
    roundtrip_test({|Let: newline in body|}, {|let x = 1 in
x|}),
    roundtrip_test(
      {|Let: nested standard|},
      {|let x = 1 in let y = 2 in x + y|},
    ),
    roundtrip_test({|Let: nested compact|}, {|let x=1 in let y=2 in x+y|}),
    /* Multiline let expressions */
    roundtrip_test({|Let: multiline def|}, {|let x =
1 in x|}),
    roundtrip_test({|Let: multiline full|}, {|let x =
1
in
x|}),
    /* Tuples */
    roundtrip_test({|Tuple: standard|}, {|(1, 2, 3)|}),
    roundtrip_test({|Tuple: compact|}, {|(1,2,3)|}),
    roundtrip_test({|Tuple: extra spaces|}, {|(1 , 2 , 3)|}),
    roundtrip_test({|Tuple: unit|}, {|()|}),
    roundtrip_test({|Tuple: spaces before commas|}, {|(1 , 2)|}),
    /* List literals */
    roundtrip_test({|List: standard|}, {|[1, 2, 3]|}),
    roundtrip_test({|List: compact|}, {|[1,2,3]|}),
    roundtrip_test({|List: empty|}, {|[]|}),
    roundtrip_test({|List: spaces before commas|}, {|[1 , 2 , 3]|}),
    /* Functions */
    roundtrip_test({|Function: standard|}, {|fun x -> x|}),
    roundtrip_test({|Function: compact|}, {|fun x->x|}),
    roundtrip_test({|Function: with body spaces|}, {|fun x ->  x|}),
    roundtrip_test({|Function: multiline body|}, {|fun x ->
x|}),
    /* Case expressions */
    roundtrip_test({|Case: single line|}, {|case x | A => 1 end|}),
    roundtrip_test(
      {|Case: multiple clauses|},
      {|case x | A => 1| B => 2 end|},
    ),
    roundtrip_test({|Case: multiline|}, {|case x
| A => 1
| B => 2
end|}),
    /* Type annotations */
    roundtrip_test({|Ascription: standard|}, {|1:Int|}),
    roundtrip_test({|Ascription: with spaces|}, {|1 : Int|}),
    /* Type aliases */
    roundtrip_test({|Type alias: standard|}, {|type t = Int in 1|}),
    roundtrip_test({|Type alias: compact|}, {|type t=Int in 1|}),
    roundtrip_test({|Type alias: multiline|}, {|type t = Int in
1|}),
    /* If expressions */
    roundtrip_test({|If: standard|}, {|if true then 1 else 2|}),
    roundtrip_test({|If: compact|}, {|if true then 1 else 2|}),
    roundtrip_test({|If: multiline|}, {|if true
then 1
else 2|}),
    /* Nested expressions */
    roundtrip_test({|Nested: parens|}, {|((1 + 2))|}),
    roundtrip_test(
      {|Nested: complex standard|},
      {|let f = fun x -> x + 1 in f(42)|},
    ),
    roundtrip_test(
      {|Nested: complex compact|},
      {|let f=fun x->x+1 in f(42)|},
    ),
    roundtrip_test({|Nested: deeply nested ops|}, {|((1 + 2) * (3 - 4))|}),
    /* Application */
    roundtrip_test({|Application: standard|}, {|f(x)|}),
    roundtrip_test({|Application: multiple args|}, {|f(x, y, z)|}),
    roundtrip_test({|Application: with spaces|}, {|f( x , y )|}),
    /* Complex mixed expressions */
    roundtrip_test(
      {|Complex: let with binop body|},
      {|let x = 1 + 2 in x * 3|},
    ),
    roundtrip_test({|Complex: function returning binop|}, {|fun x -> x + 1|}),
    roundtrip_test(
      {|Complex: if with binop|},
      {|if x > 0 then x + 1 else x - 1|},
    ),
    /* Multiline complex */
    roundtrip_test(
      {|Complex: multiline let chain|},
      {|let x = 1 in
let y = 2 in
x + y|},
    ),
    roundtrip_test(
      {|Complex: multiline function|},
      {|let f = fun x ->
  x + 1
in f(42)|},
    ),
    /* Sum types - now supported with ConstructorMap.variant_ann storing secondary */
    roundtrip_test({|Sum type: single constructor|}, {|type T = +A in T|}),
    roundtrip_test({|Sum type: two constructors|}, {|type T = +A + B in T|}),
    roundtrip_test({|Sum type: with args|}, {|type T = +A(Int) + B in T|}),
    roundtrip_test({|Sum type: spaced|}, {|type T = + A + B in T|}),
    roundtrip_test({|Sum type: compact|}, {|type T = +A+B in T|}),
    /* Filter expressions (hide/eval/pause/debug ... in) and unquote ($) */
    roundtrip_test({|Filter: hide|}, {|hide 1 in 2|}),
    roundtrip_test({|Filter: hide spaced|}, {|hide 1  in  2|}),
    roundtrip_test({|Filter: eval|}, {|eval 1 in 2|}),
    roundtrip_test({|Filter: eval spaced|}, {|eval 1  in  2|}),
    roundtrip_test({|Filter: pause|}, {|pause 1 in 2|}),
    roundtrip_test({|Filter: pause spaced|}, {|pause 1  in  2|}),
    roundtrip_test({|Filter: debug|}, {|debug 1 in 2|}),
    roundtrip_test({|Filter: debug spaced|}, {|debug 1  in  2|}),
    /* Unquote ($) - used within filter expressions for stepper */
    roundtrip_test({|Unquote: simple|}, {|eval $x in x|}),
    roundtrip_test({|Unquote: spaced|}, {|eval $ x in x|}),
    roundtrip_test({|Unquote: in hide|}, {|hide $1 in 2|}),
    /* Quoted labels - backticks are not preserved through ExpToSegment currently.
       Labels without special chars work; labels requiring backticks lose them.
       See QuotedLabel in "Forms not yet fully round-tripping" below. */
    roundtrip_test({|QuotedLabel: with spaces works|}, {|(`hello world`=42)|}),
    roundtrip_test({|QuotedLabel: empty works|}, {|(``=1)|}),
    /* Float power operator (**.) - using normalized float format */
    roundtrip_test({|FPower: standard|}, {|2.000000 **. 3.000000|}),
    roundtrip_test({|FPower: compact|}, {|2.000000**.3.000000|}),
    roundtrip_test({|FPower: extra spaces|}, {|2.000000  **.  3.000000|}),
    /* Test expressions (test ... end) */
    roundtrip_test({|Test: simple|}, {|test true end|}),
    roundtrip_test({|Test: with expression|}, {|test 1 == 1 end|}),
    roundtrip_test({|Test: spaced|}, {|test  true  end|}),
    roundtrip_test({|Test: multiline|}, {|test
true
end|}),
    /* Hinted test expressions (hint ... test ... end) */
    roundtrip_test({|HintedTest: simple|}, {|hint 1 test true end|}),
    roundtrip_test({|HintedTest: spaced|}, {|hint  1  test  true  end|}),
    /* Fix expressions (fix ... ->) */
    roundtrip_test({|Fix: simple|}, {|fix f -> f|}),
    roundtrip_test({|Fix: with body|}, {|fix f -> f(1)|}),
    roundtrip_test({|Fix: spaced|}, {|fix f  ->  f|}),
    roundtrip_test({|Fix: compact|}, {|fix f->f|}),
    /* TypFun expressions (typfun ... ->) */
    roundtrip_test({|TypFun: simple|}, {|typfun a -> 1|}),
    /* TypFun with typed body has defensive parens issue (type after :) - same as rec/poly */
    roundtrip_test({|TypFun: spaced|}, {|typfun a  ->  1|}),
    roundtrip_test({|TypFun: compact|}, {|typfun a->1|}),
    /* Use expressions (use ... in) */
    roundtrip_test({|Use: simple|}, {|use Nat in 1|}),
    roundtrip_test({|Use: spaced|}, {|use Nat  in  1|}),
    roundtrip_test({|Use: compact|}, {|use Nat in 1|}),
    /* ProofOf (proof_of ... end) - type-level */
    roundtrip_test({|ProofOf: simple|}, {|1 : proof_of 1 end|}),
    roundtrip_test({|ProofOf: spaced|}, {|1 : proof_of  1  end|}),
    /* ProofObject - SKIPPED: Form.re defines ["proof_object", "indeed"] but
       MakeTerm.re expects ["proof_object", "end"]. ExpToSegment uses Form.re
       labels, so round-trip fails. This is a Form.re bug that needs fixing. */
    /* Theorem expressions (theorem ... = ... in) */
    roundtrip_test({|Theorem: simple|}, {|theorem x = 1 in x|}),
    roundtrip_test({|Theorem: spaced|}, {|theorem x  =  1  in  x|}),
    roundtrip_test({|Theorem: compact|}, {|theorem x=1 in x|}),
  ],
);

/* ============================================================================
   ROUND-TRIP TESTING: SCOPE AND LIMITATIONS
   See plans/secondary-in-terms-v2.md for full details.
   ============================================================================

   === OUT OF SCOPE (not testing) ===

   Projectors and related display features:
   - Projectors/Refractors (^^projector_name syntax)
   - LivelitName (^livelit) - part of projector/livelit system

   Preliminary/experimental syntax:
   - BlockExp ({...}) - preliminary syntax for probe user study

   === KNOWN LIMITATIONS ===

   Defensive Parenthesization (forms with arrow trailing delimiters):
   ExpToSegment adds parentheses for forms like rec/poly/typfun/forall after `:`
   because they share low precedence with their `->` trailing delimiter.
   Related: fun/fix also use `->` but typically don't appear after `:`.
   See Issue #1913 for related edge cases with forall regrouting.
   Examples:
   - `1 : rec t -> t` becomes `1 :( rec t -> t)`
   - `1 : poly a -> a` becomes `1 :( poly a -> a)`
   - `typfun a -> fun x : a -> x` - the inner `: a` gets wrapped

   Other limitations:
   - QuotedLabel: backticks lost for simple labels (`a` becomes a). Labels
     requiring backticks (spaces, empty) work fine.
   - Float literals: normalized to full precision (2.0 becomes 2.000000)
   - ProofObject: Form.re bug says "indeed" but should be "end" - can't test

   === REMAINING WORK (needs investigation) ===

   - Explicit holes (`?`) - special handling in MakeTerm, may need adjustment
   - Grout (convex and concave) - secondary preservation unclear
   - LLMHole (??...??) - similar concerns to explicit holes

   === FORMS NOT YET TESTED ===

   - LogicalOrLegacy (\/) - legacy OR syntax, low priority
   ============================================================================ */

let skip_roundtrip_known_limitation = (name: string, input: string, ~actual: string) =>
  test_case(name, `Quick, () => {
    switch (Parser.to_term(input), Parser.to_segment(input)) {
    | (Some(term), Some(_seg)) =>
      let seg' = exp_to_segment_roundtrip(term);
      let input' = print_seg(seg');
      /* Document the actual output for clarity */
      check(string, {|Actual output (with defensive parens)|}, actual, input');
      Alcotest.skip();
    | _ => Alcotest.fail({|Failed to parse|})
    }
  });

let roundtrip_known_limitations = (
  "Round-Trip Known Limitations (Defensive Parenthesization)",
  [
    /* Rec types after type annotation get wrapped in parens.
       Input:  `1 : rec t -> t`
       Output: `1 :( rec t -> t)`
       The space before `rec` is preserved, but parens are added. */
    skip_roundtrip_known_limitation(
      {|Rec type after ascription|},
      {|1 : rec t -> t|},
      ~actual={|1 :( rec t -> t)|},
    ),
    /* Poly types after type annotation also get wrapped.
       Input:  `1 : poly a -> a`
       Output: `1 :( poly a -> a)` */
    skip_roundtrip_known_limitation(
      {|Poly type after ascription|},
      {|1 : poly a -> a|},
      ~actual={|1 :( poly a -> a)|},
    ),
    /* This issue may also affect function parameters with complex types.
       TODO: Add examples if discovered. */
  ],
);
