open Alcotest;
open Haz3lcore;
open Language;
open Base;
open EditingPrelude;

let exp_to_segment_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
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
                asc(
                  list_lit([]),
                  Typ.(
                    sum([
                      Variant("Jg", ConstructorMap.empty_variant_ann, None),
                    ])
                  ),
                )
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
      See plans/secondary-in-terms-v2.md for design details.

      TODO: Coverage gaps to address:
      - TupleExtension (...): Text roundtrip works but segment comparison fails
        due to mold precedence differences (see commented test below)
      - MultiHole: Partially tested via grout tests, but could use explicit tests
      - Invalid: Error syntax handling
      - Deferral: Partial application placeholders (_)
      - DynamicErrorHole: Runtime error markers (not parseable from text)
      - Closure: Runtime closures (not parseable from text)
   */

let exp_to_segment_roundtrip_settings: ExpToSegment.Settings.t = {
  secondary: PreserveExact,
  parenthesization: Structural, /* Don't add defensive parens for round-tripping */
  label_format: QuoteWhenNecessary, /* Only quote labels that need it */
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
    /* Labeled tuples */
    roundtrip_test({|LabeledTuple: standard|}, {|(a=1, b=2)|}),
    roundtrip_test({|LabeledTuple: compact|}, {|(a=1,b=2)|}),
    roundtrip_test({|LabeledTuple: mixed|}, {|(1, a=2, 3)|}),
    /* Projections */
    roundtrip_test({|Projection: simple|}, {|x.a|}),
    roundtrip_test({|Projection: before operator|}, {|x.a == 1|}),
    roundtrip_test({|Projection: before operator spaced|}, {|x.a  ==  1|}),
    roundtrip_test({|Projection: before in|}, {|let y = x.a in y|}),
    /* Function with labeled patterns */
    roundtrip_test({|FunLabeled: simple|}, {|fun name=n -> n|}),
    roundtrip_test({|FunLabeled: spaced|}, {|fun name=n, age=a -> n|}),
    /* List literals */
    roundtrip_test({|List: standard|}, {|[1, 2, 3]|}),
    roundtrip_test({|List: compact|}, {|[1,2,3]|}),
    roundtrip_test({|List: empty|}, {|[]|}),
    roundtrip_test({|List: spaces before commas|}, {|[1 , 2 , 3]|}),
    /* Cons operator (::) */
    roundtrip_test({|Cons: simple|}, {|1 :: []|}),
    roundtrip_test({|Cons: chained|}, {|1 :: 2 :: []|}),
    roundtrip_test({|Cons: compact|}, {|1::[]|}),
    /* List concatenation (@) */
    roundtrip_test({|ListConcat: simple|}, {|[1] @ [2]|}),
    roundtrip_test({|ListConcat: compact|}, {|[1]@[2]|}),
    roundtrip_test({|ListConcat: spaced|}, {|[1]  @  [2]|}),
    /* Sequence (;) */
    roundtrip_test({|Seq: simple|}, {|1; 2|}),
    roundtrip_test({|Seq: compact|}, {|1;2|}),
    roundtrip_test({|Seq: chained|}, {|1; 2; 3|}),
    /* Constructors */
    roundtrip_test({|Constructor: nullary|}, {|None|}),
    roundtrip_test({|Constructor: with arg|}, {|Some(1)|}),
    roundtrip_test({|Constructor: spaced arg|}, {|Some( 1 )|}),
    /* Tuple extension (...) */
    roundtrip_test({|TupleExtension: simple|}, {|(a=1) ... (b=2)|}),
    roundtrip_test({|TupleExtension: compact|}, {|(a=1)...(b=2)|}),
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
    roundtrip_test({|Deferred Application: standard|}, {|f(_ , y)|}),
    roundtrip_test(
      {|Deferred Application: with extra spaces|},
      {|f( _  , y )|},
    ),
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
    /* Sum type without leading + prefix - KNOWN LIMITATION
       Both `A + B` and `+A + B` parse to the same Sum term.
       ExpToSegment always emits the prefixed form.
       See plans/secondary-in-terms-v2.md "Sum type leading + prefix" for options. */
    test_case(
      "Sum type: no leading prefix (SKIP)",
      `Quick,
      () => {
        let _ = Alcotest.skip();
        let input = {|type T = A + B in T|};
        switch (Parser.to_term(input)) {
        | Some(term) =>
          let seg' = exp_to_segment_roundtrip(term);
          let output = print_seg(seg');
          check(string, {|Round-trip text|}, input, output);
        | None => Alcotest.fail({|Failed to parse|})
        };
      },
    ),
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
    roundtrip_test(
      {|QuotedLabel: label needing quotes (has dash)|},
      {|(`the-answer`=42)|},
    ),
    roundtrip_test(
      {|QuotedLabel: with spaces works|},
      {|(`hello world`=42)|},
    ),
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
    /* TypAp expressions (f @<Int>) */
    roundtrip_test({|TypAp: simple|}, {|f@<Int>|}),
    roundtrip_test({|TypAp: with spaces|}, {|f  @<  Int  >|}),
    /* forall expression */
    roundtrip_test({|Forall: simple|}, {|forall a -> 1|}),
    roundtrip_test({|Forall: with spaces|}, {|forall a  ->  1|}),
    roundtrip_test({|Forall: compact|}, {|forall a->1|}),
    /* Use expressions (use ... in) */
    roundtrip_test({|Use: simple|}, {|use Nat in 1|}),
    roundtrip_test({|Use: spaced|}, {|use Nat  in  1|}),
    roundtrip_test({|Use: compact|}, {|use Nat in 1|}),
    /* ProofOf (proof_of ... end) - type-level */
    roundtrip_test({|ProofOf: simple|}, {|1 : proof_of 1 end|}),
    roundtrip_test({|ProofOf: spaced|}, {|1 : proof_of  1  end|}),
    /* ProofObject (proof_object ... end) - expression-level */
    roundtrip_test({|ProofObject: simple|}, {|proof_object 1 end|}),
    roundtrip_test({|ProofObject: spaced|}, {|proof_object  1  end|}),
    roundtrip_test({|ProofObject: with expr|}, {|proof_object 1 + 2 end|}),
    /* Theorem expressions (theorem ... = ... in) */
    roundtrip_test({|Theorem: simple|}, {|theorem x = 1 in x|}),
    roundtrip_test({|Theorem: spaced|}, {|theorem x  =  1  in  x|}),
    roundtrip_test({|Theorem: compact|}, {|theorem x=1 in x|}),
    /* Module expressions - SKIP segment roundtrip due to structural differences:
       Parser creates compound form ["{","}"], pretty-printer creates atomic "{}".
       Text round-tripping works, but segment structure differs.
       TODO: Align parser and pretty-printer segment structures. */
    test_case("Module: skip segment roundtrip", `Quick, () => {
      let _ = Alcotest.skip();
      ();
    }),
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

   Legacy/experimental syntax:
   - BlockExp ({...}) - preliminary syntax for probe user study
   - LogicalOrLegacy (\/) - legacy OR syntax

   === REMAINING WORK (needs investigation) ===

   - Grout (convex and concave) - secondary preservation unclear
   - Explicit holes (`?`) - special handling in MakeTerm, may need adjustment
   - LLMHole (??...??) - similar concerns to explicit holes

   === CONFIGURABLE BEHAVIOR (addressed via settings) ===

   Defensive Parenthesization (Settings.parenthesization):
   - Defensive: Adds parens for forms like rec/poly after `:` because they
     share low precedence with their `->` trailing delimiter.
     Examples: `1 : rec t -> t` becomes `1 :( rec t -> t)`
   - Structural: Only emits parens that exist in the term structure.
     Used for round-tripping where defensive parens would break exact preservation.
   Note: `forall` is expression-level only; `poly` is the type-level equivalent.

   Label Quoting (Settings.label_format):
   - QuoteWhenNecessary: Only adds backticks for non-identifiers (original behavior)
   - AlwaysQuote: Always adds backticks to labels (for round-tripping)

   === KNOWN LIMITATIONS ===

   - Float literals: normalized to full precision (2.0 becomes 2.000000)
     This is done upstream in the parser/lexer.

   ============================================================================ */

/* Tests for forms that previously had defensive parenthesization issues.
   With parenthesization: Structural, these now round-trip correctly. */
let roundtrip_defensive_paren_tests = (
  "Round-Trip: Defensive Parenthesization Forms",
  [
    /* Rec types after type annotation - previously got wrapped in parens
       with Defensive mode. With Structural mode, round-trips correctly. */
    roundtrip_test({|Rec type after ascription|}, {|1 : rec t -> t|}),
    roundtrip_test({|Rec type spaced|}, {|1  :  rec t -> t|}),
    /* Poly types after type annotation - same issue as rec. */
    roundtrip_test({|Poly type after ascription|}, {|1 : poly a -> a|}),
    roundtrip_test({|Poly type spaced|}, {|1  :  poly a -> a|}),
    /* Note: `forall` is expression-level only (exp forall pat -> exp).
       The type-level construct is `poly` (type poly tpat -> typ). */
    /* Nested rec/poly types */
    roundtrip_test({|Nested rec types|}, {|1 : rec t -> rec u -> (t, u)|}),
  ],
);

/* Larger program round-trip tests with comments, formatting, and multi-line structure.
   These verify that secondary-in-terms handles complex nested expressions correctly. */
let roundtrip_larger_programs = (
  "Round-Trip: Larger Programs",
  [
    /* Factorial with comments and formatting */
    roundtrip_test(
      {|Factorial: single line|},
      {|let fact = fun n -> if n <= 0 then 1 else n * fact(n - 1) in fact(5)|},
    ),
    roundtrip_test(
      {|Factorial: multiline|},
      {|let fact = fun n ->
  if n <= 0
  then 1
  else n * fact(n - 1)
in fact(5)|},
    ),
    roundtrip_test(
      {|Factorial: with comments|},
      {|#recursive factorial#
let fact = fun n ->
  #base case#
  if n <= 0
  then 1
  #recursive case#
  else n * fact(n - 1)
in fact(5)|},
    ),
    /* Fibonacci with double line breaks */
    roundtrip_test(
      {|Fibonacci: single line|},
      {|let fib = fun n -> if n <= 1 then n else fib(n - 1) + fib(n - 2) in fib(10)|},
    ),
    roundtrip_test(
      {|Fibonacci: multiline with double breaks|},
      {|let fib = fun n ->
  if n <= 1
  then n
  else fib(n - 1) + fib(n - 2)


in fib(10)|},
    ),
    roundtrip_test(
      {|Fibonacci: commented|},
      {|#fibonacci sequence#
let fib = fun n ->
  if n <= 1 then n
  else fib(n - 1) + fib(n - 2)
in
#compute 10th fibonacci number#
fib(10)|},
    ),
    /* List map with case expression */
    roundtrip_test(
      {|Map: single line|},
      {|let map = fun f -> fun xs -> case xs | [] => [] | hd::tl => f(hd)::map(f)(tl) end in map(fun x -> x + 1)([1, 2, 3])|},
    ),
    roundtrip_test(
      {|Map: multiline case|},
      {|let map = fun f -> fun xs ->
  case xs
  | [] => []
  | hd::tl => f(hd)::map(f)(tl)
  end
in map(fun x -> x + 1)([1, 2, 3])|},
    ),
    roundtrip_test(
      {|Map: with comments|},
      {|#map a function over a list#
let map = fun f -> fun xs ->
  case xs
  | [] => []  #empty list base case#
  | hd::tl => f(hd)::map(f)(tl)  #recursive case#
  end
in
#increment each element#
map(fun x -> x + 1)([1, 2, 3])|},
    ),
    /* Fold with multiple function arguments */
    roundtrip_test(
      {|Fold: single line|},
      {|let fold = fun f -> fun acc -> fun xs -> case xs | [] => acc | hd::tl => fold(f)(f(acc, hd))(tl) end in fold(fun (a, b) -> a + b)(0)([1, 2, 3, 4, 5])|},
    ),
    roundtrip_test(
      {|Fold: multiline|},
      {|let fold = fun f -> fun acc -> fun xs ->
  case xs
  | [] => acc
  | hd::tl => fold(f)(f(acc, hd))(tl)
  end

in fold(fun (a, b) -> a + b)(0)([1, 2, 3, 4, 5])|},
    ),
    roundtrip_test(
      {|Fold: heavily commented|},
      {|#left fold over a list#

let fold = fun f ->  #combining function#
  fun acc ->  #initial accumulator#
  fun xs ->  #list to fold#
    case xs
    | [] => acc
    | hd::tl => fold(f)(f(acc, hd))(tl)
    end

in
#sum a list of numbers#
fold(fun (a, b) -> a + b)(0)([1, 2, 3, 4, 5])|},
    ),
    /* Filter with nested if */
    roundtrip_test(
      {|Filter: single line|},
      {|let filter = fun p -> fun xs -> case xs | [] => [] | hd::tl => if p(hd) then hd::filter(p)(tl) else filter(p)(tl) end in filter(fun x -> x > 2)([1, 2, 3, 4])|},
    ),
    roundtrip_test(
      {|Filter: multiline|},
      {|let filter = fun p -> fun xs ->
  case xs
  | [] => []
  | hd::tl =>
    if p(hd)
    then hd::filter(p)(tl)
    else filter(p)(tl)
  end
in filter(fun x -> x > 2)([1, 2, 3, 4])|},
    ),
    /* Multiple let bindings with various spacing */
    roundtrip_test(
      {|Multiple lets: compact|},
      {|let a=1 in let b=2 in let c=3 in a+b+c|},
    ),
    roundtrip_test(
      {|Multiple lets: spaced|},
      {|let a = 1 in
let b = 2 in
let c = 3 in
a + b + c|},
    ),
    roundtrip_test(
      {|Multiple lets: double breaks|},
      {|let a = 1 in

let b = 2 in

let c = 3 in

a + b + c|},
    ),
    roundtrip_test(
      {|Multiple lets: with comments|},
      {|#first binding#
let a = 1 in
#second binding#
let b = 2 in
#third binding#
let c = 3 in
#result#
a + b + c|},
    ),
    /* Nested functions and applications */
    roundtrip_test(
      {|Nested: compose|},
      {|let compose = fun f -> fun g -> fun x -> f(g(x))
in
let double = fun x -> x * 2 in
let inc = fun x -> x + 1 in
compose(double)(inc)(5)|},
    ),
    roundtrip_test(
      {|Nested: curried application|},
      {|let add = fun a -> fun b -> fun c -> a + b + c
in
let add5 = add(5)
in
let add5and10 = add5(10)
in
add5and10(15)|},
    ),
    /* Type annotations with formatting */
    roundtrip_test(
      {|Typed: multiline function|},
      {|let id : poly a -> a -> a = typfun a ->
  fun x -> x
in id @<Int>(42)|},
    ),
    /* Complex nested structure */
    roundtrip_test(
      {|Complex: deeply nested|},
      {|let result =
  let x = 1 + 2 in
  let y =
    let z = x * 3 in
    z + 4
  in
  y * 5
in result|},
    ),
    roundtrip_test(
      {|Complex: mixed constructs|},
      {|#a complex example with multiple constructs#

let process = fun data ->
  #extract and transform#
  let filtered = filter(fun x -> x > 0)(data) in
  let doubled = map(fun x -> x * 2)(filtered) in

  #aggregate#
  fold(fun (acc, x) -> acc + x)(0)(doubled)

in process([1, -2, 3, -4, 5])|},
    ),
  ],
);

/* ============================================================================
   GROUT ROUND-TRIP TESTS

   Grout represents incomplete/erroneous syntax:
   - Convex grout: EmptyHole - represents an empty/missing expression
   - Concave grout: MultiHole - represents multiple disconnected pieces

   Round-trip path: Term → ExpToSegment → Segment → MakeTerm → Term

   Key behaviors verified:
   1. EmptyHole produces convex Grout, which parses back to EmptyHole
   2. MultiHole produces elements with concave Grout between them
   3. Consecutive concave grouts combine into a single MultiHole (chainable)
   4. Secondary (whitespace) is preserved around grout pieces
   ============================================================================ */

/* Settings for structural grout tests */
let grout_structural_settings: ExpToSegment.Settings.t = {
  secondary: PreserveExact,
  parenthesization: Structural,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_filters: true,
  show_unknown_as_hole: true,
};

/* String-to-string grout tests: parse strings, verify round-trip preserves text.

   Note: We only compare text, not segments, because Parser produces Tile({label: ["?"]})
   for explicit holes while ExpToSegment produces Grout({shape: Convex}). These are
   semantically equivalent but structurally different.

   Important: These tests require whitespace between tokens (e.g., "1 2" not "12").
   While the segment data structure technically allows tiles to be directly adjacent
   with no intervening grout, this cannot occur in the editor - adjacent tiles would
   immediately glom together into a single token. Since grout is internally inserted
   by the parser/regrouter (not explicitly typed by users), we print it as empty string
   to achieve true string round-tripping. The whitespace in the input string is what
   prevents tokens from merging and allows the parser to recognize separate tiles. */
let roundtrip_grout_text_test = (name: string, input: string) =>
  test_case(name, `Quick, () => {
    switch (Parser.to_term(input)) {
    | Some(term) =>
      /* Print grout as empty string so it doesn't appear in output.
         This achieves true string round-tripping since grout is internally
         inserted, not explicitly typed by users. */
      let print_seg_grout =
        Printer.of_segment(~holes="", ~concave_holes="", ~refractors=[]);
      let seg' = exp_to_segment_roundtrip(term);
      let output = print_seg_grout(seg');
      check(string, {|Round-trip text|}, input, output);
    | None => Alcotest.fail({|Failed to parse|})
    }
  });

let roundtrip_grout_string_tests = (
  "Round-Trip: Grout (String)",
  [
    /* Incomplete syntax that should produce actual grout (not Tile with "?" label) */
    /* These test whether the parser inserts grout for incomplete expressions */
    roundtrip_grout_text_test({|Incomplete: no term|}, {||}),
    roundtrip_grout_text_test({|Incomplete: convex grout|}, {|1+|}),
    roundtrip_grout_text_test(
      {|Incomplete: convex grout with secondary|},
      {|1+ |},
    ),
    roundtrip_grout_text_test({|Incomplete: two adjacent terms|}, {|1 2|}),
    roundtrip_grout_text_test(
      {|Incomplete: three adjacent terms|},
      {|1 2 3|},
    ),
    roundtrip_grout_text_test(
      {|Incomplete: terms with extra spaces|},
      {|1  2  3|},
    ),
    roundtrip_grout_text_test({|Incomplete: var then int|}, {|x 1|}),
  ],
);

/* ============================================================================
   Term-to-Term Round-Trip Tests
   ============================================================================
   These tests verify the full round-trip: term → segment → term
   Unlike string-based tests, these compare terms directly using Exp.equal,
   which ignores IDs and parentheses. This tests structural preservation.
   ============================================================================ */

/* NOTE: Term-to-term round-trip tests (term → segment → term with strict == equality)
   were removed due to ID list length mismatch between FreshGrammar and MakeTerm.
   See plans/secondary-in-terms-v2.md "ID List Length Mismatch" section for details.

   FreshGrammar creates terms with 1 ID regardless of structure, but MakeTerm
   collects IDs from all skeleton pieces (e.g., N-1 IDs for N-element MultiHole).
   This causes strict equality to fail even though the term content is identical.

   String-based tests (grout_term_to_seg_to_string_tests, roundtrip_grout_string_tests)
   still provide coverage for grout round-tripping. */

/* ============================================================================
   PROJECTOR ROUND-TRIP TESTS

   These tests verify that projectors round-trip correctly through:
   text → segment → term → segment → text

   Projector syntax: ^^<kind>(<content>)
   Where <kind> is one of: fold, slider, sliderf, check, text, card, livelit, csv
   (probe and statics are refractors with different behavior)
   ============================================================================ */

let roundtrip_projector_tests = (
  "Round-Trip: Projectors",
  [
    /* Simple projector wrapping a literal */
    roundtrip_test({|Slider: integer|}, {|^^slider(50)|}),
    roundtrip_test({|SliderF: float|}, {|^^sliderf(0.500000)|}),
    roundtrip_test({|Checkbox: true|}, {|^^check(true)|}),
    roundtrip_test({|Checkbox: false|}, {|^^check(false)|}),
    /* Projector in expression context */
    roundtrip_test({|Slider in let|}, {|let x = ^^slider(50) in x + 1|}),
    /* Test secondary inside various wrappers */
    roundtrip_test({|Parens: simple|}, {|(1 + 2)|}),
    roundtrip_test({|Fold: simple|}, {|^^fold(1 + 2)|}),
    roundtrip_test({|Fold in function|}, {|fun x -> ^^fold(x * 2)|}),
    /* Nested projectors */
    roundtrip_test({|Nested: slider in fold|}, {|^^fold(^^slider(50) + 1)|}),
    /* Projector with spacing inside content - leading/trailing spaces in parens */
    roundtrip_test({|Parens: inner spaced|}, {|( 50 )|}),
    roundtrip_test({|Slider: inner spaced|}, {|^^slider( 50 )|}),
    roundtrip_test({|Fold: inner spaced|}, {|^^fold( 1 + 2 )|}),
    roundtrip_test({|Fold in pattern|}, {|fun ^^fold(x, y) -> x+y|}),
    roundtrip_test({|Fold in type|}, {|type T = ^^fold((Int, String)) in 1|}),
  ],
);

let all = [
  tests,
  roundtrip_tests,
  roundtrip_defensive_paren_tests,
  roundtrip_larger_programs,
  roundtrip_grout_string_tests,
  roundtrip_projector_tests,
];
