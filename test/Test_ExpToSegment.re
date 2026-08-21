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
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  use_literal_lexemes: true,
  project_tables: false,
};

let exp_to_segment =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings);

let equivalent_to_make_term = (serialized: string) => {
  switch (
    Parser.to_term(serialized, ~root=Exp),
    Parser.to_segment(serialized, ~root=Exp),
  ) {
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
        incomplete: [],
        lexeme: None,
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
          Parser.to_segment("(1, 2)", ~root=Exp),
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
          Parser.to_segment("(x=1)", ~root=Exp),
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
    test_case("Void type (empty sum) prints as Void", `Quick, () => {
      check(
        string,
        "Void type",
        "Void",
        print_seg(
          ExpToSegment.typ_to_segment(
            ~settings=exp_to_segment_settings,
            IdTagged.FreshGrammar.Typ.sum([]),
          ),
        ),
      )
    }),
    test_case("Void type round-trips through MakeTerm", `Quick, () => {
      type_equivalent_to_make_term("Void")
    }),
    test_case("Cons followed by negation", `Quick, () =>
      equivalent_to_make_term({|"":: - a|})
    ),
    test_case("Function call", `Quick, () => {
      equivalent_to_make_term("a(1, 2)")
    }),
    test_case(
      "Rul term prints its content",
      `Quick,
      () => {
        /* any_to_pretty used to print Rul as a bare convex grout,
           destroying the rules' content */
        let rul: Language.Rul.t =
          IdTagged.FreshGrammar.(
            IdTagged.fresh(
              Grammar.Rules(
                Exp.int(1),
                [(Pat.constructor("A", None), Exp.int(2))],
              ),
            )
          );
        let seg =
          ExpToSegment.any_to_segment(
            ~settings=exp_to_segment_settings,
            Rul(rul),
          );
        check(string, "rule content", "1 | A => 2", print_seg(seg));
      },
    ),
    test_case("Shard provenance reaches term annotations", `Quick, () => {
      switch (Parser.to_segment("let x = 1 ", ~root=Exp)) {
      | None => Alcotest.fail("parse failed")
      | Some(seg) =>
        let result =
          CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
        let masks =
          CanonicalCompletion.masks_of_records(result.shard_records);
        let term = MakeTerm.go_impl(~masks, result.completed_seg).term;
        /* The completed root should be a Let whose annotation records
           the let tile with originally-present shards [0, 1] */
        let found =
          term.annotation.incomplete
          |> List.exists(((_, mask: IdTagged.IdTag.incomplete_mask)) =>
               mask.present == [0, 1]
             );
        check(bool, "let tile provenance recorded", true, found);
      }
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
    test_case(
      "Nested reverse application no parens",
      `Quick,
      () => {
        let segment =
          Parser.to_term("1 |> 2 |> 3", ~root=Exp)
          |> Option.get
          |> exp_to_segment;
        let serialized = print_seg(segment);

        check(
          string,
          "Nested reverse application",
          "1 |> 2 |> 3",
          serialized,
        );
      },
    ),
    test_case(
      "Float negation produces 0.0 -. e",
      `Quick,
      () => {
        /* UnOp(Float(Minus), e) should be rewritten to 0.0 -. e
           so it round-trips through MakeTerm correctly (MakeTerm
           parses unary - as Int(Minus), breaking float evaluation) */
        let seg =
          exp_to_segment(
            IdTagged.FreshGrammar.Exp.(un_op(Float(Minus), float(3.14))),
          );
        let serialized = print_seg(seg);
        check(
          string,
          "Float negation text",
          "0.000000 -. 3.140000",
          serialized,
        );
        /* Verify round-trip: segment → term → segment preserves float minus */
        switch (MakeTerm.for_projection(seg)) {
        | Some(Exp({term: BinOp(Float(Minus), _, _), _})) => ()
        | _ =>
          Alcotest.fail("Expected BinOp(Float(Minus), ...) after round-trip")
        };
      },
    ),
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
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  use_literal_lexemes: true,
  project_tables: false,
};

let exp_to_segment_roundtrip =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_roundtrip_settings);

/* Text-only roundtrip: for forms with known segment-level infidelities
   (drv printers hardcode child sorts, e.g. Truth printed at Drv Prop where
   the parser molded it Drv Exp) where secondary preservation is still
   worth pinning. */
let roundtrip_text_test = (name: string, input: string) =>
  test_case(name, `Quick, () => {
    switch (Parser.to_term(input, ~root=Exp)) {
    | Some(term) =>
      let input2 = print_seg(exp_to_segment_roundtrip(term));
      check(string, {|Round-trip text|}, input, input2);
    | None => Alcotest.fail({|Failed to parse|})
    }
  });

/* Test that a string round-trips through segment → term → segment */
/* known_equiv_gap: strict mod-grout equivalence finding not yet fixed —
   an explicit worklist, not an acceptance. Empty as of 2026-07-02: the
   original families (TupleExtension, DeferredAp comma id, quoted
   labels) are all fixed. */
let roundtrip_test = (~known_equiv_gap=false, name: string, input: string) =>
  test_case(name, `Quick, () => {
    switch (Parser.to_segment(input, ~root=Exp)) {
    | Some(seg) =>
      /* term from the SAME parse — a second Parser.to_term call would
         mint fresh ids and defeat the strict equivalence check */
      let term = MakeTerm.go(seg).term;
      let seg' = exp_to_segment_roundtrip(term);
      let input' = print_seg(seg');
      check(string, {|Round-trip text|}, input, input');
      check(segment, {|Round-trip segments|}, seg, seg');
      if (!known_equiv_gap) {
        check(
          bool,
          {|Round-trip equiv (strict mod grout)|},
          true,
          Segment.equiv_mod_grout(seg, seg'),
        );
      };
    | None => Alcotest.fail({|Failed to parse|})
    }
  });

/* Tile ids must survive the roundtrip. The `segment` testable is
   id-ignoring, so id-fidelity regressions (e.g. the sum ctor-ap id swap)
   need this explicit check. Grout/secondary ids are excluded: grout is
   ephemeral by design. */
let rec tile_ids = (seg: Segment.t): list(string) =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         [List.nth(t.label, 0) ++ ":" ++ Id.to_string(t.id)]
         @ List.concat_map(tile_ids, t.children)
       | Secondary(_)
       | Grout(_) => []
       | Projector(pr) => ["PROJ:" ++ Id.to_string(pr.id)]
       }
     );

let roundtrip_ids_test = (name: string, input: string) =>
  test_case(name, `Quick, () => {
    /* Term must come from the SAME parse as the reference segment —
       Parser.to_term would re-parse and mint entirely fresh ids. */
    switch (Parser.to_segment(input, ~root=Exp)) {
    | Some(seg) =>
      let term = MakeTerm.go(seg).term;
      let seg' = exp_to_segment_roundtrip(term);
      check(
        list(string),
        {|Round-trip tile ids|},
        tile_ids(seg),
        tile_ids(seg'),
      );
    | _ => Alcotest.fail({|Failed to parse|})
    }
  });

let roundtrip_tests = (
  "Secondary Round-Trip",
  [
    /* Simple atoms */
    roundtrip_test({|Integer literal|}, {|42|}),
    /* Lexeme preservation: non-canonical spellings and hole tokens
       survive via IdTag.lexeme (audit classes B and A4) */
    roundtrip_test({|Int: leading zeros|}, {|007|}),
    roundtrip_test({|Float: short|}, {|3.14|}),
    roundtrip_test({|Float: exponent|}, {|1e3|}),
    roundtrip_test({|Float: trailing dot|}, {|2.|}),
    roundtrip_test({|Explicit hole|}, {|?|}),
    roundtrip_test({|Explicit hole operand|}, {|1 + ?|}),
    roundtrip_test({|LLM hole|}, {|1 + ??|}),
    roundtrip_test({|Float pattern|}, {|fun 3.14 -> 1|}),
    roundtrip_test({|Label: unnecessary backticks|}, {|(`a`=1)|}),
    roundtrip_test({|Label: backticked dot|}, {|x.`a`|}),
    roundtrip_test({|Label: backticked pat|}, {|fun (`a`=x) -> x|}),
    roundtrip_test({|Label: backticked typ|}, {|1 : (`a`=Int)|}),
    /* Unknown infix operators survive as MultiHole + op lexeme */
    roundtrip_test({|Unknown op|}, {|1 @@@ 2|}),
    roundtrip_test({|Unknown op: chained|}, {|1 @@@ 2 @@@ 3|}),
    roundtrip_test({|Unknown op: spaced|}, {|1  @@@  2|}),
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
    /* Pipeline (|>) — Ap(Reverse). Left-associative, so chained pipes
       must round-trip without gaining parens. */
    roundtrip_test({|Pipeline: simple|}, {|x |> f|}),
    roundtrip_test({|Pipeline: chained|}, {|x |> f |> g|}),
    roundtrip_test({|Pipeline: chained 4 terms|}, {|x |> f |> g |> h|}),
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
    /* Nullary ap is a distinct single-token postfix form, flagged by
       Id.nullary_ap_flag on the Tuple([]) arg — must not print as f(()) */
    roundtrip_test({|Application: nullary|}, {|f()|}),
    roundtrip_test({|Application: nullary in pattern|}, {|fun C() -> 1|}),
    roundtrip_test({|Application: literal unit arg|}, {|f(())|}),
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
    roundtrip_ids_test(
      {|Sum type: ctor-ap ids stable|},
      {|type T = +A(Int) + B in T|},
    ),
    roundtrip_ids_test(
      {|Ids stable: let/fun/case|},
      {|let f = fun x -> case x | A => 1 end in f(2)|},
    ),
    roundtrip_test({|Sum type: spaced|}, {|type T = + A + B in T|}),
    roundtrip_test({|Sum type: compact|}, {|type T = +A+B in T|}),
    /* Bare sums: the parse's id count distinguishes `A + B` from
       `+A + B` (n-1 separator ids vs n with the leading + tile) */
    roundtrip_test({|Sum type: no leading prefix|}, {|type T = A + B in T|}),
    roundtrip_test(
      {|Sum type: no leading prefix, args|},
      {|type T = A(Int) + B in T|},
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
    /* Drv (of_* end) forms: drv builders/printers thread secondary; without
       this, PreserveExact printed glommed unparseable text (of_propTruthend) */
    roundtrip_text_test({|Drv: of_prop|}, {|of_prop Truth end|}),
    roundtrip_text_test(
      {|Drv: of_alfa_exp spaced|},
      {|of_alfa_exp 1  +  2 end|},
    ),
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
    /* restricted forall (where-binder) */
    roundtrip_test(
      {|ForallWhere: simple|},
      {|forall a where a != 0 -> a == a|},
    ),
    /* function contract (fun-where binder) */
    roundtrip_test({|FunWhere: simple|}, {|fun a where a != 0 -> 100 / a|}),
    roundtrip_test(
      {|FunWhere: conjunctive guard|},
      {|fun a where a != 0 && a != 1 -> 100 / a|},
    ),
    /* material implication */
    roundtrip_test({|Implies: simple|}, {|true ==> false|}),
    roundtrip_test({|Implies: with equalities|}, {|a == b ==> c == d|}),
    roundtrip_test({|Implies: chained|}, {|a ==> b ==> c|}),
    roundtrip_test({|Implies: left-parenthesized|}, {|(a ==> b) ==> c|}),
    /* Use expressions (use ... in) */
    roundtrip_test({|Use: simple|}, {|use Nat in 1|}),
    roundtrip_test({|Use: spaced|}, {|use Nat  in  1|}),
    roundtrip_test({|Use: compact|}, {|use Nat in 1|}),
    /* Theorem expressions (theorem ... = ... proof ... in ...) */
    roundtrip_test(
      {|Theorem: simple|},
      {|theorem x = 1 proof axiom y at y on y end in x|},
    ),
    roundtrip_test(
      {|Theorem: spaced|},
      {|theorem x  =  1  proof  axiom y at y on y end  in  x|},
    ),
    roundtrip_test(
      {|Theorem: compact|},
      {|theorem x=1 proof axiom y at y on y end in x|},
    ),
    /* Proof: induction (single case, case-like syntax — no `with` keyword) */
    roundtrip_test(
      {|Induction: single-case|},
      {|theorem x = 1 proof induction y | a => axiom y at y on y end end in x|},
    ),
    /* Proof: induction (multi-case) */
    roundtrip_test(
      {|Induction: multi-case|},
      {|theorem x = 1 proof induction y | a => axiom y at y on y end | b => axiom z at z on z end end in x|},
    ),
    /* Proof: forall (quantified variable) */
    roundtrip_test(
      {|Forall: simple|},
      {|theorem x = 1 proof forall y => axiom y at y on y end in x|},
    ),
    /* Proof: assume (hypothesized proposition) */
    roundtrip_test(
      {|Assume: simple|},
      {|theorem x = 1 proof assume 2 == 2 => axiom y at y on y end in x|},
    ),
    roundtrip_test(
      {|Assume: nested|},
      {|theorem x = 1 proof assume 2 == 2 => assume 3 == 3 => axiom y at y on y end in x|},
    ),
    /* Proof: Phase-4d explicit instantiation (`with <var> = <exp>` on a
       citation step). Each with-variant is a distinct compound form
       sitting beside its plain form, so both spellings must round-trip. */
    roundtrip_test(
      {|AxiomWith: simple|},
      {|theorem x = 1 proof axiom y with n = 2 at y on y end in x|},
    ),
    roundtrip_test(
      {|AxiomWith: spaced|},
      {|theorem x = 1 proof axiom y  with  n  =  2  at y on y end in x|},
    ),
    roundtrip_test(
      {|AxiomRevWith: simple|},
      {|theorem x = 1 proof axiomrev y with n = 2 at y on y end in x|},
    ),
    roundtrip_test(
      {|RevertWith: simple|},
      {|theorem x = 1 proof revert y with n = 2 => axiom y at y on y end in x|},
    ),
    roundtrip_test(
      {|RevertWith: compound instantiation|},
      {|theorem x = 1 proof revert y with n = 2 + 3 => axiom y at y on y end in x|},
    ),
    /* Phase 4e's `contradiction <fact> with <var> = <exp> end` reuses the
       same with-clause shards on a TERMINAL step. */
    roundtrip_test(
      {|ContradictionWith: simple|},
      {|theorem x = 1 proof contradiction y with n = 2 end in x|},
    ),
    roundtrip_test(
      {|ContradictionWith: compound rewrite|},
      {|theorem x = 1 proof contradiction y == 1 with n = 2 + 3 end in x|},
    ),
    roundtrip_test(
      {|Contradiction: plain still round-trips|},
      {|theorem x = 1 proof contradiction y == 1 end in x|},
    ),
    /* The plain forms must keep round-tripping unchanged: `with` is only
       absorbed by a citation step that is still missing its own shards. */
    roundtrip_test(
      {|Revert: plain still round-trips|},
      {|theorem x = 1 proof revert y => axiom y at y on y end in x|},
    ),
    roundtrip_test(
      {|Algebrite: `with` still belongs to rewrite|},
      {|theorem x = 1 proof rewrite y with z at 0 end in x|},
    ),
    /* Module expressions: empty module roundtrip */
    roundtrip_test({|Module: empty|}, {|{}|}),
    /* Module text round-trip tests */
    roundtrip_test({|Module: single let|}, {|{ let x = 1 }|}),
    roundtrip_test({|Module: multiple lets|}, {|{ let x = 1; let y = 2 }|}),
    roundtrip_test({|Module: compact spacing|}, {|{let x = 1}|}),
    /* Bare-expression items: ModExp shares its rep id with the inner exp;
       these caught a double-emission of the shared secondary. */
    roundtrip_test({|Module: bare exp item|}, {|{ 1 }|}),
    roundtrip_test({|Module: bare exp items|}, {|{ 1; 2 }|}),
    roundtrip_test({|Module: let then bare exp|}, {|{ let x = 1; x }|}),
    roundtrip_test(
      {|Module: with type alias|},
      {|{ type T = Int; let x = 1 }|},
    ),
    /* ModuleExp round-trip tests */
    roundtrip_test({|ModuleExp: basic|}, {|module M = { let x = 1 } in M|}),
    roundtrip_test(
      {|ModuleExp: with sig annotation|},
      {|module M : { let x : Int } = { let x = 1 } in M|},
    ),
    roundtrip_test(
      {|ModuleExp: nested module keyword|},
      {|module M = { module N = { let x = 1 } } in M|},
    ),
    /* Module: with module keyword item */
    roundtrip_test(
      {|Module: module keyword item|},
      {|{ module Inner = { let x = 1 } }|},
    ),
    /* Sig text round-trip tests */
    roundtrip_test(
      {|Sig: single let|},
      {|let m : { let x : Int } = { let x = 1 } in m|},
    ),
    roundtrip_test(
      {|Sig: multiple lets|},
      {|let m : { let x : Int; let y : Bool } = { let x = 1; let y = true } in m|},
    ),
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

/* Defensive printing must be idempotent: parse → print → parse → print
   reaches a fixed point after the first print. Otherwise every editor
   round-trip (e.g. EditorTransform patches on proof edits) rewrites the
   program with one more layer of parens — the ascription-type force-wrap
   used to stack Parens onto types that were already Parens (#forall
   x:(([Int])) after two induction-case edits). Settings mirror
   EditorTransform.exp_to_segment. */
let editable_print = (exp: Exp.t): string =>
  print_seg(
    ExpToSegment.exp_to_segment(
      exp,
      ~settings={
        ...ExpToSegment.Settings.editable(~inline=true),
        use_literal_lexemes: false,
      },
    ),
  );

let defensive_idempotence_test = (name: string, input: string) =>
  test_case(name, `Quick, () => {
    switch (Parser.to_term(input, ~root=Exp)) {
    | Some(term) =>
      let once = editable_print(term);
      switch (Parser.to_term(once, ~root=Exp)) {
      | Some(term') =>
        let twice = editable_print(term');
        check(string, {|Defensive print fixed point|}, once, twice);
      | None => Alcotest.fail({|Failed to re-parse printed text: |} ++ once)
      };
    | None => Alcotest.fail({|Failed to parse|})
    }
  });

let defensive_idempotence_tests = (
  "Defensive Print Idempotence",
  [
    defensive_idempotence_test(
      {|Forall with list-type ascription|},
      {|forall x:[Int] -> x == x|},
    ),
    defensive_idempotence_test(
      {|Forall with atomic-type ascription|},
      {|forall x:Int -> x == x|},
    ),
    defensive_idempotence_test(
      {|Forall with pre-parenthesized ascription|},
      {|forall x:([Int]) -> x == x|},
    ),
    defensive_idempotence_test(
      {|Forall with arrow-type ascription|},
      {|forall f:(Int -> Int) -> f == f|},
    ),
    defensive_idempotence_test(
      {|Fun with ascribed pattern|},
      {|fun (x : Int) -> x|},
    ),
    defensive_idempotence_test(
      {|Let with ascribed pattern|},
      {|let x : [Int] = [1] in x|},
    ),
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
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  use_literal_lexemes: true,
  project_tables: false,
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
    switch (Parser.to_term(input, ~root=Exp)) {
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

/* Roundtrip for INCOMPLETE inputs via the canonical-completion path:
   visible segment -> complete (recording shard provenance) -> term ->
   print (strip pass truncates completed tiles back to original shards).
   Text compared with grout hidden on both sides (regrout normalizes). */
let check_incomplete_roundtrip = (seg: Segment.t): unit => {
  let result = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
  let masks = CanonicalCompletion.masks_of_records(result.shard_records);
  let term = MakeTerm.go_impl(~masks, result.completed_seg).term;
  let seg2 = exp_to_segment_roundtrip(term);
  let print_g =
    Printer.of_segment(~holes="", ~concave_holes="", ~refractors=[]);
  check(
    string,
    "roundtrip text (grout hidden)",
    print_g(seg),
    print_g(seg2),
  );
  /* id fidelity: the visible tiles keep their ids through completion,
     parsing, printing, and stripping */
  check(list(string), "roundtrip tile ids", tile_ids(seg), tile_ids(seg2));
  check(
    bool,
    "roundtrip equiv (strict mod grout)",
    true,
    Segment.equiv_mod_grout(seg, seg2),
  );
};

let roundtrip_incomplete_test = (name: string, input: string) =>
  test_case(name, `Quick, () => {
    switch (Parser.to_segment(input, ~root=Exp)) {
    | None => Alcotest.fail("parse failed")
    | Some(seg) => check_incomplete_roundtrip(seg)
    }
  });

let result_display_test =
  test_case(
    "Result display: hole flavor, canonical literals, stuck ops",
    `Quick,
    () => {
      /* Mirror the browser result pipeline: evaluate, replace_all_ids (as
         evaluate_and_limit does), print with of_core display settings. */
      let display = (e: Language.Exp.t) =>
        print_seg(
          PrettySegment.prettify(
            ExpToSegment.exp_to_segment(
              ~settings=
                ExpToSegment.Settings.of_core(~inline=false, CoreSettings.on),
              e,
            ),
          ),
        );
      let run = src =>
        Test_Evaluator_Prelude.(evaluate(elaborate(parse_exp(src))))
        |> Language.Exp.replace_all_ids;
      /* hole flavor survives evaluation and re-idding (B8) */
      check(string, "explicit hole", "1 + ?", display(run("1 + ?")));
      check(string, "llm hole", "1 + ??", display(run("1 + ??")));
      /* hole flavor also survives VarLookup's fast_copy */
      check(
        string,
        "hole via lookup",
        "??",
        display(run("let x = ?? in x")),
      );
      /* unknown ops are stuck, not evaluated-to-last (B9) */
      check(string, "stuck op", "1 @@@ 2", display(run("1 @@@ 2")));
      /* plain juxtaposition (no op lexeme) keeps eval-to-last semantics */
      check(
        string,
        "juxtaposition evals to last",
        "2",
        display(run("1 2")),
      );
      check(
        string,
        "juxtaposition chain evals to last",
        "3",
        display(run("1 2 3")),
      );
      /* literals canonicalize consistently in result views (B7) */
      check(
        string,
        "literal via let",
        "7",
        display(run("let x = 007 in x")),
      );
      check(string, "literal direct", "7", display(run("007")));
    },
  );

let roundtrip_incomplete_tests = (
  "Round-Trip: Incomplete (completion provenance)",
  [
    result_display_test,
    roundtrip_incomplete_test({|Let missing in|}, {|let x = 1 |}),
    roundtrip_incomplete_test({|If missing else|}, {|if true then 1 |}),
    roundtrip_incomplete_test({|Fun missing arrow|}, {|fun x |}),
    roundtrip_incomplete_test({|Unclosed parens|}, {|(1, 2|}),
    roundtrip_incomplete_test({|Case missing end|}, {|case x | A => 1 |}),
    roundtrip_incomplete_test(
      {|Nested incomplete lets|},
      {|let a = 1 in let b = 2 |},
    ),
    /* Module/Sig items: masks land on Mod/Sig terms, exercised via the
       f_mod/f_sig map_term hooks */
    roundtrip_incomplete_test({|Module: incomplete ModLet|}, {|{ type T }|}),
    roundtrip_incomplete_test(
      {|Module: incomplete item + brace|},
      {|{ let x = 1; let y |},
    ),
    roundtrip_incomplete_test(
      {|Sig: incomplete SigType|},
      {|1 : { type T }|},
    ),
    /* Typed bare rules are standalone |, => token tiles (real rule
       tiles only form inside a case), so these roundtrip via the
       unknown-op lexeme machinery, not the case wrap. Edit-derived
       orphan chains (real rule tiles) are wrapped — see the golden
       tests in Test_CanonicalCompletion. */
    roundtrip_incomplete_test({|Orphaned rule|}, {|1 | A => 2|}),
    roundtrip_incomplete_test(
      {|Orphaned rules, two clauses|},
      {|1 | A => 2 | B => 3|},
    ),
    roundtrip_incomplete_test({|Orphaned rule, no scrutinee|}, {|| A => 1|}),
    roundtrip_incomplete_test(
      {|Orphaned rule, compound scrutinee|},
      {|1 + 2 | A => 3|},
    ),
    roundtrip_incomplete_test(
      {|Orphaned rule inside parens|},
      {|(1 | A => 2) + 3|},
    ),
    /* Leading-shard loss: closing delimiters without openers; completion
       prepends the missing openers at partition start (reverse order so
       the later closer opens outermost).
       NOTE: a typed lone `end` does NOT expand into a case shard the way
       `)` expands into a paren shard — inserting it remolds rule tiles
       apart into standalone token tiles. Keyword-form leading loss
       (missing case/let) only arises from destructive edits; testing it
       needs editing actions, not Parser typing. */
    roundtrip_incomplete_test({|Unopened parens|}, {|1, 2)|}),
    roundtrip_incomplete_test({|Unopened bracket|}, {|1, 2]|}),
    roundtrip_incomplete_test({|Two unopened parens|}, {|1) + 2)|}),
    /* Middle-missing shards: targeted put-down strands the interior
       delimiter in the backpack, leaving e.g. a let tile with shards
       [0,2]; completion fills the gap in place with a hole */
    roundtrip_incomplete_test({|Let missing equals|}, {|let x in 2|}),
    roundtrip_incomplete_test({|If missing then|}, {|if true else 2|}),
    /* Keyword-form leading loss is only reachable via destructive edits:
       select and destruct the case opener, then roundtrip the resulting
       segment (case tile with shards [1]) */
    test_case(
      "Leading loss via edit: deleted case opener",
      `Quick,
      () => {
        let z = Test_Editing.mk_zipper({|§case ¦x | A => 1 end|});
        let z =
          [Action.Destruct(Local(Right, ByChar))]
          |> Test_Editing.perform(z);
        let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
        check_incomplete_roundtrip(seg);
      },
    ),
    roundtrip_incomplete_test({|Complete control|}, {|let x = 1 in x|}),
    roundtrip_incomplete_test(
      {|Complete case control|},
      {|case x | A => 1 | B => 2 end|},
    ),
  ],
);

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
    /* NOTE: top-level orphaned rules (`1 | A => 2`) do not roundtrip
       through the PLAIN parse path (MakeTerm's generic hole path drops
       the tile tokens at Exp sort) — but they DO roundtrip through the
       canonical-completion path, which wraps them in a synthesized
       case/end; see "Orphaned rule" in roundtrip_incomplete_tests. */
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

/* P2: segment fixpoint. Render a generated exp to text, parse it to a
   parser-canonical segment, and check MakeTerm . PreserveExact-print is
   the identity on it: text always; tile ids only for hole-free renders
   (explicit-hole tiles print back as grout — audit class A4, hole
   flavor not yet in terms). */
let arb_segment_fixpoint =
  QCheck.Test.make(
    ~name="ExpToSegment: PreserveExact segment fixpoint on generated exps",
    ~count=50,
    QCheck_Util.arb_exp(~minimal_idents=true, 5),
    exp => {
      let text =
        exp
        |> ExpToSegment.exp_to_segment(
             ~settings=ExpToSegment.Settings.editable(~inline=true),
             _,
           )
        |> Printer.of_segment(~holes="?", ~refractors=[], _);
      switch (Parser.to_segment(text, ~root=Exp)) {
      | None => true /* unparseable render: out of scope for P2 */
      | Some(seg) =>
        let term = MakeTerm.go(seg).term;
        let seg2 = exp_to_segment_roundtrip(term);
        print_seg(seg) == print_seg(seg2)
        && tile_ids(seg) == tile_ids(seg2)
        && Segment.equiv_mod_grout(seg, seg2)
        /* P3 closure: reparsing the print gives the same term */
        && Language.Exp.fast_equal_with_lexemes(term, MakeTerm.go(seg2).term);
      };
    },
  );

/* Deterministically vary whitespace outside string literals: multi-space
   runs, newlines, and comments in place of single spaces. Strings are
   skipped (no escapes in Hazel; quote parity tracks in-string state). */
let perturb_spaces = (seed: int, text: string): string => {
  let out = ref([]);
  let emit = piece => out := [piece, ...out^];
  let in_string = ref(false);
  let k = ref(seed land 0xffff);
  let next = () => {
    k := (k^ * 1103515245 + 12345) land 0x3fffffff;
    k^ mod 6;
  };
  String.iter(
    c => {
      if (c == '"') {
        in_string := ! in_string^;
      };
      if (c == ' ' && ! in_string^) {
        switch (next()) {
        | 0 => emit("  ")
        | 1 => emit("\n")
        | 2 => emit(" \n ")
        | 3 => emit(" #c# ")
        | _ => emit(" ")
        };
      } else {
        emit(String.make(1, c));
      };
    },
    text,
  );
  String.concat("", List.rev(out^));
};

let arb_perturbed_fixpoint =
  QCheck.Test.make(
    ~name=
      "ExpToSegment: secondary-perturbed segment fixpoint on generated exps",
    ~count=50,
    QCheck.pair(
      QCheck_Util.arb_exp(~minimal_idents=true, 5),
      QCheck.small_nat,
    ),
    ((exp, seed)) => {
      let text =
        exp
        |> ExpToSegment.exp_to_segment(
             ~settings=ExpToSegment.Settings.editable(~inline=true),
             _,
           )
        |> Printer.of_segment(~holes="?", ~refractors=[], _);
      let text = perturb_spaces(seed, text);
      switch (Parser.to_segment(text, ~root=Exp)) {
      | None => true
      | Some(seg) =>
        let term = MakeTerm.go(seg).term;
        let seg2 = exp_to_segment_roundtrip(term);
        print_seg(seg) == print_seg(seg2)
        && tile_ids(seg) == tile_ids(seg2)
        && Segment.equiv_mod_grout(seg, seg2)
        /* P3 closure: reparsing the print gives the same term */
        && Language.Exp.fast_equal_with_lexemes(term, MakeTerm.go(seg2).term);
      };
    },
  );

let property_tests = (
  "Round-Trip: Property",
  [
    QCheck_alcotest.to_alcotest(~speed_level=`Slow, arb_segment_fixpoint),
    QCheck_alcotest.to_alcotest(~speed_level=`Slow, arb_perturbed_fixpoint),
  ],
);

/* pad_ids: padding/replacement ids must be derived, not minted —
   printing is a pure function of the term */
let pad_ids_tests = (
  "ExpToSegment.PadIds",
  [
    Alcotest.test_case(
      "padding is deterministic",
      `Quick,
      () => {
        let base = Id.mk();
        Alcotest.(check(bool))(
          "two pads agree",
          true,
          ExpToSegment.pad_ids(3, [base])
          == ExpToSegment.pad_ids(3, [base]),
        );
      },
    ),
    Alcotest.test_case(
      "duplicate replacement is deterministic and distinct",
      `Quick,
      () => {
        let base = Id.mk();
        let a = ExpToSegment.pad_ids(2, [base, base]);
        let b = ExpToSegment.pad_ids(2, [base, base]);
        Alcotest.(check(bool))("stable", true, a == b);
        Alcotest.(check(bool))(
          "no dups",
          true,
          List.length(List.sort_uniq(Id.compare, a)) == 2,
        );
      },
    ),
    Alcotest.test_case(
      "derived stream avoids Id.next chains",
      `Quick,
      () => {
        let base = Id.mk();
        let padded = ExpToSegment.pad_ids(4, [base]) |> List.tl;
        let nexts = [
          Id.next(base),
          Id.next(Id.next(base)),
          Id.next(Id.next(Id.next(base))),
        ];
        Alcotest.(check(bool))(
          "disjoint from next chain",
          true,
          List.for_all(id => !List.mem(id, nexts), padded),
        );
      },
    ),
  ],
);

/* Expressions embedded in PROOF steps get the same defensive parens as
   any other expression. Steps inserted by the stepper UI are BUILT, not
   parsed, so they carry no explicit Parens node: `parenthesize` has to
   descend through the proof to add them, otherwise a `fun`/`fun where`
   in application-head position reparses as the argument being applied
   inside the body (`(fun x -> 100 / x)(y)` printing as
   `fun x -> 100 / x(y)`). */
let built_fun_ap = (~guard: bool, ()): Exp.t => {
  let body = Test_Evaluator_Prelude.parse_exp({|100 / x|});
  let pat = Pat.fresh(Var("x"));
  let f =
    Exp.fresh(
      guard
        ? FunWhere(pat, Test_Evaluator_Prelude.parse_exp({|x != 0|}), body)
        : Fun(pat, body, None, None),
    );
  Exp.fresh(Ap(Forward, f, Test_Evaluator_Prelude.parse_exp({|y|})));
};

let theorem_around = (pf: TermBase.Proof.term): Exp.t =>
  Exp.fresh(
    Theorem(
      Pat.fresh(Var("t")),
      Test_Evaluator_Prelude.parse_exp({|1 == 1|}),
      Proof.fresh(pf),
      Exp.fresh(Var("t")),
    ),
  );

let proof_paren_test =
    (name: string, pf: TermBase.Proof.term, expected: string) =>
  test_case(
    name,
    `Quick,
    () => {
      let printed = editable_print(theorem_around(pf));
      check(string, "printed with defensive parens", expected, printed);
      /* and it reparses to the same text */
      switch (Parser.to_term(printed, ~root=Exp)) {
      | Some(t) =>
        check(string, "reparse fixed point", printed, editable_print(t))
      | None => Alcotest.fail("failed to reparse: " ++ printed)
      };
    },
  );

let proof_embedded_paren_tests = (
  "Defensive Parens: Proof-Embedded Expressions",
  [
    proof_paren_test(
      {|axiom `on` term: fun where in application head|},
      AxiomStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(0)))),
        at_exp: built_fun_ap(~guard=true, ()),
        direction: Util.Direction.Right,
        equality: Exp.fresh(Var("refl_eq")),
        instantiation: None,
      }),
      {|theorem t = 1 == 1 proof axiom refl_eq at 0 on(fun x where x != 0 -> 100 / x)(y) end in t|},
    ),
    proof_paren_test(
      {|axiom `on` term: plain fun in application head|},
      AxiomStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(0)))),
        at_exp: built_fun_ap(~guard=false, ()),
        direction: Util.Direction.Right,
        equality: Exp.fresh(Var("refl_eq")),
        instantiation: None,
      }),
      {|theorem t = 1 == 1 proof axiom refl_eq at 0 on(fun x -> 100 / x)(y) end in t|},
    ),
    proof_paren_test(
      {|assume hypothesis: fun where in application head|},
      Assume(built_fun_ap(~guard=true, ()), Proof.fresh(EmptyHole)),
      {|theorem t = 1 == 1 proof assume(fun x where x != 0 -> 100 / x)(y) => ? in t|},
    ),
    proof_paren_test(
      {|contradiction argument: fun where in application head|},
      Contradiction(built_fun_ap(~guard=true, ()), None),
      {|theorem t = 1 == 1 proof contradiction(fun x where x != 0 -> 100 / x)(y) end in t|},
    ),
    proof_paren_test(
      {|eval `at` term: fun where in application head|},
      EvalStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(0)))),
        at_exp: built_fun_ap(~guard=true, ()),
      }),
      {|theorem t = 1 == 1 proof eval(fun x where x != 0 -> 100 / x)(y) at 0 end in t|},
    ),
  ],
);

let all = [
  tests,
  pad_ids_tests,
  proof_embedded_paren_tests,
  roundtrip_tests,
  roundtrip_incomplete_tests,
  roundtrip_defensive_paren_tests,
  defensive_idempotence_tests,
  roundtrip_larger_programs,
  roundtrip_grout_string_tests,
  roundtrip_projector_tests,
  property_tests,
];
