/* Table-driven coverage of MakeTerm against Form's definition rows.
 *
 * Form.rows_of is an open, data-driven table: adding a row (a family at
 * a new sort) does not force a MakeTerm arm into existence, so a row
 * with no parser arm silently parses to a hole. This test closes that
 * loop: for EVERY row in Form.forms it builds a minimal instance (the
 * form's tile, each inner child a minimal atom of the child's sort, and
 * a minimal operand on each concave side), parses it with MakeTerm at
 * the row's out sort, and requires that neither the root nor any kid
 * comes back a hole (EmptyHole / MultiHole / Invalid / Drv Hole /
 * Unknown(Hole) / sum BadEntry).
 *
 * Every row is run twice: once plain and once with each single-piece
 * kid wrapped in a fold projector, the shape that `for_projection`
 * allows the user to create. A projector is a convex wrapper; parsing
 * through it must yield the same kind of term as the bare kid.
 *
 * Rows that legitimately cannot parse in isolation are listed in
 * `known_failures` with a reason; the test asserts the failure set is
 * EXACTLY that list, so a newly unparseable row fails here, and fixing
 * a listed one fails here too (prompting removal from the list). */
open Alcotest;
open Haz3lcore;

/* A minimal atom of each sort. Rul and Sig have no single-token
   inhabitant, so their atoms are small compound segments. */
let tok = (sort: Sort.t, t: Token.t): Piece.t =>
  Piece.mk_tile((Form.Tok(t), sort), []);

let atom = (sort: Sort.t): Segment.t =>
  switch (sort) {
  | Exp
  | Any => [tok(Exp, "x")]
  | Pat => [tok(Pat, "x")]
  | Typ => [tok(Typ, "Int")]
  | TPat => [tok(TPat, "t")]
  | MPat => [tok(MPat, "M")]
  | Mod => [tok(Mod, "x")]
  | Rul => [
      tok(Exp, "x"),
      Piece.mk_tile((Compound(Rule), Rul), [[tok(Pat, "y")]]),
      tok(Exp, "z"),
    ]
  | Sig => [Piece.mk_tile((Compound(SigLet), Sig), []), tok(Pat, "x")]
  | Drv(Exp | Jdmt | Ctx | Prop) => [tok(Drv(Exp), "True")]
  | Drv(Pat) => [tok(Drv(Pat), "x")]
  | Drv(Typ) => [tok(Drv(Typ), "Num")]
  | Drv(TPat) => [tok(Drv(TPat), "a")]
  };

/* Two derivation case rules, `True | x => True | y => True`: Drv case
   takes exactly two (see drv_exp_term). */
let drv_rule = (p: Token.t): Piece.t =>
  Piece.mk_tile((Compound(Rule), Drv(Exp)), [[tok(Drv(Pat), p)]]);
let drv_rules: Segment.t = [
  tok(Drv(Exp), "True"),
  drv_rule("x"),
  tok(Drv(Exp), "True"),
  drv_rule("y"),
  tok(Drv(Exp), "True"),
];

type position =
  | L
  | In
  | R;

/* The kid at a position of a row's instance. Some rows only accept
   particular operands, where the generic atom would be a legitimate
   parse error for that row; those get the operand they need. */
let kid_atom =
    (fam: Form.family, out: Sort.t, pos: position, sort: Sort.t): Segment.t =>
  switch (fam, out, pos) {
  /* sum entries are constructors, not types */
  | (Plus | SumSingle, Typ, _) => [tok(Typ, "A")]
  /* Drv projections are only .fst/.snd */
  | (Dot, Drv(Exp), R) => [tok(Drv(Exp), "fst")]
  /* the pattern grammar only folds a minus into a literal */
  | (UnaryMinus, Pat, R) => [tok(Pat, "1")]
  /* Drv pattern applications are only L(p)/R(p) */
  | (Ap, Drv(Pat), L) => [tok(Drv(Pat), "L")]
  /* Drv case takes exactly two rules */
  | (Case, Drv(Exp), In) => drv_rules
  | (Rule, Drv(Exp), L) => [
      tok(Drv(Exp), "True"),
      drv_rule("x"),
      tok(Drv(Exp), "True"),
    ]
  | _ => atom(sort)
  };

/* Wrap a kid in a fold projector, as for_projection lets a user do.
   A projector's syntax is a single piece: a single-piece kid is the
   syntax itself, a Sig kid (no single-token inhabitant) goes in
   parentheses, which MakeTerm strips (Piece.unparenthesize). Other
   multi-piece kids (Rul, which for_projection rejects, and the Drv
   case rules) stay bare. */
let wrap_projector = (sort: Sort.t, seg: Segment.t): Segment.t =>
  switch (seg, sort) {
  | ([p], _) => [Piece.Projector(ProjectorCore.mk(Fold, p, ""))]
  | (_, Sig) =>
    let parens = Piece.mk_tile((Compound(Parens), Sig), [seg]);
    [Piece.Projector(ProjectorCore.mk(Fold, parens, ""))];
  | _ => seg
  };

let is_concave = (n: Nib.t): bool =>
  switch (n.shape) {
  | Concave(_) => true
  | Convex => false
  };

/* The instance of a row: [left operand] tile(children) [right operand].
   Returns the segment and the kid segments (to check each kid). */
let instance =
    (~wrap: bool, fam: Form.family, mold: Mold.t)
    : (Segment.t, list(Segment.t)) => {
  let kid = (pos, s) => {
    let seg = kid_atom(fam, mold.out, pos, s);
    wrap ? wrap_projector(s, seg) : seg;
  };
  let children = List.map(kid(In), mold.in_);
  let tile = Piece.mk_tile((Compound(fam), mold.out), children);
  let (l, r) = mold.nibs;
  let left = is_concave(l) ? [kid(L, l.sort)] : [];
  let right = is_concave(r) ? [kid(R, r.sort)] : [];
  (
    List.concat(left) @ [tile] @ List.concat(right),
    left @ children @ right,
  );
};

/* Parse a segment at a given sort, resetting MakeTerm's accumulators as
   go_impl does. Returns the root term and the id -> term map. */
let parse_at = (sort: Sort.t, seg: Segment.t): (Language.Any.t, TermMap.t) => {
  MakeTerm.map := TermMap.empty;
  MakeTerm.term_data := Id.Map.empty;
  MakeTerm.projectors := Id.Map.empty;
  MakeTerm.projector_list := [];
  MakeTerm.adopted_ids := [];
  MakeTerm.secondary_map := Segment.SecondaryCollection.collect(seg);
  MakeTerm.shard_masks := Id.Map.empty;
  MakeTerm.pending_lexeme := None;
  let any = MakeTerm.go_s(sort, Segment.skel(~sort, seg), seg);
  (any, MakeTerm.map^);
};

let is_mod_kid: Language.Any.t => bool =
  fun
  | Mod(_) => true
  | _ => false;
let is_sig_kid: Language.Any.t => bool =
  fun
  | Sig(_) => true
  | _ => false;

/* Is this term (at its root) a parse failure? Exhaustive over sorts.
   Mod/Sig MultiHoles of two or more items are the parser's encoding of
   `;` sequences (flattened by ModBody), not holes. */
let is_hole = (any: Language.Any.t): bool =>
  switch (any) {
  | Exp(e) =>
    switch (e.term) {
    | EmptyHole
    | MultiHole(_)
    | Invalid(_) => true
    | _ => false
    }
  | Pat(p) =>
    switch (p.term) {
    | EmptyHole
    | MultiHole(_)
    | Invalid(_) => true
    | _ => false
    }
  | Typ(t) =>
    switch (t.term) {
    | Unknown(Hole(_)) => true
    | Sum(entries) =>
      List.exists(
        fun
        | Language.ConstructorMap.BadEntry(_) => true
        | Variant(_) => false,
        entries,
      )
    | _ => false
    }
  | TPat(tp) =>
    switch (tp.term) {
    | EmptyHole
    | MultiHole(_)
    | Invalid(_) => true
    | Var(_) => false
    }
  | Rul(r) =>
    switch (r.term) {
    | MultiHole(_)
    | Invalid(_) => true
    | Rules(_) => false
    }
  | Drv(Exp(e)) =>
    switch (e.term) {
    | Hole(_) => true
    | _ => false
    }
  | Drv(Pat(p)) =>
    switch (p.term) {
    | Hole(_) => true
    | _ => false
    }
  | Drv(Typ(t)) =>
    switch (t.term) {
    | Hole(_) => true
    | _ => false
    }
  | Drv(TPat(tp)) =>
    switch (tp.term) {
    | Hole(_) => true
    | _ => false
    }
  | Mod(m) =>
    switch (m.term) {
    | MultiHole(kids) =>
      List.length(kids) < 2 || !List.for_all(is_mod_kid, kids)
    | EmptyHole
    | Invalid(_) => true
    | ModLet(_)
    | ModType(_)
    | ModExp(_)
    | ModuleMod(_) => false
    }
  | Sig(s) =>
    switch (s.term) {
    | MultiHole(kids) =>
      List.length(kids) < 2 || !List.for_all(is_sig_kid, kids)
    | EmptyHole
    | Invalid(_) => true
    | SigLet(_)
    | SigType(_) => false
    }
  | MPat(mp) =>
    switch (mp.term) {
    | EmptyHole
    | MultiHole(_)
    | Invalid(_) => true
    | Var(_)
    | Asc(_) => false
    }
  | Any () => true
  };

/* The top-level piece ids of a kid segment (tile or projector ids). */
let kid_ids = (seg: Segment.t): list(Id.t) =>
  seg |> List.filter(p => !Piece.is_secondary(p)) |> List.map(Piece.id);

let case_name = (~wrap, fam, sort) =>
  Form.show_family(fam)
  ++ " @ "
  ++ Sort.to_string(sort)
  ++ (wrap ? " (projected kids)" : "");

/* Parse a row instance; Some(reason) on failure. */
let check_row = (~wrap: bool, fam: Form.family, mold: Mold.t): option(string) => {
  let (seg, kids) = instance(~wrap, fam, mold);
  switch (parse_at(mold.out, seg)) {
  | exception e => Some("raised " ++ Printexc.to_string(e))
  | (root, map) =>
    if (is_hole(root)) {
      Some("root parses as a hole");
    } else {
      let bad_kid =
        kids
        |> List.concat_map(kid_ids)
        |> List.exists(id =>
             switch (Id.Map.find_opt(id, map)) {
             | Some(t) => is_hole(t)
             | None => false
             }
           );
      bad_kid ? Some("a kid parses as a hole") : None;
    }
  };
};

/* Rows that do not parse in isolation, by design or as tracked gaps.
   Keep this list exact: the test fails if it is stale either way. */
let known_failures: list((Form.family, Sort.t, bool, string)) = {
  let both = (fam: Form.family, sort: Sort.t, why: string) => [
    (fam, sort, false, why),
    (fam, sort, true, why),
  ];
  List.concat([
    /* A bare `A(Int)` at Typ is not a type: constructor applications
       parse as a MultiHole that only parse_sum_term (a sum context)
       reads back. Covered by the sum-type tests below. */
    both(
      Form.Ap,
      Typ,
      "ctor application outside a sum is not a type (by design)",
    ),
    /* Parens at TPat is a hack row (#1913) with no TPat term to
       parse into. */
    both(Form.Parens, TPat, "hack row (#1913); TPat has no Parens term"),
    /* Tracked gap: a pattern minus folds into the bare literal token
       it precedes, so folding the `1` of a `-1` pattern leaves the
       minus stranded (a hole). */
    [
      (
        Form.UnaryMinus,
        Pat,
        true,
        "folded literal under a pattern minus (tracked gap)",
      ),
    ],
    /* Display-only rows: there is no DrvGrammar constructor for
       substitution or glb. ExpToSegment builds these tiles to print
       RuleFormula rule specs; they are not user syntax, so a typed
       instance is always a hole. */
    both(Form.Subst, Drv(Exp), "display-only (RuleFormula printer)"),
    both(Subst, Drv(Typ), "display-only (RuleFormula printer)"),
    both(Glb, Drv(Typ), "display-only (RuleFormula printer)"),
  ]);
};

let failures = (): list((Form.family, Sort.t, bool, string)) =>
  Form.forms
  |> List.concat_map(((fam, def: Form.def)) =>
       [false, true]
       |> List.filter_map(wrap =>
            check_row(~wrap, fam, def.mold)
            |> Option.map(why => (fam, def.mold.out, wrap, why))
          )
     )
  /* the duplicated Typ Dot row (see Form.rows_of) */
  |> List.sort_uniq(compare);

let show_failure = ((fam, sort, wrap, why)) =>
  case_name(~wrap, fam, sort) ++ ": " ++ why;

let form_coverage_test =
  test_case(
    "every Form row parses (plain and through a projector)",
    `Quick,
    () => {
      let actual =
        failures()
        |> List.map(((f, s, w, _)) => (f, s, w))
        |> List.sort(compare);
      let expected =
        known_failures
        |> List.map(((f, s, w, _)) => (f, s, w))
        |> List.sort(compare);
      let show = l =>
        l
        |> List.map(((f, s, w)) => case_name(~wrap=w, f, s))
        |> String.concat("\n");
      if (actual != expected) {
        let unexpected = List.filter(x => !List.mem(x, expected), actual);
        let fixed = List.filter(x => !List.mem(x, actual), expected);
        Alcotest.failf(
          "Form/MakeTerm coverage mismatch.\nUnexpected failures:\n%s\n\nListed as known failures but now parse (remove from known_failures):\n%s\n\nAll failures with reasons:\n%s",
          show(unexpected),
          show(fixed),
          failures() |> List.map(show_failure) |> String.concat("\n"),
        );
      };
    },
  );

/* ---- Regression tests for specific MakeTerm / printer gaps ---- */

let parse_term = (s: string): Language.Exp.t =>
  switch (Parser.to_segment(s, ~root=Exp)) {
  | Some(seg) => MakeTerm.go(seg).term
  | None => Alcotest.fail("failed to parse: " ++ s)
  };

/* text -> segment -> term -> segment -> text, preserving secondary */
let reprint = (s: string): string =>
  parse_term(s)
  |> Test_ExpToSegment.exp_to_segment_roundtrip
  |> EditingPrelude.print_seg;

let reprint_test = (name, s) =>
  test_case(name, `Quick, () =>
    check(string, "term reprints its source", s, reprint(s))
  );

/* No static errors at all (the folded subterm must not be an error) */
let no_static_errors = (name, s) =>
  test_case(
    name,
    `Quick,
    () => {
      let e = Test_Statics_Prelude.parse_exp(s);
      let errs =
        Test_Statics_Prelude.statics(e)
        |> Test_Statics_Prelude.errors
        |> List.concat_map(snd)
        |> List.map(Language.Mark.show);
      check(list(string), "static errors", [], errs);
    },
  );

let sum_entries = (e: Language.Exp.t): list(string) =>
  switch (e.term) {
  | TyAlias(_, {term: Sum(entries), _}, _) =>
    entries
    |> List.map(
         fun
         | Language.ConstructorMap.Variant(c, _, None) => c
         | Variant(c, _, Some(_)) => c ++ "(_)"
         | BadEntry(_) => "BadEntry",
       )
  | _ => Alcotest.fail("expected a sum-type alias")
  };

/* Folding a constructor in a sum-type definition keeps it a
   constructor (it used to become a BadEntry, so statics reported
   VariantExpected and the constructor was unbound). */
let folded_ctor_tests = [
  test_case("folded nullary ctor is a variant", `Quick, () =>
    check(
      list(string),
      "entries",
      ["A", "B"],
      sum_entries(parse_term("type T = ^^fold(A) + B in ?")),
    )
  ),
  test_case("folded ctor-ap head is a variant", `Quick, () =>
    check(
      list(string),
      "entries",
      ["A(_)", "B"],
      sum_entries(parse_term("type T = ^^fold(A)(Int) + B in ?")),
    )
  ),
  no_static_errors(
    "folded ctor is bound and typechecks",
    "type T = ^^fold(A) + B in (A : T)",
  ),
  no_static_errors(
    "folded ctor-ap head typechecks",
    "type T = ^^fold(A)(Int) + B in (A(1) : T)",
  ),
];

/* Folding a tuple label keeps it a label (it used to become a
   MultiHole label, a label error). */
let tuple_label_names = (e: Language.Exp.t): list(string) =>
  switch (e.term) {
  | Parens({term: Tuple(es), _}) =>
    es
    |> List.map((e: Language.Exp.t) =>
         switch (e.term) {
         | TupLabel({term: Label(l), _}, _) => l
         | _ => "not a label"
         }
       )
  | _ => Alcotest.fail("expected a parenthesized tuple")
  };

let folded_label_tests = [
  test_case("folded exp tuple label is a label", `Quick, () =>
    check(
      list(string),
      "labels",
      ["x", "y"],
      tuple_label_names(parse_term("(^^fold(x) = 1, y = 2)")),
    )
  ),
  test_case("folded dot-projection label is a label", `Quick, () =>
    switch (parse_term("t.^^fold(x)").term) {
    | Dot(_, {term: Label("x"), _}) => ()
    | _ => Alcotest.fail("expected Dot(_, Label(x))")
    }
  ),
  no_static_errors(
    "folded exp label typechecks",
    "let t = (^^fold(x) = 1, y = 2) in t.x",
  ),
  no_static_errors(
    "folded pat label typechecks",
    "let (^^fold(x) = a, y = b) = (x = 1, y = 2) in a",
  ),
  no_static_errors(
    "folded typ label typechecks",
    "let t : (^^fold(x) = Int, y = Int) = (x = 1, y = 2) in t.x",
  ),
];

/* Derivation `f()` / `L()`: ApEmpty had a Drv(Exp) row but no arm */
let drv_ap_empty_tests = [
  test_case("drv f() parses as an application", `Quick, () =>
    switch (parse_term("of_jdmt f() end").term) {
    | DrvQuote(Exp({term: Ap(_, {term: Triv, _}), _}), _) => ()
    | _ => Alcotest.fail("expected DrvQuote(Ap(f, ()))")
    }
  ),
  test_case("drv L() parses as an injection", `Quick, () =>
    switch (parse_term("of_jdmt L() end").term) {
    | DrvQuote(Exp({term: InjL({term: Triv, _}), _}), _) => ()
    | _ => Alcotest.fail("expected DrvQuote(InjL(()))")
    }
  ),
  reprint_test("drv f() reprints as f()", "of_jdmt f() end"),
];

let tests = (
  "MakeTerm.FormCoverage",
  [form_coverage_test]
  @ folded_ctor_tests
  @ folded_label_tests
  @ drv_ap_empty_tests,
);
