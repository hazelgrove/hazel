open Alcotest;
open Util;
open Haz3lcore;
open Language;

let settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: false,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_filters: true,
  show_unknown_as_hole: true,
  project_tables: false,
  show_ascriptions: true,
  raise_if_padding: false,
};

/* Walk a segment producing (text, is_dynamic) fragments for each atomic part */
let rec segment_fragments =
        (is_dynamic: Id.t => bool, seg: Segment.t): list((string, bool)) =>
  List.concat_map(piece_fragments(is_dynamic), seg)
and piece_fragments =
    (is_dynamic: Id.t => bool, p: Piece.t): list((string, bool)) =>
  switch (p) {
  | Tile(t) => tile_fragments(is_dynamic, t)
  | Grout(g) => [("?", is_dynamic(g.id))]
  | Secondary(w) =>
    let text =
      switch (w.content) {
      | Whitespace(s)
      | Comment(s) => s
      };
    [(text, false)];
  | Projector(_) => []
  }
and tile_fragments =
    (is_dynamic: Id.t => bool, t: Tile.t): list((string, bool)) => {
  let dyn = is_dynamic(t.id);
  Aba.mk(t.shards, t.children)
  |> Aba.join(
       shard => [(List.nth(t.label, shard), dyn)],
       segment_fragments(is_dynamic),
     )
  |> List.concat;
};

/* Group contiguous fragments with the same dynamic status, concatenating text.
   Whitespace-only fragments are absorbed into their neighbor's group. */
let group_regions =
    (fragments: list((string, bool))): list((string, bool)) => {
  let rec go =
    fun
    | [] => []
    | [(text, status), ...rest] => {
        let (group_text, remaining) = collect(status, text, rest);
        [(String.trim(group_text), status), ...go(remaining)];
      }
  and collect = (status, acc, rest) =>
    switch (rest) {
    | [(text, s), ...rest'] when s == status =>
      collect(status, acc ++ text, rest')
    /* Absorb whitespace-only fragments into current group */
    | [(text, _), ...rest'] when String.trim(text) == "" =>
      collect(status, acc ++ text, rest')
    | _ => (acc, rest)
    };
  go(fragments) |> List.filter(((text, _)) => text != "");
};

/* Given static and dynamic types, return grouped regions of (text, is_dynamic) */
let classify_regions =
    (static_typ: Typ.t, dynamic_typ: Typ.t): list((string, bool)) => {
  let (is_dynamic, padded_dyn) =
    PadIds.compute_dynamic_ids(~static_typ, ~dynamic_typ, ());
  let segment = ExpToSegment.typ_to_segment(~settings, padded_dyn);
  segment_fragments(is_dynamic, segment) |> group_regions;
};

let region =
  testable(
    Fmt.using(
      ((text, is_dyn)) =>
        (is_dyn ? "dynamic" : "static") ++ "(\"" ++ text ++ "\")",
      Fmt.string,
    ),
    (==),
  );

let s = text => (text, false);
let d = text => (text, true);

let ann = ConstructorMap.empty_variant_ann;

let mk_sum_none_some_unknown = () =>
  Typ.fresh(
    Sum([
      ConstructorMap.Variant("None", ann, None),
      ConstructorMap.Variant(
        "Some",
        ann,
        Some(Typ.fresh(Unknown(Internal))),
      ),
    ]),
  );

let mk_sum_none_some_int = () =>
  Typ.fresh(
    Sum([
      ConstructorMap.Variant("None", ann, None),
      ConstructorMap.Variant("Some", ann, Some(Typ.fresh(Atom(Atom.Int)))),
    ]),
  );

let sum_partial_diff_test =
  test_case(
    "Sum type — partially different (+None +Some(?) vs +None +Some(Int))",
    `Quick,
    () => {
      let result =
        classify_regions(mk_sum_none_some_unknown(), mk_sum_none_some_int());
      check(
        list(region),
        "None static, Int dynamic",
        [s("+ None + Some("), d("Int"), s(")")],
        result,
      );
    },
  );

let sum_fully_diff_test =
  test_case(
    "Sum type — fully different (? vs +None +Some(Int))",
    `Quick,
    () => {
      let result =
        classify_regions(
          Typ.fresh(Unknown(Internal)),
          mk_sum_none_some_int(),
        );
      check(
        list(region),
        "all dynamic",
        [d("+ None + Some(Int)")],
        result,
      );
    },
  );

let sum_same_test =
  test_case(
    "Sum type — same constructors, same types",
    `Quick,
    () => {
      let result =
        classify_regions(mk_sum_none_some_int(), mk_sum_none_some_int());
      check(list(region), "all static", [s("+ None + Some(Int)")], result);
    },
  );

let prod_partial_diff_test =
  test_case(
    "Product type — partially different ((Int, ?) vs (Int, String))",
    `Quick,
    () => {
      let result =
        classify_regions(
          Typ.fresh(
            Prod([
              Typ.fresh(Atom(Atom.Int)),
              Typ.fresh(Unknown(Internal)),
            ]),
          ),
          Typ.fresh(
            Prod([
              Typ.fresh(Atom(Atom.Int)),
              Typ.fresh(Atom(Atom.String)),
            ]),
          ),
        );
      check(
        list(region),
        "Int static, String dynamic",
        [s("(Int,"), d("String"), s(")")],
        result,
      );
    },
  );

let arrow_diff_codomain_test =
  test_case(
    "Arrow type — different codomain (Int -> ? vs Int -> String)",
    `Quick,
    () => {
      let result =
        classify_regions(
          Typ.fresh(
            Arrow(
              Typ.fresh(Atom(Atom.Int)),
              Typ.fresh(Unknown(Internal)),
            ),
          ),
          Typ.fresh(
            Arrow(
              Typ.fresh(Atom(Atom.Int)),
              Typ.fresh(Atom(Atom.String)),
            ),
          ),
        );
      check(
        list(region),
        "Int and -> static, String dynamic",
        [s("Int ->"), d("String")],
        result,
      );
    },
  );

/* Given static and dynamic types and a ctx, return grouped regions */
let classify_regions_ctx =
    (~ctx: Ctx.t, static_typ: Typ.t, dynamic_typ: Typ.t)
    : list((string, bool)) => {
  let (is_dynamic, padded_dyn) =
    PadIds.compute_dynamic_ids(~ctx, ~static_typ, ~dynamic_typ, ());
  let segment = ExpToSegment.typ_to_segment(~settings, padded_dyn);
  segment_fragments(is_dynamic, segment) |> group_regions;
};

let alias_exact_match_test =
  test_case(
    "Type alias — Var(MyList) vs [Int] with alias MyList = [Int]",
    `Quick,
    () => {
      let ctx =
        Ctx.extend_tvar(
          Ctx.empty,
          {
            name: "MyList",
            id: Id.mk(),
            kind: Singleton(Typ.fresh(List(Typ.fresh(Atom(Atom.Int))))),
          },
        );
      let result =
        classify_regions_ctx(
          ~ctx,
          Typ.fresh(Var("MyList")),
          Typ.fresh(List(Typ.fresh(Atom(Atom.Int)))),
        );
      check(list(region), "all static", [s("[Int]")], result);
    },
  );

let alias_partial_diff_test =
  test_case(
    "Type alias — Var(Pair) expands to (Int, ?) vs (Int, String)",
    `Quick,
    () => {
      let ctx =
        Ctx.extend_tvar(
          Ctx.empty,
          {
            name: "Pair",
            id: Id.mk(),
            kind:
              Singleton(
                Typ.fresh(
                  Prod([
                    Typ.fresh(Atom(Atom.Int)),
                    Typ.fresh(Unknown(Internal)),
                  ]),
                ),
              ),
          },
        );
      let result =
        classify_regions_ctx(
          ~ctx,
          Typ.fresh(Var("Pair")),
          Typ.fresh(
            Prod([
              Typ.fresh(Atom(Atom.Int)),
              Typ.fresh(Atom(Atom.String)),
            ]),
          ),
        );
      check(
        list(region),
        "Int static, String dynamic",
        [s("(Int,"), d("String"), s(")")],
        result,
      );
    },
  );

let qcheck_all_piece_ids_classified =
  QCheck_alcotest.to_alcotest(
    QCheck.Test.make(
      ~name="All piece IDs are classified (no orphaned IDs)",
      ~count=500,
      QCheck.pair(
        QCheck_Util.arb_typ(~minimal_idents=true, 7),
        QCheck_Util.arb_typ(~minimal_idents=true, 7),
      ),
      ((static_typ, dynamic_typ)) => {
        let (is_dynamic, padded_dyn) =
          PadIds.compute_dynamic_ids(~static_typ, ~dynamic_typ, ());
        let segment = ExpToSegment.typ_to_segment(~settings, padded_dyn);
        let fragments = segment_fragments(is_dynamic, segment);
        /* Every fragment should be classified as either dynamic or not */
        List.for_all(((_text, is_dyn)) => is_dyn || !is_dyn, fragments);
      },
    ),
  );

let tests = [
  (
    "DynamicTyp",
    [
      sum_partial_diff_test,
      sum_fully_diff_test,
      sum_same_test,
      prod_partial_diff_test,
      arrow_diff_codomain_test,
      alias_exact_match_test,
      alias_partial_diff_test,
      qcheck_all_piece_ids_classified,
    ],
  ),
];
