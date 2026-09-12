/* Printing a type as a segment, and the ids naming the tokens it printed as.

   Code.re colours a tile when its id is in the set, so the ids have to line
   up with the tiles the printer actually emits. Two ways that can fail,
   neither caught by checking the padding alone: an id in the set but never
   emitted colours nothing, and a tile emitted for a runtime-derived part but
   left out stays the static colour. Parens are where both bite -- preparing
   for printing inserts them as real nodes that the printer emits. */

open Alcotest;
open Util;
open Haz3lcore;
open Language;

/* The settings the projector prints with. Testing any other configuration
   would test something the projector never runs. */
let settings = ProjectorInfo.seg_settings(~inline=true);

/* The ids of the tiles alone. Code.re classes tiles and ignores Grout and
   Secondary, so this is the set of ids a decoration can actually colour. */
let rec tile_ids = (s: Segment.t): list(Id.t) =>
  List.concat_map(tile_ids_of_piece, s)
and tile_ids_of_piece = (p: Piece.t): list(Id.t) =>
  switch (p) {
  | Tile(t) => [Piece.id(p), ...tile_ids(List.concat(t.children))]
  | Grout(_)
  | Secondary(_)
  | Projector(_) => []
  };

/* Walk a segment producing (text, classes) fragments for each atomic part */
let rec segment_fragments =
        (classes: Id.t => list(string), seg: Segment.t)
        : list((string, list(string))) =>
  List.concat_map(piece_fragments(classes), seg)
and piece_fragments =
    (classes: Id.t => list(string), p: Piece.t)
    : list((string, list(string))) =>
  switch (p) {
  | Tile(t) => tile_fragments(classes, t)
  | Grout(g) => [("?", classes(g.id))]
  | Secondary(w) =>
    let text =
      switch (w.content) {
      | Whitespace(s)
      | Comment(s) => s
      };
    [(text, [])];
  | Projector(_) => []
  }
and tile_fragments =
    (classes: Id.t => list(string), t: Tile.t)
    : list((string, list(string))) => {
  let clss = classes(t.id);
  Aba.mk(t.shards, t.children)
  |> Aba.join(
       shard => [(List.nth(t.label, shard), clss)],
       segment_fragments(classes),
     )
  |> List.concat;
};

/* Group contiguous fragments with the same class status, concatenating text.
   Whitespace-only fragments are absorbed into their neighbor's group. */
let group_regions =
    (fragments: list((string, list(string))))
    : list((string, list(string))) => {
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

/* A type written as source, so a test reads as the type it is about. Not for
   types whose construction carries something the parser cannot express --
   constructor annotations, ids shared with a context. */
let typ = (src: string): Typ.t =>
  switch (
    Parser.to_segment(src, ~root=Typ) |> Option.map(MakeTerm.for_projection)
  ) {
  | Some(Some(Typ(t))) => t
  | _ => Alcotest.failf("could not parse the type `%s`", src)
  };

/* The regions of the printed type, each tagged with the classes Code.re
   would give it. */
let classify_regions =
    (~ctx: option(Ctx.t)=?, static_typ: Typ.t, dynamic_typ: Typ.t)
    : list((string, list(string))) => {
  let (segment, dynamic_ids) =
    TypToSegment.typ_to_segment_with_diff_ids(
      ~settings,
      ~ctx?,
      ~against=static_typ,
      dynamic_typ,
    );
  let classes = id => Id.Set.mem(id, dynamic_ids) ? ["dynamic"] : [];
  segment_fragments(classes, segment) |> group_regions;
};

let region =
  testable(
    Fmt.using(
      ((text, clss)) => {
        let label =
          switch (clss) {
          | [] => "static"
          | _ => String.concat(" ", clss)
          };
        label ++ "(\"" ++ text ++ "\")";
      },
      Fmt.string,
    ),
    (==),
  );

let s = text => (text, []);
let d = text => (text, ["dynamic"]);

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
      let result = classify_regions(typ("(Int, ?)"), typ("(Int, String)"));
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
      let result = classify_regions(typ("Int -> ?"), typ("Int -> String"));
      check(
        list(region),
        "Int and -> static, String dynamic",
        [s("Int ->"), d("String")],
        result,
      );
    },
  );

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
            kind: Singleton(typ("[Int]")),
          },
        );
      let result = classify_regions(~ctx, typ("MyList"), typ("[Int]"));
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
            kind: Singleton(typ("(Int, ?)")),
          },
        );
      let result =
        classify_regions(~ctx, typ("Pair"), typ("(Int, String)"));
      check(
        list(region),
        "Int static, String dynamic",
        [s("(Int,"), d("String"), s(")")],
        result,
      );
    },
  );

let sum_missing_constructor_test =
  test_case(
    "Sum type — dynamic missing constructor (+None +Some(Int) vs +Some(Int))",
    `Quick,
    () => {
      let result =
        classify_regions(
          mk_sum_none_some_int(),
          Typ.fresh(
            Sum([
              ConstructorMap.Variant(
                "Some",
                ann,
                Some(Typ.fresh(Atom(Atom.Int))),
              ),
            ]),
          ),
        );
      /* Dynamic is missing None, so entire dynamic Sum is highlighted */
      check(list(region), "all dynamic", [d("+ Some(Int)")], result);
    },
  );

let alias_on_dynamic_side_test =
  test_case(
    "Type alias — [Int] vs Var(MyList) with alias MyList = [Int]",
    `Quick,
    () => {
      let ctx =
        Ctx.extend_tvar(
          Ctx.empty,
          {
            name: "MyList",
            id: Id.mk(),
            kind: Singleton(typ("[Int]")),
          },
        );
      let result = classify_regions(~ctx, typ("[Int]"), typ("MyList"));
      /* Printed as the alias name, but no dynamic highlighting since
         the alias expands to the same type */
      check(list(region), "all static", [s("MyList")], result);
    },
  );

let region_tests = [
  sum_partial_diff_test,
  sum_fully_diff_test,
  sum_same_test,
  sum_missing_constructor_test,
  prod_partial_diff_test,
  arrow_diff_codomain_test,
  alias_exact_match_test,
  alias_partial_diff_test,
  alias_on_dynamic_side_test,
];

/* The dynamic ids, and the one segment they describe, reported two ways.
   The id sets are deliberately different: `emitted` includes Grout and
   Secondary, because a runtime-derived node can legitimately print as Grout
   -- an Unknown does, with show_unknown_as_hole off -- and including it is
   harmless. `tiles` is what Code.re actually colours, so it is the right set
   to require full coverage of. */
type printed = {
  dynamic_ids: Id.Set.t,
  emitted: Id.Set.t,
  tiles: Id.Set.t,
};

let dynamic_ids_and_printed =
    (~static_typ: Typ.t, ~dynamic_typ: Typ.t): printed => {
  let (seg, dynamic_ids) =
    TypToSegment.typ_to_segment_with_diff_ids(
      ~settings,
      ~against=static_typ,
      dynamic_typ,
    );
  {
    dynamic_ids,
    emitted: Segment.ids(seg) |> Id.Set.of_list,
    tiles: tile_ids(seg) |> Id.Set.of_list,
  };
};

/* SOUNDNESS. Every id in the set must appear somewhere in the segment. An id that
   appears nowhere describes nothing, and means the dynamic_ids and the segment were
   computed from different types. */
let qcheck_dynamic_ids_are_emitted =
  QCheck.Test.make(
    ~name="every dynamic id appears in the printed segment",
    ~count=300,
    QCheck.pair(
      QCheck_Util.arb_typ(~minimal_idents=false, 12),
      QCheck_Util.arb_typ(~minimal_idents=false, 12),
    ),
    ((static_typ, dynamic_typ)) => {
      let {dynamic_ids, emitted, _} =
        dynamic_ids_and_printed(~static_typ, ~dynamic_typ);
      Id.Set.subset(dynamic_ids, emitted);
    },
  );

/* COMPLETENESS, in the case that needs no oracle: if statics knew nothing
   then the whole type came from runtime, so every tile must be green. */
let qcheck_fully_dynamic_colours_everything =
  QCheck.Test.make(
    ~name="a wholly runtime-derived type has every tile green",
    ~count=300,
    QCheck_Util.arb_typ(~minimal_idents=false, 12),
    dynamic_typ => {
      /* Only meaningful when runtime refined something: if the dynamic type
         is itself unknown, nothing was learned and nothing should be green. */
      QCheck.assume(
        switch (Typ.term_of(dynamic_typ)) {
        | Unknown(_) => false
        | _ => true
        },
      );
      let {dynamic_ids, tiles, _} =
        dynamic_ids_and_printed(
          ~static_typ=Typ.fresh(Unknown(Internal)),
          ~dynamic_typ,
        );
      Id.Set.subset(tiles, dynamic_ids);
    },
  );

/* A type runtime merely confirmed has nothing to colour. */
let qcheck_identical_colours_nothing =
  QCheck.Test.make(
    ~name="a type identical to the static one dynamic_ids nothing",
    ~count=300,
    QCheck_Util.arb_typ(~minimal_idents=false, 12),
    typ => {
      let {dynamic_ids, _} =
        dynamic_ids_and_printed(~static_typ=typ, ~dynamic_typ=typ);
      Id.Set.is_empty(dynamic_ids);
    },
  );

/* The invariant the whole scheme rests on: a prepared type already carries
   every id printing it consumes, so the printer never mints one. An id
   minted while printing is in the DOM but in no type, so nothing can name
   it and the token it labels can never be coloured. */
let qcheck_prepared_ids_are_sufficient =
  QCheck.Test.make(
    ~name="a prepared type carries every id printing it consumes",
    ~count=500,
    QCheck_Util.arb_typ(~minimal_idents=false, 20),
    typ =>
    TypToSegment.ids_sufficient(~settings, typ)
  );

/* Unit pins for the id counts typ_to_pretty pads from. */
let count_tests =
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "necessary_ids matches what each form consumes",
      `Quick,
      () => {
        let check_count = (name, expected, typ) =>
          check(Alcotest.int, name, expected, PadIds.necessary_ids(typ));
        check_count("unit prints from rep_id", 1, Prod([]) |> Typ.temp);
        check_count(
          "a pair needs one separator",
          1,
          Prod([int(), bool()]) |> Typ.temp,
        );
        check_count(
          "a triple needs two separators",
          2,
          Prod([int(), bool(), string()]) |> Typ.temp,
        );
        check_count("Void prints from rep_id", 1, Sum([]) |> Typ.temp);
        check_count("Int prints from rep_id", 1, int());
        check_count(
          "an empty sig prints from rep_id",
          1,
          Sig([]) |> Typ.temp,
        );
        let sig_item = (): Sig.t => Sig.temp(EmptyHole);
        check_count(
          "a one-item sig prints from rep_id",
          1,
          Sig([sig_item()]) |> Typ.temp,
        );
        check_count(
          "a three-item sig needs two separators plus rep_id",
          3,
          Sig([sig_item(), sig_item(), sig_item()]) |> Typ.temp,
        );
      },
    ),
  ];
let tests = [
  ("TypToSegment.Regions", region_tests),
  (
    "TypToSegment.Ids",
    count_tests
    @ [
      QCheck_alcotest.to_alcotest(qcheck_dynamic_ids_are_emitted),
      QCheck_alcotest.to_alcotest(qcheck_fully_dynamic_colours_everything),
      QCheck_alcotest.to_alcotest(qcheck_identical_colours_nothing),
      QCheck_alcotest.to_alcotest(qcheck_prepared_ids_are_sufficient),
    ],
  ),
];
