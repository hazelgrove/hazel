/* W2a parity gates for ResidentProgram: the worker-side derivation
   (resident segments → MakeTerm.Incr → DefStatics) must produce the
   SAME per-item error/warning summary as an independent main-side
   derivation (monolithic MakeTerm.go → DefStatics from scratch), on
   full syncs and on per-item delta syncs; roster mismatches demand
   resync. These are the asserting gates plan §4.5 requires. */

open Alcotest;
open Haz3lcore;
open Language;

let settings = CoreSettings.on;

let reference_summary = (~root, ~generation, seg: Segment.t) => {
  let term =
    switch (root) {
    | Sort.Exp => MakeTerm.go(seg).term
    | _ => MakeTerm.go_mod_root(seg).term
    };
  ResidentProgram.Summary.of_def_statics(
    ~generation,
    ~piece_ids=ResidentProgram.piece_ids(seg),
    DefStatics.calc(~settings, term),
  );
};

let check_summary = (name, expected, actual) =>
  check(
    bool,
    name ++ ": resident summary == independent main derivation",
    true,
    ResidentProgram.Summary.equal(expected, actual),
  );

let exp_src = "let a = 1 in
let f = fun x -> x + a in
let bad : String = 2 in
let unused = 3 in
f(bad)";

let exp_src' = "let a = 1 in
let f = fun x -> x + a in
let bad : String = \"ok\" in
let unused = 3 in
f(bad)";

/* NOTE: small handwritten Mod-root sources hit FastParse (menhir)
   gaps easily; the Mod gate runs on the corpus instead. */

let parse = (~root, src) =>
  switch (CorpusUtil.parse(~root, src)) {
  | Some(seg) => seg
  | None => Alcotest.fail("corpus parse failed")
  };

let full_sync_case = (name, root, src) =>
  test_case(
    name,
    `Quick,
    () => {
      let seg = parse(~root, src);
      let rp =
        ResidentProgram.sync_full(~settings, ~generation=1, ~root, seg, None);
      check_summary(
        name,
        reference_summary(~root, ~generation=1, seg),
        ResidentProgram.summarize(rp),
      );
    },
  );

/* one-item delta: edit the text, reparse, ship only the changed slice
   (identified by fingerprint against the previous roster) */
let delta_case = (name, root, src, src') =>
  test_case(
    name,
    `Quick,
    () => {
      let seg = parse(~root, src);
      let rp =
        ResidentProgram.sync_full(~settings, ~generation=1, ~root, seg, None);
      /* production-shaped delta: main's segment evolves incrementally
         (unchanged items keep their piece ids — the identity-restore
         invariant), so build the post-edit state by splicing ONE
         freshly parsed slice into the ORIGINAL items */
      let seg' = parse(~root, src');
      let items' = ResidentProgram.items_of_segment(seg');
      let changed =
        List.combine(rp.items, items')
        |> List.filter_map(
             ((old: ResidentProgram.item, nu: ResidentProgram.item)) =>
             old.i_print == nu.i_print
               ? None : Some((old.i_id, nu.i_seg, nu.i_print))
           );
      check(
        int,
        name ++ ": exactly one item changed",
        1,
        List.length(changed),
      );
      let roster =
        List.combine(rp.items, items')
        |> List.map(((old: ResidentProgram.item, nu: ResidentProgram.item)) =>
             old.i_print == nu.i_print
               ? (old.i_id, old.i_print) : (nu.i_id, nu.i_print)
           );
      switch (
        ResidentProgram.sync_items(
          ~settings,
          ~generation=2,
          ~changed,
          ~roster,
          rp,
        )
      ) {
      | Error(_) => Alcotest.fail(name ++ ": delta sync rejected")
      | Ok(rp') =>
        /* the reference must share the resident's PIECES (summaries
           carry ids; an independent reparse of the same text ids
           everything differently): derive monolithically, from
           scratch, over the post-splice segment itself */
        check_summary(
          name,
          reference_summary(
            ~root,
            ~generation=2,
            ResidentProgram.segment_of_items(rp'.items),
          ),
          ResidentProgram.summarize(rp'),
        )
      };
    },
  );

let mismatch_case =
  test_case(
    "corrupted roster demands resync",
    `Quick,
    () => {
      let seg = parse(~root=Sort.Exp, exp_src);
      let rp =
        ResidentProgram.sync_full(
          ~settings,
          ~generation=1,
          ~root=Sort.Exp,
          seg,
          None,
        );
      let roster =
        List.map(
          (it: ResidentProgram.item) => (it.i_id, it.i_print + 1),
          rp.items,
        );
      switch (
        ResidentProgram.sync_items(
          ~settings,
          ~generation=2,
          ~changed=[],
          ~roster,
          rp,
        )
      ) {
      | Error(RosterMismatch) => ()
      | Error(_) => Alcotest.fail("wrong error")
      | Ok(_) => Alcotest.fail("mismatched roster accepted")
      };
    },
  );

let corpus_case =
  test_case(
    "mega-1k full-sync parity",
    `Slow,
    () => {
      let seg =
        switch (
          Option.bind(
            CorpusUtil.mega_src("mega-1k.hz"),
            CorpusUtil.parse(~root=Sort.Exp),
          )
        ) {
        | Some(seg) => seg
        | None => Alcotest.fail("mega-1k parse failed")
        };
      let rp =
        ResidentProgram.sync_full(
          ~settings,
          ~generation=1,
          ~root=Sort.Exp,
          seg,
          None,
        );
      check_summary(
        "mega-1k",
        reference_summary(~root=Sort.Exp, ~generation=1, seg),
        ResidentProgram.summarize(rp),
      );
    },
  );

let corpus_mod_case =
  test_case(
    "mega-mod-1k full-sync parity (Mod root)",
    `Slow,
    () => {
      let seg =
        switch (
          Option.bind(
            CorpusUtil.mega_src("mega-mod-1k.hz"),
            CorpusUtil.parse(~root=Sort.Mod),
          )
        ) {
        | Some(seg) => seg
        | None => Alcotest.fail("mega-mod-1k parse failed")
        };
      let rp =
        ResidentProgram.sync_full(
          ~settings,
          ~generation=1,
          ~root=Sort.Mod,
          seg,
          None,
        );
      check_summary(
        "mega-mod-1k",
        reference_summary(~root=Sort.Mod, ~generation=1, seg),
        ResidentProgram.summarize(rp),
      );
    },
  );

/* Mod-root DELTA parity — repro harness for the browser shadow
   mismatch (Mega-Mod slides mismatched on every post-load delta):
   full-sync the corpus, then ship a hydration-shaped delta (all items
   replaced by a fresh parse of the same text) and an incremental
   derivation must still agree with a from-scratch one. */
let corpus_mod_delta_case =
  test_case(
    "mega-mod-1k all-items delta parity (Mod root)",
    `Slow,
    () => {
      let src =
        switch (CorpusUtil.mega_src("mega-mod-1k.hz")) {
        | Some(src) => src
        | None => Alcotest.fail("no corpus")
        };
      let seg = parse(~root=Sort.Mod, src);
      let rp =
        ResidentProgram.sync_full(
          ~settings,
          ~generation=1,
          ~root=Sort.Mod,
          seg,
          None,
        );
      let items' =
        ResidentProgram.items_of_segment(parse(~root=Sort.Mod, src));
      let changed =
        List.combine(rp.items, items')
        |> List.map(((old: ResidentProgram.item, nu: ResidentProgram.item)) =>
             (old.i_id, nu.i_seg, nu.i_print)
           );
      let roster =
        List.map(
          (it: ResidentProgram.item) => (it.i_id, it.i_print),
          items',
        );
      switch (
        ResidentProgram.sync_items(
          ~settings,
          ~generation=2,
          ~changed,
          ~roster,
          rp,
        )
      ) {
      | Error(_) => Alcotest.fail("delta rejected")
      | Ok(rp') =>
        let expected =
          reference_summary(
            ~root=Sort.Mod,
            ~generation=2,
            ResidentProgram.segment_of_items(rp'.items),
          );
        let actual = ResidentProgram.summarize(rp');
        if (!ResidentProgram.Summary.equal(expected, actual)) {
          /* print the differing items for diagnosis */
          List.combine(expected.s_items, actual.s_items)
          |> List.iteri(
               (
                 i,
                 (
                   e: ResidentProgram.Summary.item_summary,
                   a: ResidentProgram.Summary.item_summary,
                 ),
               ) =>
               if (e != a) {
                 Printf.printf(
                   "item %d: expected errs=%d warns=%d / actual errs=%d warns=%d\n",
                   i,
                   List.length(e.s_errors),
                   List.length(e.s_warnings),
                   List.length(a.s_errors),
                   List.length(a.s_warnings),
                 );
               }
             );
          Alcotest.fail("mod delta parity MISMATCH (see prints)");
        };
      };
    },
  );

let tests = [
  (
    "ResidentProgram",
    [
      full_sync_case("exp full sync", Sort.Exp, exp_src),
      delta_case("exp one-item delta", Sort.Exp, exp_src, exp_src'),
      mismatch_case,
      corpus_case,
      corpus_mod_case,
      corpus_mod_delta_case,
    ],
  ),
];
