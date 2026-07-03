open Haz3lcore;
open Language;

/* Editor-action fuzzer: random action sequences from Zipper.init sample
   the actual editor-reachable segment domain — the only generator that
   does. Typed and edit-derived states genuinely differ (rule tiles only
   form inside cases; deleted openers, standalone |/=> tokens, dangling
   boundary grout are edit-reachable only), so parser-canonical renders
   and whitespace perturbation (the P2 generators) under-sample the
   domain this suite exists to cover.

   Property, on the final state's visible segment: the canonical-
   completion roundtrip is the identity — completed segment is deeply
   complete, and print∘parse recovers the visible segment (text with
   grout hidden, plus the strict Segment.equiv_mod_grout quotient:
   tile ids, labels, molds, shards, secondary).

   Failed or inapplicable actions are skipped: fuzzing explores the
   action space, not the success space. */

let settings = CoreSettings.on;

let apply = (z: Zipper.t, a: Action.t): Zipper.t => {
  let go = (a: Action.t, z: Zipper.t) => {
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let statics =
      CachedStatics.init_from_term(~settings, ~is_dynamic_term=true, term);
    Perform.go(
      ~settings,
      ~statics,
      ~syntax=CachedSyntax.init(z),
      a,
      {
        zipper: z,
        col_target: None,
      },
    );
  };
  switch (go(a, z, ~root=Sort.Exp)) {
  | Ok(z) => z
  | Error(_) => z
  | exception _ => z
  };
};

/* Character alphabet: enough to glom keywords (let/in/fun/case/end),
   operators (incl. unknown-op fodder), delimiters, holes, literals.
   No ^ (projectors) or drv keywords: declared property-domain
   exclusions. */
let chars = [|
  "l",
  "e",
  "t",
  "i",
  "n",
  "f",
  "u",
  "c",
  "a",
  "s",
  "d",
  "x",
  "y",
  "1",
  "2",
  "+",
  "*",
  "=",
  ">",
  "|",
  "-",
  ",",
  "(",
  ")",
  "[",
  "]",
  "\"",
  ":",
  "?",
|];

let action_of = (n: int): Action.t => {
  let n = n land 0x3fffffff;
  switch (n mod 10) {
  | 0
  | 1
  | 2
  | 3
  | 4 => Insert(chars[n / 10 mod Array.length(chars)])
  | 5 => Insert(" ")
  | 6 => Insert("\n")
  | 7 => Move(Local(Left, ByChar))
  | 8 => Move(Local(Right, ByChar))
  | _ => Destruct(Local(Left, ByChar))
  };
};

let print_g =
  Printer.of_segment(~holes="", ~concave_holes="", ~refractors=[]);

let roundtrips = (seg: Segment.t): bool => {
  let result = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
  let masks =
    result.shard_records
    |> List.fold_left(
         (m, r: CanonicalCompletion.shard_record) =>
           Id.Map.add(r.tile_id, r.original_shards, m),
         Id.Map.empty,
       );
  let term = MakeTerm.go_impl(~masks, result.completed_seg).term;
  let seg2 = Test_ExpToSegment.exp_to_segment_roundtrip(term);
  let ok =
    Segment.deep_tile_complete(result.completed_seg)
    && print_g(seg) == print_g(seg2)
    && Segment.equiv_mod_grout(~mold_sorts=false, seg, seg2);
  if (!ok) {
    /* surface the state for shrinking/debugging */
    let show = s =>
      "\""
      ++ String.escaped(Printer.of_segment(~holes="?", ~refractors=[], s))
      ++ "\"";
    let why =
      (Segment.deep_tile_complete(result.completed_seg) ? "" : " INCOMPLETE")
      ++ (print_g(seg) == print_g(seg2) ? "" : " TEXT")
      ++ (
        Segment.equiv_mod_grout(~mold_sorts=false, seg, seg2) ? "" : " EQUIV"
      );
    print_endline(
      "FUZZ FAIL [" ++ why ++ " ]: " ++ show(seg) ++ " -> " ++ show(seg2),
    );
  };
  ok;
};

let fuzz_roundtrip =
  QCheck.Test.make(
    ~name="editor-action fuzz: completion roundtrip on reachable states",
    ~count=40,
    QCheck.(list_of_size(Gen.int_range(10, 60), int_bound(1000000))),
    ns => {
      let z =
        List.fold_left(
          (z, n) => apply(z, action_of(n)),
          Zipper.init(),
          ns,
        );
      let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
      roundtrips(seg);
    },
  );

/* Repro helper: rebuild an edit-derived state from typed text (for
   promoting shrunk fuzz counterexamples to targeted tests). For
   counterexamples that need moves/destructs, fold action_of over the
   shrunk int list instead. */
let type_string = (s: string): Zipper.t =>
  s
  |> String.to_seq
  |> List.of_seq
  |> List.map(c => Action.Insert(String.make(1, c)))
  |> List.fold_left(apply, Zipper.init());

/* The property runs un-gated; suite runs are seeded (run_node.sh
   defaults QCHECK_SEED) so only explicit seed sweeps hunt new
   counterexamples. A failure is a FINDING — shrink it (the reported
   int list decodes via action_of), fix or file it, promote the shrunk
   case to a regression below. KNOWN OPEN FAMILIES (rare, from random
   sweeps 2026-07-03):
   - adjacent operator tokens where one is `:`-like drop one token
     (`ce:(?+:u ...)` -> `ce:(? u ...)`): two ops in one Bin middle
     share a single lexeme slot.
   - an EQUIV-only case with identical display (`s  s  ld: [l ...`) —
     piece-level diff needed to attribute. */

/* Edit-derived regression cases (typed via the action harness, so they
   exercise editor molding/glomming, not parser-canonical states). These
   run unconditionally, unlike the gated property. */
let typed_case = (name, str) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let seg =
        Zipper.unselect_and_zip(~erase_buffer=true, type_string(str));
      Alcotest.(check(bool))(name, true, roundtrips(seg));
    },
  );

let typed_regressions = (
  "Round-Trip: Edit-Derived",
  [
    typed_case("adjacent op run after ascription", "?: ?+=>+f"),
    typed_case("adjacent op run infix", "1 +=>+ 2"),
    typed_case("adjacent op run stars", "1 ***** 2"),
    typed_case("stranded prefix minus on typ side", ": -"),
    typed_case("keyword prefix in op position", "?]l"),
    typed_case("singleton labeled list element", "=]"),
    typed_case("unit in list", "[()"),
    typed_case("cross-sort list closer at typ", "?:?]"),
    typed_case("typ-side postfix parens", "?:1("),
    typed_case("line-initial prefix minus", "(\n\n-?"),
    typed_case("typ-side incomplete paren", "?:?("),
    typed_case("orphan closer under incomplete parens", "(?\n(]"),
  ],
);

/* Replayed action sequences for shrunk counterexamples that pure
   typing cannot reproduce. */
let replay_case = (name, ns) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let z =
        List.fold_left(
          (z, n) => apply(z, action_of(n)),
          Zipper.init(),
          ns,
        );
      let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
      Alcotest.(check(bool))(name, true, roundtrips(seg));
    },
  );

let replay_regressions = (
  "Round-Trip: Edit-Derived Replays",
  [
    replay_case("glom () over stray grout", [637940, 260237, 440440, 0]),
    replay_case("nested orphan closers", [637940, 260237, 440490, 242670]),
    replay_case(
      "parens-vs-ap mold drift",
      [661140, 29276, 402716, 596180, 0],
    ),
    replay_case(
      "orphan ] between : and *",
      [65520, 12340, 726577, 750450, 84640],
    ),
    replay_case(
      "crossed synthesized delimiters",
      [991440, 65530, 6, 750450, 84640],
    ),
    replay_case(
      "complete non-ctor post-parens at typ",
      [850550, 320, 390390],
    ),
  ],
);

let tests = [
  typed_regressions,
  replay_regressions,
  (
    "Round-Trip: Fuzz",
    [QCheck_alcotest.to_alcotest(~speed_level=`Slow, fuzz_roundtrip)],
  ),
];
