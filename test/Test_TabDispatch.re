open Alcotest;
open Haz3lcore;
open Web;

/* What Tab does, and that the quiver shows it.

   Ownership — which completion records the caret's Tab acts on — is
   computed ONCE (CompletionQuery.chips_at_caret) and handed to the
   quiver as the bubble drawn at the caret, so "the bubble's first
   delimiter is what Tab types" holds by construction. What remains
   to test:
   - CURATED Tab expectations: the ownership definition itself (the
     caret's inter-content run, engine order). Andrew, 2026-09-02:
     under `then 4` on a fresh line the bubble read "else ? end in ?"
     but Tab dropped `end` — the engine had glued `else` to `4` and
     appended `end in` past the linebreak, and the old query took the
     NEAREST anchor (the indentation space) over the engine-first
     record.
   - TAB FAITHFULNESS (property, every caret of random edit-derived
     states): typing Tab's text does not change the completed program
     (modulo whitespace) — Tab realizes the completion it shows rather
     than steering it elsewhere. A failure prints the state; shrink
     via the int list (Test_RoundtripFuzz.action_of), promote below.
   - DISPLAY PLUMBING (same sweep): the caret's bubble exists iff the
     caret owns records, sits at the caret, leads with them; bubbles
     never overlap. */

let font_metrics: FontMetrics.t = {
  row_height: 20.0,
  col_width: 10.0,
};

/* char-exact movement (indentation_ux skips leading whitespace on
   arrow moves, hiding the mouse-reachable mid-indentation carets the
   sweep must visit) */
let settings = {
  ...Language.CoreSettings.on,
  indentation_ux: false,
};

let go = (z: Zipper.t, a: Action.t): Action.Result.t(Zipper.t) => {
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
    ~root=Exp,
  );
};

/* fuzzing explores the action space: failed actions are skipped */
let apply = (z: Zipper.t, a: Action.t): Zipper.t =>
  switch (go(z, a)) {
  | Ok(z) => z
  | Error(_) => z
  | exception _ => z
  };

let type_string = (z: Zipper.t, s: string): Zipper.t =>
  s
  |> Token.to_list
  |> List.map(c => Action.Insert(c))
  |> List.fold_left(apply, z);

let state_string = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", ~caret="|", z);

let show_delims = (ds: list(CanonicalCompletion.delimiter_info)) =>
  "["
  ++ (
    ds
    |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
    |> String.concat(" ")
  )
  ++ "]";

/* Tab's paste text at this caret, if the caret owns a chip */
let tab_head = (z: Zipper.t): option(string) =>
  CompletionQuery.chip_at_caret(z)
  |> Option.map(ins => CompletionQuery.tab_text(z, ins))
  |> Option.join;

/* the completed program, whitespace and holes erased */
let completed_text = (z: Zipper.t): string => {
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  CanonicalCompletion.for_editor(seg).completed_seg
  |> Printer.of_segment(~holes="", ~concave_holes="", ~refractors=[])
  |> String.to_seq
  |> Seq.filter(c => c != ' ' && c != '\n' && c != '\t')
  |> String.of_seq;
};

let missing_shards = (z: Zipper.t): int =>
  Zipper.unselect_and_zip(~erase_buffer=true, z)
  |> Segment.incomplete_tiles_deep
  |> List.map(t => List.length(Tile.missing_shard_indices(t)))
  |> List.fold_left((+), 0);

let show_action = (a: Action.t) =>
  switch (a) {
  | Paste(text) => Printf.sprintf("Paste %S", text)
  | ApplyCompletion(One(_)) => "ApplyCompletion(One)"
  | _ => "?"
  };

/* TAB FAITHFULNESS, two strengths:
   - LOCAL (default): Tab discharges the promised obligation and
     nothing else — a Paste completes exactly one missing shard (the
     text became a delimiter: not glommed into a neighbor, not a new
     stranded tile), a materialization completes at least one.
   - GLOBAL (~strict): the completed program is unchanged, i.e. the
     engine's plan is reachable by accepting THIS chip first.

   GLOBAL does not hold in general and is NOT a display/Tab defect —
   the quiver is a forecast of a heuristic completion and Tab is an
   edit, so the rest of the forecast may re-derive. Left as a KNOWN
   HOLE (andrew, 2026-09-03). What a 60-state × every-caret sweep
   found (17 strict violations), three separate causes:
   - stranded-closer pairing (9): with two or more stranded closers,
     completing one changes which span another closer's opener wraps
     (`?) x ⏎ a) ?` -> `(?) x ⏎ (a) ?`, but after the first opener
     lands the second hoists: `((?) x ⏎ a) ?`). Genuine order
     sensitivity in opener placement.
   - hole-only differences (8): the engine synthesizes a hole inside
     an empty pair (`[?]`) where typing the closer yields the empty
     literal (`[]`). Cosmetic.
   - completion-time re-indent × child-relative indentation (the
     `[ ( ⏎ i¦` pin): typing `]` closes the list, auto re-indent
     moves `i` in two spaces, and inside the list's child the
     partition heuristic counts the `(`'s indent from the child's
     start (1), not its screen column (2), so the indented `i` now
     reads as the paren's continuation: `[ (?) ⏎ i?]` becomes
     `[ ( ⏎ i)]`. Holds with re-indent off. Candidate fix: compare
     screen columns in nested partitions (untested).
   The pins below keep the first and third visible; when either
   changes, flip the pin. */
let unfaithful = (~strict=false, z: Zipper.t): option(string) =>
  switch (CompletionQuery.tab_action(z)) {
  | None => None
  | Some(a) =>
    switch (go(z, a)) {
    | Error(_)
    | exception _ =>
      Some(
        Printf.sprintf(
          "%s FAILED in:\n%s",
          show_action(a),
          state_string(z),
        ),
      )
    | Ok(z') =>
      let (m, m') = (missing_shards(z), missing_shards(z'));
      let discharged =
        switch (a) {
        | Paste(_) => m' == m - 1
        | _ => m' < m
        };
      let (before, after) = (completed_text(z), completed_text(z'));
      if (!discharged) {
        Some(
          Printf.sprintf(
            "%s did not discharge exactly the promised shard (missing %d -> %d) in:\n%s",
            show_action(a),
            m,
            m',
            state_string(z),
          ),
        );
      } else if (strict && before != after) {
        Some(
          Printf.sprintf(
            "%s re-planned the completion:\n%s\n  before: %s\n  after:  %s",
            show_action(a),
            state_string(z),
            before,
            after,
          ),
        );
      } else {
        None;
      };
    }
  };

/* DISPLAY PLUMBING: the quiver draws the owned list at the caret */
let display_broken = (z: Zipper.t): option(string) => {
  let syntax = CachedSyntax.init(z);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let caret = Zipper.Caret.point(syntax.measured, z);
  let caret_pos = Some((caret.row, caret.col));
  let owned = CompletionQuery.chips_at_caret(~seg, z);
  let bs =
    QuiverDec.bubbles(
      ~measured=syntax.measured,
      ~font_metrics,
      ~caret_pos,
      ~owned,
      seg,
    );
  let owned_delims =
    List.concat_map(
      (ins: CanonicalCompletion.insertion) => ins.delimiters,
      owned,
    );
  let leads_with = (ds: list(CanonicalCompletion.delimiter_info)) =>
    List.length(ds) >= List.length(owned_delims)
    && Util.ListUtil.split_n(List.length(owned_delims), ds)
    |> fst
    |> List.map((d: CanonicalCompletion.delimiter_info) =>
         (d.text, d.of_shard)
       )
    == List.map(
         (d: CanonicalCompletion.delimiter_info) => (d.text, d.of_shard),
         owned_delims,
       );
  let caret_bubbles =
    List.filter((c: QuiverDec.positioned_insertion) => c.owned, bs);
  let fail = msg => Some(msg ++ " in:\n" ++ state_string(z));
  switch (owned, caret_bubbles) {
  | ([], []) => None
  | ([], _) => fail("bubble marked owned with nothing owned")
  | (_, [c]) when caret_pos != Some((c.row, c.col)) =>
    fail("owned bubble not at the caret")
  | (_, [c]) when !leads_with(c.delimiters) =>
    fail(
      "owned bubble "
      ++ show_delims(c.delimiters)
      ++ " does not lead with "
      ++ show_delims(owned_delims),
    )
  | (_, [_]) =>
    /* no two bubbles overlap on a row */
    let w = (c: QuiverDec.positioned_insertion) =>
      float_of_int(QuiverDec.delimiters_len(c.delimiters) + 2)
      *. font_metrics.col_width
      *. QuiverDec.chip_font_scale;
    let sorted =
      List.sort(
        (a: QuiverDec.positioned_insertion, b: QuiverDec.positioned_insertion) =>
          compare((a.row, a.col), (b.row, b.col)),
        bs,
      );
    let rec overlaps = l =>
      switch (l) {
      | [a, b, ...tl] =>
        a.QuiverDec.row == b.QuiverDec.row
        && float_of_int(b.col)
        *. font_metrics.col_width < float_of_int(a.col)
        *. font_metrics.col_width
        +. w(a)
          ? true : overlaps([b, ...tl])
      | _ => false
      };
    overlaps(sorted) ? fail("bubbles overlap") : None;
  | (_, _) => fail("several owned bubbles")
  };
};

/* every caret position of a state: home, then step right until stuck
   (ByChar enters token interiors, so Inner carets are covered) */
let positions = (z: Zipper.t): list(Zipper.t) => {
  let step = (z, d) =>
    switch (go(z, Move(Local(d, ByChar)))) {
    | Ok(z') => Some(z')
    | Error(_)
    | exception _ => None
    };
  let rec home = z =>
    switch (step(z, Left)) {
    | Some(z') => home(z')
    | None => z
    };
  let rec walk = (z, acc) =>
    switch (step(z, Right)) {
    | Some(z') => walk(z', [z', ...acc])
    | None => List.rev(acc)
    };
  let z0 = home(z);
  walk(z0, [z0]);
};

let sweep = (z: Zipper.t): list(string) =>
  positions(z)
  |> List.concat_map(z =>
       List.filter_map(x => x, [unfaithful(z), display_broken(z)])
     );

let check_sweep = (name, z) =>
  test_case(name ++ ": faithful + drawn at every caret", `Quick, () =>
    check(list(string), name, [], sweep(z))
  );

let check_tab = (name, ~expected, z) =>
  test_case(name ++ ": tab types " ++ String.escaped(expected), `Quick, () =>
    check(option(string), name, Some(expected), tab_head(z))
  );

/* strip the auto-indent of the caret's line (Enter indents; fuzz
   states reach col 0) */
let rec dedent = (z: Zipper.t): Zipper.t =>
  switch (List.rev(fst(z.relatives.siblings))) {
  | [Secondary(w), ..._] when Secondary.is_space(w) =>
    dedent(apply(z, Destruct(Local(Left, ByChar))))
  | _ => z
  };

/* andrew's program: a let over a case over an if, mid-entry after
   `then 4` */
let stack =
  type_string(
    Zipper.init(),
    "let foo(bar: Int, baz: Bool) =\ncase baz\n| true =>\nif bar < 1000\nthen 4",
  );
let stack_enter = apply(stack, Insert("\n"));
let stack_blank = apply(stack_enter, Insert("\n"));
let stack_col0 =
  apply(
    apply(stack_enter, Destruct(Local(Left, ByChar))),
    Destruct(Local(Left, ByChar)),
  );
let stack_mid_indent = apply(stack_enter, Move(Local(Left, ByChar)));
let stack_witness = type_string(stack_enter, "e");

let curated = [
  check_tab("stack after 4", ~expected=" else ", stack),
  /* the reported case: engine splits [else] | [end in] across the
     linebreak; the caret owns both, else leads */
  check_tab("stack, Enter", ~expected="else ", stack_enter),
  check_tab(
    "stack, Enter, caret inside indentation",
    ~expected="else ",
    stack_mid_indent,
  ),
  check_tab(
    "stack, Enter, backspaced to col 0",
    ~expected="else ",
    stack_col0,
  ),
  check_tab("stack, blank line", ~expected="else ", stack_blank),
  /* typed e: witness chip pastes the remainder */
  check_tab("stack, Enter, typed e", ~expected="lse", stack_witness),
  /* the caret owns the ] (anchored on the stranded `)`); the ( rests
     by the `[` and must not lead */
  check_tab(
    "stranded ) inside [: tab closes the list",
    ~expected="]",
    type_string(Zipper.init(), "[?)"),
  ),
  check_sweep("stack after 4", stack),
  check_sweep("stack, Enter", stack_enter),
  check_sweep("stack, blank line", stack_blank),
  check_sweep("stack, Enter, typed e", stack_witness),
  check_sweep("stranded ) inside [", type_string(Zipper.init(), "[?)")),
  check_sweep("stranded ] inside (", type_string(Zipper.init(), "(?]")),
  check_sweep("stranded closer 1]", type_string(Zipper.init(), "1]")),
  check_sweep(
    "incomplete let inside a closed list",
    type_string(Zipper.init(), "[let x = 1]"),
  ),
  check_sweep(
    "junction drop: = at the grout",
    type_string(Zipper.init(), "let x 1 in x"),
  ),
  check_sweep(
    "nested case under let, rule body",
    type_string(
      Zipper.init(),
      "let x = case true\n| false => case false\n| true => 1",
    ),
  ),
  check_sweep(
    "if under rule with content below",
    type_string(Zipper.init(), "let f =\ncase 0\n| 0 =>\nif \n\n1"),
  ),
  /* two stranded closers: a TYPED ( pairs with the most recently
     stranded ) (backpack order), the bubble pairs positionally — so
     an opener chip materializes instead of pasting ... */
  test_case(
    "two stranded closers: opener materializes",
    `Quick,
    () => {
      let z =
        type_string(Zipper.init(), "?) x\na) ?") |> positions |> List.hd;
      check(
        bool,
        "ApplyCompletion(One)",
        true,
        switch (CompletionQuery.tab_action(z)) {
        | Some(ApplyCompletion(One(_))) => true
        | _ => false
        },
      );
    },
  ),
  /* KNOWN HOLE pins (see unfaithful): these assert the re-planning
     is still present so a change surfaces here */
  test_case("KNOWN HOLE: stranded-closer pairing re-plans", `Quick, () =>
    check(
      bool,
      "strict sweep still finds it",
      true,
      type_string(Zipper.init(), "?) x\na) ?")
      |> positions
      |> List.exists(z => unfaithful(~strict=true, z) != None),
    )
  ),
  test_case(
    "KNOWN HOLE: re-indent after closing the list re-plans the paren",
    `Quick,
    () =>
    check(
      bool,
      "strict sweep still finds it",
      true,
      type_string(dedent(type_string(Zipper.init(), " [ (\n")), "i")
      |> positions
      |> List.exists(z => unfaithful(~strict=true, z) != None),
    )
  ),
  /* coalescing geometry (fuzz-found): the merged bubble is drawn at
     the owned member's pin, not the group's leftmost — overlap must
     be judged there, or the redrawn bubble lands on a neighbor */
  check_sweep(
    "overlap judged at the drawn pin",
    type_string(Zipper.init(), "(?:(?] 1"),
  ),
];

/* Random editor-reachable states (the roundtrip fuzzer's action
   space) x every caret position. */
let fuzz_tab =
  QCheck.Test.make(
    ~name="editor-action fuzz: Tab is faithful and drawn, at every caret",
    ~count=30,
    QCheck.(list_of_size(Gen.int_range(10, 40), int_bound(1000000))),
    ns => {
      let z =
        List.fold_left(
          (z, n) => apply(z, Test_RoundtripFuzz.action_of(n)),
          Zipper.init(),
          ns,
        );
      switch (sweep(z)) {
      | [] => true
      | ms =>
        List.iter(m => print_endline("TAB FAIL: " ++ m), ms);
        false;
      };
    },
  );

let tests = [
  ("TabDispatch: curated", curated),
  (
    "TabDispatch: fuzz",
    [QCheck_alcotest.to_alcotest(~speed_level=`Slow, fuzz_tab)],
  ),
];
