open Alcotest;
open Haz3lcore;
open Language;

/* What Tab does, and that the quiver shows it.

   Ownership — which records of the assist stream the caret's Tab acts
   on — is computed ONCE (CompletionQuery.chips_owned) and handed to
   the layout as the bubble drawn at the caret, so "the bubble's first
   delimiter is what Tab types" holds by construction. What remains
   to test:
   - CURATED Tab expectations: the ownership definition itself (the
     caret's inter-content run, witnesses first, then stream order).
     Andrew, 2026-09-02: under `then 4` on a fresh line the bubble read
     "else ? end in ?" but Tab dropped `end` — the engine had glued
     `else` to `4` and appended `end in` past the linebreak, and the
     old query took the NEAREST anchor over the stream-first record.
   - TAB FAITHFULNESS (property, every caret of random edit-derived
     states), two strengths:
     LOCAL (default): Tab discharges exactly the promised obligation —
     an engine delimiter Paste completes one missing shard (the text
     became a delimiter: not glommed, not a new stranded tile); a
     materialization completes at least one; TyDi material (commas,
     type suggestions) must simply apply.
     GLOBAL (~strict): the completed program is unchanged — the
     engine's plan is reachable by accepting THIS chip first. Does
     not hold in general and is not a display/Tab defect: heuristics
     are order-dependent (stranded-closer pairing re-derives once
     another closer completes; a completion-time re-indent is read by
     child-relative indentation). Left as a KNOWN HOLE (andrew,
     2026-09-03); pinned below so a change surfaces.
   - DISPLAY PLUMBING (same sweep): the caret's bubble exists iff the
     caret owns displayed records, sits at the caret, leads with them. */

/* char-exact movement (indentation_ux skips leading whitespace on
   arrow moves, hiding the mouse-reachable mid-indentation carets the
   sweep must visit) */
let settings = {
  ...CoreSettings.on,
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

/* the live display: the fork's assist stream and segment, exactly as
   CachedSyntax renders them (armed, so ghosts are spliced) */
type display = {
  assist: list(CanonicalCompletion.insertion),
  shown: list(CanonicalCompletion.insertion),
  segment: Segment.t,
  marks: list((Id.t, option(int))),
  measured: Measured.t,
  caret_pos: option((int, int)),
};

let display_of = (z: Zipper.t): display => {
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let obligations = TypeObligations.derive(info_map);
  let fork = DisplayFork.mk(~info_map, ~obligations, ~armed=true, z);
  let measured =
    Measured.of_segment(fork.segment, Id.Map.empty, Id.Map.empty);
  let caret = Zipper.Caret.point(measured, z);
  {
    assist: fork.assist,
    segment: fork.segment,
    marks: fork.ghost_marks,
    shown:
      CompletionQuery.chips_displayed(~ghosted=fork.ghosted, fork.assist),
    measured,
    caret_pos: Some((caret.row, caret.col)),
  };
};

/* Tab's action at this caret over the live assist stream, slicing the
   displayed completion as the editor does */
let tab_action_of = (z: Zipper.t, d: display): option(Action.t) =>
  CompletionQuery.tab_action(~display=d.segment, ~marks=d.marks, z, d.assist);
let tab_action = (z: Zipper.t): option(Action.t) =>
  tab_action_of(z, display_of(z));

let tab_head = (z: Zipper.t): option(string) =>
  switch (tab_action(z)) {
  | Some(Paste(t)) => Some(t)
  | Some(ApplyCompletion(One(_))) => Some("<materialize>")
  | _ => None
  };

let show_action = (a: Action.t) =>
  switch (a) {
  | Paste(text) => Printf.sprintf("Paste %S", text)
  | ApplyCompletion(One(_)) => "ApplyCompletion(One)"
  | _ => "?"
  };

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

let is_engine_record = (ins: CanonicalCompletion.insertion) =>
  switch (ins.delimiters) {
  | [{of_shard: Some(_), _}, ..._] => true
  | _ => false
  };

let unfaithful = (~strict=false, z: Zipper.t): option(string) => {
  let d = display_of(z);
  switch (tab_action_of(z, d)) {
  | None => None
  | Some(a) =>
    let engine =
      switch (CompletionQuery.chip_among(z, d.assist)) {
      | Some(ins) => is_engine_record(ins)
      | None => false
      };
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
        | _ when !engine => true /* TyDi material: applying is enough */
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
      } else if (strict && engine && before != after) {
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
    };
  };
};

/* DISPLAY PLUMBING: the layout draws the owned list at the caret */
let display_broken = (z: Zipper.t): option(string) => {
  let d = display_of(z);
  let owned = CompletionQuery.chips_owned(z, d.shown);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let bubbles =
    QuiverLayout.layout(
      ~measured=d.measured,
      ~col_width=10.0,
      ~caret_pos=d.caret_pos,
      ~owned,
      ~seg,
      d.shown,
    )
    |> List.map(fst);
  let owned_delims =
    List.concat_map(
      (ins: CanonicalCompletion.insertion) => ins.delimiters,
      owned,
    );
  let key =
    List.map((d: CanonicalCompletion.delimiter_info) =>
      (d.text, d.of_shard)
    );
  let leads_with = (ds: list(CanonicalCompletion.delimiter_info)) =>
    List.length(ds) >= List.length(owned_delims)
    && key(fst(Util.ListUtil.split_n(List.length(owned_delims), ds)))
    == key(owned_delims);
  let fail = msg => Some(msg ++ " in:\n" ++ state_string(z));
  switch (
    owned,
    List.filter((c: QuiverLayout.positioned_insertion) => c.owned, bubbles),
  ) {
  | ([], []) => None
  | ([], _) => fail("bubble marked owned with nothing owned")
  | (_, [c]) when d.caret_pos != Some((c.row, c.col)) =>
    fail("owned bubble not at the caret")
  | (_, [c]) when !leads_with(c.delimiters) =>
    fail(
      "owned bubble "
      ++ show_delims(c.delimiters)
      ++ " does not lead with "
      ++ show_delims(owned_delims),
    )
  | (_, [_]) => None
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
  | [Secondary(w), ..._] when Haz3lcore.Secondary.is_space(w) =>
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
  /* tab-slice: the paste is read off the displayed completion —
     `else` plus its display pads; the hole between them contributes
     nothing but both pads travel (the flagged choice in the design
     doc: accepting past an unfilled hole materializes its pads) */
  check_tab("stack after 4", ~expected=" else  ", stack),
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
        switch (tab_action(z)) {
        | Some(ApplyCompletion(One(_))) => true
        | _ => false
        },
      );
    },
  ),
  /* KNOWN HOLE pins (see the header): these assert the re-planning is
     still present so a change surfaces here */
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
