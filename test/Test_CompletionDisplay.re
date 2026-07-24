open Alcotest;
open Haz3lcore;
open Language;

/* Completion-display harness: render what the user actually sees —
   the DISPLAY segment with ghosts spliced at their anchors, caret
   as ¦, ghost runs in ⟪⟫ — plus the chip stream. The string IS the
   test: display_case("string_replace(a,¦ ?⟪, ?)⟫") types the text
   before ¦ and asserts the whole rendering.

   REVIEW STANDARD (not just pass/fail): a pinned trajectory is a
   user experience. Before pinning, read it in the user's frame —
   what moved that didn't need to? what was promised and then
   retracted? The load-bearing property is CONSTANCY: typing a
   promised character changes provenance styling only, never the
   rendered text (the promise defines a trajectory; along it the
   display must be still). constancy_audit checks this mechanically,
   but it's an ALARM, not a judge — when it fires, either the
   display regressed or the property mis-specifies the ergonomics;
   judge before pinning either way. Formatting compromises (promised
   spacing that vanishes on materialization) get flagged to andrew,
   not silently pinned. */

let string_testable = testable(Fmt.string, String.equal);

/* the LIVE pipeline, not a mirror of it: PromiseRender.mk — the same
   single home CachedSyntax renders from (the display projected from
   the reified artifact). TyDi suggestions ride the shared assist
   stream (T2). Statics derive from the SAME zipper we render — a
   re-typed program mints fresh ids and every id-keyed merge silently
   misses. */
let display_parts =
    (~inline_persist=false, z: Zipper.t)
    : (
        Segment.t,
        Zipper.t,
        list((Id.t, option(int))),
        list(((Id.t, int), int)),
        list((Id.t, (Id.t, int, int))),
        list(CanonicalCompletion.insertion),
        list(CanonicalCompletion.insertion),
      ) => {
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let obligations = TypeObligations.derive(info_map);
  let fork =
    PromiseRender.mk(
      ~info_map,
      ~obligations,
      ~armed=true,
      ~inline_persist,
      z,
    );
  (
    fork.segment,
    z,
    fork.ghost_marks,
    fork.typed_lens,
    fork.caret_witnesses,
    fork.assist,
    fork.ghosted,
  );
};

/* LIVE-CADENCE parts: statics exactly as the live editor feeds the
   fork — CachedStatics.init (two-pass: obligations kept from pass 1,
   info_map from the REIFIED pass 2 when a deficit is owed) computed
   from `statics_z`, the zipper as the statics debounce last saw it.
   statics_z == z is the SETTLED state (the deferred refresh landed):
   statics fresh but reified — the frame a paused user stares at.
   statics_z lagging by N keystrokes is the mid-burst frame. */
let display_parts_live = (~statics_z: Zipper.t, z: Zipper.t) => {
  let statics =
    CachedStatics.init(
      ~settings=CoreSettings.on,
      ~is_dynamic_term=false,
      ~stitch=x => x,
      ~root=Sort.Exp,
      statics_z,
    );
  let fork =
    PromiseRender.mk(
      ~info_map=statics.info_map,
      ~obligations=statics.obligations,
      ~armed=true,
      z,
    );
  (
    fork.segment,
    z,
    fork.ghost_marks,
    fork.typed_lens,
    fork.caret_witnesses,
    fork.assist,
    fork.ghosted,
  );
};

/* PROMISE-BACKED parts: identical to display_parts but the display
   segment comes from PromiseRender.mk (projection) rather than
   DisplayFork.mk (reconstruction). The parity suite renders both
   through display_state_of and asserts equality. */
let display_parts_promise = display_parts;

let display_state_of =
    (~parts, ~chips as show_chips=true, z: Zipper.t): string => {
  let (seg, zc, marks, typed_lens, caret_witnesses, assist, ghosted) =
    parts(z);
  let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  let is_marked = (id: Id.t, sh: option(int)) =>
    List.exists(
      ((mid, msh): (Id.t, option(int))) => Id.equal(mid, id) && msh == sh,
      marks,
    );
  /* WITNESS sub-token: a ghost-marked shard (tile, i) carrying a
     typed_len splits into an UNMARKED typed prefix and a MARKED ghost
     remainder — the display shows `i⟪n⟫`, not `⟪in⟫` (the first char
     is the user's). */
  let typed_len_of = (id: Id.t, i: int): option(int) =>
    List.find_map(
      (((tid, sh), n): ((Id.t, int), int)) =>
        Id.equal(tid, id) && sh == i ? Some(n) : None,
      typed_lens,
    );
  let split_witness =
      (id: Id.t, i: int, meas: Measured.measurement)
      : list((bool, bool, Measured.measurement)) =>
    switch (typed_len_of(id, i)) {
    | Some(n) when is_marked(id, Some(i)) =>
      let mid: Util.Point.t = {
        row: meas.origin.row,
        col: meas.origin.col + n,
      };
      [
        (
          false,
          false,
          {
            origin: meas.origin,
            last: mid,
          }: Measured.measurement,
        ),
        (
          true,
          false,
          {
            origin: mid,
            last: meas.last,
          }: Measured.measurement,
        ),
      ];
    | _ => [(is_marked(id, Some(i)), false, meas)]
    };
  /* reading-order (marked, is_ws, measurement) atoms */
  let rec atoms = (sg: Segment.t): list((bool, bool, Measured.measurement)) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          let ms = Measured.find_shards(~msg="DisplayState", t, measured);
          Util.Aba.mk(t.shards, t.children)
          |> Util.Aba.join(
               i => split_witness(t.id, i, List.assoc(i, ms)),
               atoms,
             )
          |> List.concat;
        | Grout(g) => [
            /* holes render UN-tinted even in ghost zones (the
               andrew-approved look: the hole you're about to fill
               reads plain; ghost tint covers tokens). The old pins'
               hole-inside-span look for owed commas was marker-shift
               arithmetic, not marks truth. OPEN STYLING QUESTION
               (docketed): should owed holes tint? */
            (is_marked(g.id, None), false, Measured.find_g(g, measured)),
          ]
        | Secondary(w) => [
            (is_marked(w.id, None), true, Measured.find_w(w, measured)),
          ]
        | Projector(_) => []
        },
      sg,
    );
  /* marked runs → ⟪ at first origin, ⟫ at last end; unmarked
     whitespace BRIDGES a run (oracle pads carry no provenance) but
     never extends its end */
  let runs = {
    let (closed, open_) =
      List.fold_left(
        ((rs, cur), (m, ws, meas: Measured.measurement)) =>
          switch (m, cur) {
          | (false, None) => (rs, None)
          | (false, Some(r)) => ws ? (rs, Some(r)) : ([r, ...rs], None)
          | (true, None) => (rs, Some((meas.origin, meas.last)))
          | (true, Some((o, _))) => (rs, Some((o, meas.last)))
          },
        ([], None),
        atoms(seg),
      );
    List.rev(Option.to_list(open_) @ closed);
  };
  let caret = DisplayCaret.point(~caret_witnesses, measured, zc);
  /* measured-faithful text (width transfer): consumed spaces are
     omitted so printed columns match measured columns, except one
     printed ?/~ char per zero-width Pinch hole — markers shift by
     the pinch count on their row (strictly-before for caret/
     run-opens, inclusive for run-ends) */
  let cells = GroutCells.classify(seg);
  let text =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      ~indent="",
      ~measured,
      GroutCells.drop_consumed_spaces(seg),
    );
  let grout_positions: list((Id.t, int, int)) = {
    let rec go = (sg: Segment.t) =>
      List.concat_map(
        (p: Piece.t) =>
          switch (p) {
          | Grout(g) =>
            switch (Measured.find_g(g, measured)) {
            | m => [(g.id, m.origin.row, m.origin.col)]
            | exception _ => []
            }
          | Tile(t) => List.concat_map(go, t.children)
          | _ => []
          },
        sg,
      );
    go(seg);
  };
  let hole_shift = (~incl: bool, p: Util.Point.t): Util.Point.t => {
    ...p,
    col:
      p.col
      + GroutCells.pinch_shift(
          cells,
          ~grout_positions,
          ~incl,
          ~row=p.row,
          ~col=p.col,
        ),
  };
  /* insert markers back-to-front so points stay valid; at a shared
     point later inserts land left, so descending priority yields
     ⟫¦ at a run end and ¦⟪ at a run start */
  /* witness sub-token remainders (typed_lens) render as ghost runs
     too: the typed prefix is user text, the remainder is system —
     the fuzzer's pre-caret compare excises ⟪⟫ spans, so remainders
     must be inside spans */
  let witness_runs: list((Util.Point.t, Util.Point.t)) =
    typed_lens
    |> List.filter_map((((tid, i), n)) => {
         let rec find_tile = (sg: Segment.t): option(Tile.t) =>
           List.fold_left(
             (acc, p: Piece.t) =>
               switch (acc, p) {
               | (Some(_), _) => acc
               | (None, Tile(t)) =>
                 Id.equal(t.id, tid)
                   ? Some(t)
                   : List.fold_left(
                       (a, c) => a == None ? find_tile(c) : a,
                       None,
                       t.children,
                     )
               | (None, _) => None
               },
             None,
             sg,
           );
         switch (find_tile(seg)) {
         | None => None
         | Some(t) =>
           switch (
             Measured.find_shards(t, measured)
             |> List.find_opt(((j, _)) => j == i)
           ) {
           | None => None
           | Some((_, m)) =>
             let o: Util.Point.t = {
               ...m.origin,
               col: m.origin.col + n,
             };
             Util.Point.compare(o, m.last) < 0 ? Some((o, m.last)) : None;
           }
         };
       });
  /* a witness remainder already inside a splice run must not nest a
     second span */
  let covered = ((o, l): (Util.Point.t, Util.Point.t)): bool =>
    runs
    |> List.exists(((o', l'): (Util.Point.t, Util.Point.t)) =>
         Util.Point.compare(o', o) <= 0 && Util.Point.compare(l, l') <= 0
       );
  let witness_runs = witness_runs |> List.filter(r => !covered(r));
  let mark_list =
    [(hole_shift(~incl=false, caret), 1, "¦")]
    @ List.concat_map(
        ((o, l): (Util.Point.t, Util.Point.t)) =>
          [
            (hole_shift(~incl=false, o), 2, "⟪"),
            (hole_shift(~incl=true, l), 0, "⟫"),
          ],
        runs @ witness_runs,
      );
  let disp =
    mark_list
    |> List.sort(((p1, pr1, _), (p2, pr2, _)) => {
         let c = Util.Point.compare(p1: Util.Point.t, p2: Util.Point.t);
         c != 0 ? - c : - compare(pr1: int, pr2: int);
       })
    |> List.fold_left(
         (rows, (pt, _, s)) => Printer.insert_string(s, pt, rows),
         String.split_on_char('\n', text),
       )
    |> String.concat("\n");
  /* suppression comes from THE one policy home */
  let chips_shown = CanonicalCompletion.chips_displayed(~ghosted, assist);
  let chips_str =
    chips_shown
    |> List.map((i: CanonicalCompletion.insertion) =>
         i.delimiters
         |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
         |> String.concat("+")
       )
    |> String.concat(" | ");
  show_chips ? disp ++ "   CHIPS[" ++ chips_str ++ "]" : disp;
};

let display_state = (~chips=true, z: Zipper.t): string =>
  display_state_of(~parts=display_parts, ~chips, z);

/* the INLINE-PERSIST trial: same renderer, persist flag on */
let display_state_persist = (~chips=true, z: Zipper.t): string =>
  display_state_of(~parts=display_parts(~inline_persist=true), ~chips, z);

/* live-cadence render: statics from `lag` keystrokes back (0 =
   settled/reified), display from the current zipper */
let display_state_live = (~chips=true, ~statics_z, z: Zipper.t): string =>
  display_state_of(~parts=display_parts_live(~statics_z), ~chips, z);

/* Per-keystroke trajectory of typing `text` at the ¦ in `ctx` — the
   SCENARIO MATRIX axis: same form entered on a blank editor, above
   existing content, between content, or into a hole mid-program.
   Contexts are typed first (real ids), then each step renders. */
let trajectory_in = (~ctx="¦", text: string): string => {
  let base = Test_Editing.mk(ctx);
  let ins = Token.to_list(text) |> List.map(c => Action.Insert(c));
  let rec steps = (k, acc) =>
    if (k > List.length(ins)) {
      List.rev(acc);
    } else {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          base @ List.filteri((i, _) => i < k, ins),
        );
      steps(k + 1, [display_state(z), ...acc]);
    };
  steps(1, []) |> String.concat("\n");
};

let trajectory = (text: string): string => trajectory_in(~ctx="¦", text);

/* live-cadence trajectory: each step renders the step-k zipper with
   statics from the step-(k-lag) zipper through the LIVE statics
   pipeline (reified). lag=0 pins the settled frame after every
   keystroke; lag>0 pins the mid-burst debounce gap. */
let trajectory_live_in = (~lag=0, ~ctx="¦", text: string): string => {
  let base = Test_Editing.mk(ctx);
  let ins = Token.to_list(text) |> List.map(c => Action.Insert(c));
  /* ONE incremental run — ids must be shared between the rendered
     zipper and the statics zipper, as they are live (a re-typed
     program mints fresh ids and every id-keyed merge silently
     misses) */
  let z0 = Test_Editing.perform(Zipper.init(), base);
  let states: array(Zipper.t) = {
    let (states, _) =
      List.fold_left(
        ((acc, z), a) => {
          let z = Test_Editing.perform(z, [a]);
          ([z, ...acc], z);
        },
        ([], z0),
        ins,
      );
    List.rev(states) |> Array.of_list;
  };
  let at = j => j <= 0 ? z0 : states[j - 1];
  let rec steps = (k, acc) =>
    if (k > Array.length(states)) {
      List.rev(acc);
    } else {
      let z = at(k);
      let statics_z = at(k - lag);
      steps(k + 1, [display_state_live(~statics_z, z), ...acc]);
    };
  steps(1, []) |> String.concat("\n");
};

/* per-backspace trajectory from the ¦ in ctx — DELETION flows jank
   differently than entry (promises grow instead of shrinking) */
let trajectory_bk = (~ctx: string, n: int): string => {
  let base = Test_Editing.mk(ctx);
  let rec steps = (k, acc) =>
    if (k > n) {
      List.rev(acc);
    } else {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          base @ List.init(k, _ => Action.Destruct(Local(Left, ByChar))),
        );
      steps(k + 1, [display_state(z), ...acc]);
    };
  steps(1, []) |> String.concat("\n");
};

/* One-liner form: the string is both input and expectation. Text
   before ¦ (ghosts stripped) is typed left-to-right; the rendered
   display state must equal the whole string. */
let split_first = (needle: string, s: string): option((string, string)) => {
  let nl = String.length(needle);
  let sl = String.length(s);
  let rec go = i =>
    if (i + nl > sl) {
      None;
    } else if (String.sub(s, i, nl) == needle) {
      Some((String.sub(s, 0, i), String.sub(s, i + nl, sl - i - nl)));
    } else {
      go(i + 1);
    };
  go(0);
};

let strip_ghosts = (s: string): string => {
  let rec go = s =>
    switch (split_first("⟪", s)) {
    | None => s
    | Some((pre, rest)) =>
      switch (split_first("⟫", rest)) {
      | None => pre ++ rest
      | Some((_, post)) => go(pre ++ post)
      }
    };
  go(s);
};

/* CONSTANCY: for each keystroke that matches the first character
   right of the caret in the prior display (= typing the promised
   material, ghost or promised formatting), the marker-stripped
   renders before and after must be equal. Returns violation lines;
   trajectories pin the empty string. */
let constancy_audit = (text: string): string => {
  let acts = Test_Editing.mk(text ++ "¦");
  let n_acts = List.length(acts);
  let state = k =>
    display_state(
      ~chips=false,
      Test_Editing.perform(
        Zipper.init(),
        List.filteri((i, _) => i < k, acts),
      ),
    );
  let remove_all = (needle: string, s: string): string => {
    let rec go = s =>
      switch (split_first(needle, s)) {
      | None => s
      | Some((pre, post)) => pre ++ go(post)
      };
    go(s);
  };
  let strip = (s: string): string =>
    s |> remove_all("¦") |> remove_all("⟪") |> remove_all("⟫");
  /* first char right of the caret, markers skipped */
  let promised = (s: string): option(char) =>
    switch (split_first("¦", s)) {
    | None => None
    | Some((_, r)) =>
      let r = r |> remove_all("⟪") |> remove_all("⟫");
      String.length(r) > 0 ? Some(r.[0]) : None;
    };
  let violations = ref([]);
  let prev = ref(state(1));
  for (k in 2 to n_acts) {
    let cur = state(k);
    let key = String.length(text) >= k ? Some(text.[k - 1]) : None;
    switch (key, promised(prev^)) {
    | (Some(c), Some(p)) when c == p && strip(prev^) != strip(cur) =>
      violations :=
        [Printf.sprintf("'%c': %s -> %s", c, prev^, cur), ...violations^]
    | _ => ()
    };
    prev := cur;
  };
  List.rev(violations^) |> String.concat("\n");
};

let display_case = (spec: string) =>
  test_case(
    spec,
    `Quick,
    () => {
      let typed =
        switch (split_first("¦", strip_ghosts(spec))) {
        | Some((pre, _)) => pre
        | None => Alcotest.fail("display_case: no caret in: " ++ spec)
        };
      let z =
        Test_Editing.perform(Zipper.init(), Test_Editing.mk(typed ++ "¦"));
      check(string_testable, spec, spec, display_state(~chips=false, z));
    },
  );

/* === PROMISE-RENDER PARITY (stage 1) ===
   Render the SAME zipper through the current reconstruction fork and
   the new projection fork, both via display_state_of, and assert
   equality. WAIVER classes (differences that are acceptable because
   the current rendering is annotated jank or because stage 1 defers
   sub-token styling) are enumerated per test with a comment. The
   text harness sees neither styling (unbolded parens) nor the
   ghost/real distinction inside a token, so those waivers are
   invisible here; the ones that DO surface are witness remainders
   (current = comment text `ing_capitalize(`; promise = full real
   token `string_capitalize(`) and the indentation-seam doubled
   space — both flagged and pinned separately as "promise-witness"
   trajectories with judged expected values below. */

/* the two renders of a zipper (chips shown), reconstruction vs
   projection */
let parity_pair = (z: Zipper.t): (string, string) => (
  display_state_of(~parts=display_parts, z),
  display_state_of(~parts=display_parts_promise, z),
);

/* per-keystroke parity over typing `text` at ¦ in `ctx`: returns the
   list of (step, current, promise) where the two differ */
let parity_diffs_in =
    (~ctx="¦", text: string): list((string, string, string)) => {
  let base = Test_Editing.mk(ctx);
  let ins = Token.to_list(text) |> List.map(c => Action.Insert(c));
  let rec steps = (k, acc) =>
    if (k > List.length(ins)) {
      List.rev(acc);
    } else {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          base @ List.filteri((i, _) => i < k, ins),
        );
      let (cur, prom) = parity_pair(z);
      steps(
        k + 1,
        cur == prom ? acc : [(string_of_int(k), cur, prom), ...acc],
      );
    };
  steps(1, []);
};

/* a parity test asserting the current==promise render for the whole
   trajectory (empty diff = full parity); when `waiver` is nonempty it
   is the expected residual diff (the enumerated jank), pinned so a
   regression that WIDENS the gap reds the suite */
let parity_case = (name: string, ~ctx="¦", ~waiver="", text: string) =>
  test_case(
    name,
    `Quick,
    () => {
      let diffs = parity_diffs_in(~ctx, text);
      let rendered =
        diffs
        |> List.map(((k, c, p)) =>
             "step " ++ k ++ ":\n  cur=" ++ c ++ "\n  prom=" ++ p
           )
        |> String.concat("\n");
      check(string_testable, name, waiver, rendered);
    },
  );

/* parity over an explicitly-built zipper (for scenarios display_case
   / trajectory can't express) */
let parity_case_z = (name: string, ~waiver="", mk_z: unit => Zipper.t) =>
  test_case(
    name,
    `Quick,
    () => {
      let (cur, prom) = parity_pair(mk_z());
      check(
        string_testable,
        name,
        waiver == "" ? cur : waiver,
        waiver == "" ? prom : prom,
      );
    },
  );

/* JOINED-STEP RE-PIN RECORD (2026-07-21): the grout-free edit state
   moved three judged classes through this corpus, tokens and caret
   byte-identical in every pin (auto-verified at re-pin time):
   1. derived holes are SYSTEM material — they render inside ghost
      zones (`(¦⟪?, ...` not `(¦?⟪, ...`);
   2. junction holes sit at the owner's line end / policy positions
      (raw-vs-display's caret-jump probe HEALED: raw and display
      pre-caret now agree);
   3. within-run hole positions follow the placement policy.
   One real bug fixed en route: multi-delimiter insertions with an
   embedded witness projected the full shard beside the typed prefix
   (`= ? =>`); they now degrade to the remainder-ghost channel. */
let tests = [
  (
    "CompletionDisplay: target-0",
    [
      /* SNAPSHOT: left-to-right entry of the string_replace call.
         v2 anchored splice — ghosts land at their run's true
         position, after any real hole (v1's caret-locked buffer
         showed `a,¦⟪, ?)⟫?`: material past the visible hole). */
      test_case("string_replace left-to-right snapshot", `Quick, () =>
        check(
          string_testable,
          "t0",
          {|s¦   CHIPS[]
st¦⟪ring_capitalize⟫   CHIPS[]
str¦⟪ing_capitalize⟫   CHIPS[]
stri¦⟪ng_capitalize⟫   CHIPS[]
strin¦⟪g_capitalize⟫   CHIPS[]
string¦⟪_capitalize⟫   CHIPS[]
string_¦⟪capitalize⟫   CHIPS[]
string_r¦⟪eplace⟫   CHIPS[]
string_re¦⟪place⟫   CHIPS[]
string_rep¦⟪lace⟫   CHIPS[]
string_repl¦⟪ace⟫   CHIPS[]
string_repla¦⟪ce⟫   CHIPS[]
string_replac¦⟪e⟫   CHIPS[]
string_replace¦   CHIPS[]
string_replace(¦⟪?, ?, ?)⟫   CHIPS[]
string_replace(a¦⟪, ?, ?)⟫   CHIPS[]
string_replace(a,¦ ?⟪, ?)⟫   CHIPS[]
string_replace(a, ¦?⟪, ?)⟫   CHIPS[]
string_replace(a, b¦⟪, ?)⟫   CHIPS[]
string_replace(a, b,¦ ?⟪)⟫   CHIPS[]
string_replace(a, b, ¦?⟪)⟫   CHIPS[]
string_replace(a, b, c¦⟪)⟫   CHIPS[]
string_replace(a, b, c)¦   CHIPS[]|},
          trajectory("string_replace(a, b, c)"),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: one-liners",
    /* the string IS the test: text before ¦ is typed, the whole
       string is the expected rendering (ghost order = true landing
       order — the two middle cases were v1's buffer-jank states) */
    [
      /* empty parens presume: the full promise from `(` on */
      display_case("string_replace(¦⟪?, ?, ?)⟫"),
      display_case("string_replace(a¦⟪, ?, ?)⟫"),
      display_case("string_replace(a,¦ ?⟪, ?)⟫"),
      display_case("string_replace(a, b,¦ ?⟪)⟫"),
      display_case("string_replace(a, b, c¦⟪)⟫"),
      display_case("let x = 4 i¦⟪n ?⟫"),
      /* trailing space: the ghost hugs the caret (slide_to_caret) —
         a closer drawn left of the caret would portray typing
         outside the completed call */
      display_case("string_replace(a, b, c ¦⟪)⟫"),
      display_case("string_replace(a, b, c  ¦⟪)⟫"),
      /* caret INSIDE the auto-closed string literal: the promise is
         anchored on the host token, so Inner carets still ghost */
      display_case("string_replace(\"¦\"⟪, ?, ?)⟫"),
      /* ap-head suggestion outside any tuple context: the synthesized
         `f(?)` promise (JUDGED improvement with the ap-close rule —
         the ghost used to end at the unbalanced `(`) */
      display_case("let x : String = st¦⟪ring_capitalize(?) in ?⟫"),
    ],
  ),
  (
    "CompletionDisplay: matrix",
    /* forms x contexts: the same entry must read sensibly on a blank
       editor, above existing content, and into a hole mid-program.
       PROBE first, felt-read, then pin. */
    [
      /* padding oracle: holes padded, typed spaces consume pads —
         text constant through every promised keystroke. JUDGED
         improvement (single-channel port): the witness remainder is
         fork material with marks now, so `i¦⟪n⟫ ?` pads its hole —
         the old buffer line rendered the unpadded `in?` residual. */
      test_case("let entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "let-blank",
          {|l¦   CHIPS[]
le¦⟪t ⟫   CHIPS[]
let¦ ? ⟪= ? in ?⟫   CHIPS[]
let ¦? ⟪= ? in ?⟫   CHIPS[]
let x¦ ⟪= ? in ?⟫   CHIPS[]
let x ¦⟪= ? in ?⟫   CHIPS[]
let x =¦ ? ⟪in ?⟫   CHIPS[]
let x = ¦? ⟪in ?⟫   CHIPS[]
let x = 1¦ ⟪in ?⟫   CHIPS[]
let x = 1 ¦⟪in ?⟫   CHIPS[]
let x = 1 i¦⟪n ?⟫   CHIPS[]
let x = 1 in¦?   CHIPS[]|},
          trajectory("let x = 1 in"),
        )
      ),
      /* Full shape normalization (regrout/reassemble/remold) keeps
         every step parseable — real-token slots included. KNOWN
         JANK remaining: missing hole pads (as above); and `let? ¦`
         — regrout puts the typed space AFTER the hole, so the caret
         visually jumps past it (zipper behavior, predates the
         fork — investigate vs dev). */
      test_case("let entry above existing content", `Quick, () =>
        check(
          string_testable,
          "let-above",
          {|l¦~
string_replace(a, b, c)   CHIPS[]
le¦⟪t ⟫~
string_replace(a, b, c)   CHIPS[]
let¦ ? ⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let ¦? ⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let x¦ ⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let x ¦⟪= ? in⟫
string_replace(a, b, c)   CHIPS[]
let x =¦ ? ⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = ¦? ⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = 1¦ ⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = 1 ¦⟪in⟫
string_replace(a, b, c)   CHIPS[]
let x = 1 i¦⟪n⟫
string_replace(a, b, c)   CHIPS[]
let x = 1 in¦
string_replace(a, b, c)   CHIPS[]|},
          trajectory_in(~ctx="¦\nstring_replace(a, b, c)", "let x = 1 in"),
        )
      ),
      /* DELETION (andrew's backspace repro): back through
         `let  =  in` — watch for pre-caret reflow (policy: display
         never changes strictly before the cursor) and the prefix
         behavior when `in` decays to `i`. Pre-caret text is
         byte-stable throughout (policy holds). KNOWN JANK, first
         line: witness absorption doesn't fire for a decayed keyword
         shard, so `i` gets a synthesized `in` beside it instead of
         prefix-joining (`i¦⟪n⟫`) — needs middle-shard witness in the
         completion engine. `let?` snug: the pre-caret pad is
         correctly WITHHELD (policy) — matches the raw zipper and
         dev; pads appear only once the caret returns there. */
      test_case("backspace through let = in", `Quick, () =>
        check(
          string_testable,
          "let-bk",
          {|let ?=  i¦ ⟪in ?⟫   CHIPS[]
let ?=  ¦? ⟪in ?⟫   CHIPS[]
let ?= ¦? ⟪in ?⟫   CHIPS[]
let ?=¦ ? ⟪in ?⟫   CHIPS[]
let  ¦? ⟪= ? in ?⟫   CHIPS[]
let ¦? ⟪= ? in ?⟫   CHIPS[]|},
          trajectory_bk(~ctx="let  =  in¦", 6),
        )
      ),
      /* CLEAN: mid-program insertion into a hole reads right
         throughout */
      test_case("ap entry into a hole mid-program", `Quick, () =>
        check(
          string_testable,
          "ap-in-hole",
          {|let a = s¦ in a + 1   CHIPS[]
let a = st¦⟪ring_capitalize⟫ in a + 1   CHIPS[]
let a = str¦⟪ing_capitalize⟫ in a + 1   CHIPS[]
let a = stri¦⟪ng_capitalize⟫ in a + 1   CHIPS[]
let a = strin¦⟪g_capitalize⟫ in a + 1   CHIPS[]
let a = string¦⟪_capitalize⟫ in a + 1   CHIPS[]
let a = string_¦⟪capitalize⟫ in a + 1   CHIPS[]
let a = string_r¦⟪eplace⟫ in a + 1   CHIPS[]
let a = string_re¦⟪place⟫ in a + 1   CHIPS[]
let a = string_rep¦⟪lace⟫ in a + 1   CHIPS[]
let a = string_repl¦⟪ace⟫ in a + 1   CHIPS[]
let a = string_repla¦⟪ce⟫ in a + 1   CHIPS[]
let a = string_replac¦⟪e⟫ in a + 1   CHIPS[]
let a = string_replace¦ in a + 1   CHIPS[]
let a = string_replace(¦⟪?, ?, ?)⟫ in a + 1   CHIPS[]
let a = string_replace(x¦⟪, ?, ?)⟫ in a + 1   CHIPS[]|},
          trajectory_in(~ctx="let a = ¦ in a + 1", "string_replace(x"),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: inventory",
    /* exploratory sweep (andrew): new forms, leaving things
       incomplete, refactors that break existing form delimiters */
    [
      /* `=¦` line: the end-ghost survives mid-arrow typing (witness
         boundaries); JUDGED improvement (single-channel port): the
         `=>` witness ghosts inline as `⟪>⟫` — the old buffer-only
         path never fired on the case arrow (ex-known-jank), so it
         fell back to a chip */
      test_case("case entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "case-entry",
          {|c¦   CHIPS[]
ca¦⟪se ⟫   CHIPS[]
cas¦⟪e ⟫   CHIPS[]
case¦ ? ⟪end⟫   CHIPS[]
case ¦? ⟪end⟫   CHIPS[]
case 1¦ ⟪end⟫   CHIPS[]
case 1 ¦⟪end⟫   CHIPS[]
case 1 |¦ ? ⟪=> ? end⟫   CHIPS[]
case 1 | ¦? ⟪=> ? end⟫   CHIPS[]
case 1 | 2¦ ⟪=> ? end⟫   CHIPS[]
case 1 | 2 ¦⟪=> ? end⟫   CHIPS[]
case 1 | 2 =¦⟪>⟫ ? ⟪end⟫   CHIPS[]
case 1 | 2 =>¦ ? ⟪end⟫   CHIPS[]
case 1 | 2 => ¦? ⟪end⟫   CHIPS[]
case 1 | 2 => 3¦ ⟪end⟫   CHIPS[]|},
          trajectory("case 1 | 2 => 3"),
        )
      ),
      test_case("if entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "if-entry",
          {|i¦   CHIPS[]
if¦ ? ⟪then ? else ?⟫   CHIPS[]
if ¦? ⟪then ? else ?⟫   CHIPS[]
if 1¦ ⟪then ? else ?⟫   CHIPS[]
if 1 ¦⟪then ? else ?⟫   CHIPS[]
if 1 <¦ ? ⟪then ? else ?⟫   CHIPS[]|},
          trajectory("if 1 <"),
        )
      ),
      /* COEXISTENCE: TyDi's witness completes the token, chip
         promise follows — one continuous ghost (was: witness
         suppressed the chip ghosts to chips) */
      test_case("annotation tuple: TyDi x chip ghosts", `Quick, () =>
        check(
          string_testable,
          "annot-tydi",
          {|let a : (¦⟪?) = ? in ?⟫   CHIPS[]
let a : (S¦⟪) = ? in ?⟫   CHIPS[]
let a : (St¦⟪ring) = ? in ?⟫   CHIPS[]|},
          trajectory_in(~ctx="let a : ¦", "(St"),
        )
      ),
      /* CLEAN: deleting `)` inside a complete let recovers in place —
         ghost closer immediately, `in y` unmoved, pads correct */
      test_case("break the closer of a complete call", `Quick, () =>
        check(
          string_testable,
          "break-closer",
          {|let y = string_replace(a, b, c¦⟪)⟫ in y   CHIPS[]
let y = string_replace(a, b, ¦?⟪)⟫ in y   CHIPS[]
let y = string_replace(a, b,¦ ?⟪)⟫ in y   CHIPS[]|},
          trajectory_bk(~ctx="let y = string_replace(a, b, c)¦ in y", 3),
        )
      ),
      /* deleting the `(` of a complete call: the completion proposes
         re-opening from the LEFT (side-Left leading run) — as a
         ghost that would sit strictly before the caret, so it is
         SUPPRESSED to a chip (splice_precedes_caret; the caret is
         Inner in the preceding name after this deletion) */
      test_case("break the opener of a complete call", `Quick, () =>
        check(
          string_testable,
          "break-opener",
          "string_replace\xc2\xa6a, b, c)   CHIPS[(]",
          trajectory_bk(~ctx="string_replace(\xc2\xa6a, b, c)", 1),
        )
      ),
      /* CLEAN: moving away dismisses the ghost, chips persist for
         the abandoned site, typing below never re-conjures it, and
         line 1 stays byte-stable (the ~ is the real junction hole
         joining the incomplete call to the next line) */
      test_case("abandon incomplete call, work below", `Quick, () =>
        check(
          string_testable,
          "abandon",
          {|string_replace(a¦⟪, ?, ?)⟫ ~
1 + 1   CHIPS[]
---
string_replace(a~
1 + 1¦   CHIPS[,+,+)]
---
string_replace(a~
1 + 1 + 2¦   CHIPS[,+,+)]|},
          {
            let z =
              Test_Editing.perform(
                Zipper.init(),
                Test_Editing.mk("¦\n1 + 1")
                @ (
                  Token.to_list("string_replace(a")
                  |> List.map(c => Action.Insert(c))
                ),
              );
            let after_typing = display_state(z);
            let z =
              Test_Editing.perform(
                z,
                [Test_Editing.move_point(~row=1, ~col=5, ())],
              );
            let after_move = display_state(z);
            let z =
              Test_Editing.perform(
                z,
                Token.to_list(" + 2") |> List.map(c => Action.Insert(c)),
              );
            let after_more = display_state(z);
            after_typing ++ "\n---\n" ++ after_move ++ "\n---\n" ++ after_more;
          },
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: matrix-2",
    /* second sweep: fun, list literal, nested call, breaking an
       inner delimiter inside an outer complete form */
    [
      /* last line, JUDGED improvement (single-channel port): the
         mid-arrow witness (`-` of `->`) ghosts its remainder inline
         — symbolic witnesses used to fall back to chips (same class
         as the case arrow) */
      test_case("fun entry, blank editor", `Quick, () =>
        check(
          string_testable,
          "fun-entry",
          {|f¦   CHIPS[]
fu¦⟪n ⟫   CHIPS[]
fun¦ ? ⟪-> ?⟫   CHIPS[]
fun ¦? ⟪-> ?⟫   CHIPS[]
fun x¦ ⟪-> ?⟫   CHIPS[]
fun x ¦⟪-> ?⟫   CHIPS[]
fun x -¦⟪> ?⟫   CHIPS[]|},
          trajectory("fun x -"),
        )
      ),
      test_case("list literal entry", `Quick, () =>
        check(
          string_testable,
          "list-entry",
          {|[¦⟪?]⟫   CHIPS[]
[1¦⟪]⟫   CHIPS[]
[1,¦ ?⟪]⟫   CHIPS[]
[1, ¦?⟪]⟫   CHIPS[]
[1, 2¦⟪]⟫   CHIPS[]|},
          trajectory("[1, 2"),
        )
      ),
      /* CLEAN (structured lookahead): the T2 payload is head+tail —
         witness `string_capitalize(`, hole, `)` — so the ghost shows
         a real hole in the promised parens. The lookahead's own
         separator commas are dropped: the T1 deficit at the same
         anchor already promises them (the flat string double-counted
         as `,,`). Text constant through the whole witness — INCLUDING
         the `(` keystroke: once inside the inner ap, the promise
         reads inner-closer-first (`), ?, ?)`), the outer commas after
         it (RE-JUDGED per andrew: the old `, ?, ?))` order put the
         outer tuple's commas inside the inner ap — Tab typed them
         there forever, the infinite-comma loop). */
      test_case("nested call entry", `Quick, () =>
        check(
          string_testable,
          "nested-call",
          {|let s = s¦ in s   CHIPS[]
let s = st¦⟪ring_capitalize⟫ in s   CHIPS[]
let s = str¦⟪ing_capitalize⟫ in s   CHIPS[]
let s = stri¦⟪ng_capitalize⟫ in s   CHIPS[]
let s = strin¦⟪g_capitalize⟫ in s   CHIPS[]
let s = string¦⟪_capitalize⟫ in s   CHIPS[]
let s = string_¦⟪capitalize⟫ in s   CHIPS[]
let s = string_r¦⟪eplace⟫ in s   CHIPS[]
let s = string_re¦⟪place⟫ in s   CHIPS[]
let s = string_rep¦⟪lace⟫ in s   CHIPS[]
let s = string_repl¦⟪ace⟫ in s   CHIPS[]
let s = string_repla¦⟪ce⟫ in s   CHIPS[]
let s = string_replac¦⟪e⟫ in s   CHIPS[]
let s = string_replace¦ in s   CHIPS[]
let s = string_replace(¦⟪?, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(s¦⟪, ?, ?)⟫ in s   CHIPS[]
let s = string_replace(st¦⟪ring_capitalize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(str¦⟪ing_capitalize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(stri¦⟪ng_capitalize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(strin¦⟪g_capitalize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string¦⟪_capitalize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_¦⟪capitalize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_c¦⟪apitalize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_ca¦⟪pitalize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_cap¦⟪italize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capi¦⟪talize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capit¦⟪alize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capita¦⟪lize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capital¦⟪ize(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitali¦⟪ze(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitaliz¦⟪e(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitalize¦⟪(?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitalize(¦⟪?), ?, ?)⟫ in s   CHIPS[]
let s = string_replace(string_capitalize(x¦⟪), ?, ?)⟫ in s   CHIPS[]|},
          trajectory_in(
            ~ctx="let s = ¦ in s",
            "string_replace(string_capitalize(x",
          ),
        )
      ),
      /* lookahead with an operator tail: a List ctx suggests an ap
         PLUS cons — head witness, hole, `)`, `::`. No T1 at the site,
         so the tail keeps all its delimiters. The `::` hugs the `)`
         (minted comments always hug left — the witness-remainder
         rule); acceptable: the promise reads as one chunk. */
      test_case("lookahead with operator tail (cons)", `Quick, () =>
        check(
          string_testable,
          "lookahead-cons",
          {|let l : [Int] = s¦ ⟪in ?⟫   CHIPS[]
let l : [Int] = st¦⟪ring_length(?):: in ?⟫   CHIPS[]|},
          trajectory_in(~ctx="let l : [Int] = ¦", "st"),
        )
      ),
      /* CLEAN: inner closer deletion recovers in place under the
         outer complete let */
      test_case("break inner paren inside complete let", `Quick, () =>
        check(
          string_testable,
          "break-inner",
          {|let y = (1 + 2¦⟪)⟫ in y   CHIPS[]
let y = (1 + ¦?⟪)⟫ in y   CHIPS[]|},
          trajectory_bk(~ctx="let y = (1 + 2)¦ in y", 2),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: multi-ghost",
    /* a linebreak can split one merged promise into several
       insertions all valid at the caret — every caret-zone insertion
       ghosts now (one used to ghost, the rest fell back to chips:
       andrew's ghost-vs-quiver-after-Enter report) */
    [
      test_case("case bar + Enter: end AND in both ghost", `Quick, () =>
        check(
          string_testable,
          "mg1",
          /* last line: end AND in both ghost at the caret, quiver
             empty (was CHIPS[end]). RE-JUDGED 2026-07-22 under
             measured-faithful rendering: phantom doubled indent
             (add_indent prefix over stored spaces) is gone — rows
             sit at stored-space columns; typed chars precede the
             caret at true columns with the witness remainder inside
             the span (ca¦⟪se …⟫); the old `⟪  in⟫` doubled-space
             seam healed with it. */
          {|let f(b : Bool) =?
  ¦⟪in ?⟫   CHIPS[]
let f(b : Bool) =
  c¦ ⟪in ?⟫   CHIPS[]
let f(b : Bool) =
  ca¦⟪se in ?⟫   CHIPS[]
let f(b : Bool) =
  cas¦⟪e in ?⟫   CHIPS[]
let f(b : Bool) =
  case¦ ? ⟪end in ?⟫   CHIPS[]
let f(b : Bool) =
  case ¦? ⟪end in ?⟫   CHIPS[]
let f(b : Bool) =
  case b¦ ⟪end in ?⟫   CHIPS[]
let f(b : Bool) =
  case ba¦ ⟪end in ?⟫   CHIPS[]
let f(b : Bool) =
  case bar¦ ⟪end in ?⟫   CHIPS[]
let f(b : Bool) =
  case bar
  ¦⟪end in ?⟫   CHIPS[]|},
          trajectory_in(~ctx="let f(b : Bool) =¦", "\ncase bar\n"),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: fn-authoring",
    /* andrew's canonical function-authoring scenario, tail states */
    [
      test_case("case arm underscore then space", `Quick, () =>
        check(
          string_testable,
          "fa1",
          /* the space keystroke is TEXT-CONSTANT: the typed space
             splits the merged =>+end+in run, both halves slide to
             the caret, and the same-slid-ref tie resolves by
             ORIGINAL material order. RE-JUDGED 2026-07-22 under
             measured-faithful rendering: the en⟫d bracket-offset
             artifact (span marks landing mid-token on multiline
             states) is HEALED — spans sit at honest columns; the
             phantom doubled indent on continuation rows is gone. */
          {|let new_fun(foo: Int, bar: Bool) =
      case foo
      | 1 => bar
   ¦⟪end in ?⟫   CHIPS[]
let new_fun(foo: Int, bar: Bool) =
      case foo
      | 1 => bar
    ¦⟪end in ?⟫   CHIPS[]
let new_fun(foo: Int, bar: Bool) =
      case foo
      | 1 => bar
     ¦⟪end in ?⟫   CHIPS[]
let new_fun(foo: Int, bar: Bool) =
      case foo
      | 1 => bar
      ¦⟪end in ?⟫   CHIPS[]
let new_fun(foo: Int, bar: Bool) =
      case foo
      | 1 => bar
      |¦ ? ⟪=> ? end in ?⟫   CHIPS[]
let new_fun(foo: Int, bar: Bool) =
      case foo
      | 1 => bar
      | ¦? ⟪=> ? end in ?⟫   CHIPS[]
let new_fun(foo: Int, bar: Bool) =
      case foo
      | 1 => bar
      | _¦ ⟪=> ? end in ?⟫   CHIPS[]
let new_fun(foo: Int, bar: Bool) =
      case foo
      | 1 => bar
      | _ ¦⟪=> ? end in ?⟫   CHIPS[]|},
          trajectory_in(
            ~ctx=
              "let new_fun(foo: Int, bar: Bool) =\n    case foo\n    | 1 => bar\n¦",
            "    | _ ",
          ),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: probes-andrew-3",
    /* andrew's live reports 2026-07-12, all fixed: (1) witness chip
       suppressed while TyDi ghosts it — suppression now has ONE home
       (chips_displayed) shared by deco and harness; (2) `ue then`
       spaced — comments are content-width in the oracle, not
       separators; (3) else-promise anchors AFTER the visible
       witnessed `t(hen)` — witnessed shards are walk boundaries
       anchoring at their absorbed token */
    [
      test_case("let a = 1 i: ghost + chip both?", `Quick, () =>
        check(
          string_testable,
          "p1",
          {|let a = 1 i¦⟪n ?⟫   CHIPS[]|},
          trajectory_in(~ctx="let a = 1 ¦", "i"),
        )
      ),
      test_case("if tr: TyDi + then-ghost spacing", `Quick, () =>
        check(
          string_testable,
          "p2",
          {|if t¦ ⟪then ? else ?⟫   CHIPS[]
if tr¦⟪ue then ? else ?⟫   CHIPS[]|},
          trajectory_in(~ctx="if ¦", "tr"),
        )
      ),
      test_case("if true t: else chip before then?", `Quick, () =>
        check(
          string_testable,
          "p3",
          {|if true t¦⟪hen⟫ ? ⟪else ?⟫   CHIPS[]|},
          trajectory_in(~ctx="if true ¦", "t"),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: raw-vs-display",
    [
      test_case(
        "space after let: zipper order vs display order",
        `Quick,
        () => {
          /* raw zipper rendering, no display fork — localizes the
             caret-jump: if raw is `let ¦?` and display is `let? ¦`,
             the DISPLAY regrout (normalize_display) reordered it */
          let raw_state = (code: string): string => {
            let z =
              Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
            let seg = Zipper.unselect_and_zip(z);
            let measured =
              Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
            let caret = Zipper.Caret.point(measured, z);
            FeltPrint.measured_print(~measured, seg)
            |> String.split_on_char('\n')
            |> Printer.insert_string(
                 "¦",
                 FeltPrint.measured_caret(~measured, seg, caret),
               )
            |> String.concat("\n");
          };
          let disp = (code: string): string => {
            let z =
              Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
            display_state(~chips=false, z);
          };
          check(
            string_testable,
            "raw vs display",
            /* FIXED: minted display grout hops after typed spaces
               (finish_display reorder) — the rendered caret matches
               the zipper; the display only ADDS material right of it */
            {|raw[let ]: let ¦
disp[let ]: let ¦? ⟪= ? in ?⟫
raw[let x ]: let x ¦
disp[let x ]: let x ¦⟪= ? in ?⟫
raw[above]: let ¦
string_replace(a, b, c)|},
            "raw[let ]: "
            ++ raw_state("let ¦")
            ++ "\ndisp[let ]: "
            ++ disp("let ¦")
            ++ "\nraw[let x ]: "
            ++ raw_state("let x ¦")
            ++ "\ndisp[let x ]: "
            ++ disp("let x ¦")
            ++ "\nraw[above]: "
            ++ {
              let z =
                Test_Editing.perform(
                  Zipper.init(),
                  Test_Editing.mk("¦\nstring_replace(a, b, c)")
                  @ (
                    Token.to_list("let ") |> List.map(c => Action.Insert(c))
                  ),
                );
              let seg = Zipper.unselect_and_zip(z);
              let measured =
                Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
              let caret = Zipper.Caret.point(measured, z);
              FeltPrint.measured_print(~measured, seg)
              |> String.split_on_char('\n')
              |> Printer.insert_string(
                   "¦",
                   FeltPrint.measured_caret(~measured, seg, caret),
                 )
              |> String.concat("\n");
            },
          );
        },
      ),
    ],
  ),
  (
    "CompletionDisplay: crash-repro",
    [
      test_case(
        "keyword completion above later content",
        `Quick,
        () => {
          /* le¦ on line 1, string_replace("") on line 3; typing t
             completes the let keyword — the spliced display segment
             must stay parseable (Skel.push_output crash) */
          let z =
            Test_Editing.perform(
              Zipper.init(),
              Test_Editing.mk("le\n\nstring_replace(\"\")¦"),
            );
          let z =
            Test_Editing.perform(
              z,
              [
                Test_Editing.move_point(~row=0, ~col=2, ()),
                Action.Insert("t"),
              ],
            );
          check(
            string_testable,
            "no crash",
            "let¦ ? ⟪= ? in⟫\n\nstring_replace(\"\")   CHIPS[,+,]",
            display_state(z),
          );
        },
      ),
    ],
  ),
  (
    "CompletionDisplay: keyword-interleave",
    /* siblings of the case/fun/in Failure("nth") crash: keyword
       forms interleaved so orphan shards reassemble onto fallback
       molds. Judged: a keyword materializing inside another form's
       slot demotes the host's pending delimiters to chips; absorbed
       closers (in/then/end) land as the host's real shards. */
    [
      test_case("fun case", `Quick, () =>
        check(
          string_testable,
          "fun-case",
          {|f¦   CHIPS[]
fu¦⟪n ⟫   CHIPS[]
fun¦ ? ⟪-> ?⟫   CHIPS[]
fun ¦? ⟪-> ?⟫   CHIPS[]
fun c¦ ⟪-> ?⟫   CHIPS[]
fun ca¦ ⟪-> ?⟫   CHIPS[]
fun cas¦ ⟪-> ?⟫   CHIPS[]
fun case¦ ? ⟪end⟫   CHIPS[->]
fun case ¦? ⟪end⟫   CHIPS[->]|},
          trajectory("fun case "),
        )
      ),
      test_case("let fun in", `Quick, () =>
        check(
          string_testable,
          "let-fun-in",
          {|l¦   CHIPS[]
le¦⟪t ⟫   CHIPS[]
let¦ ? ⟪= ? in ?⟫   CHIPS[]
let ¦? ⟪= ? in ?⟫   CHIPS[]
let f¦ ⟪= ? in ?⟫   CHIPS[]
let fu¦ ⟪= ? in ?⟫   CHIPS[]
let fun¦ ? ⟪-> ? in ?⟫   CHIPS[=]
let fun ¦? ⟪-> ? in ?⟫   CHIPS[=]
let fun i¦ ⟪-> ? in ?⟫   CHIPS[=]
let fun?in¦?   CHIPS[-> | =]
let fun?in ¦?   CHIPS[-> | =]|},
          trajectory("let fun in "),
        )
      ),
      test_case("if fun then", `Quick, () =>
        check(
          string_testable,
          "if-fun-then",
          {|i¦   CHIPS[]
if¦ ? ⟪then ? else ?⟫   CHIPS[]
if ¦? ⟪then ? else ?⟫   CHIPS[]
if f¦ ⟪then ? else ?⟫   CHIPS[]
if fu¦ ⟪then ? else ?⟫   CHIPS[]
if fun¦ ? ⟪-> ? then ? else ?⟫   CHIPS[]
if fun ¦? ⟪-> ? then ? else ?⟫   CHIPS[]
if fun t¦ ⟪-> ? then ? else ?⟫   CHIPS[]
if fun th¦ ⟪-> ? then ? else ?⟫   CHIPS[]
if fun the¦ ⟪-> ? then ? else ?⟫   CHIPS[]
if fun?then¦ ? ⟪else ?⟫   CHIPS[->]
if fun?then ¦? ⟪else ?⟫   CHIPS[->]|},
          trajectory("if fun then "),
        )
      ),
      test_case("case fun end", `Quick, () =>
        check(
          string_testable,
          "case-fun-end",
          {|c¦   CHIPS[]
ca¦⟪se ⟫   CHIPS[]
cas¦⟪e ⟫   CHIPS[]
case¦ ? ⟪end⟫   CHIPS[]
case ¦? ⟪end⟫   CHIPS[]
case f¦ ⟪end⟫   CHIPS[]
case fu¦⟪n end⟫   CHIPS[]
case fun¦ ? ⟪-> ? end⟫   CHIPS[]
case fun ¦? ⟪-> ? end⟫   CHIPS[]
case fun e¦ ⟪-> ? end⟫   CHIPS[]
case fun en¦ ⟪-> ? end⟫   CHIPS[]
case fun?end¦   CHIPS[->]|},
          trajectory("case fun end"),
        )
      ),
      test_case("case if |", `Quick, () =>
        check(
          string_testable,
          "case-if-bar",
          {|c¦   CHIPS[]
ca¦⟪se ⟫   CHIPS[]
cas¦⟪e ⟫   CHIPS[]
case¦ ? ⟪end⟫   CHIPS[]
case ¦? ⟪end⟫   CHIPS[]
case i¦ ⟪end⟫   CHIPS[]
case if¦ ? ⟪then ? else ? end⟫   CHIPS[]
case if ¦? ⟪then ? else ? end⟫   CHIPS[]
case if |¦   CHIPS[then+else | =>+end]
case if | ¦   CHIPS[then+else | =>+end]|},
          trajectory("case if | "),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: tab-dispatch",
    /* Tab reads THE assist stream: tab_chip picks the zone chip
       (witness remainder preferred — the nearest promise), tab_text
       is the paste payload */
    [
      test_case(
        "Tab payloads across suggestion states",
        `Quick,
        () => {
          let payload = (code: string): string => {
            let z =
              Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
            let (_, zc, _, _, _, assist, _) = display_parts(z);
            switch (CanonicalCompletion.tab_chip(zc, assist)) {
            | None => "NONE"
            | Some(ins) =>
              switch (CanonicalCompletion.tab_text(zc, ins)) {
              | None => "NO-TEXT"
              | Some(t) => "<" ++ t ++ ">"
              }
            };
          };
          check(
            string_testable,
            "tab",
            "<ring_capitalize(> | <n> | <>> | <in >",
            String.concat(
              " | ",
              [
                /* T2 variable completion wins over the let's in-chip */
                payload("let x : String = st¦"),
                /* engine witness remainder */
                payload("let x = 1 i¦"),
                /* case arrow witness */
                payload("case 1 | 2 =¦"),
                /* no witness in zone: the closer chip */
                payload("let x = 1 ¦"),
              ],
            ),
          );
        },
      ),
      /* T2 lookahead acceptance CHUNKS, multi-Tab CONVERGENCE: the
         first Tab pastes the head remainder only — caret lands inside
         the promised parens. Later Tabs dispatch what the recomputed
         stream owes at that state, INNER CLOSER FIRST, then the outer
         site's commas, then the outer closer (RE-JUDGED per andrew:
         the retired pin accepted commas before the inner closer — Tab
         pasted every comma into the INNER ap, which never fed the
         outer deficit: the live infinite-comma loop). Statics at LIVE
         cadence (reified). Each Tab must strictly reduce the total
         owed material (OWED = assist-stream delimiter count) and
         never revisit a state; the trajectory ends at NONE. */
      test_case(
        "Tab chunks a lookahead completion to convergence",
        `Quick,
        () => {
          let step = (z: Zipper.t): (string, option(Zipper.t)) => {
            let (_, zc, _, _, _, assist, _) =
              display_parts_live(~statics_z=z, z);
            let owed =
              assist
              |> List.map((i: CanonicalCompletion.insertion) =>
                   List.length(i.delimiters)
                 )
              |> List.fold_left((+), 0);
            let state =
              Printf.sprintf("%s   OWED[%d]", Test_Editing.printer(z), owed);
            switch (CanonicalCompletion.tab_chip(zc, assist)) {
            | None => (state, None)
            | Some(ins) =>
              switch (CanonicalCompletion.tab_text(zc, ins)) {
              | None => (state, None)
              | Some(t) => (
                  state,
                  Some(Test_Editing.perform(z, [Paste(t)])),
                )
              }
            };
          };
          let rec run = (z, n, acc) =>
            if (n <= 0) {
              List.rev(acc);
            } else {
              switch (step(z)) {
              | (state, None) => List.rev(["NONE", state, ...acc])
              | (state, Some(z)) => run(z, n - 1, [state, ...acc])
              };
            };
          let states_of = (code: string): list(string) => {
            let z =
              Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
            run(z, 12, []);
          };
          /* mechanical loop alarms: owed strictly decreases, no state
             repeats (the pin already shows both; these fail loudly if
             a regression reintroduces the loop past the pin's horizon) */
          let audit = (states: list(string)) => {
            let owed_of = (s: string): option(int) =>
              switch (split_first("OWED[", s)) {
              | Some((_, rest)) =>
                switch (split_first("]", rest)) {
                | Some((n, _)) => int_of_string_opt(n)
                | None => None
                }
              | None => None
              };
            let owed_seq = states |> List.filter_map(owed_of);
            let rec strictly_dec = l =>
              switch (l) {
              | [a, b, ...tl] => a > b && strictly_dec([b, ...tl])
              | _ => true
              };
            if (!strictly_dec(owed_seq)) {
              Alcotest.fail(
                "owed material not strictly decreasing: "
                ++ String.concat("\n", states),
              );
            };
            if (List.length(List.sort_uniq(compare, states))
                != List.length(states)) {
              Alcotest.fail(
                "state repeated: " ++ String.concat("\n", states),
              );
            };
            states;
          };
          /* raw-zipper states (no fork): the `?,? )` spacing is the
             pre-existing Paste/regrout reshuffle when a closer lands
             left of a hole — same result as typing `)` there by
             hand; flagged, not display truth (the fork pads
             ghost-bearing frames only). The property pinned here is
             CONVERGENCE and SITE-correctness: inner closer, then
             each tuple's commas landing in THEIR OWN tuple, then the
             outer closer, then nothing. */
          check(
            string_testable,
            "tab-chunks",
            {|let s = string_replace(st¦ in s   OWED[5]
let s = string_replace(string_capitalize(¦ in s   OWED[4]
let s = string_replace(string_capitalize()¦ in s   OWED[3]
let s = string_replace(string_capitalize(), ¦ in s   OWED[2]
let s = string_replace(string_capitalize(), , ¦ in s   OWED[1]
let s = string_replace(string_capitalize(), , )¦ in s   OWED[0]
NONE|},
            states_of("let s = string_replace(st¦ in s")
            |> audit
            |> String.concat("\n"),
          );
          /* doubly-deficient nesting: both sites' commas serviced at
             their own depth, innermost first */
          check(
            string_testable,
            "tab-chunks-nested2",
            {|let s = string_replace(string_replace(st¦ in s   OWED[8]
let s = string_replace(string_replace(string_capitalize(¦ in s   OWED[7]
let s = string_replace(string_replace(string_capitalize()¦ in s   OWED[6]
let s = string_replace(string_replace(string_capitalize(), ¦ in s   OWED[5]
let s = string_replace(string_replace(string_capitalize(), , ¦ in s   OWED[4]
let s = string_replace(string_replace(string_capitalize(), , )¦ in s   OWED[3]
let s = string_replace(string_replace(string_capitalize(), , ), ¦ in s   OWED[2]
let s = string_replace(string_replace(string_capitalize(), , ), , ¦ in s   OWED[1]
let s = string_replace(string_replace(string_capitalize(), , ), , )¦ in s   OWED[0]
NONE|},
            states_of("let s = string_replace(string_replace(st¦ in s")
            |> audit
            |> String.concat("\n"),
          );
        },
      ),
    ],
  ),
  (
    "CompletionDisplay: live-cadence parity",
    /* the harness's display_parts derives statics FRESH and
       PRE-REIFICATION from the rendered zipper; the live editor feeds
       the fork CachedStatics' output — debounce-stale during a burst
       and REIFIED once owed commas exist (the settled info_map anas
       the anchor at its ELEMENT type, not the raw Prod). That input
       gap is exactly where the 2026-07 live regressions hid while the
       fresh pins stayed green (ghost `ing_capitalize(, ?, ?)` —
       ap-hole and inner closer missing). These pins render the SAME
       trajectory through the live pipeline and assert it equals the
       fresh rendering, line for line. */
    [
      test_case("nested call: settled statics (lag 0, reified)", `Quick, () =>
        check(
          string_testable,
          "parity-lag0",
          trajectory_in(
            ~ctx="let s = ¦ in s",
            "string_replace(string_capitalize(x",
          ),
          trajectory_live_in(
            ~lag=0,
            ~ctx="let s = ¦ in s",
            "string_replace(string_capitalize(x",
          ),
        )
      ),
      /* mid-burst: statics one keystroke behind. Token-extending
         keystrokes keep their tile id, so the id-keyed type facts
         still land — the display equals the fresh rendering here
         too (a burst that MINTS new sites is covered by the
         synthesize_new_sites stale_tests in Test_TypeObligations). */
      test_case("nested call: mid-burst statics (lag 1)", `Quick, () =>
        check(
          string_testable,
          "parity-lag1",
          trajectory_in(
            ~ctx="let s = ¦ in s",
            "string_replace(string_capitalize(x",
          ),
          trajectory_live_in(
            ~lag=1,
            ~ctx="let s = ¦ in s",
            "string_replace(string_capitalize(x",
          ),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: constancy",
    [
      test_case("string_replace entry types through its promise", `Quick, () =>
        check(
          string_testable,
          "no constancy violations",
          "",
          constancy_audit("string_replace(a, b, c)"),
        )
      ),
    ],
  ),
  (
    "CompletionDisplay: promise-render stability",
    /* STAGE 2: PromiseRender.mk IS the live path (display_state renders
       through it), so the pinned trajectories above already pin what
       the user sees. The old stage-1 parity harness (current-vs-promise
       byte equality) is MOOT now that promise IS current — display_parts
       and display_parts_promise are the same function, so every case
       here is a tautology (empty diff) that serves as a determinism +
       crash-free STABILITY smoke over the corpus: the projection runs
       twice and agrees, and never raises across these trajectories.
       The judged pin SHIFTS from swapping in the projection (real T1
       commas, sub-token witnesses, real ap-close parens) are recorded
       in the trajectory pins above, not here. */
    [
      parity_case("target-0", "string_replace(a, b, c)"),
      parity_case("matrix-let-blank", "let x = 1 in"),
      parity_case(
        "matrix-let-above",
        ~ctx="¦\nstring_replace(a, b, c)",
        "let x = 1 in",
      ),
      parity_case(
        "matrix-ap-in-hole",
        ~ctx="let a = ¦ in a + 1",
        "string_replace(x",
      ),
      parity_case("inventory-case", "case 1 | 2 => 3"),
      parity_case("inventory-if", "if 1 <"),
      parity_case("inventory-annot", ~ctx="let a : ¦", "(St"),
      parity_case("matrix2-fun", "fun x -"),
      parity_case("matrix2-list", "[1, 2"),
      parity_case(
        "matrix2-nested",
        ~ctx="let s = ¦ in s",
        "string_replace(string_capitalize(x",
      ),
      parity_case("matrix2-cons", ~ctx="let l : [Int] = ¦", "st"),
      parity_case("multi-ghost", ~ctx="let f(b : Bool) =¦", "\ncase bar\n"),
      parity_case(
        "fn-authoring",
        ~ctx=
          "let new_fun(foo: Int, bar: Bool) =\n    case foo\n    | 1 => bar\n¦",
        "    | _ ",
      ),
      parity_case("probes-andrew-3-p2", ~ctx="if ¦", "tr"),
      parity_case("probes-andrew-3-p3", ~ctx="if true ¦", "t"),
      parity_case("keyword-interleave-fun-case", "fun case "),
      parity_case("keyword-interleave-let-fun-in", "let fun in "),
      parity_case("keyword-interleave-if-fun-then", "if fun then "),
      parity_case("keyword-interleave-case-fun-end", "case fun end"),
      parity_case("keyword-interleave-case-if-bar", "case if | "),
      /* explicitly-built scenarios the trajectory form can't express */
      parity_case_z("break-opener", () => {
        let z =
          Test_Editing.perform(
            Zipper.init(),
            Test_Editing.mk("string_replace(\xc2\xa6a, b, c)")
            @ [Action.Destruct(Local(Left, ByChar))],
          );
        z;
      }),
      parity_case_z("abandon-move-below", () => {
        let z =
          Test_Editing.perform(
            Zipper.init(),
            Test_Editing.mk("¦\n1 + 1")
            @ (
              Token.to_list("string_replace(a")
              |> List.map(c => Action.Insert(c))
            ),
          );
        Test_Editing.perform(
          z,
          [Test_Editing.move_point(~row=1, ~col=5, ())]
          @ (Token.to_list(" + 2") |> List.map(c => Action.Insert(c))),
        );
      }),
      /* INSPECTOR-ON-GHOST-HOLE (spec step 6): a presumed hole in the
         display carries the SAME id statics analyzed, by construction —
         the display's artifact = place(reify(obs, completed)) is the
         very segment CachedStatics splices before its second pass;
         reify mints commas by deterministic Id.next chains and
         GroutPlace re-derives every hole with segment-determined ids.
         So an inspector or error landing on a ghost hole finds it in
         the info_map. This test asserts the coincidence directly, and
         in the strong form the wiring makes true: EVERY grout id in
         the artifact is present in the spliced info_map. */
      test_case(
        "presumed hole id is in the reified info_map",
        `Quick,
        () => {
          let z =
            Test_Editing.perform(
              Zipper.init(),
              Test_Editing.mk("string_replace(a¦"),
            );
          let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
          let MakeTerm.{term, _} =
            MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
          let (info_map0, _) =
            Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
          /* frame-fresh obligations: `f` is a fn of a tuple, so `f(1`
             owes commas + holes; synthesize_new_sites finds the ap */
          let obs = TypeObligations.derive(info_map0);
          let frame_obs =
            TypeObligations.frame_obligations(z, ~info_map=info_map0, obs);
          let art = PromiseArtifact.mk(~obligations=frame_obs, seg);
          /* a presumed (synthesized) convex hole in the reified artifact */
          let orig = PromiseArtifact.collect_ids(seg);
          let rec first_ghost_hole = (sg: Segment.t): option(Id.t) =>
            List.fold_left(
              (acc, p: Piece.t) =>
                switch (acc, p) {
                | (Some(_), _) => acc
                | (None, Grout({id, shape: Convex}))
                    when !Hashtbl.mem(orig, id) =>
                  Some(id)
                | (None, Tile(t)) =>
                  List.fold_left(
                    (a, c) => a == None ? first_ghost_hole(c) : a,
                    None,
                    t.children,
                  )
                | (None, _) => None
                },
              None,
              sg,
            );
          switch (first_ghost_hole(art.reified)) {
          | None => Alcotest.fail("no presumed hole in the reified artifact")
          | Some(hole_id) =>
            /* run statics on the reified term exactly as CachedStatics'
               second pass does */
            let MakeTerm.{term: rterm, _} =
              MakeTerm.from_zip_for_sem_spliced(z, ~root=Sort.Exp, ~splice=sg =>
                GroutPlace.place(TypeObligations.reify(frame_obs, sg))
              );
            let (info_map, _) =
              Statics.mk(
                CoreSettings.on,
                Builtins.ctx_init(Some(Int)),
                rterm,
              );
            check(
              Alcotest.bool,
              "presumed hole id in reified info_map",
              true,
              Id.Map.mem(hole_id, info_map),
            );
            /* the strong form: no artifact hole can miss the map */
            let rec grout_ids = (sg: Segment.t): list(Id.t) =>
              List.concat_map(
                (p: Piece.t) =>
                  switch (p) {
                  | Grout(g) => [g.id]
                  | Tile(t) => List.concat_map(grout_ids, t.children)
                  | _ => []
                  },
                sg,
              );
            let missing =
              grout_ids(art.reified)
              |> List.filter(id => !Id.Map.mem(id, info_map));
            check(
              Alcotest.int,
              "every artifact grout id in reified info_map",
              0,
              List.length(missing),
            );
          };
        },
      ),
    ],
  ),
];
