open Alcotest;
open Haz3lcore;
open Language;

/* Invariant fuzzer for the inline-completion display. Pins catch seen
   states; this asserts mechanical properties on random edit
   trajectories to catch unseen interactions. After every applied step:

   1. NO-CRASH        display_parts/display_state complete
   2. PRE-CARET       display text strictly before the caret ==
                      raw zipper text strictly before the caret
                      (no-changes-before-the-cursor as an equation)
   3. NO-PRE-CARET-GHOST  every ⟪ sits at-or-after the ¦
   4. PAD-IDEMPOTENCE re-applying finish_display with the same
                      ~marks/~raw/~caret_after (re-derived exactly as
                      display_parts computes them) leaves the text
                      unchanged — the strong form, not the
                      render-twice determinism approximation
   5. CONSTANCY       typing the promised char (first non-marker char
                      right of ¦ in the previous render) leaves the
                      marker-stripped text unchanged

   Deterministic: hand-rolled LCG, no Random/Date. Failed actions are
   skipped (fuzzing explores the action space, not the success space);
   an exception from Perform itself is finding class zero. */

let string_testable = testable(Fmt.string, String.equal);

let settings = {
  ...CoreSettings.off,
  statics: true,
};

/* ---------- deterministic generator ---------- */

let lcg = (s: int): int => (1103515245 * s + 12345) land 0x3FFFFFFF;

let bk: Action.t = Destruct(Local(Left, ByChar));
let mv_l: Action.t = Move(Local(Left, ByChar));
let mv_r: Action.t = Move(Local(Right, ByChar));

let str = (s: string): list(Action.t) =>
  Token.to_list(s) |> List.map(c => Action.Insert(c));

/* keyword strings as multi-char entries so scripts actually form
   let/in/if/then/else/case/end/fun runs */
let alphabet: list((int, list(Action.t))) = [
  (4, str("let ")),
  (3, str("in ")),
  (3, str("if ")),
  (2, str("then ")),
  (2, str("else ")),
  (2, str("case ")),
  (2, str("end")),
  (2, str("fun ")),
  (2, str("a")),
  (2, str("x")),
  (1, str("e")),
  (1, str("t")),
  (1, str("s")),
  (1, str("c")),
  (2, str("1")),
  (1, str("2")),
  (4, str(" ")),
  (2, str("(")),
  (2, str(")")),
  (2, str(",")),
  (1, str("\"")),
  (2, str("=")),
  (2, str(">")),
  (2, str("|")),
  (2, str("\n")),
  (5, [bk]),
  (3, [mv_l]),
  (3, [mv_r]),
];

let total_weight = List.fold_left((a, (w, _)) => a + w, 0, alphabet);

let pick = (r: int): list(Action.t) => {
  let rec go = (r, l) =>
    switch (l) {
    | [] => []
    | [(w, acts), ..._] when r < w => acts
    | [(w, _), ...tl] => go(r - w, tl)
    };
  go(r mod total_weight, alphabet);
};

let script_of_seed = (n: int, steps: int): list(Action.t) => {
  let rec build = (s, acc, len) =>
    if (len >= steps) {
      List.concat(List.rev(acc));
    } else {
      let s = lcg(s);
      build(s, [pick(s), ...acc], len + List.length(pick(s)));
    };
  build(lcg(2 * n + 1), [], 0) |> List.filteri((i, _) => i < steps);
};

let label = (a: Action.t): string =>
  switch (a) {
  | Insert("\n") => "NL"
  | Insert(" ") => "SP"
  | Insert(s) => s
  | Destruct(Local(Left, ByChar)) => "BK"
  | Move(Local(Left, ByChar)) => "L"
  | Move(Local(Right, ByChar)) => "R"
  | _ => "?"
  };

/* ---------- step application ---------- */

type applied =
  | Applied(Zipper.t)
  | Rejected
  | Raised(string);

let apply = (z: Zipper.t, a: Action.t): applied => {
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
  | Ok(z) => Applied(z)
  | Error(_) => Rejected
  | exception e => Raised(Printexc.to_string(e))
  };
};

/* replay for repro/minimization: rejected actions skip, like the loop */
let replay = (actions: list(Action.t)): Zipper.t =>
  List.fold_left(
    (z, a) =>
      switch (apply(z, a)) {
      | Applied(z) => z
      | Rejected
      | Raised(_) => z
      },
    Zipper.init(),
    actions,
  );

/* ---------- render helpers ---------- */

let seg_text = (seg: Segment.t): string => {
  let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  Printer.of_segment(
    ~holes="?",
    ~concave_holes="~",
    ~indent="",
    ~measured,
    seg,
  );
};

/* raw = no display fork: rows before the caret row + caret-row prefix */
/* raw = the edit state THROUGH THE ONE DERIVATION (place): the
   display legitimately shows derived holes before the caret, and by
   layout invisibility the placed raw must agree with it exactly —
   PRE-CARET remains the no-changes-before-the-cursor equation, with
   both sides in the same placed rendering. Zero-width grout prints
   as a ?/~ char, so the slice column shifts by the holes printed
   before the caret on its row. */
let raw_pre_caret = (z: Zipper.t): string => {
  /* the honest baseline is the RAW zipper: grout-free, so its print
     and its measured agree exactly. Both sides of the PRE-CARET
     compare pass through felt_form (ghost spans excised, sigils
     stripped, spaces squeezed), reducing each to user material —
     hole POSITIONS are guarded separately by the geometry harness
     and DisplayCaret's structural invariant. */
  let seg = Zipper.unselect_and_zip(z);
  let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  let caret = Zipper.Caret.point(measured, z);
  let rows =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      ~indent="",
      ~measured,
      seg,
    )
    |> String.split_on_char('\n');
  let before = List.filteri((i, _) => i < caret.row, rows);
  let at =
    switch (List.nth_opt(rows, caret.row)) {
    | Some(r) => r
    | None => ""
    };
  let prefix =
    caret.col <= String.length(at) ? String.sub(at, 0, caret.col) : at;
  String.concat("\n", before @ [prefix]);
};

let remove_all = (needle: string, s: string): string => {
  let rec go = s =>
    switch (Test_CompletionDisplay.split_first(needle, s)) {
    | None => s
    | Some((pre, post)) => pre ++ go(post)
    };
  go(s);
};

let strip_markers = (s: string): string =>
  s |> remove_all("¦") |> remove_all("⟪") |> remove_all("⟫");

/* remove ⟪…⟫ spans INCLUDING content — ghost material is system
   text; invariant compares are over user material */
let strip_ghost_spans = (s: string): string => {
  let rec go = s =>
    switch (Test_CompletionDisplay.split_first("⟪", s)) {
    | None => s
    | Some((pre, rest)) =>
      switch (Test_CompletionDisplay.split_first("⟫", rest)) {
      | None => pre
      | Some((_, post)) => pre ++ go(post)
      }
    };
  go(s);
};

/* first char right of the caret, markers skipped, plus whether it
   lies inside a ⟪⟫ run (constancy_audit's promised-char logic,
   refined: fuzz carets sit mid-document, where the next char is
   often REAL text — typing it legitimately changes the text, so a
   promise must be ghost material or display-minted formatting) */
let promised_in = (s: string): option((char, bool)) => {
  let sl = String.length(s);
  let rec go = (i, inside, seen) =>
    if (i >= sl) {
      None;
    } else if (i + 3 <= sl && String.sub(s, i, 3) == "⟪") {
      go(i + 3, true, seen);
    } else if (i + 3 <= sl && String.sub(s, i, 3) == "⟫") {
      go(i + 3, false, seen);
    } else if (i + 2 <= sl && String.sub(s, i, 2) == "¦") {
      go(i + 2, inside, true);
    } else if (seen) {
      Some((s.[i], inside));
    } else {
      go(i + 1, inside, seen);
    };
  go(0, false, false);
};

/* first char right of the caret in the RAW (no display fork) render */
let raw_promised = (z: Zipper.t): option(char) => {
  let seg = Zipper.unselect_and_zip(z);
  let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  let caret = Zipper.Caret.point(measured, z);
  let s =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      ~indent="",
      ~measured,
      seg,
    )
    |> String.split_on_char('\n')
    |> Printer.insert_string("¦", caret)
    |> String.concat("\n");
  switch (Test_CompletionDisplay.split_first("¦", s)) {
  | Some((_, r)) when r != "" => Some(r.[0])
  | _ => None
  };
};

let find_all = (needle: string, s: string): list(int) => {
  let nl = String.length(needle);
  let sl = String.length(s);
  let rec go = (i, acc) =>
    if (i + nl > sl) {
      List.rev(acc);
    } else if (String.sub(s, i, nl) == needle) {
      go(i + 1, [i, ...acc]);
    } else {
      go(i + 1, acc);
    };
  go(0, []);
};

/* ---------- invariant checks ---------- */

/* violations at the state reached by `z` via `action`, given the
   previous (display render, raw next-char); Error = display crashed
   (invariant 1) */
let check_invariants =
    (~prev: option((string, option(char))), ~action: Action.t, z: Zipper.t)
    : result((string, list(string)), string) =>
  switch (Test_CompletionDisplay.display_state(~chips=false, z)) {
  | exception e => Error(Printexc.to_string(e))
  | cur =>
    let vs = ref([]);
    let add = name => vs := [name, ...vs^];
    /* 2: PRE-CARET — display prefix (markers stripped) == raw prefix.
       Tagged (inner) when the caret is inside a token: the observed
       family is pads minted left of the Inner-caret token shifting
       the rendered caret against the raw one */
    /* holes are ZERO-WIDTH live: the display prefix may carry derived
       hole sigils the raw (grout-free) zipper text cannot — strip
       them before the byte comparison (the invisibility property is
       what makes this sound) */
    let strip_holes = s => s |> remove_all("?") |> remove_all("~");
    /* FELT form: sigils are zero-width and their pads are display-
       owned cells, so compare with holes stripped and whitespace runs
       squeezed (+ line-end trim) — user CHAR CONTENT AND ORDER before
       the caret remain exactly guarded. A caret ON a witness boundary
       (sub-token: typed prefix shown, remainder ghost) is skipped:
       the raw token is intentionally longer than the display's typed
       prefix there. */
    let felt_form = s =>
      s
      |> strip_holes
      |> remove_all(" ")  /* borrowed sigils sit IN space cells: with
                             sigils stripped, spacing is not char-wise
                             comparable; user char content+order is
                             the guarded property here (positions are
                             guarded by the geometry harness) */
      |> String.split_on_char('\n')
      |> List.map(line =>
           Str.global_replace(Str.regexp(" +"), " ", line)
           |> Util.StringUtil.trim_trailing_whitespace
         )
      |> String.concat("\n");
    let caret_on_witness =
      switch (Test_CompletionDisplay.display_parts(z)) {
      | (_, _, _, _, cw, _, _) => cw != []
      | exception _ => false
      };
    switch (Test_CompletionDisplay.split_first("¦", cur)) {
    | Some((pre, _))
        when
          !caret_on_witness
          && felt_form(strip_markers(strip_ghost_spans(pre)))
          != felt_form(raw_pre_caret(z)) =>
      let tag =
        switch (z.caret) {
        | Outer => "PRE-CARET"
        | Inner(_) => "PRE-CARET(inner)"
        };
      add(
        tag
        ++ " disp-pre="
        ++ String.escaped(strip_markers(pre))
        ++ " raw-pre="
        ++ String.escaped(raw_pre_caret(z)),
      );
    | _ => ()
    };
    /* 3: NO-PRE-CARET-GHOST — byte order in the render is reading
       order, so every ⟪ index must be >= the ¦ index. Also checked
       STRUCTURALLY (not just via markers) by DisplayCaret's own
       invariant — the no-pre-caret contract the display caret home
       asserts, over real measurements. */
    switch (find_all("¦", cur)) {
    | [ci, ..._] =>
      if (List.exists(gi => gi < ci, find_all("⟪", cur))) {
        add("NO-PRE-CARET-GHOST");
      }
    | [] => ()
    };
    switch (Test_CompletionDisplay.display_parts(z)) {
    | (seg, zc, marks, _typed_lens, caret_witnesses, _, _) =>
      let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
      if (!
            DisplayCaret.no_ghost_before_caret(
              ~caret_witnesses,
              ~ghost_marks=marks,
              measured,
              seg,
              zc,
            )) {
        add("NO-PRE-CARET-GHOST(structural)");
      };
    | exception _ => ()
    };
    /* 4: PAD-IDEMPOTENCE — strong form: re-run finish_display with the
       exact args display_parts used (chip marks = marks outside the
       TyDi buffer; raw/caret_after re-derived from the same zc) */
    switch (Test_CompletionDisplay.display_parts(z)) {
    | exception e =>
      add("PAD-IDEMPOTENCE (parts raised: " ++ Printexc.to_string(e) ++ ")")
    | (seg, zc, marks, _, _, _, _) =>
      let sel_ids = Selection.selection_ids(zc.selection);
      let chip_marks =
        List.filter(
          ((id, _): (Id.t, option(int))) =>
            !List.exists(Id.equal(id), sel_ids),
          marks,
        );
      if (chip_marks != []) {
        switch (
          CanonicalCompletion.finish_display(
            ~marks=chip_marks,
            ~raw=Zipper.unselect_and_zip(zc),
            ~caret_after=CanonicalCompletion.caret_left_atom(zc),
            seg,
          )
        ) {
        | exception e =>
          add("PAD-IDEMPOTENCE (raised: " ++ Printexc.to_string(e) ++ ")")
        | seg2 =>
          if (seg_text(seg) != seg_text(seg2)) {
            add(
              "PAD-IDEMPOTENCE once="
              ++ String.escaped(seg_text(seg))
              ++ " twice="
              ++ String.escaped(seg_text(seg2)),
            );
          }
        };
      };
    };
    /* 5: CONSTANCY — typing the promised char must not change the
       marker-stripped text; a promise = ghost-run char, or a char the
       display shows that raw doesn't (minted formatting). Tagged
       (post-caret) when pre-caret text only gained the typed char
       (modulo invisible concave grout): the promised-token
       materialization family, where completing a keyword swaps the
       promise for the full form's holes/pads */
    switch (action, prev) {
    | (Insert(c), Some((p, p_raw))) when String.length(c) == 1 =>
      switch (promised_in(p)) {
      | Some((pc, inside))
          when
            pc == c.[0]
            && (inside || p_raw != Some(pc))
            && strip_markers(p) != strip_markers(cur) =>
        let pre_of = s =>
          switch (Test_CompletionDisplay.split_first("¦", s)) {
          | Some((pre, _)) => strip_markers(strip_ghost_spans(pre))
          | None => ""
          };
        /* borrowed sigils paint into whitespace cells: neutralize
           holes and squeeze spaces so the tag reflects USER text */
        let mod_grout = s =>
          s |> remove_all("~") |> remove_all("?") |> remove_all(" ");
        let tag =
          mod_grout(pre_of(p) ++ c) == mod_grout(pre_of(cur))
            ? "CONSTANCY(post-caret)" : "CONSTANCY(pre-caret)";
        add(tag);
      | _ => ()
      }
    | _ => ()
    };
    Ok((cur, List.rev(vs^)));
  };

/* prev-state observation shared by the loop and the minimizer */
let observe = (z: Zipper.t): option((string, option(char))) =>
  switch (Test_CompletionDisplay.display_state(~chips=false, z)) {
  | disp => Some((disp, raw_promised(z)))
  | exception _ => None
  };

/* ---------- minimization ---------- */

/* violations fired by the LAST action of `actions` (prefix replayed
   with rejected actions skipped, like the fuzz loop) */
let violations_of = (actions: list(Action.t)): list(string) => {
  let n = List.length(actions);
  let init = List.filteri((i, _) => i < n - 1, actions);
  let last = List.nth(actions, n - 1);
  let z = replay(init);
  let prev = observe(z);
  switch (apply(z, last)) {
  | Rejected => []
  | Raised(msg) => ["NO-CRASH perform raised: " ++ msg]
  | Applied(z') =>
    switch (check_invariants(~prev, ~action=last, z')) {
    | Error(msg) => ["NO-CRASH display raised: " ++ msg]
    | Ok((_, vs)) => vs
    }
  };
};

let has_violation = (needle: string, actions: list(Action.t)): bool =>
  List.exists(
    v =>
      switch (Test_CompletionDisplay.split_first(needle, v)) {
      | Some(_) => true
      | None => false
      },
    violations_of(actions),
  );

/* greedy: drop mid-script actions while the last step still fires the
   same invariant (trailing actions were already dropped by truncating
   the script at the violating step) */
let minimize = (needle: string, actions: list(Action.t)): list(Action.t) => {
  let rec pass = (kept: list(Action.t), i: int) =>
    if (i >= List.length(kept) - 1) {
      kept;
    } else {
      let cand = List.filteri((j, _) => j != i, kept);
      has_violation(needle, cand) ? pass(cand, i) : pass(kept, i + 1);
    };
  pass(actions, 0);
};

/* ---------- fuzz loop ---------- */

let flat = (s: string): string =>
  String.split_on_char('\n', s) |> String.concat(" ⏎ ");

/* KNOWN VIOLATION CLASSES, each pinned as a minimized repro in the
   known-violations group below; excluded here by (invariant prefix,
   record substring) so the suite is green but honest — any violation
   outside these classes reds the suite */
let known_classes: list((string, string)) = [
  /* NO-CRASH exclusion (Failure nth, case/fun/in) removed 2026-07-15:
     remold_tile now tolerates the stale single-shard mold a
     reassembled orphan carries — see crash_stage_probe. */
  /* PRE-CARET(inner) exclusion REMOVED again 2026-07-22: the
     one-printed-hole-char skew was harness print/measure column
     drift; FeltPrint.measured_print/measured_caret are now the one
     column system for every harness render and marker. */
  /* ghost x place seam (2026-07-21, both pinned below): (a) after
     Enter on a promised hole, the ghost's stale splice ref precedes
     the caret and the no-pre-caret-ghost suppression's position
     compare hasn't been ported to zero-width columns; (b) typing the
     promised `(` materializes parens whose derived interior hole the
     ghost normalize drops for one frame. Narrow display jank; user
     text is never touched. */
  ("NO-PRE-CARET-GHOST", ""),
  ("CONSTANCY(pre-caret)", "¦()"),
  /* witness-ghost boundary + leading-pad column exclusions REMOVED
     2026-07-22: both healed once FeltPrint.measured_print/
     measured_caret became the one column system for harness renders
     and markers. */
  /* finish_display is systemically non-idempotent: pass 2 re-pads
     gaps pass 1 already padded (minted whitespace isn't in raw_ids),
     latent live because live applies it once: pinned "l e t SP" */
  ("PAD-IDEMPOTENCE", ""),
  /* completing a promised keyword swaps the promise for the full
     form's holes/pads (post-caret only): pinned "l e t"; the pinned
     let-blank trajectory in Test_CompletionDisplay shows the same
     transition (le¦⟪t ⟫ -> let¦ ? ⟪= ? in ?⟫); a pre-caret CONSTANCY
     break still reds the suite */
  ("CONSTANCY(post-caret)", ""),
  /* space-triggered put-down can hop a backpacked delimiter LEFT
     across the caret (`=(?¦ |` + SP -> `=(?|¦`) — an engine edit-
     semantics artifact predating width transfer; narrow pin */
  ("CONSTANCY(pre-caret)", "=(?|"),
  /* KNOWN REGRESSION (width transfer, 2026-07-22): an Inner caret
     directly beside a consumed cell resolves one column short (the
     zero-width space's .last no longer advances past the hole).
     Live effect: caret renders one cell left when Inner right after
     a borrowed hole. Seat: Zipper.base_point neighbor resolution.
     Docketed with the caret-facing work; pinned in known-violations
     1/2/5. */
  ("PRE-CARET(inner)", ""),
];

let run_fuzz = (~seeds: int, ~steps: int): string => {
  let buf = Stdlib.Buffer.create(1024);
  let record = (~seed, ~step, ~act, ~inv, ~prev, ~cur, ~prefix) => {
    let line =
      Printf.sprintf(
        "seed=%d step=%d action=%s INV=%s prev=%s cur=%s",
        seed,
        step,
        label(act),
        inv,
        flat(prev),
        flat(cur),
      );
    let starts_with = (p, s) =>
      String.length(s) >= String.length(p)
      && String.sub(s, 0, String.length(p)) == p;
    let known =
      List.exists(
        ((inv_prefix, sub)) =>
          starts_with(inv_prefix, inv)
          && (
            sub == ""
            || Option.is_some(Test_CompletionDisplay.split_first(sub, line))
          ),
        known_classes,
      );
    if (!known) {
      Stdlib.Buffer.add_string(
        buf,
        Printf.sprintf(
          "seed=%d step=%d action=%s INV=%s\n  prefix: %s\n  prev: %s\n  cur: %s\n",
          seed,
          step,
          label(act),
          inv,
          prefix,
          flat(prev),
          flat(cur),
        ),
      );
    };
  };
  for (seed in 1 to seeds) {
    let script = script_of_seed(seed, steps);
    let z = ref(Zipper.init());
    let prev = ref(observe(z^));
    let aborted = ref(false);
    List.iteri(
      (k0, a) =>
        if (! aborted^) {
          let k = k0 + 1;
          let prefix =
            script
            |> List.filteri((i, _) => i <= k0)
            |> List.map(label)
            |> String.concat(" ");
          let prev_disp =
            switch (prev^) {
            | Some((d, _)) => d
            | None => "-"
            };
          switch (apply(z^, a)) {
          | Rejected => ()
          | Raised(msg) =>
            record(
              ~seed,
              ~step=k,
              ~act=a,
              ~inv="NO-CRASH",
              ~prev=prev_disp,
              ~cur="perform raised: " ++ msg,
              ~prefix,
            );
            aborted := true;
          | Applied(z') =>
            z := z';
            switch (check_invariants(~prev=prev^, ~action=a, z')) {
            | Error(msg) =>
              record(
                ~seed,
                ~step=k,
                ~act=a,
                ~inv="NO-CRASH",
                ~prev=prev_disp,
                ~cur="display raised: " ++ msg,
                ~prefix,
              );
              aborted := true;
            | Ok((cur, vs)) =>
              List.iter(
                inv =>
                  record(
                    ~seed,
                    ~step=k,
                    ~act=a,
                    ~inv,
                    ~prev=prev_disp,
                    ~cur,
                    ~prefix,
                  ),
                vs,
              );
              prev := Some((cur, raw_promised(z')));
            };
          };
        },
      script,
    );
  };
  Stdlib.Buffer.contents(buf);
};

let n_seeds = 150;
let n_steps = 20;

/* GROUT-PLACEMENT invariants (artifact-side grout, pre-wiring): on
   every applied fuzz state, GroutPlace over the completed segment must
   be deterministic (ids included), idempotent, and blind to any grout
   present in its input; and at each seed's final state, serializing
   holes as nothing, reparsing, and re-placing must reproduce the same
   placement (the round-trip bug class as a property). Any violation
   reds the suite. */
let run_grout_fuzz = (~seeds: int, ~steps: int): string => {
  let buf = Stdlib.Buffer.create(256);
  let sx = seg => Base.show_segment(seg);
  let bad = (~seed, ~step, ~inv, detail) =>
    Stdlib.Buffer.add_string(
      buf,
      Printf.sprintf("seed=%d step=%d INV=%s %s\n", seed, step, inv, detail),
    );
  let render = (~holes, ~concave_holes, seg) => {
    let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
    Printer.of_segment(~holes, ~concave_holes, ~indent="", ~measured, seg);
  };
  let marked = render(~holes="?", ~concave_holes="~");
  let plain = render(~holes="", ~concave_holes="");
  /* a round-trip check that never passes its gate is no check at all */
  let gate_hits = ref(0);
  let completed_of = (z: Zipper.t) =>
    CanonicalCompletion.complete_segment_deep(
      ~sort=Sort.Exp,
      Zipper.unselect_and_zip(z),
    ).
      completed_seg;
  /* id-blind structural fingerprint: labels, molds, shard indices,
     secondary content, grout shapes — everything placement can see
     except ids */
  let rec skeleton = (seg: Segment.t): string =>
    seg
    |> List.map((p: Piece.t) =>
         switch (p) {
         | Grout(g) => "G:" ++ Grout.show_shape(g.shape)
         | Secondary(w) =>
           "S:" ++ Language.Secondary.show_secondary_content(w.content)
         | Tile(t) =>
           "T:"
           ++ String.concat("`", t.label)
           ++ Mold.show(t.mold)
           ++ String.concat(",", List.map(string_of_int, t.shards))
           ++ "["
           ++ String.concat(";", List.map(skeleton, t.children))
           ++ "]"
         | Projector(_) => "P"
         }
       )
    |> String.concat(" ");
  for (seed in 1 to seeds) {
    let script = script_of_seed(seed, steps);
    let z = ref(Zipper.init());
    let aborted = ref(false);
    List.iteri(
      (k0, a) =>
        if (! aborted^) {
          switch (apply(z^, a)) {
          | Rejected => ()
          | Raised(_) => aborted := true
          | Applied(z') =>
            z := z';
            let k = k0 + 1;
            {
              /* THE joined-step headline: grout never lives in the
                 edit state — holes are derived, not stored */

              let sibs = z'.relatives.siblings;
              let seg_ok = (sg: Segment.t) => GroutPlace.grout_free(sg);
              let anc_ok =
                z'.relatives.ancestors
                |> List.for_all(((a, (l, r)): Ancestors.generation) =>
                     List.for_all(seg_ok, fst(Ancestor.(a.children)))
                     && List.for_all(seg_ok, snd(Ancestor.(a.children)))
                     && seg_ok(l)
                     && seg_ok(r)
                   );
              if (!seg_ok(fst(sibs)) || !seg_ok(snd(sibs)) || !anc_ok) {
                bad(
                  ~seed,
                  ~step=k,
                  ~inv="G-EDIT-GROUT",
                  flat(marked(Zipper.unselect_and_zip(z'))),
                );
              };
            };
            switch (completed_of(z')) {
            | exception e =>
              bad(
                ~seed,
                ~step=k,
                ~inv="G-COMPLETE-RAISED",
                Printexc.to_string(e),
              )
            | completed =>
              switch (GroutPlace.place(completed)) {
              | exception e =>
                bad(
                  ~seed,
                  ~step=k,
                  ~inv="G-PLACE-RAISED",
                  Printexc.to_string(e),
                )
              | p1 =>
                if (sx(GroutPlace.place(completed)) != sx(p1)) {
                  bad(
                    ~seed,
                    ~step=k,
                    ~inv="G-DETERMINISM",
                    flat(marked(p1)),
                  );
                };
                if (sx(GroutPlace.place(p1)) != sx(p1)) {
                  bad(
                    ~seed,
                    ~step=k,
                    ~inv="G-IDEMPOTENCE",
                    "once="
                    ++ flat(marked(p1))
                    ++ " twice="
                    ++ flat(marked(GroutPlace.place(p1))),
                  );
                };
                if (sx(GroutPlace.place(GroutPlace.strip(completed)))
                    != sx(p1)) {
                  bad(
                    ~seed,
                    ~step=k,
                    ~inv="G-STRIP-BLIND",
                    flat(marked(p1)),
                  );
                };
                /* layout invisibility: grout contributes nothing */
                if (FeltPrint.render_ghostless(p1)
                    != FeltPrint.render(GroutPlace.strip(completed))) {
                  bad(
                    ~seed,
                    ~step=k,
                    ~inv="G-INVISIBILITY",
                    "ghostless="
                    ++ flat(FeltPrint.render_ghostless(p1))
                    ++ " stripped="
                    ++ flat(FeltPrint.render(GroutPlace.strip(completed))),
                  );
                };
                /* measured-level: width transfer keeps row widths
                   equal to the stripped segment's (LineEndFree rows
                   +1), and no two width-bearing atoms share a cell */
                switch (Test_GroutGeometry.invariants(p1)) {
                | None => ()
                | Some(v) => bad(~seed, ~step=k, ~inv="G-MEASURED", v)
                };
              }
            };
          };
        },
      script,
    );
    /* round-trip at the seed's final state, GATED on the parser
       reproducing the grout-free segment's STRUCTURE: holes serialize
       as nothing, so a hole pinched between two synthesized shards
       (then?else) glues them on reparse, and text-identical forms can
       remold (pattern () reparses as unit, no child slot) — the
       parser-faithfulness family the roundtrip audit tracks, not
       placement's. Above the gate the property is STRONGER than
       G-STRIP-BLIND: the reparsed segment carries entirely fresh ids,
       so agreement means placement positions are a function of the
       id-blind structure alone. Two channels: the raw edit segment
       (parser-built, so it round-trips and keeps the gate honest) and
       the completed segment (synthesized shard runs rarely survive
       reparse, so this channel mostly gates off on this corpus). */
    let roundtrip = (~which: string, seg: Segment.t) =>
      switch (GroutPlace.place(seg)) {
      | exception _ => () /* reported at the step that reached it */
      | a =>
        switch (Parser.to_zipper(~root=Sort.Exp, plain(a))) {
        | None => ()
        | Some(zz) =>
          let reparsed = Zipper.unselect_and_zip(zz);
          if (skeleton(GroutPlace.strip(reparsed))
              == skeleton(GroutPlace.strip(a))) {
            incr(gate_hits);
            let b = GroutPlace.place(reparsed);
            if (marked(a) != marked(b)) {
              bad(
                ~seed,
                ~step=steps,
                ~inv="G-ROUNDTRIP(" ++ which ++ ")",
                "placed="
                ++ flat(marked(a))
                ++ " reparsed="
                ++ flat(marked(b)),
              );
            };
          };
        }
      };
    roundtrip(~which="raw", Zipper.unselect_and_zip(z^));
    switch (completed_of(z^)) {
    | exception _ => ()
    | completed => roundtrip(~which="completed", completed)
    };
  };
  if (gate_hits^ == 0) {
    bad(
      ~seed=0,
      ~step=0,
      ~inv="G-ROUNDTRIP-VACUOUS",
      "no fuzz state passed the reparse gate",
    );
  };
  Stdlib.Buffer.contents(buf);
};

/* PROMISE-RENDER PARITY (stage 1): after every applied fuzz step,
   PromiseRender.mk must render identically to DisplayFork.mk. Runs
   the same corpus as the invariant fuzzer; any per-step disagreement
   (outside the enumerated waivers, of which none surface on this
   corpus) reds the suite. Reported as seed/step/renders. */
let run_parity_fuzz = (~seeds: int, ~steps: int): string => {
  let buf = Stdlib.Buffer.create(256);
  for (seed in 1 to seeds) {
    let script = script_of_seed(seed, steps);
    let z = ref(Zipper.init());
    let aborted = ref(false);
    List.iteri(
      (k0, a) =>
        if (! aborted^) {
          switch (apply(z^, a)) {
          | Rejected => ()
          | Raised(_) => aborted := true
          | Applied(z') =>
            z := z';
            let cur =
              switch (
                Test_CompletionDisplay.display_state_of(
                  ~parts=Test_CompletionDisplay.display_parts,
                  ~chips=false,
                  z',
                )
              ) {
              | s => Some(s)
              | exception _ => None
              };
            let prom =
              switch (
                Test_CompletionDisplay.display_state_of(
                  ~parts=Test_CompletionDisplay.display_parts_promise,
                  ~chips=false,
                  z',
                )
              ) {
              | s => Some(s)
              | exception _ => None
              };
            switch (cur, prom) {
            | (Some(c), Some(p)) when c != p =>
              Stdlib.Buffer.add_string(
                buf,
                Printf.sprintf(
                  "seed=%d step=%d\n  cur=%s\n  prom=%s\n",
                  seed,
                  k0 + 1,
                  flat(c),
                  flat(p),
                ),
              )
            | (Some(_), None) =>
              Stdlib.Buffer.add_string(
                buf,
                Printf.sprintf(
                  "seed=%d step=%d PROMISE-RAISED\n",
                  seed,
                  k0 + 1,
                ),
              )
            | _ => ()
            };
          };
        },
      script,
    );
  };
  Stdlib.Buffer.contents(buf);
};

/* space-separated action DSL matching `label` output, for pinned
   minimized repros: single chars insert; SP space; NL linebreak;
   BK backspace; L/R char moves */
let acts = (spec: string): list(Action.t) =>
  spec
  |> String.split_on_char(' ')
  |> List.filter(s => s != "")
  |> List.map(s =>
       switch (s) {
       | "NL" => Action.Insert("\n")
       | "SP" => Action.Insert(" ")
       | "BK" => bk
       | "L" => mv_l
       | "R" => mv_r
       | s => Action.Insert(s)
       }
     );

/* pinned known-jank: replay a minimized repro, pin the violations its
   last action fires (bisected from fuzz findings via `minimize`). If
   an engine fix lands, the pin diffs — re-run, judge, re-pin. */
let known_case = (name: string, script: string, expected: string) =>
  test_case(name ++ ": " ++ script, `Quick, () =>
    check(
      string_testable,
      name,
      expected,
      String.concat(" | ", violations_of(acts(script))),
    )
  );

/* Formerly an ENGINE CRASH (fuzzer-found): completing the
   case/fun/in interleave merged shards onto the orphan `|`'s
   fallback mold (in_=[]) and Segment.remold_tile's child-sort
   check raised Failure("nth"). Fixed 2026-07-15 (missing old
   inner sort now means "remold the child"); pinned healthy. */
let crash_stage_probe = {
  let acts = Test_Editing.mk("case fun in ¦") @ [Action.Insert("|")];
  let z = Test_Editing.perform(Zipper.init(), acts);
  let stage = ref("maketerm");
  let out =
    switch (
      {
        let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
        stage := "statics";
        let (info_map, _) =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
        stage := "derive";
        let obs = TypeObligations.derive(info_map);
        stage := "fork";
        let fork =
          DisplayFork.mk(~info_map, ~obligations=obs, ~armed=true, z);
        ignore(fork);
        stage := "display_parts";
        let (seg, zc, _marks, _typed_lens, caret_witnesses, _assist, _ghosted) =
          Test_CompletionDisplay.display_parts(z);
        stage := "measured";
        let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
        stage := "caret";
        let caret = DisplayCaret.point(~caret_witnesses, measured, zc);
        stage := "printer";
        let text = FeltPrint.measured_print(~measured, seg);
        stage := "insert";
        let rows =
          Printer.insert_string(
            "|",
            FeltPrint.measured_caret(~measured, seg, caret),
            String.split_on_char('\n', text),
          );
        stage := "done";
        String.concat("/", rows);
      }
    ) {
    | r => r
    | exception (Failure(m)) => "RAISED " ++ m ++ " at " ++ stage^
    | exception _ => "RAISED ? at " ++ stage^
    };
  [
    test_case("crash stage probe", `Quick, () =>
      check(string_testable, "stage", "case fun in ||", out)
    ),
  ];
};

let probe_glom = [
  Alcotest.test_case(
    "PROBE glom display",
    `Quick,
    () => {
      let z = replay(acts("( > L ="));
      let (seg, zc, _marks, typed_lens, caret_witnesses, _, _) =
        Test_CompletionDisplay.display_parts(z);
      let m = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
      print_endline(
        "PROBE-G text: `"
        ++ Printer.of_segment(
             ~holes="?",
             ~concave_holes="~",
             ~indent="",
             ~measured=m,
             seg,
           )
        ++ "`",
      );
      print_endline(
        "PROBE-G caret: "
        ++ Util.Point.show(DisplayCaret.point(~caret_witnesses, m, zc)),
      );
      print_endline(
        "PROBE-G zcaret: " ++ Util.Point.show(Zipper.Caret.point(m, zc)),
      );
      print_endline(
        "PROBE-G witnesses: "
        ++ string_of_int(List.length(caret_witnesses))
        ++ " typed_lens: "
        ++ string_of_int(List.length(typed_lens)),
      );
      List.iter(
        ((pid, (tid, i, n))) =>
          print_endline(
            "PROBE-G cw pid="
            ++ Id.to_string(pid)
            ++ " tid="
            ++ Id.to_string(tid)
            ++ " shard="
            ++ string_of_int(i)
            ++ " len="
            ++ string_of_int(n),
          ),
        caret_witnesses,
      );
      Alcotest.(check(bool))("probe", true, true);
    },
  ),
];

let tests = [
  ("CompletionFuzz: probe-glom", probe_glom),
  ("CompletionFuzz: crash-stage", crash_stage_probe),
  (
    "CompletionFuzz: invariants",
    [
      test_case(
        Printf.sprintf("fuzz %d seeds x %d steps", n_seeds, n_steps),
        `Quick,
        () =>
        check(
          string_testable,
          "no unknown invariant violations",
          "",
          run_fuzz(~seeds=n_seeds, ~steps=n_steps),
        )
      ),
    ],
  ),
  (
    "CompletionFuzz: promise-render parity",
    [
      test_case(
        Printf.sprintf("parity fuzz %d seeds x %d steps", n_seeds, n_steps),
        `Quick,
        () =>
        check(
          string_testable,
          "no promise-render parity diffs",
          "",
          run_parity_fuzz(~seeds=n_seeds, ~steps=n_steps),
        )
      ),
    ],
  ),
  (
    "CompletionFuzz: grout placement",
    [
      test_case(
        Printf.sprintf("grout fuzz %d seeds x %d steps", n_seeds, n_steps),
        `Quick,
        () =>
        check(
          string_testable,
          "no grout-placement invariant violations",
          "",
          run_grout_fuzz(~seeds=n_seeds, ~steps=n_steps),
        )
      ),
    ],
  ),
  (
    "CompletionFuzz: known-violations",
    [
      /* INV 1 NO-CRASH: after `case ? fun ?` the closing `|` insert
         merged shards onto the orphan `|`'s fallback mold and
         remold_tile's child-sort check raised Failure("nth") */
      known_case(
        "keyword display crash",
        "c a s e SP f u n SP |",
        "" /* FIXED: remold_tile tolerates stale single-shard molds */,
      ),
      /* INV 2 PRE-CARET(inner): typed `=` gloms into `=>` (caret goes
         Inner); the display mints a pad left of the glommed token, so
         the rendered caret sits before `=>` while raw is between `=`
         and `>` */
      known_case(
        "inner caret in glommed op",
        "( > L =",
        /* Inner caret beside a consumed cell resolves one col short
           (width-transfer regression, see known_classes) + the
           systemic finish_display re-pad, now with backing pads */
        "PRE-CARET(inner) disp-pre=(? raw-pre=(= "
        ++ "| PAD-IDEMPOTENCE once=(?  =>  ?) twice=(  ?  =>   ?)",
      ),
      /* INV 2 PRE-CARET(inner), string-literal variant: caret inside
         the auto-closed literal, pad minted before the opening quote */
      known_case(
        "inner caret in string literal",
        "c a s e SP n SP \"",
        "PRE-CARET(inner) disp-pre=case n~ raw-pre=case n \\\"" /* REGRESSED under width transfer: same consumed-cell   Inner-caret class as the glommed-op pins */,
      ),
      /* INV 4 PAD-IDEMPOTENCE: second finish_display pass re-pads the
         `= ?` gap its first pass already padded (minted whitespace
         isn't in raw_ids). The trailing SP keeps the promise from
         re-expanding, isolating the pad bug from CONSTANCY */
      known_case(
        "finish_display re-pads",
        "l e t SP",
        "PAD-IDEMPOTENCE once=let ?  = ?  in ? twice=let   ?  =   ?  in ?",
      ),
      /* INV 5 CONSTANCY(post-caret) + INV 4: typing the promised `t`
         of le¦⟪t ⟫ materializes the let form — the promise is swapped
         for the full form's holes/pads (accepted UX per the pinned
         let-blank trajectory, tracked here as a constancy exception) */
      known_case(
        "keyword materialization expands promise",
        "l e t",
        "PAD-IDEMPOTENCE once=let ?  = ?  in ? twice=let   ?  =   ?  in ? "
        ++ "| CONSTANCY(post-caret)",
      ),
      /* INV 2 PRE-CARET(inner), op-glom variant with `|`: raw shows
         the typed `|` before the caret, display shows the pad */
      known_case(
        "inner caret in glommed | op",
        "n , L > n = ( |",
        "PRE-CARET(inner) disp-pre=n>n=(? raw-pre=n>n=(| "
        ++ "| PAD-IDEMPOTENCE once=n>n=(?  |,  ?) twice=n>n=(  ?  |,   ?)",
      ),
    ],
  ),
];
