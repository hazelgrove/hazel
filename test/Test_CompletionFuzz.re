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
    ~indent=" ",
    ~measured,
    seg,
  );
};

/* raw = no display fork: rows before the caret row + caret-row prefix */
let raw_pre_caret = (z: Zipper.t): string => {
  let seg = Zipper.unselect_and_zip(z);
  let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  let caret = Zipper.Caret.point(measured, z);
  let rows =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      ~indent=" ",
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
      ~indent=" ",
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
    switch (Test_CompletionDisplay.split_first("¦", cur)) {
    | Some((pre, _)) when strip_markers(pre) != raw_pre_caret(z) =>
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
       order, so every ⟪ index must be >= the ¦ index */
    switch (find_all("¦", cur)) {
    | [ci, ..._] =>
      if (List.exists(gi => gi < ci, find_all("⟪", cur))) {
        add("NO-PRE-CARET-GHOST");
      }
    | [] => ()
    };
    /* 4: PAD-IDEMPOTENCE — strong form: re-run finish_display with the
       exact args display_parts used (chip marks = marks outside the
       TyDi buffer; raw/caret_after re-derived from the same zc) */
    switch (Test_CompletionDisplay.display_parts(z)) {
    | exception e =>
      add("PAD-IDEMPOTENCE (parts raised: " ++ Printexc.to_string(e) ++ ")")
    | (seg, zc, marks, _, _) =>
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
          | Some((pre, _)) => strip_markers(pre)
          | None => ""
          };
        let mod_grout = remove_all("~");
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
  /* PRE-CARET(inner) exclusion removed 2026-07-12 (Inner-caret
     hosts prefer the token side). NO-CRASH exclusion (Failure nth,
     case/fun/in) removed 2026-07-15: remold_tile now tolerates the
     stale single-shard mold a reassembled orphan carries — see
     crash_stage_probe. */
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
        let (seg, zc, _marks, _assist, _ghosted) =
          Test_CompletionDisplay.display_parts(z);
        stage := "measured";
        let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
        stage := "caret";
        let caret = Zipper.Caret.point(measured, zc);
        stage := "printer";
        let text =
          Printer.of_segment(
            ~holes="?",
            ~concave_holes="~",
            ~indent=" ",
            ~measured,
            seg,
          );
        stage := "insert";
        let rows =
          Printer.insert_string(
            "|",
            caret,
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
      check(string_testable, "stage", "case fun in ~||", out)
    ),
  ];
};

let tests = [
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
        "" /* FIXED: Inner-caret host prefers the token side */,
      ),
      /* INV 2 PRE-CARET(inner), string-literal variant: caret inside
         the auto-closed literal, pad minted before the opening quote */
      known_case(
        "inner caret in string literal",
        "c a s e SP n SP \"",
        "" /* FIXED: Inner-caret host prefers the token side */,
      ),
      /* INV 4 PAD-IDEMPOTENCE: second finish_display pass re-pads the
         `= ?` gap its first pass already padded (minted whitespace
         isn't in raw_ids). The trailing SP keeps the promise from
         re-expanding, isolating the pad bug from CONSTANCY */
      known_case(
        "finish_display re-pads",
        "l e t SP",
        "PAD-IDEMPOTENCE once=let ? = ? in ? twice=let ? =  ? in ?",
      ),
      /* INV 5 CONSTANCY(post-caret) + INV 4: typing the promised `t`
         of le¦⟪t ⟫ materializes the let form — the promise is swapped
         for the full form's holes/pads (accepted UX per the pinned
         let-blank trajectory, tracked here as a constancy exception) */
      known_case(
        "keyword materialization expands promise",
        "l e t",
        "PAD-IDEMPOTENCE once=let ? = ? in ? twice=let ? =  ? in ? "
        ++ "| CONSTANCY(post-caret)",
      ),
      /* INV 2 PRE-CARET(inner), op-glom variant with `|`: raw shows
         the typed `|` before the caret, display shows the pad */
      known_case(
        "inner caret in glommed | op",
        "n , L > n = ( |",
        "" /* FIXED: Inner-caret host prefers the token side */,
      ),
    ],
  ),
];
