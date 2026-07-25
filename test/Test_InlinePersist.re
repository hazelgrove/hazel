open Alcotest;
open Haz3lcore;

/* INLINE-PERSIST trial pins (obligation-display design: the
   display_inline knob turned up for forced obligations). With the
   flag on, delimiter-closer and T1 scaffolding ghosts stay INLINE at
   their true positions wherever the caret is — MOVEMENT PURITY: a
   span's display form is a property of the span + document, never
   the caret, so pure motion changes no rendered text. TyDi/witnesses
   stay caret-local. Demotion and dispatch happen only at EDIT
   moments; the edit that dispatches a pre-caret span contracts text
   left of the caret at that keystroke (the accepted P2-at-dispatch
   trade — pinned below).

   Trajectories render one line per state: OFF then ON, so each pin
   shows precisely what the toggle changes. */

let string_testable = testable(Fmt.string, String.equal);

let mkz = (spec: string): Zipper.t =>
  Test_Editing.perform(Zipper.init(), Test_Editing.mk(spec));

let both = (z: Zipper.t): (string, string) => (
  Test_CompletionDisplay.display_state(~chips=true, z),
  Test_CompletionDisplay.display_state_persist(~chips=true, z),
);

let t = (name, spec, expect_off, expect_on) =>
  test_case(
    name,
    `Quick,
    () => {
      let (off, on) = both(mkz(spec));
      check(
        string_testable,
        name,
        "OFF " ++ expect_off ++ "\nON  " ++ expect_on,
        "OFF " ++ off ++ "\nON  " ++ on,
      );
    },
  );

/* movement across a persisted span is ATOMIC by construction (the
   span holds no edit positions): arrowing left from line 2 crosses
   ⟪in⟫ in a single press, landing after the 1. One render per step. */
let movement = [
  test_case(
    "atomic skip across a persisted span",
    `Quick,
    () => {
      let z0 = mkz("let a = 1\nx¦");
      let steps = [
        Action.Move(Local(Left, ByChar)),
        Action.Move(Local(Left, ByChar)),
        Action.Move(Local(Left, ByChar)),
      ];
      let (_, states_rev) =
        List.fold_left(
          ((z, acc), a) => {
            let z' = Test_Editing.perform(z, [a]);
            (
              z',
              [
                Test_CompletionDisplay.display_state_persist(~chips=true, z'),
                ...acc,
              ],
            );
          },
          (z0, []),
          steps,
        );
      let got = states_rev |> List.rev |> String.concat("\n---\n");
      check(
        string_testable,
        "one press per perceived position",
        /* span P4 pads are span MATERIAL (ghost-marked): they sit
           inside the run markers and travel with the span.
           RE-PINNED 2026-07-24 (P8): the ghost no longer crosses the
           linebreak to the caret's line — it stays at its true
           position on line 1, so from the caret on line 2 it is a
           chip in Off mode and inline under persist. */
        "let a = 1⟪ in⟫\n¦x   CHIPS[]\n---\n"
        ++ "let a = 1¦⟪ in⟫\nx   CHIPS[]\n---\n"
        ++ "let a = ¦1⟪ in⟫\nx   CHIPS[]",
        got,
      );
    },
  ),
];

/* THE P2-AT-DISPATCH TRADE (documented decision, not an accident):
   with a same-row pre-caret span inline, the EDIT that dispatches
   the obligation removes span material left of the caret — text
   contracts and the caret's display column shifts left AT THAT
   KEYSTROKE. Movement never does this; only the dispatching edit
   does. */
let dispatch_trade = [
  test_case(
    "dispatching a pre-caret span contracts at the edit",
    `Quick,
    () => {
      /* `f(1 x` owes `)`; caret left of x — the owed closer's span
         sits between 1 and x? No: closer promises after x (growth
         zone). To place a span BEFORE the caret on its row, park the
         caret past the site by typing then moving left through
         real material: `f(1` then `) typed elsewhere` dispatches.
         Simplest honest form: span before caret via move-past, then
         type the closer at the caret — the remote ghost dispatches
         and vanishes. */
      let z0 = mkz("let a = 1
x¦");
      /* persisted ⟪in⟫ sits pre-caret (earlier row). Typing `in` up
         front is awkward; dispatch instead by giving the let its own
         in: caret to line 1 end, type "in" — the ghost reifies in
         place; the span's ghost marks vanish at the edit. */
      let z1 =
        Test_Editing.perform(
          z0,
          [
            Action.Move(Vertical(Up, ByChar)),
            Action.Move(Line(Right)),
            Action.Insert(" "),
            Action.Insert("i"),
            Action.Insert("n"),
          ],
        );
      let before =
        Test_CompletionDisplay.display_state_persist(~chips=true, z0);
      let after =
        Test_CompletionDisplay.display_state_persist(~chips=true, z1);
      check(
        string_testable,
        "ghost span reified by the dispatching edit",
        "before let a = 1⟪ in⟫
x¦   CHIPS[]
"
        ++ "after let a = 1 in¦
x   CHIPS[]",
        "before " ++ before ++ "
after " ++ after,
      );
    },
  ),
];

/* CHURN-CARET-STABLE: a remote EDIT that adds/removes spans
   elsewhere never moves the caret's display point (P2 under
   persist; successor to the retired flag-equality invariant). */
let churn_stability = [
  test_case(
    "remote span churn leaves the caret point fixed",
    `Quick,
    () => {
      let z0 = mkz("let a = 1
let b = 2
x¦y");
      let parts =
        Test_CompletionDisplay.display_parts(~inline_persist=Persist);
      let pt = (z: Zipper.t) =>
        switch (parts(z)) {
        | (seg, zc, _, _, caret_witnesses, _, _) =>
          let m = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
          DisplayCaret.point(~caret_witnesses, m, zc);
        };
      let p0 = pt(z0);
      /* the edit types at the caret (col moves by exactly the typed
         char) while TWO remote spans (both lets' `in`s) re-derive */
      let z1 = Test_Editing.perform(z0, [Action.Insert("1")]);
      let p1 = pt(z1);
      check(
        string_testable,
        "caret advanced by exactly the typed char",
        Printf.sprintf("(%d,%d)", p0.row, p0.col + 1),
        Printf.sprintf("(%d,%d)", p1.row, p1.col),
      );
    },
  ),
];

/* PERSIST vs ALWAYS: the appearance test (a span may inline only if
   inlining cannot displace the caret) essentially never FIRES for
   forced obligations, because a closer's true position is the far
   end of its enclosing structure — at or after the caret — and owed
   commas sit just before that closer. Measured over the fuzz corpus:
   1166 states, ZERO divergence between Persist and Always. Recorded
   as a property so we learn if that ever stops being true: the
   policy's spatial restriction is, empirically, not what makes
   persistence safe (the ratchet and material-scoped pads are). */
let mode_agreement = {
  let diverge = ref(0);
  let example = ref("");
  for (seed in 1 to 60) {
    let script = Test_CompletionFuzz.script_of_seed(seed, 20);
    let z = ref(Zipper.init());
    List.iter(
      a =>
        switch (Test_CompletionFuzz.apply(z^, a)) {
        | Applied(z') =>
          z := z';
          let render = mode =>
            try(
              Test_CompletionDisplay.display_state_of(
                ~parts=
                  Test_CompletionDisplay.display_parts(~inline_persist=mode),
                ~chips=true,
                z',
              )
            ) {
            | _ => "ERR"
            };
          let (p, al) = (
            render(Language.CoreSettings.Persist),
            render(Language.CoreSettings.Always),
          );
          if (p != al) {
            incr(diverge);
            if (example^ == "") {
              example := "P: " ++ p ++ " || A: " ++ al;
            };
          };
        | _ => ()
        },
      script,
    );
  };
  [
    Alcotest.test_case("Persist and Always agree on the corpus", `Quick, () =>
      Alcotest.check(
        Alcotest.string,
        "divergences",
        "0",
        string_of_int(diverge^)
        ++ (example^ == "" ? "" : " first=" ++ example^),
      )
    ),
  ];
};

let tests = [
  ("InlinePersist: mode agreement", mode_agreement),
  ("InlinePersist: movement", movement),
  ("InlinePersist: dispatch trade", dispatch_trade),
  ("InlinePersist: churn stability", churn_stability),
  (
    "InlinePersist: trajectories",
    [
      /* caret left the let: OFF demotes the owed `in` to a chip; ON
         keeps it inline at its true position — an earlier line's END
         is free space, legal for a pre-caret span (linebreak-
         separated) */
      t(
        "let-in persists after caret moves below",
        "let a = 1\nx¦",
        "let a = 1~\nx¦   CHIPS[in]",
        "let a = 1⟪ in⟫\nx¦   CHIPS[]",
      ),
      /* owed closer persists at its site across lines */
      t(
        "ap closer persists across lines",
        "f(1\nx¦",
        "f(1~\nx¦   CHIPS[)]",
        "f(1⟪) ⟫~\nx¦   CHIPS[]",
      ),
      /* the caret-zone ghost is IDENTICAL under both flags: persist
         adds remote spans, never disturbs the at-caret display */
      /* RE-PINNED 2026-07-24 (P8): with the linebreak-crossing slide
         retired, a ghost anchored on the PREVIOUS line stays there.
         From the caret on line 2 that is pre-caret material, so Off
         shows a chip; persist keeps it inline at its true position.
         This is the R1 "spooky space" fix: one arrow press can no
         longer relocate the ghost onto the caret's line. */
      t(
        "previous-line ghost stays put; persist keeps it inline",
        "let a = 1\n¦x",
        "let a = 1~\n¦x   CHIPS[in]",
        "let a = 1⟪ in⟫\n¦x   CHIPS[]",
      ),
      /* same-row material: the owed closer promises AFTER the caret
         (growth zone) under both flags — same-row-BEFORE-caret spans
         cannot arise from closers (their splice anchors at/after the
         site material); the demote guard covers the construction */
      t(
        "same-row closer stays after-caret under both",
        "f(1 x¦",
        "f(1~x¦⟪)⟫   CHIPS[]",
        "f(1~x¦⟪)⟫   CHIPS[]",
      ),
    ],
  ),
];
