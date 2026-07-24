open Alcotest;
open Haz3lcore;

/* INLINE-PERSIST trial pins (obligation-display design: the
   display_inline knob turned up for forced obligations). With the
   flag on, delimiter-closer and T1 scaffolding ghosts stay INLINE at
   their true positions when the caret leaves; TyDi/witnesses stay
   caret-local; a span at-or-before the caret in reading order
   demotes to chips exactly as with the flag off (the legality
   scope: nothing may displace the caret).

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
        "let a = 1\n¦⟪in⟫ x   CHIPS[]\n---\n"
        ++ "let a = 1¦ ⟪in⟫\nx   CHIPS[]\n---\n"
        ++ "let a = ¦1 ⟪in⟫\nx   CHIPS[]",
        got,
      );
    },
  ),
];

let tests = [
  ("InlinePersist: movement", movement),
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
        "let a = 1⟪in⟫\nx¦   CHIPS[]",
      ),
      /* owed closer persists at its site across lines */
      t(
        "ap closer persists across lines",
        "f(1\nx¦",
        "f(1~\nx¦   CHIPS[)]",
        "f(1⟪)⟫~\nx¦   CHIPS[]",
      ),
      /* the caret-zone ghost is IDENTICAL under both flags: persist
         adds remote spans, never disturbs the at-caret display */
      t(
        "caret-zone ghost unchanged by persist",
        "let a = 1\n¦x",
        "let a = 1\n¦⟪in⟫ x   CHIPS[]",
        "let a = 1\n¦⟪in⟫ x   CHIPS[]",
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
