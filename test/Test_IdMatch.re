/**
 * Tests and measurements for IdMatch, the structural (AST) diff that carries
 * piece ids across a re-parsed edit.
 *
 * Three things are checked here:
 *
 * 1. HOW MANY IDS SURVIVE. That is the whole objective, so it is asserted as
 *    an exact number on named edits rather than left as a vague "more". Each
 *    case records the count under three policies --- no matching at all, the
 *    isomorphic (GumTree top-down) phase alone, and the shipped policy --- so
 *    that the contribution of each phase is visible and stays visible.
 *
 * 2. THE PROGRAM IS UNCHANGED. Transplanting an id renames a node; it must
 *    not alter a single tile, shape or character. Checked structurally on the
 *    segment and textually end to end.
 *
 * 3. IDS STAY UNIQUE. A duplicate id is the one way this feature could turn
 *    into a correctness bug rather than a missed speedup, so every program
 *    the tests touch is swept for duplicates.
 */
open Alcotest;
open Haz3lcore;

let settings = {
  ...Language.CoreSettings.off,
  statics: true,
};

let seg_of = (code: string): Segment.t =>
  switch (Parser.to_segment(code, ~root=Exp)) {
  | Some(seg) => seg
  | None => failwith("Test_IdMatch: could not parse " ++ code)
  };

let zipper_of = (code: string): Zipper.t =>
  switch (Parser.to_zipper(code, ~root=Exp)) {
  | Some(z) => z
  | None => failwith("Test_IdMatch: could not parse " ++ code)
  };

let perform = (z: Zipper.t, a: Action.t): Zipper.t => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(~settings, ~is_dynamic_term=true, term);
  switch (
    Perform.go(
      ~settings,
      ~statics,
      ~syntax=CachedSyntax.init(z),
      ~root=Sort.Exp,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => z
  | Error(err) => failwith("Test_IdMatch: " ++ Action.Failure.show(err))
  };
};

let program_ids = (z: Zipper.t): Id.Set.t =>
  Id.Set.of_list(IdMatch.ids_seg(Zipper.unselect_and_zip(z)));

let text = (z: Zipper.t): string => Printer.of_zipper(~holes="?", z);

/* The expression ids the evaluator's cache is actually keyed on. Collecting
   these (rather than the segment's piece ids) is what confirms the premise of
   working at the syntax layer at all: MakeTerm derives them from tile ids, so
   preserving a tile id is what makes a cache entry reachable. */
let exp_ids = (z: Zipper.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  ignore(
    Language.Exp.map_term(
      ~f_exp=
        (continue_, e) => {
          acc := Id.Set.add(Language.IdTagged.rep_id(e), acc^);
          continue_(e);
        },
      term,
    ),
  );
  acc^;
};

/* ------------------------------------------------------------ measurement */

/* One segment-level edit, measured under one policy. `preserved` is the
   number of pieces in the replacement that ended up carrying an id from the
   replaced syntax. */
let measure_seg =
    (~policy: IdMatch.Policy.t, old_code: string, new_code: string)
    : IdMatch.stats => {
  let old_seg = seg_of(old_code);
  let new_seg = seg_of(new_code);
  let (out, stats) =
    IdMatch.transplant_with_stats(~policy, ~old_seg, ~new_seg, ());
  /* invariants, asserted on every measurement rather than in a separate case,
     so that no edit can be measured without also being checked */
  check(
    bool,
    "program unchanged: " ++ new_code,
    true,
    IdMatch.same_seg(new_seg, out),
  );
  check(
    list(string),
    "no duplicate ids: " ++ new_code,
    [],
    IdMatch.duplicate_ids(out) |> List.map(Id.to_string),
  );
  let old_ids = Id.Set.of_list(IdMatch.ids_seg(old_seg));
  let fresh_ids = Id.Set.of_list(IdMatch.ids_seg(new_seg));
  check(
    bool,
    "every id is fresh or came from the replaced region: " ++ new_code,
    true,
    List.for_all(
      id => Id.Set.mem(id, fresh_ids) || Id.Set.mem(id, old_ids),
      IdMatch.ids_seg(out),
    ),
  );
  check(bool, "match was accepted: " ++ new_code, false, stats.rejected);
  stats;
};

/* An edit named for the report, with the id counts it is expected to yield
   under each policy. `nodes` is the number of pieces in the replacement, so
   these read as "preserved out of nodes". */
type case = {
  name: string,
  old_code: string,
  new_code: string,
  nodes: int,
  under_none: int,
  under_isomorphic: int,
  under_default: int,
};

/* The measured figures. They are the deliverable, so they are written down
   here and checked, not merely printed. `under_none` is the status quo
   before this pass existed: every piece of the replacement is brand new. */
let cases = [
  {
    name: "tuple component changed",
    old_code: "(21, 22)",
    new_code: "(20, 22)",
    nodes: 5,
    under_none: 0,
    under_isomorphic: 0,
    under_default: 4,
  },
  {
    name: "replacement is textually identical",
    old_code: "(21, 22)",
    new_code: "(21, 22)",
    nodes: 5,
    under_none: 0,
    under_isomorphic: 5,
    under_default: 5,
  },
  {
    name: "one operand of a sum changed",
    old_code: "1 + 2 + 3",
    new_code: "1 + 2 + 4",
    nodes: 9,
    under_none: 0,
    under_isomorphic: 8,
    under_default: 8,
  },
  {
    name: "function body changed",
    old_code: "fun x -> x + 1",
    new_code: "fun x -> x + 2",
    nodes: 10,
    under_none: 0,
    under_isomorphic: 9,
    under_default: 9,
  },
  {
    name: "definition inside a let changed",
    old_code: "let a = 1 in a",
    new_code: "let a = 2 in a",
    nodes: 9,
    under_none: 0,
    under_isomorphic: 2,
    under_default: 8,
  },
  {
    name: "element appended to a list",
    old_code: "[1, 2, 3]",
    new_code: "[1, 2, 3, 4]",
    nodes: 11,
    under_none: 0,
    under_isomorphic: 0,
    under_default: 8,
  },
  {
    name: "nothing in common",
    old_code: "(21, 22)",
    new_code: "\"hello\"",
    nodes: 1,
    under_none: 0,
    under_isomorphic: 0,
    under_default: 0,
  },
  {
    name: "expression wrapped and scaled",
    old_code: "x + 1",
    new_code: "(x + 1) * 2",
    nodes: 10,
    under_none: 0,
    under_isomorphic: 2,
    under_default: 2,
  },
  {
    name: "tuple components swapped",
    old_code: "(a, b)",
    new_code: "(b, a)",
    nodes: 5,
    under_none: 0,
    under_isomorphic: 0,
    under_default: 3,
  },
  {
    name: "binder renamed",
    old_code: "fun x -> x + 1",
    new_code: "fun y -> y + 1",
    nodes: 10,
    under_none: 0,
    under_isomorphic: 5,
    under_default: 8,
  },
  {
    name: "hole filled",
    old_code: "1 + ",
    new_code: "1 + 2",
    nodes: 5,
    under_none: 0,
    under_isomorphic: 4,
    under_default: 4,
  },
  {
    name: "case arm added",
    old_code: "case x\n| 1 => 10\nend",
    new_code: "case x\n| 1 => 10\n| 2 => 20\nend",
    nodes: 18,
    under_none: 0,
    under_isomorphic: 0,
    under_default: 11,
  },
  {
    name: "big literal edit deep in a function",
    old_code: "fun n -> if n < 2 then n else fib(n - 1) + fib(n - 2)",
    new_code: "fun n -> if n < 3 then n else fib(n - 1) + fib(n - 2)",
    nodes: 34,
    under_none: 0,
    under_isomorphic: 23,
    under_default: 33,
  },
  {
    name: "deeply nested leaf changed",
    old_code: "(1, (2, (3, 4)))",
    new_code: "(1, (2, (3, 5)))",
    nodes: 13,
    under_none: 0,
    under_isomorphic: 0,
    under_default: 12,
  },
];

let case_tests =
  List.concat_map(
    c =>
      [
        test_case(
          c.name ++ " [no matching]",
          `Quick,
          () => {
            let stats =
              measure_seg(
                ~policy=IdMatch.Policy.none,
                c.old_code,
                c.new_code,
              );
            check(int, "nodes in replacement", c.nodes, stats.new_nodes);
            check(int, "ids preserved", c.under_none, stats.preserved);
          },
        ),
        test_case(
          c.name ++ " [isomorphic phase only]",
          `Quick,
          () => {
            let stats =
              measure_seg(
                ~policy=IdMatch.Policy.isomorphic_only,
                c.old_code,
                c.new_code,
              );
            check(int, "ids preserved", c.under_isomorphic, stats.preserved);
          },
        ),
        test_case(
          c.name ++ " [shipped policy]",
          `Quick,
          () => {
            let stats =
              measure_seg(
                ~policy=IdMatch.Policy.default,
                c.old_code,
                c.new_code,
              );
            check(int, "ids preserved", c.under_default, stats.preserved);
          },
        ),
      ],
    cases,
  );

/* ---------------------------------------------------------- end to end */

/* Replay a real Structural action against a live zipper under a given policy
   and report (ids in the new program that also existed in the old, ids in the
   new program, program text). Going through Perform rather than calling
   IdMatch directly is the point: it is the path an agent edit actually
   takes. */
let replay =
    (~policy: IdMatch.Policy.t, program: string, actions: list(Action.t))
    : (int, int, string) => {
  let saved = IdMatch.Policy.current^;
  IdMatch.Policy.current := policy;
  let result =
    switch (
      {
        let z0 = zipper_of(program);
        let before = program_ids(z0);
        let z1 = List.fold_left(perform, z0, actions);
        let after = program_ids(z1);
        check(
          list(string),
          "no duplicate ids in the edited program",
          [],
          IdMatch.duplicate_ids(Zipper.unselect_and_zip(z1))
          |> List.map(Id.to_string),
        );
        (
          Id.Set.cardinal(Id.Set.inter(before, after)),
          Id.Set.cardinal(after),
          text(z1),
        );
      }
    ) {
    | r => r
    | exception e =>
      IdMatch.Policy.current := saved;
      raise(e);
    };
  IdMatch.Policy.current := saved;
  result;
};

let tuple_program = "let z = (21, 22) in\nlet (p, q) = z in\np + q";
let tuple_edit = [
  Action.Structural(Action.Structural.Update(Definition, "z", "(20, 22)")),
];

let exp_level_survivors = (~policy: IdMatch.Policy.t): (int, int) => {
  let saved = IdMatch.Policy.current^;
  IdMatch.Policy.current := policy;
  let z0 = zipper_of(tuple_program);
  let before = exp_ids(z0);
  let z1 = List.fold_left(perform, z0, tuple_edit);
  let after = exp_ids(z1);
  IdMatch.Policy.current := saved;
  (Id.Set.cardinal(Id.Set.inter(before, after)), Id.Set.cardinal(after));
};

let end_to_end = [
  test_case(
    "expression ids (what the cache is keyed on) survive the edit",
    `Quick,
    () => {
      let (kept_off, total_off) =
        exp_level_survivors(~policy=IdMatch.Policy.none);
      let (kept_on, total_on) =
        exp_level_survivors(~policy=IdMatch.Policy.default);
      check(int, "expressions in the edited program", 10, total_on);
      check(int, "same number of expressions", total_off, total_on);
      /* Without matching only the 6 expressions outside the replaced
         definition survive. With it, the tuple, the parentheses and the
         untouched `22` survive too; only the changed literal is new. */
      check(int, "expression ids surviving without matching", 6, kept_off);
      check(int, "expression ids surviving with matching", 9, kept_on);
    },
  ),
  test_case(
    "Update Definition (21, 22) -> (20, 22): ids survive",
    `Quick,
    () => {
      let (kept_off, total_off, text_off) =
        replay(~policy=IdMatch.Policy.none, tuple_program, tuple_edit);
      let (kept_on, total_on, text_on) =
        replay(~policy=IdMatch.Policy.default, tuple_program, tuple_edit);
      /* The edit must produce the same program either way: id assignment is a
         name, not content. */
      check(string, "program text is policy-independent", text_off, text_on);
      check(int, "program size is policy-independent", total_off, total_on);
      /* The numbers. Without matching, only the syntax OUTSIDE the replaced
         definition survives; with it, the unchanged parts of the definition do
         too. */
      check(int, "ids surviving without matching", 22, kept_off);
      check(int, "ids surviving with matching", 26, kept_on);
    },
  ),
  test_case(
    "repeated edits keep preserving ids",
    `Quick,
    () => {
      let (kept, total, _) =
        replay(
          ~policy=IdMatch.Policy.default,
          tuple_program,
          [
            Action.Structural(
              Action.Structural.Update(Definition, "z", "(20, 22)"),
            ),
            Action.Structural(
              Action.Structural.Update(Definition, "z", "(19, 22)"),
            ),
            Action.Structural(
              Action.Structural.Update(Definition, "z", "(18, 22)"),
            ),
          ],
        );
      /* Each step re-diffs against the previous step's syntax, so `22` and the
         tuple scaffolding carry their original ids through all three. */
      check(int, "ids surviving three successive edits", 26, kept);
      check(int, "program size unchanged", 30, total);
    },
  ),
  test_case(
    "body update preserves the untouched operand",
    `Quick,
    () => {
      let program = "let a = 1 in\nlet b = 2 in\na + b";
      let edit = [
        Action.Structural(Action.Structural.Update(Body, "b", "a * b")),
      ];
      let (kept_off, _, text_off) =
        replay(~policy=IdMatch.Policy.none, program, edit);
      let (kept_on, _, text_on) =
        replay(~policy=IdMatch.Policy.default, program, edit);
      check(string, "program text is policy-independent", text_off, text_on);
      check(
        bool,
        "matching preserves strictly more",
        true,
        kept_on > kept_off,
      );
    },
  ),
];

/* A sweep over assorted programs and Structural edits. The point is not the
   id counts --- it is that the edit lands on exactly the same program with or
   without the pass, and that no program anywhere in the sweep ends up with a
   duplicate id. Every case here also runs the duplicate check inside `replay`. */
let sweep: list((string, string, list(Action.t))) = [
  (
    "literal inside a tuple",
    "let z = (21, 22) in\nlet (p, q) = z in\np + q",
    [
      Action.Structural(
        Action.Structural.Update(Definition, "z", "(20, 22)"),
      ),
    ],
  ),
  (
    "tuple grows an element",
    "let z = (1, 2) in\nz",
    [
      Action.Structural(
        Action.Structural.Update(Definition, "z", "(1, 2, 3)"),
      ),
    ],
  ),
  (
    "definition replaced by something unrelated",
    "let a = 1 + 2 in\na",
    [
      Action.Structural(
        Action.Structural.Update(Definition, "a", "\"unrelated\""),
      ),
    ],
  ),
  (
    "definition becomes a function",
    "let f = 1 in\nf",
    [
      Action.Structural(
        Action.Structural.Update(Definition, "f", "fun n -> n + 1"),
      ),
    ],
  ),
  (
    "pattern renamed",
    "let a = 1 in\na + 1",
    [Action.Structural(Action.Structural.Update(Pattern, "a", "b"))],
  ),
  (
    "body rewritten",
    "let a = 1 in\nlet b = 2 in\na + b",
    [Action.Structural(Action.Structural.Update(Body, "b", "a * b + 1"))],
  ),
  (
    "nested let in a definition",
    "let outer = (let inner = 1 in inner + 1) in\nouter",
    [
      Action.Structural(
        Action.Structural.Update(
          Definition,
          "outer",
          "(let inner = 2 in inner + 1)",
        ),
      ),
    ],
  ),
  (
    "case gains an arm",
    "let f = fun x -> case x\n| 1 => 10\nend in\nf(1)",
    [
      Action.Structural(
        Action.Structural.Update(
          Definition,
          "f",
          "fun x -> case x\n| 1 => 10\n| 2 => 20\nend",
        ),
      ),
    ],
  ),
  (
    "recursive function body edited",
    "let fib : Int -> Int =\n  fun n -> if n < 2 then n else fib(n - 1) + fib(n - 2)\nin\nfib(5)",
    [
      Action.Structural(
        Action.Structural.Update(
          Definition,
          "fib",
          "fun n -> if n < 3 then n else fib(n - 1) + fib(n - 2)",
        ),
      ),
    ],
  ),
];

let sweep_tests =
  List.map(
    ((name, program, edit)) =>
      test_case(
        "sweep: " ++ name,
        `Quick,
        () => {
          let (kept_off, total_off, text_off) =
            replay(~policy=IdMatch.Policy.none, program, edit);
          let (kept_on, total_on, text_on) =
            replay(~policy=IdMatch.Policy.default, program, edit);
          check(string, "same program text", text_off, text_on);
          check(int, "same number of nodes", total_off, total_on);
          check(
            bool,
            "matching never preserves fewer ids",
            true,
            kept_on >= kept_off,
          );
        },
      ),
    sweep,
  );

let tests = [
  ("IdMatch.segment", case_tests),
  ("IdMatch.end_to_end", end_to_end),
  ("IdMatch.sweep", sweep_tests),
];
