open Alcotest;
open Language;
open Util;
open Haz3lcore;
open Poly;

module Rows = Web.StepperRows;
module Picker = Web.StepperPicker;
module Stepper = Web.StepperView;

let parse_exp = Test_Evaluator_Prelude.parse_exp;
let print = Test_ProofMap.print_exp;

let settings = CoreSettings.on;

/* A cell evaluated and checked as the editor does it. */
module Fixture = {
  type t = {
    statics: CachedStatics.t,
    cell: Web.EvalResult.Model.t,
    id: Id.t,
    theorem: Web.Theorems.Model.theorem,
    rows: list(Rows.t),
  };

  let statics_of_term = (term: Exp.t): CachedStatics.t =>
    CachedStatics.init_from_term(~settings, ~is_dynamic_term=false, term);

  let statics = (src: string): CachedStatics.t =>
    statics_of_term(parse_exp(src));

  /* Evaluates on the main thread, or, with `pending`, sends the request to
   * a worker that never answers. */
  let calculate = (~pending=false, ~is_edited, statics, cell) =>
    Web.EvalResult.Update.calculate(
      ~settings,
      ~queue_worker=pending ? Some(_ => ()) : None,
      ~is_edited,
      statics,
      cell,
    );

  let of_cell = (~name, statics, cell: Web.EvalResult.Model.t): t => {
    let theorems = cell.theorems;
    switch (
      Web.Theorems.Model.theorems(theorems)
      |> List.find(~f=((_, thm: Web.Theorems.Model.theorem)) =>
           thm.name == name
         )
    ) {
    | Some((id, theorem)) => {
        statics,
        cell,
        id,
        theorem,
        rows: Rows.of_theorem(~proof_map=theorems.proof_map, theorem.proof),
      }
    | None => Alcotest.fail("no theorem " ++ name)
    };
  };

  let mk = (~name="t", src: string): t => {
    let statics = statics(src);
    of_cell(
      ~name,
      statics,
      calculate(~is_edited=true, statics, Web.EvalResult.Model.init),
    );
  };

  /* Sends a stepper action through the cell, and recalculates, as the
   * page does after an action. */
  let act = (f: t, action: Stepper.Update.t): t => {
    let updated =
      Web.EvalResult.Update.update(
        ~settings=Web.Settings.Model.init,
        TheoremsAction(TheoremUpdate(f.id, action)),
        f.cell,
      );
    of_cell(
      ~name=f.theorem.name,
      f.statics,
      calculate(~is_edited=false, f.statics, updated.model),
    );
  };

  let all = (f: t) => Rows.all(f.rows);

  let local = (f: t, row: Rows.t): Stepper.Model.local =>
    switch (Stepper.Model.find(row.key, f.theorem.stepper)) {
    | Some(local) => local
    | None => Alcotest.fail("the row has no state")
    };

  let goal = (f: t, row: Rows.t): Picker.Goal.t => local(f, row).goal;
};

let goal = (row: Rows.t): string =>
  switch (row.goal) {
  | Some(g) => print(g)
  | None => Alcotest.fail("the row has no goal")
  };

let kind = (row: Rows.t): string =>
  switch (row.kind) {
  | Hole => "hole"
  | End => "end"
  | Step(_) => Web.StepperView.View.justification(row.step)
  };

let kinds = (rows: list(Rows.t)) => List.map(~f=kind, rows);

let body = (row: Rows.t): list(Rows.t) =>
  switch (row.kind) {
  | Step(Body(rows)) => rows
  | _ => Alcotest.fail("not a form with a body: " ++ kind(row))
  };

let premises = (row: Rows.t): (list(Rows.t), list(Rows.t)) =>
  switch (row.kind) {
  | Step(Premises({sub, body})) => (sub, body)
  | _ => Alcotest.fail("not a have: " ++ kind(row))
  };

let cases = (row: Rows.t): list(list(Rows.t)) =>
  switch (row.kind) {
  | Step(Cases(cases)) => List.map(~f=snd, cases)
  | _ => Alcotest.fail("not an induction: " ++ kind(row))
  };

let first = (rows: list(Rows.t)): Rows.t =>
  switch (rows) {
  | [row, ..._] => row
  | [] => Alcotest.fail("no rows")
  };

let last = (rows: list(Rows.t)): Rows.t =>
  switch (ListUtil.last_opt(rows)) {
  | Some(row) => row
  | None => Alcotest.fail("no rows")
  };

/* The facts in scope at a row, by name. */
let facts = (row: Rows.t): list((string, string)) =>
  switch (Rows.ctx(row)) {
  | Some(ctx) =>
    SemanticCtx.facts(ctx)
    |> List.map(~f=((name, _, e)) => (name, print(e)))
  | None => Alcotest.fail("the row has no scope")
  };

/* The offered eval steps' displayed nodes, printed. */
let offered = (goal: Picker.Goal.t): list(string) =>
  Picker.Goal.step_ids(goal)
  |> List.filter_map(~f=id => Exp.find_by_id(id, goal.written))
  |> List.map(~f=print);

let written = (p: Proof.t): string =>
  Proof(p)
  |> ExpToSegment.any_to_segment(
       ~settings=ExpToSegment.Settings.editable(~inline=true),
     )
  |> Printer.of_segment(~holes="?", ~refractors=[]);

/* Every tile id in a segment, children and folded projector interiors
 * included. A repeated id makes `Highlight.of_tile` raise a shard
 * mismatch. */
let rec segment_tile_ids = (seg: Segment.t): list(Id.t) =>
  List.concat_map(
    ~f=
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) => [
            Piece.id(p),
            ...segment_tile_ids(List.concat(t.children)),
          ]
        | Projector(pr) => segment_tile_ids([pr.syntax])
        | Grout(_)
        | Secondary(_) => []
        },
    seg,
  );

let duplicated = (ids: list(Id.t)): list(Id.t) => {
  let seen = Stdlib.Hashtbl.create(64);
  List.filter(
    ~f=
      id =>
        switch (Stdlib.Hashtbl.find_opt(seen, id)) {
        | Some () => true
        | None =>
          Stdlib.Hashtbl.add(seen, id, ());
          false;
        },
    ids,
  );
};

/* Which rows a proof becomes. */
module Walk = {
  let tests = [
    test_case(
      "a hole proof is one hole row",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 1 + 2 == 3 proof ? in t");
        check(list(string), "rows", ["hole"], kinds(f.rows));
        check(
          string,
          "its goal is the statement",
          "1 + 2 == 3",
          goal(first(f.rows)),
        );
      },
    ),
    test_case(
      "a chain ending in a step gets an end row with the step's outgoing goal",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 + 4 == 5 proof from 1 + 4 do step end in t",
          );
        check(list(string), "rows", ["step", "end"], kinds(f.rows));
        let (step, end_) = (first(f.rows), last(f.rows));
        check(
          bool,
          "the end row is keyed by the step, on the outgoing side",
          true,
          end_.key == (Proof.rep_id(step.step), Outgoing),
        );
        check(string, "its goal", "5 == 5", goal(end_));
        check(
          list(string),
          "it offers the next redex",
          ["5 == 5"],
          offered(Fixture.goal(f, end_)),
        );
      },
    ),
    test_case(
      "a chain ending in a hole gets no end row",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 + 4 == 5 proof from 1 + 4 do step end; ? in t",
          );
        check(list(string), "rows", ["step", "hole"], kinds(f.rows));
        check(string, "the hole's goal", "5 == 5", goal(last(f.rows)));
      },
    ),
    test_case(
      "a hole mid-chain passes its goal on",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 + 4 == 5 proof ?; from 1 + 4 do step end in t",
          );
        check(
          list(string),
          "rows",
          ["hole", "step", "end"],
          kinds(f.rows),
        );
        check(
          string,
          "the step's goal",
          "1 + 4 == 5",
          goal(List.nth_exn(f.rows, 1)),
        );
      },
    ),
    test_case(
      "a step inserted before another heads the chain before the reparse",
      `Quick,
      () => {
        let step = s => Proof.fresh(AlgebriteStep(parse_exp(s)));
        let (a, b, c) = (step("1"), step("2"), step("3"));
        let (head, tail) =
          Rows.split(Proof.fresh(Seq(Proof.fresh(Seq(a, b)), c)));
        check(
          bool,
          "the head is the inserted step",
          true,
          phys_equal(head, a),
        );
        switch (tail) {
        | Some({term: Seq(b', c'), _}) =>
          check(
            bool,
            "then the old chain",
            true,
            phys_equal(b', b) && phys_equal(c', c),
          )
        | _ => Alcotest.fail("the tail is not the rest of the chain")
        };
      },
    ),
    test_case(
      "an axiom row is justified by the cited equality",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 == 1 proof from 1 == 1 do rewrite refl end end in t",
          );
        check(list(string), "rows", ["refl", "end"], kinds(f.rows));
        check(string, "it proves the goal", "true", goal(last(f.rows)));
      },
    ),
    test_case(
      "a cases row nests each case's rows",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall (b : Bool) -> b || false == b proof cases b | true => ? | false => ? end in t",
          );
        let cases = cases(first(f.rows));
        check(int, "two cases", 2, List.length(cases));
        check(
          list(string),
          "each case's goal has its pattern for the scrutinee",
          ["true || false == true", "false || false == false"],
          List.map(~f=rows => goal(first(rows)), cases),
        );
        check(
          bool,
          "each case's scope has its case equation",
          true,
          List.for_all(
            ~f=
              rows =>
                List.Assoc.mem(
                  facts(first(rows)),
                  "case_eq",
                  ~equal=String.equal,
                ),
            cases,
          ),
        );
      },
    ),
    test_case(
      "an induction case's scope has its inductive hypotheses and no case equation",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall (xs : [Int]) -> xs == xs proof induction xs | [] => ? | y :: rest => ? end in t",
          );
        switch (cases(first(f.rows))) {
        | [nil, cons] =>
          check(
            list(string),
            "the cons case's new facts",
            ["ih_rest"],
            List.map(~f=fst, facts(first(cons)))
            |> List.filter(~f=name =>
                 !
                   List.Assoc.mem(
                     facts(first(nil)),
                     name,
                     ~equal=String.equal,
                   )
               ),
          );
          check(
            bool,
            "no case has a case equation",
            false,
            List.Assoc.mem(
              facts(first(nil)),
              "case_eq",
              ~equal=String.equal,
            )
            || List.Assoc.mem(
                 facts(first(cons)),
                 "case_eq",
                 ~equal=String.equal,
               ),
          );
        | _ => Alcotest.fail("expected two cases")
        };
      },
    ),
    test_case(
      "a step that doesn't apply is broken, and offers its repairs",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall x: Int -> x == 1 ==> 1 + 1 == 2 proof assume x == 1 => from 1 do rewrite assume end end in t",
          );
        let row = first(body(first(f.rows)));
        switch (Rows.Status.of_row(row)) {
        | Broken(RuleDoesNotApply(_)) => ()
        | status =>
          Alcotest.fail(
            "status: "
            ++ (
              Rows.Status.broken_mark(status)
              |> Option.value_map(~default="not broken", ~f=ProofMark.show)
            ),
          )
        };
        let flips = (f: Fixture.t, row: Rows.t) =>
          Picker.Patches.flip_direction(
            ~info_map=f.statics.info_map,
            ~proof_map=f.cell.theorems.proof_map,
            row.step,
          )
          != None;
        check(bool, "it can be flipped", true, flips(f, row));
        let f =
          Fixture.mk(
            "theorem t = 1 + 4 == 5 proof from 1 + 4 == 5 do rewrite refl end end in t",
          );
        check(
          bool,
          "reflexivity, the same both ways, can't",
          false,
          flips(f, first(f.rows)),
        );
      },
    ),
    test_case(
      "an induction offers another case only while its cases don't cover its scrutinee",
      `Quick,
      () => {
        let adds_case = src => {
          let f = Fixture.mk(src);
          let row = first(f.rows);
          Picker.Patches.add_case(
            row.step,
            ~marks=
              Option.value_map(row.entry, ~default=[], ~f=(e: ProofMap.entry) =>
                e.marks
              ),
          )
          != None;
        };
        check(
          bool,
          "one case of two",
          true,
          adds_case(
            "theorem t = forall b: Bool -> b || !b proof induction b | true => ? end in t",
          ),
        );
        check(
          bool,
          "both",
          false,
          adds_case(
            "theorem t = forall b: Bool -> b || !b proof induction b | true => ? | false => ? end in t",
          ),
        );
      },
    ),
  ];
};

/* A form's body, and a have's premises, are chains of their own, with
 * the goal and scope the checker gave them. */
module Scopes = {
  let assume_src = {|theorem t = forall x -> x == 1 ==> x == 1 proof assume x == 1 => from x do rewrite assume end end; from 1 == 1 do rewrite refl end end in t|};
  let have_src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};

  let tests = [
    test_case(
      "assume: the body renders as nested rows",
      `Quick,
      () => {
        let f = Fixture.mk(assume_src);
        check(
          list(string),
          "outer rows",
          ["assume", "end"],
          kinds(f.rows),
        );
        check(
          list(string),
          "body rows",
          ["assume", "refl", "end"],
          kinds(body(first(f.rows))),
        );
      },
    ),
    test_case(
      "assume: the body's scope has the `assume` hypothesis",
      `Quick,
      () => {
        let f = Fixture.mk(assume_src);
        check(
          option(string),
          "the hypothesis",
          Some("x == 1"),
          List.Assoc.find(
            facts(first(body(first(f.rows)))),
            "assume",
            ~equal=String.equal,
          ),
        );
        check(
          bool,
          "and the assume itself is checked without it",
          false,
          List.Assoc.mem(
            facts(first(f.rows)),
            "assume",
            ~equal=String.equal,
          ),
        );
      },
    ),
    test_case(
      "assume: implication intro strips the antecedent from the body's goal",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall x -> x == 2 ==> x + 1 == 3 proof assume x == 2 => ? in t",
          );
        check(
          string,
          "the body's goal",
          "x + 1 == 3",
          goal(first(body(first(f.rows)))),
        );
      },
    ),
    test_case(
      "revert: the body's goal is the fact implying the old goal",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall x -> x == 1 ==> x == 1 proof assume x == 1 => revert x == 1 => ? in t",
          );
        let revert = first(body(first(f.rows)));
        check(string, "the row", "revert", kind(revert));
        let hole = first(body(revert));
        check(string, "the body's goal", "x == 1 ==> x == 1", goal(hole));
        check(
          bool,
          "the reverted fact is still citable",
          true,
          List.Assoc.mem(facts(hole), "assume", ~equal=String.equal),
        );
      },
    ),
    test_case(
      "generalize: the body's goal is re-quantified, without facts about the variable",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall x -> x == 1 ==> x == x proof assume x == 1 => generalize x => ? in t",
          );
        let generalize = first(body(first(f.rows)));
        check(
          bool,
          "the hypothesis is in scope before the generalize",
          true,
          List.Assoc.mem(facts(generalize), "assume", ~equal=String.equal),
        );
        let hole = first(body(generalize));
        switch (hole.goal |> Option.map(~f=Exp.term_of)) {
        | Some(Forall(_)) => ()
        | _ => Alcotest.fail("the body's goal is not re-quantified")
        };
        check(
          bool,
          "and gone inside it",
          false,
          List.Assoc.mem(facts(hole), "assume", ~equal=String.equal),
        );
      },
    ),
    test_case(
      "have: the subproof is a chain rooted at the proposition",
      `Quick,
      () => {
        let f = Fixture.mk(have_src);
        let (sub, _) = premises(first(f.rows));
        check(list(string), "the subproof's rows", ["hole"], kinds(sub));
        check(string, "its goal", "1 == 1", goal(first(sub)));
        check(
          bool,
          "a row to write in: it has state",
          true,
          Stepper.Model.find(first(sub).key, f.theorem.stepper) != None,
        );
      },
    ),
    test_case(
      "have: a completed subproof renders its rows and reaches true",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            {|theorem t = forall x -> x == 1 proof have 1 == 1 proof from 1 == 1 do rewrite refl end end => ? in t|},
          );
        let (sub, _) = premises(first(f.rows));
        check(
          list(string),
          "the subproof's rows",
          ["refl", "end"],
          kinds(sub),
        );
        check(
          option(string),
          "it reaches true",
          Some("true"),
          Rows.reached(sub) |> Option.map(~f=print),
        );
      },
    ),
    test_case(
      "have: the body continues the outer goal, with `have` in scope",
      `Quick,
      () => {
        let f = Fixture.mk(have_src);
        let (_, body) = premises(first(f.rows));
        check(list(string), "the body's rows", ["hole"], kinds(body));
        check(string, "its goal", "x == 1", goal(first(body)));
        check(
          option(string),
          "the hypothesis",
          Some("1 == 1"),
          List.Assoc.find(facts(first(body)), "have", ~equal=String.equal),
        );
      },
    ),
    test_case(
      "have: the subproof is checked in the enclosing scope",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            {|theorem t = forall x -> x == 1 proof have 1 == 1 proof have 2 == 2 proof ? => ? => ? in t|},
          );
        let (sub, _) = premises(first(f.rows));
        let (_, inner_body) = premises(first(sub));
        let haves =
          facts(first(inner_body))
          |> List.filter(~f=((name, _)) => name == "have")
          |> List.map(~f=snd);
        check(
          list(string),
          "only the inner have's hypothesis",
          ["2 == 2"],
          haves,
        );
      },
    ),
    test_case(
      "have: the obligation panel's Prove-here shape has both premises' rows",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "let f = fun x -> 100 / x in theorem t = forall y -> f(y) == f(y) proof have y != 0 proof ? => ? in t",
          );
        let (sub, body) = premises(first(f.rows));
        check(string, "the subproof's goal", "y != 0", goal(first(sub)));
        check(string, "the body's goal", "f(y) == f(y)", goal(first(body)));
        check(
          list(string),
          "both are holes",
          ["hole", "hole"],
          kinds([first(sub), first(body)]),
        );
      },
    ),
  ];
};

/* The stored state: one entry per row, kept while the row lasts. */
/* A goal holding a lambda, which the display folds unless function
 * bodies are shown. */
module Settings_change = {
  let src = "let twice = fun f -> fun x -> f(f(x)) in theorem t = twice(fun y -> y + 1)(3) == 5 proof ? in t";

  /* The goal's `twice(fun y -> y + 1)` and its `3`. */
  let lhs = (goal: Picker.Goal.t): (Exp.t, Exp.t) =>
    switch (Exp.term_of(goal.written)) {
    | BinOp(_, lhs, _) =>
      switch (Exp.term_of(lhs)) {
      | Ap(_, applied, three) => (applied, three)
      | _ => Alcotest.fail("not an application")
      }
    | _ => Alcotest.fail("not an equation")
    };
  let applied = goal => fst(lhs(goal));
  let three = goal => snd(lhs(goal));

  let show_fn_bodies = (settings: CoreSettings.t): CoreSettings.t => {
    ...settings,
    evaluation: {
      ...settings.evaluation,
      show_fn_bodies: true,
    },
  };

  let text = (goal: Picker.Goal.t): string =>
    Printer.of_zipper(~holes="?", goal.editor.editor.state.zipper);
};

module State = {
  let tab = (local: Stepper.Model.local) =>
    Option.bind(local.box, ~f=(box: Stepper.Model.box) => box.tab);

  let tests = [
    test_case(
      "the row where a chain continues has a write box, and a step row none",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (1 + 1) * 2 == 4 proof from 1 + 1 do step end in t",
          );
        check(
          list(bool),
          "has a box",
          [false, true],
          List.map(~f=row => Fixture.local(f, row).box != None, f.rows),
        );
        let row = last(f.rows);
        check(
          bool,
          "no tab picked",
          true,
          tab(Fixture.local(f, row)) == None,
        );
        let f = Fixture.act(f, (row.key, Tab(Step)));
        check(
          bool,
          "the tab picked",
          true,
          tab(Fixture.local(f, row)) == Some(Step),
        );
      },
    ),
    test_case(
      "every hole has a write box, in the middle of a proof too",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (1 + 1) * 2 == 4 proof ?; from 1 + 1 do step end in t",
          );
        check(
          list(pair(string, bool)),
          "rows and boxes",
          [("hole", true), ("step", false), ("end", true)],
          List.map(
            ~f=row => (kind(row), Fixture.local(f, row).box != None),
            f.rows,
          ),
        );
      },
    ),
    test_case(
      "a focus's steps end in a write box only when the focus ends its chain",
      `Quick,
      () => {
        let inner_end = src => {
          let f = Fixture.mk(src);
          switch (first(f.rows).kind) {
          | Step(Focus(rows)) =>
            let row = last(rows);
            (row.continues, Fixture.local(f, row).box != None);
          | _ => Alcotest.fail("not a C")
          };
        };
        check(
          pair(bool, bool),
          "last in its chain",
          (true, true),
          inner_end(
            "theorem t = (1 + 1) * 2 == 4 proof from (1 + 1) * 2 do from 1 + 1 do step end; step end in t",
          ),
        );
        check(
          pair(bool, bool),
          "with a step after it",
          (false, false),
          inner_end(
            "theorem t = (1 + 1) * 2 == 4 proof from (1 + 1) * 2 do from 1 + 1 do step end; step end; step in t",
          ),
        );
      },
    ),
    test_case(
      "an empty box showing a ghost keeps its caret before the hole",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = (1 + 1) * 2 == 4 proof ? in t");
        let row = first(f.rows);
        let at_start = (f: Fixture.t) =>
          switch (Fixture.local(f, row).box) {
          | Some(box) =>
            let z = box.editor.editor.state.zipper;
            fst(Siblings.neighbors(z.relatives.siblings)) == None
            && z.relatives.ancestors == [];
          | None => Alcotest.fail("no box")
          };
        List.iter(
          ~f=
            ((label, move)) =>
              check(
                bool,
                label,
                true,
                at_start(
                  Fixture.act(
                    f,
                    (row.key, BoxEditor(Perform(Move(move)))),
                  ),
                ),
              ),
          [("End", Action.End), ("Right", Local(Right, ByChar))],
        );
      },
    ),
    test_case(
      "a recalculation keeps each row's editor while its goal is unchanged",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => from 1 + 1 do step end in t|},
          );
        let f = Fixture.act(f, (first(f.rows).key, Goal(Move(Start))));
        let again =
          Stepper.Update.calculate(
            ~settings=Web.Theorems.with_proof_settings(settings),
            f.rows,
            f.theorem.stepper,
          );
        List.iter(
          ~f=
            (row: Rows.t) =>
              switch (
                Stepper.Model.find(row.key, f.theorem.stepper),
                Stepper.Model.find(row.key, again),
              ) {
              | (Some(before), Some(after)) =>
                check(
                  bool,
                  kind(row) ++ ": the same editor",
                  true,
                  phys_equal(before.goal.editor, after.goal.editor),
                )
              | (None, None) => ()
              | _ => Alcotest.fail(kind(row) ++ ": state came or went")
              },
          Fixture.all(f),
        );
      },
    ),
    test_case(
      "a changed goal is prepared again, keeping the row's box",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 1 + 2 == 3 proof ? in t");
        let row = first(f.rows);
        let f = Fixture.act(f, (row.key, Tab(Strategy)));
        let changed = {
          ...row,
          goal: Some(parse_exp("2 + 2 == 4")),
        };
        let model =
          Stepper.Update.calculate(~settings, [changed], f.theorem.stepper);
        switch (Stepper.Model.find(row.key, model)) {
        | Some(local) =>
          check(
            string,
            "the new goal",
            "2 + 2 == 4",
            print(local.goal.written),
          );
          check(
            bool,
            "the box keeps its tab",
            true,
            tab(local) == Some(Strategy),
          );
        | None => Alcotest.fail("the row lost its state")
        };
      },
    ),
    test_case(
      "a settings change prepares a goal again, keeping its selection and box",
      `Quick,
      () => {
        let f = Fixture.mk(Settings_change.src);
        let row = first(f.rows);
        let applied = Settings_change.applied(Fixture.goal(f, row));
        let f = Fixture.act(f, (row.key, Tab(Strategy)));
        let f =
          Fixture.act(
            f,
            (
              row.key,
              Goal(Select(Term(Id(Exp.rep_id(applied), Right)))),
            ),
          );
        let before = Fixture.local(f, row);
        let shown = Settings_change.show_fn_bodies(before.goal.settings);
        let model =
          Stepper.Update.calculate(
            ~settings=shown,
            f.rows,
            f.theorem.stepper,
          );
        switch (Stepper.Model.find(row.key, model)) {
        | Some(local) =>
          check(
            bool,
            "built with the new settings",
            true,
            local.goal.settings == shown,
          );
          check(
            string,
            "printed with function bodies",
            "twice(fun y -> y + 1)(3) == 5",
            Settings_change.text(local.goal),
          );
          check(
            option(string),
            "the same node selected",
            Some(print(Settings_change.applied(local.goal))),
            Option.map(~f=print, Picker.Goal.selected(local.goal)),
          );
          check(
            bool,
            "the box keeps its tab",
            true,
            tab(local) == Some(Strategy),
          );
        | None => Alcotest.fail("the row lost its state")
        };
      },
    ),
    test_case(
      "a settings change keeps the caret by the same node",
      `Quick,
      () => {
        let f = Fixture.mk(Settings_change.src);
        let row = first(f.rows);
        let three = Settings_change.three(Fixture.goal(f, row));
        let f =
          Fixture.act(
            f,
            (row.key, Goal(Move(Goal(TileId(Exp.rep_id(three)))))),
          );
        let before = Fixture.local(f, row);
        let model =
          Stepper.Update.calculate(
            ~settings=Settings_change.show_fn_bodies(before.goal.settings),
            f.rows,
            f.theorem.stepper,
          );
        switch (Stepper.Model.find(row.key, model)) {
        | Some(local) =>
          check(
            option(string),
            "the caret at the 3",
            Some("3"),
            Indicated.index(local.goal.editor.editor.state.zipper)
            |> Option.map(~f=id =>
                 switch (Exp.find_by_id(id, local.goal.written)) {
                 | Some(e) => print(e)
                 | None => "not in the goal"
                 }
               ),
          )
        | None => Alcotest.fail("the row lost its state")
        };
      },
    ),
    test_case(
      "opening the history doesn't prepare goals again",
      `Quick,
      () => {
        let f = Fixture.mk(Settings_change.src);
        let row = first(f.rows);
        let before = Fixture.local(f, row);
        let settings = before.goal.settings;
        let model =
          Stepper.Update.calculate(
            ~settings={
              ...settings,
              evaluation: {
                ...settings.evaluation,
                stepper_history: !settings.evaluation.stepper_history,
              },
            },
            f.rows,
            f.theorem.stepper,
          );
        switch (Stepper.Model.find(row.key, model)) {
        | Some(local) =>
          check(
            bool,
            "the same editor",
            true,
            phys_equal(local.goal.editor, before.goal.editor),
          )
        | None => Alcotest.fail("the row lost its state")
        };
      },
    ),
    test_case(
      "a row's state goes when its step does",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 + 4 == 5 proof from 1 + 4 do step end in t",
          );
        let other = Fixture.mk("theorem t = 1 + 4 == 5 proof ? in t");
        let model =
          Stepper.Update.calculate(~settings, other.rows, f.theorem.stepper);
        check(
          bool,
          "none of the old rows has state",
          true,
          List.for_all(
            ~f=(row: Rows.t) => Stepper.Model.find(row.key, model) == None,
            f.rows,
          ),
        );
        check(
          bool,
          "the new row has",
          true,
          Stepper.Model.find(first(other.rows).key, model) != None,
        );
      },
    ),
    test_case(
      "a row with no goal has no state",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 1 == 1 proof ? in t");
        let row = {
          ...first(f.rows),
          goal: None,
        };
        check(
          bool,
          "no state",
          true,
          Stepper.Model.find(
            row.key,
            Stepper.Update.calculate(~settings, [row], Stepper.Model.init),
          )
          == None,
        );
      },
    ),
    test_case(
      "a step just written has its row before the next check",
      `Quick,
      () => {
        /* Its argument's sub-editor has to be there to take focus. */
        let term = parse_exp("theorem t = 1 + 4 == 5 proof ? in t");
        let statics = Fixture.statics_of_term(term);
        let cell =
          Fixture.calculate(
            ~is_edited=true,
            statics,
            Web.EvalResult.Model.init,
          );
        let edited =
          Exp.map_term(
            ~f_proof=
              (cont, p: Proof.t) =>
                switch (p.term) {
                | EmptyHole =>
                  Proof.fresh(
                    Picker.Terms.assume(~exp=Exp.fresh(EmptyHole)),
                  )
                | _ => cont(p)
                },
            term,
          );
        let statics = Fixture.statics_of_term(edited);
        let f =
          Fixture.of_cell(
            ~name="t",
            statics,
            Fixture.calculate(~pending=true, ~is_edited=true, statics, cell),
          );
        check(list(string), "rows", ["assume", "end"], kinds(f.rows));
        check(bool, "not checked yet", true, first(f.rows).goal == None);
      },
    ),
    test_case(
      "a folded theorem keeps its stepper",
      `Quick,
      () => {
        let names = src =>
          Fixture.statics(src)
          |> Fixture.calculate(~is_edited=true, _, Web.EvalResult.Model.init)
          |> (cell => Web.Theorems.Model.theorems(cell.theorems))
          |> List.map(~f=((_, thm: Web.Theorems.Model.theorem)) => thm.name);
        let t = "theorem t = 1 == 1 proof ? in ";
        let u = "theorem u = 2 == 2 proof ? in ";
        check(
          list(string),
          "whole program",
          ["t", "u"],
          names("^^fold(" ++ t ++ u ++ "t)"),
        );
        check(
          list(string),
          "the rest of the program",
          ["t", "u"],
          names(t ++ "^^fold(" ++ u ++ "t)"),
        );
        check(
          list(string),
          "statement",
          ["t"],
          names("theorem t = ^^fold(1 == 1) proof ? in t"),
        );
        check(
          list(string),
          "under a let",
          ["u"],
          names("let x = ^^fold(" ++ u ++ "1) in x"),
        );
      },
    ),
    test_case(
      "a theorem's stepper goes when the theorem does",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 == 1 proof ? in theorem u = 2 == 2 proof ? in t",
          );
        check(
          int,
          "two theorems",
          2,
          List.length(Web.Theorems.Model.theorems(f.cell.theorems)),
        );
        let statics = Fixture.statics("theorem t = 1 == 1 proof ? in t");
        let cell = Fixture.calculate(~is_edited=true, statics, f.cell);
        check(
          list(string),
          "only t is left",
          ["t"],
          Id.Map.bindings(cell.theorems.thm_map)
          |> List.map(~f=((_, thm: Web.Theorems.Model.theorem)) => thm.name),
        );
      },
    ),
  ];
};

/* What the write box offers, as a function of the goal and the
 * selection. */
module Picking = {
  let root_goal = src => {
    let f = Fixture.mk(src);
    Fixture.goal(f, first(f.rows));
  };

  let node = (goal: Picker.Goal.t, printed: string): Exp.t => {
    let found = ref(None);
    let _: Exp.t =
      Exp.map_term(
        ~f_exp=
          (cont, e) => {
            if (found^ == None && print(e) == printed) {
              found := Some(e);
            };
            cont(e);
          },
        goal.written,
      );
    switch (found^) {
    | Some(e) => e
    | None => Alcotest.fail("no " ++ printed ++ " in the goal")
    };
  };

  /* The ghost's rule, the step Tab writes, and what the ghost shows. */
  let ghost = (~selected=None, goal) =>
    Picker.Ghost.mk(goal, ~selected)
    |> Option.map(~f=(g: Picker.Ghost.t) =>
         (g.rule, written(Picker.focused(g.pick)), print(g.result))
       );

  let ghost_t = option(triple(string, string, string));

  let labels = (strategies: list(Picker.Strategies.strategy)) =>
    List.map(~f=(s: Picker.Strategies.strategy) => s.label, strategies);

  /* What the strategy labelled `label` writes. */
  let writes = (strategies: list(Picker.Strategies.strategy), label) =>
    switch (
      List.find(
        ~f=(s: Picker.Strategies.strategy) => s.label == label,
        strategies,
      )
    ) {
    | Some(s) => written(Proof.fresh(s.writes))
    | None => Alcotest.fail("no " ++ label ++ " strategy")
    };

  let equation = "theorem t = 1 + 2 + 3 == 6 proof ? in t";
  let implication = "theorem t = forall x -> x == 2 ==> x + 1 == 3 proof ? in t";

  let tests = [
    test_case(
      "an empty box's ghost is the goal's next eval step, written where the checker takes it",
      `Quick,
      () =>
      check(
        ghost_t,
        "ghost",
        Some(("step", "from $e + 2 do step end", "3 + 3 == 6")),
        ghost(root_goal(equation)),
      )
    ),
    test_case("a goal reflexivity closes has the ghost `true`", `Quick, () =>
      check(
        ghost_t,
        "ghost",
        Some(("refl", "rewrite refl end", "true")),
        ghost(
          root_goal("theorem t = forall (x : Int) -> x == x proof ? in t"),
        ),
      )
    ),
    test_case(
      "with part of the goal selected, the ghost is that part's own step and what the part becomes",
      `Quick,
      () => {
        let goal = root_goal("theorem t = 2 * (1 + 1) == 4 proof ? in t");
        check(
          ghost_t,
          "ghost",
          Some(("step", "from ($e + $e) do step end", "2")),
          ghost(~selected=Some(node(goal, "1 + 1")), goal),
        );
        check(
          ghost_t,
          "a part with no step has none",
          None,
          ghost(~selected=Some(node(goal, "4")), goal),
        );
      },
    ),
    test_case(
      "a selected call of a named function contracts; with nothing selected, Tab steps as before",
      `Quick,
      () => {
        let goal =
          root_goal(
            "let inc = fun x -> x + 1 in theorem t = inc(2 + 3) == 6 proof ? in t",
          );
        let call = node(goal, "inc(2 + 3)");
        check(
          bool,
          "the Step tab marks the call",
          true,
          List.mem(
            Picker.Goal.step_ids(goal),
            Exp.rep_id(call),
            ~equal=Id.equal,
          ),
        );
        check(
          ghost_t,
          "selected",
          Some(("step", "from $e($e) do step end", "2 + 3 + 1")),
          ghost(~selected=Some(call), goal),
        );
        check(
          ghost_t,
          "nothing selected",
          Some(("step", "from $e + $e do step end", "inc(5) == 6")),
          ghost(goal),
        );
      },
    ),
    test_case(
      "the goal's own introduction comes first: Assume for an implication",
      `Quick,
      () => {
        let goal = root_goal(implication);
        let intros = Picker.Strategies.intros(goal);
        check(list(string), "intros", ["Assume"], labels(intros));
        check(
          string,
          "a hole to assume",
          "assume ? => ?",
          writes(intros, "Assume"),
        );
        check(
          list(string),
          "an equation has none",
          [],
          labels(Picker.Strategies.intros(root_goal(equation))),
        );
        check(
          list(string),
          "an implication in parentheses",
          ["Assume"],
          labels(
            Picker.Strategies.intros(
              root_goal(
                "theorem t = forall x -> (x == 2 ==> x + 1 == 3) proof ? in t",
              ),
            ),
          ),
        );
      },
    ),
    test_case(
      "a function equality's introduction is Funext, with a hole for its argument",
      `Quick,
      () => {
        let goal =
          root_goal(
            "theorem t = (fun x: Int -> x + 1) == (fun x: Int -> 1 + x) proof ? in t",
          );
        check(
          string,
          "funext",
          "funext ? => ?",
          writes(Picker.Strategies.intros(goal), "Funext"),
        );
      },
    ),
    test_case(
      "no Funext for functions whose argument isn't data",
      `Quick,
      () => {
        let offered = src =>
          Picker.Strategies.intros(root_goal(src))
          |> List.exists(~f=(s: Picker.Strategies.strategy) =>
               String.equal(s.label, "Funext")
             );
        check(
          bool,
          "function argument",
          false,
          offered(
            "theorem t = (fun h: (Int -> Int) -> 0) == (fun h: (Int -> Int) -> 0) proof ? in t",
          ),
        );
        check(
          bool,
          "unknown argument type",
          false,
          offered("theorem t = (fun x -> x) == (fun x -> x) proof ? in t"),
        );
      },
    ),
    test_case(
      "then the strategies any goal can use, each saying what it writes",
      `Quick,
      () => {
        let general = Picker.Strategies.general();
        check(
          list(string),
          "strategies",
          ["Induction", "Cases", "Have", "Generalize", "Revert"],
          labels(general),
        );
        check(
          list(string),
          "without a tooltip",
          [],
          general
          @ Picker.Strategies.intros(root_goal(implication))
          |> List.filter(~f=(s: Picker.Strategies.strategy) =>
               s.tooltip == ""
             )
          |> labels,
        );
      },
    ),
    test_case(
      "every strategy writes its form with holes to fill in",
      `Quick,
      () => {
        let general = Picker.Strategies.general();
        check(
          list(string),
          "writes",
          [
            "induction ? end",
            "cases ? end",
            "have ? proof ? => ?",
            "generalize ? => ?",
            "revert ? => ?",
          ],
          List.map(~f=writes(general), labels(general)),
        );
      },
    ),
    test_case(
      "Forall is offered on a forall goal",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall x: Int -> x > 0 ==> (forall y: Int -> y == y) proof assume x > 0 => ? in t",
          );
        let goal = Fixture.goal(f, first(body(first(f.rows))));
        check(
          bool,
          "Forall",
          true,
          List.mem(
            labels(Picker.Strategies.intros(goal)),
            "Forall",
            ~equal=String.equal,
          ),
        );
      },
    ),
    test_case(
      "with no eval step left, the ghost is reflexivity inside the goal",
      `Quick,
      () => {
        let goal =
          root_goal(
            "theorem t = forall n: Int -> n == 1 ==> n == n proof ? in t",
          );
        switch (Picker.Ghost.mk(goal, ~selected=None)) {
        | Some(g) =>
          check(string, "the rule", "refl", g.rule);
          check(
            string,
            "what the goal becomes",
            "n == 1 ==> true",
            print(g.result),
          );
        | None => Alcotest.fail("no ghost")
        };
      },
    ),
    test_case(
      "on an equation, the ghost steps a part only one side has, the left first",
      `Quick,
      () => {
        let goal =
          root_goal(
            "let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in theorem t = forall ys: [Int] -> len([] @ ys) == len([]) + len(ys) proof ? in t",
          );
        check(
          ghost_t,
          "ghost",
          Some((
            "step",
            "from $e @ $e do step end",
            "len(ys) == len([]) + len(ys)",
          )),
          ghost(goal),
        );
      },
    ),
  ];
};

/* Taking an offered step writes it, and the cell stepper advances. */
module Regressions = {
  /* What writing `pick` at `row` puts into the proof. */
  let written_at = (row: Rows.t, pick: Picker.pick): string =>
    switch (Picker.write(row, pick)) {
    | ProofPatch({replacement, _}) => written(replacement)
    | _ => Alcotest.fail("not a proof patch")
    };

  /* The cell stepper's goals for `src` after taking the only step the
   * last row offers `steps` times. */
  let cell_walk = (src: string, ~steps: int): list(string) => {
    let statics = Fixture.statics(src);
    let cell =
      Fixture.calculate(~is_edited=true, statics, Web.EvalResult.Model.init);
    let update = (action, cell) => {
      let updated =
        Web.EvalResult.Update.update(
          ~settings=Web.Settings.Model.init,
          action,
          cell,
        );
      Fixture.calculate(~is_edited=false, statics, updated.model);
    };
    let stepper = (cell: Web.EvalResult.Model.t) =>
      switch (cell.display) {
      | Stepper(s) => s
      | Evaluation(_) => Alcotest.fail("the stepper is off")
      };
    let rows = cell =>
      Rows.chain(
        ~proof_map=stepper(cell).proof_map,
        Web.EvalResult.CellStepper.proof(stepper(cell)),
      );
    let step = cell => {
      let row = last(rows(cell));
      let goal =
        switch (Stepper.Model.find(row.key, stepper(cell).stepper)) {
        | Some(local) => local.goal
        | None => Alcotest.fail("the last row has no state")
        };
      switch (Picker.take_step(goal, 0)) {
      | Some(pick) => update(StepperPatch(Picker.write(row, pick)), cell)
      | None => Alcotest.fail("no step offered at " ++ print(goal.written))
      };
    };
    let cell =
      List.fold_left(
        ~f=(cell, _) => step(cell),
        ~init=update(ToggleStepper, cell),
        List.init(steps, ~f=Fn.id),
      );
    List.map(~f=goal, rows(cell));
  };

  let tests = [
    test_case(
      "Step and Proof strategy drop a selection of part of the goal, and Write keeps it",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 2 * (1 + 1) == 4 proof ? in t");
        let row = first(f.rows);
        let part = Picking.node(Fixture.goal(f, row), "(1 + 1)");
        let f =
          Fixture.act(
            f,
            (row.key, Goal(Select(Term(Id(Exp.rep_id(part), Right))))),
          );
        let selected = (f: Fixture.t) =>
          Option.map(~f=print, Picker.Goal.selected(Fixture.goal(f, row)));
        let f = Fixture.act(f, (row.key, Tab(Write)));
        check(option(string), "Write", Some("(1 + 1)"), selected(f));
        let f = Fixture.act(f, (row.key, Tab(Strategy)));
        check(option(string), "Proof strategy", None, selected(f));
      },
    ),
    test_case(
      "a step inside a call's argument is offered at the argument, not at the call",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in theorem t = forall xs: [Int] -> len(xs @ []) == len(xs) proof induction xs | [] => ? | y :: ys => ? end in t",
          );
        let row = first(List.hd_exn(cases(first(f.rows))));
        let goal = Fixture.goal(f, row);
        check(
          list(string),
          "offered",
          ["len([])", "[] @ []"],
          offered(goal),
        );
        List.iteri(
          ~f=
            (n, _) =>
              switch (Picker.take_step(goal, n)) {
              | Some(pick) =>
                check(
                  bool,
                  written(Picker.focused(pick)) ++ " checks",
                  true,
                  Picker.outgoing(goal, goal.written, Picker.focused(pick))
                  != None,
                )
              | None => Alcotest.fail("an offered step writes nothing")
              },
          goal.eval_steps,
        );
      },
    ),
    test_case(
      "a strategy's `?` argument leaves its step unfinished, not wrong",
      `Quick,
      () =>
      List.iter(
        ~f=
          step => {
            let f =
              Fixture.mk(
                "theorem t = forall n: Int -> n > 5 ==> n > 3 proof assume n > 5 => "
                ++ step
                ++ " in t",
              );
            check(
              list(string),
              step ++ ": marks",
              [],
              List.concat_map(
                ~f=
                  (row: Rows.t) =>
                    switch (row.entry) {
                    | Some(entry) => List.map(~f=ProofMark.show, entry.marks)
                    | None => []
                    },
                Fixture.all(f),
              ),
            );
            check(
              string,
              step ++ ": status",
              "incomplete",
              Web.Obligations.Status.(
                label(of_proof(f.cell.theorems.proof_map, f.theorem.proof))
              ),
            );
          },
        ["revert ? => ?", "generalize ? => ?", "have ? proof ? => ?"],
      )
    ),
    test_case(
      "theorem stepper: taking an offered step writes the checker's eval step in place of the hole",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 1 + 2 + 3 == 6 proof ? in t");
        let row = first(f.rows);
        let goal = Fixture.goal(f, row);
        check(list(string), "offered", ["1 + 2"], offered(goal));
        switch (Picker.take_step(goal, 0)) {
        | Some(pick) =>
          check(
            string,
            "written",
            "from $e + 2 do step end",
            written_at(row, pick),
          )
        | None => Alcotest.fail("no step offered")
        };
      },
    ),
    test_case(
      "cell stepper: taking a step advances to the next goal", `Quick, () =>
      check(
        list(string),
        "the goals",
        ["1 + 2 + 3", "3 + 3", "6"],
        cell_walk("1 + 2 + 3", ~steps=2),
      )
    ),
    test_case(
      "cell stepper: a goal holding a negative number still steps at its root",
      `Quick,
      () =>
      check(
        list(string),
        "the goals",
        ["0 - 5 + 2", "-5 + 2", "-3"],
        cell_walk("0 - 5 + 2", ~steps=2),
      )
    ),
  ];
};

/* What the stepper writes into the proof text, and that it reads back. A
 * form with nothing to search goes in with a hole argument, and the
 * caret jumps into that argument, so the hole has to be a caret target. */
module Insertion = {
  let parse_zipper = (s: string): Zipper.t =>
    switch (Parser.to_zipper(s, ~root=Exp)) {
    | Some(z) => z
    | None => Alcotest.fail("failed to parse zipper: " ++ s)
    };

  let theorem_proof = (z: Zipper.t): Proof.t =>
    switch (
      Test_ProofMap.find_theorem_proof(
        MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
      )
    ) {
    | Some(p) => p
    | None => Alcotest.fail("no theorem proof")
    };

  /* Writes `term` over the theorem's (hole) proof, as picking on its hole
   * row does. */
  let insert_zipper = (~src: string, term: TermBase.Proof.term): Zipper.t => {
    let z = parse_zipper(src);
    EditorTransform.apply_patch(
      z,
      EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(theorem_proof(z)),
        Proof.fresh(term),
      ),
    );
  };

  let insert = (~src: string, term: TermBase.Proof.term): string =>
    insert_zipper(~src, term) |> Printer.of_zipper(~holes="?");

  let reparsed = (out: string): Proof.t =>
    switch (Test_ProofMap.find_theorem_proof(parse_exp(out))) {
    | Some(p) => p
    | None => Alcotest.fail("no theorem proof after insertion")
    };

  let contains = (haystack: string, needle: string): bool => {
    let hl = String.length(haystack);
    let nl = String.length(needle);
    let rec go = i =>
      i
      + nl <= hl
      && (String.sub(haystack, ~pos=i, ~len=nl) == needle || go(i + 1));
    nl == 0 || go(0);
  };

  let check_contains = (~msg: string, haystack: string, needle: string) =>
    check(
      bool,
      msg ++ ": expected to find\n" ++ needle ++ "\nin:\n" ++ haystack,
      true,
      contains(haystack, needle),
    );

  let hole = () => Exp.fresh(EmptyHole);
  let implication = "theorem t = forall x -> x == 2 ==> x == 2 proof ? in t";

  let tests = [
    test_case(
      "Assume with no argument writes `assume ? =>`",
      `Quick,
      () => {
        let out = insert(~src=implication, Picker.Terms.assume(~exp=hole()));
        check_contains(~msg="the step", out, "assume ? =>");
        check_contains(
          ~msg="the statement survives",
          out,
          "forall x -> x == 2 ==> x == 2",
        );
      },
    ),
    test_case(
      "Generalize with no argument writes `generalize ? =>`", `Quick, () =>
      check_contains(
        ~msg="the step",
        insert(
          ~src="theorem t = forall x -> x == x proof ? in t",
          Picker.Terms.generalize(~exp=hole()),
        ),
        "generalize ?",
      )
    ),
    test_case(
      "an empty-argument form reparses with a hole argument", `Quick, () =>
      switch (
        reparsed(insert(~src=implication, Picker.Terms.assume(~exp=hole())))
      ) {
      | {term: Assume({term: EmptyHole, _}, None, {term: EmptyHole, _}), _} =>
        ()
      | p => Alcotest.fail("reparsed as " ++ Proof.show(p))
      }
    ),
    test_case(
      "a new form's argument hole is a caret target, and the form keeps its id",
      `Quick,
      () =>
      List.iter(
        ~f=
          ((label, src, term)) => {
            let arg_id =
              switch (Picker.Terms.arg_id(term)) {
              | Some(id) => id
              | None => Alcotest.fail(label ++ ": no argument id")
              };
            let step = Proof.fresh(term);
            let z = parse_zipper(src);
            let z =
              EditorTransform.apply_patch(
                z,
                EditorTransform.mk_proof_patch(
                  ~target_id=Proof.rep_id(theorem_proof(z)),
                  step,
                ),
              );
            check(
              bool,
              label ++ ": the caret can jump to the argument",
              true,
              Move.jump_to_id_indicated(z, arg_id) != None,
            );
            /* The view focuses the new step's argument by the step's id. */
            check(
              bool,
              label ++ ": the step keeps its id",
              true,
              Proof.rep_id(theorem_proof(z)) == Proof.rep_id(step),
            );
          },
        [
          ("assume", implication, Picker.Terms.assume(~exp=hole())),
          ("generalize", implication, Picker.Terms.generalize(~exp=hole())),
          /* Revert is picked, not typed, but focused the same way so the
           * pick can be adjusted. */
          (
            "revert",
            implication,
            Picker.Terms.revert(~exp=parse_exp("x == 2")),
          ),
          (
            "have",
            "theorem t = forall x -> x == 1 proof ? in t",
            Picker.Terms.have(~exp=hole()),
          ),
        ],
      )
    ),
    test_case("a step with no argument has no argument id", `Quick, () =>
      check(
        bool,
        "none",
        true,
        Picker.Terms.arg_id(Proof.fresh(EmptyHole).term) == None,
      )
    ),
    test_case("a picked Assume argument writes `assume <e> => ?`", `Quick, () =>
      check_contains(
        ~msg="the step",
        insert(
          ~src=implication,
          Picker.Terms.assume(~exp=parse_exp("x == 2")),
        ),
        "assume x == 2 =>",
      )
    ),
    test_case("a picked Revert fact writes `revert <e> => ?`", `Quick, () =>
      check_contains(
        ~msg="the step",
        insert(
          ~src="theorem t = forall x -> x == 1 proof ? in t",
          Picker.Terms.revert(~exp=parse_exp("x == 1")),
        ),
        "revert x == 1 =>",
      )
    ),
    test_case(
      "a picked Generalize argument writes `generalize <e> => ?`", `Quick, () =>
      check_contains(
        ~msg="the step",
        insert(
          ~src="theorem t = forall x -> x == x proof ? in t",
          Picker.Terms.generalize(~exp=parse_exp("x")),
        ),
        "generalize x =>",
      )
    ),
    test_case(
      "Have with no argument writes `have ? proof ? =>`",
      `Quick,
      () => {
        let out =
          insert(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            Picker.Terms.have(~exp=hole()),
          );
        check_contains(~msg="the proposition", out, "have ?");
        check_contains(~msg="an open subproof", out, "proof ?");
        check_contains(~msg="the arrow", out, "=>");
      },
    ),
    test_case("an empty-argument have reparses with three holes", `Quick, () =>
      switch (
        reparsed(
          insert(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            Picker.Terms.have(~exp=hole()),
          ),
        )
      ) {
      | {
          term:
            Have(
              {term: EmptyHole, _},
              None,
              {term: EmptyHole, _},
              {term: EmptyHole, _},
            ),
          _,
        } =>
        ()
      | p => Alcotest.fail("reparsed as " ++ Proof.show(p))
      }
    ),
    test_case(
      "a picked Have proposition writes `have <e> proof ? => ?`", `Quick, () =>
      check_contains(
        ~msg="the step",
        insert(
          ~src="theorem t = forall x -> x == 1 proof ? in t",
          Picker.Terms.have(~exp=parse_exp("1 == 1")),
        ),
        "have 1 == 1 proof ?",
      )
    ),
    test_case("an inserted form reparses as that form", `Quick, () =>
      switch (
        reparsed(
          insert(
            ~src=implication,
            Picker.Terms.assume(~exp=parse_exp("x == 2")),
          ),
        )
      ) {
      | {term: Assume(e, None, {term: EmptyHole, _}), _} =>
        check(string, "the argument", "x == 2", print(e))
      | p => Alcotest.fail("reparsed as " ++ Proof.show(p))
      }
    ),
    test_case(
      "a step picked on a hole replaces it, on a step goes before it, on an end row extends the chain",
      `Quick,
      () => {
        let src = "theorem t = 1 + 4 == 5 proof ?; from 1 + 4 do step end in t";
        let z = parse_zipper(src);
        let proof_map = Fixture.mk(src).cell.theorems.proof_map;
        let rows = Rows.chain(~proof_map, theorem_proof(z));
        check(list(string), "rows", ["hole", "step", "end"], kinds(rows));
        /* The chain after picking `from 2 + 2 do step end` on `row`: each
         * row's hole or focus selector. */
        let pick = row => {
          let term =
            Picker.Terms.focus(
              ~selector=parse_exp("2 + 2"),
              ~index=0,
              Proof.fresh(EvalStep),
            );
          EditorTransform.apply_patch(
            z,
            Picker.Patches.write(row, Proof.fresh(term)),
          )
          |> theorem_proof
          |> Rows.chain(~proof_map=ProofMap.empty)
          |> List.filter_map(~f=(row: Rows.t) =>
               switch (row.kind, row.step.term) {
               | (Hole, _) => Some("?")
               | (Step(_), Focus({selector, _})) => Some(print(selector))
               | _ => None
               }
             );
        };
        check(
          list(string),
          "on the hole",
          ["2 + 2", "1 + 4"],
          pick(List.nth_exn(rows, 0)),
        );
        check(
          list(string),
          "on the step",
          ["?", "2 + 2", "1 + 4"],
          pick(List.nth_exn(rows, 1)),
        );
        check(
          list(string),
          "on the end row",
          ["?", "1 + 4", "2 + 2"],
          pick(List.nth_exn(rows, 2)),
        );
      },
    ),
  ];
};

/* Goals as rows show them: as written, naming the program's
 * definitions. */
module Display = {
  let sum = {|let sum(xs: [Int]) = case xs
| [] => 0
| x::xs => x + sum(xs)
end
in
|};

  let rec has = (pred, e: Exp.t): bool =>
    pred(e) || List.exists(~f=has(pred), Occurrence.children(e));

  let is_fun = (e: Exp.t) =>
    switch (e.term) {
    | Fun(_)
    | FixF(_)
    | Closure(_) => true
    | _ => false
    };

  let tests = [
    test_case(
      "the Step tab offers a step on each operand",
      `Quick,
      () => {
        let f =
          Fixture.mk("theorem t = (1 + 2) * (3 + 4) == 21 proof ? in t");
        let g = Fixture.goal(f, first(f.rows));
        check(
          list(string),
          "offered",
          ["(1 + 2)", "(3 + 4)"],
          Picker.Goal.step_ids(g)
          |> List.filter_map(~f=id => Exp.find_by_id(id, g.written))
          |> List.map(~f=print)
          |> List.sort(~compare),
        );
      },
    ),
    test_case(
      "a refuted theorem offers no write box, an unfinished one does",
      `Quick,
      () => {
        let box_at_end = src => {
          let f = Fixture.mk(src);
          switch (List.rev(f.rows)) {
          | [last, ..._] => Fixture.local(f, last).box != None
          | [] => Alcotest.fail("no rows")
          };
        };
        check(
          bool,
          "refuted",
          false,
          box_at_end(
            "theorem t = 1 + 1 == 3 proof from 1 + 1 do step end; step in t",
          ),
        );
        check(
          bool,
          "unfinished",
          true,
          box_at_end(
            "theorem t = 1 + 1 == 2 proof from 1 + 1 do step end in t",
          ),
        );
      },
    ),
    test_case(
      "the last row of a focus is no goal still to prove",
      `Quick,
      () => {
        let unproven = src => {
          let f = Fixture.mk(src);
          Fixture.all(f)
          |> List.filter(
               ~f=Rows.is_unproven(~proof_map=f.cell.theorems.proof_map),
             )
          |> kinds;
        };
        check(
          list(string),
          "in a focus",
          [],
          unproven(
            "theorem t = (2 + 3) * 2 == 10 proof from (2 + 3) * 2 do from 2 + 3 do step end; step end; step in t",
          ),
        );
        check(
          list(string),
          "after an unfinished assume",
          ["end"],
          unproven(
            "theorem t = forall x: Int -> x == 1 ==> x + 0 == 1 proof assume x == 1 => ? in t",
          ),
        );
      },
    ),
    test_case(
      "an arm whose pattern binds a name the goal uses is refused",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall xs: [Int] -> forall x: Int -> forall z: Int -> (xs @ [x]) @ [z] == xs @ [x, z] proof induction xs | [] => ? | [x] => ? | z :: rest => ? end in t",
          );
        let row = first(f.rows);
        switch (row.kind) {
        | Step(Cases(cases)) =>
          check(
            list(option(string)),
            "refused",
            [None, Some("x"), Some("z")],
            List.map(
              ~f=((pat, _)) => Rows.refused_binding(row, pat),
              cases,
            ),
          )
        | _ => Alcotest.fail("the first row is not a split")
        };
      },
    ),
    test_case(
      "a focus ordinal counts occurrences in the goal as shown",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            sum
            ++ "theorem t = sum == fold_left(_, sum, 0) proof from 0 do algebra 0 + 0 end end in t",
          );
        check(list(string), "rows", ["algebra", "end"], kinds(f.rows));
        check(
          string,
          "the end row",
          "sum == fold_left(_, sum, 0 + 0)",
          goal(last(f.rows)),
        );
      },
    ),
    test_case(
      "a row shows its goal naming the definitions",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "let f = fun x -> 100 / x in theorem t = forall y -> f(y) == f(y) proof ? in t",
          );
        let goal = Fixture.goal(f, first(f.rows));
        check(string, "as written", "f(y) == f(y)", print(goal.written));
        check(bool, "no function inlined", false, has(is_fun, goal.written));
      },
    ),
    test_case(
      "an end row shows the outgoing goal naming the definitions",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "let f = fun x -> x in theorem t = f == f proof funext x => ? in t",
          );
        check(
          list(string),
          "rows",
          ["extensionality", "end"],
          kinds(f.rows),
        );
        let goal = Fixture.goal(f, last(f.rows));
        check(bool, "no function inlined", false, has(is_fun, goal.written));
      },
    ),
    test_case(
      "applications of a recursive function offer eval steps",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            sum
            ++ "theorem t = sum([]) == fold_left(_, sum, 0)([]) proof ? in t",
          );
        check(
          bool,
          "steps offered",
          true,
          offered(Fixture.goal(f, first(f.rows))) != [],
        );
      },
    ),
    test_case(
      "an end row's tile ids, underlined ones included, are unique",
      `Quick,
      () => {
        /* A duplicate makes `Highlight.of_tile` raise a shard mismatch. */
        let f =
          Fixture.mk(
            sum
            ++ "theorem t = sum([]) == fold_left(_, sum, 0)([]) proof from fold_left(_, sum, 0)([]) do step end in t",
          );
        let goal = Fixture.goal(f, last(f.rows));
        let ids =
          goal.editor.editor.state.zipper
          |> Zipper.unselect_and_zip
          |> segment_tile_ids;
        check(
          list(string),
          "duplicated ids",
          [],
          duplicated(ids) |> List.map(~f=Id.to_string),
        );
        check(
          list(string),
          "underlined ids that are not unique",
          [],
          Picker.Goal.step_ids(goal)
          |> List.filter(~f=id =>
               List.length(List.filter(~f=Id.equal(id), ids)) > 1
             )
          |> List.map(~f=Id.to_string),
        );
      },
    ),
    test_case(
      "a funext body offers eval of an applied recursive function",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            sum
            ++ "theorem t = sum == fold_left(_, sum, 0) proof funext xs => ? in t",
          );
        let goal = Fixture.goal(f, first(body(first(f.rows))));
        check(
          string,
          "the pointwise goal",
          "sum(xs) == fold_left(_, sum, 0)(xs)",
          print(goal.written),
        );
        check(bool, "steps offered", true, offered(goal) != []);
      },
    ),
  ];
};

/* How a step draws below its row: a line, or a block of side conditions
 * and the goals it creates. */
module Structure = {
  let inv = {|theorem inv = forall x -> x != 0 ==> x / x == 1 proof ? in theorem t = forall x -> forall y -> x / x + y / y == 2 proof from x / x do rewrite inv end end; from y / y do rewrite inv end end in t|};

  let layout =
    Alcotest.testable(
      (fmt, l: Rows.layout) =>
        switch (l) {
        | Line => Format.fprintf(fmt, "Line")
        | Block({conditions, arms}) =>
          Format.fprintf(
            fmt,
            "Block([%s], arms: %b)",
            String.concat(~sep=", ", List.map(~f=string_of_int, conditions)),
            arms,
          )
        },
      (==),
    );

  let kinds_of = (row: Rows.t) =>
    Rows.side_conditions(row)
    |> List.map(~f=(ob: Obligation.t) => Obligation.show_kind(ob.kind));

  let tests = [
    test_case(
      "a step's side conditions are dots on its line until opened",
      `Quick,
      () => {
        let f = Fixture.mk(inv);
        let (x, y) =
          switch (f.rows) {
          | [x, y, ..._] => (x, y)
          | _ => Alcotest.fail("two steps expected")
          };
        /* `inv` is unfinished, so citing it rests on its statement. */
        check(
          list(string),
          "a premise and the citation each",
          ["Premise", "Citation"],
          kinds_of(x),
        );
        check(
          list(bool),
          "all open",
          [true, true, true, true],
          List.map(
            ~f=Obligation.is_pending,
            Rows.side_conditions(x) @ Rows.side_conditions(y),
          ),
        );
        check(layout, "collapsed", Line, Rows.layout(x, ~expanded=[]));
        check(
          layout,
          "opened",
          Block({
            conditions: [0],
            arms: false,
          }),
          Rows.layout(x, ~expanded=[0]),
        );
        check(
          layout,
          "an index past the conditions opens nothing",
          Line,
          Rows.layout(x, ~expanded=[3]),
        );
      },
    ),
    test_case(
      "opening a condition is kept across passes, with its row",
      `Quick,
      () => {
        let f = Fixture.mk(inv);
        let row = first(f.rows);
        let f = Fixture.act(f, (row.key, ToggleCondition(0)));
        check(list(int), "open", [0], Fixture.local(f, row).expanded);
        let model =
          Stepper.Update.calculate(~settings, f.rows, f.theorem.stepper);
        check(
          list(int),
          "after another pass",
          [0],
          switch (Stepper.Model.find(row.key, model)) {
          | Some(local) => local.expanded
          | None => Alcotest.fail("the row lost its state")
          },
        );
        let f = Fixture.act(f, (row.key, ToggleCondition(0)));
        check(
          list(int),
          "closed again",
          [],
          Fixture.local(f, row).expanded,
        );
      },
    ),
    test_case(
      "a form's body and an induction's cases are arms",
      `Quick,
      () => {
        let forall =
          Fixture.mk(
            "theorem t = true ==> forall x : Int -> x == x proof assume true => forall x => from x == x do rewrite refl end end in t",
          );
        let assume = first(forall.rows);
        check(
          layout,
          "assume",
          Block({
            conditions: [],
            arms: true,
          }),
          Rows.layout(assume, ~expanded=[]),
        );
        check(
          layout,
          "forall",
          Block({
            conditions: [],
            arms: true,
          }),
          Rows.layout(first(body(assume)), ~expanded=[]),
        );
        let induction =
          Fixture.mk(
            "theorem t = forall xs : [Int] -> xs == xs proof induction xs | [] => ? | y :: ys => ? end in t",
          );
        check(
          layout,
          "induction",
          Block({
            conditions: [],
            arms: true,
          }),
          Rows.layout(first(induction.rows), ~expanded=[]),
        );
      },
    ),
    test_case(
      "have and alias keep their goal: the body continues the chain",
      `Quick,
      () => {
        let have =
          Fixture.mk(
            "theorem t = forall z -> 8 / z == 8 / z proof have z != 0 proof ? => from 8 / z == 8 / z do rewrite refl end end in t",
          );
        let row = first(have.rows);
        check(
          list(string),
          "no end row after the have",
          ["have"],
          kinds(have.rows),
        );
        check(list(string), "its premises are its arms", [], kinds_of(row));
        check(
          layout,
          "a block with the sub-proof",
          Block({
            conditions: [],
            arms: true,
          }),
          Rows.layout(row, ~expanded=[]),
        );
        check(
          option(list(string)),
          "continued by its body",
          Some(["refl", "end"]),
          Rows.continuation(row) |> Option.map(~f=kinds),
        );
        let alias =
          Fixture.mk(
            "theorem t = forall x -> x == 1 ==> x == 1 proof assume x == 1 => alias h = x == 1 => ? in t",
          );
        let row = first(body(first(alias.rows)));
        check(
          layout,
          "alias is a line",
          Line,
          Rows.layout(row, ~expanded=[]),
        );
        check(
          option(list(string)),
          "continued by its body",
          Some(["hole"]),
          Rows.continuation(row) |> Option.map(~f=kinds),
        );
      },
    ),
    test_case(
      "obligations are tagged with their kind",
      `Quick,
      () => {
        let kinds_at = (src, pick) => kinds_of(pick(Fixture.mk(src).rows));
        check(
          list(string),
          "a division the instantiation brings in",
          ["Definedness"],
          kinds_at(
            "theorem t = forall z: Int -> 8 / z > 0 || true proof rewrite or_true end in t",
            first,
          ),
        );
        check(
          list(string),
          "a term with no value the step needs standard",
          ["Standardness"],
          kinds_at(
            "let loop = fun (u : Int) -> loop(u) + 1 in theorem t = 0 * loop(0) == 0 proof from 0 * loop(0) do algebra 0 end end; rewrite refl end in t",
            first,
          ),
        );
      },
    ),
  ];
};

/* A focus of one step is drawn as that step; a focus of several opens a
 * C of its own steps; and a step picked inside what a focus just rewrote
 * joins the focus. What a focus selects is underlined in the goal above
 * it. */
module Focusing = {
  /* The node of `goal`'s display underlined for `row`'s focus, printed,
   * and which of the display's occurrences of it it is, counting from 0. */
  let underlined = (goal: Picker.Goal.t, row: Rows.t): (string, int) =>
    switch (
      Option.bind(Picker.Goal.acts_on(goal, row), ~f=id =>
        Exp.find_by_id(id, goal.written)
      )
    ) {
    | Some(node) => (
        print(node),
        Occurrence.index(node, goal.written) |> Option.value(~default=-1),
      )
    | None => Alcotest.fail("nothing underlined")
    };
  let underline = pair(string, int);

  /* What picking `step` on `node`, a node of `goal`, writes at `row`. */
  let eval_written = (row: Rows.t, goal: Picker.Goal.t, node: Exp.t): string =>
    switch (
      Picker.write(
        row,
        {
          at: Picker.target(goal, node),
          step: Proof.fresh(EvalStep),
        },
      )
    ) {
    | ProofPatch({replacement, _}) => written(replacement)
    | _ => Alcotest.fail("not a proof patch")
    };

  /* The goal `src`'s proof ends on, failing if the checker marks a step. */
  let ends_on = (src: string): string => {
    let f = Fixture.mk(src);
    List.iter(
      ~f=
        (row: Rows.t) =>
          switch (row.entry) {
          | Some({marks: [_, ..._] as marks, _}) =>
            Alcotest.fail(
              "marked: "
              ++ String.concat(~sep=", ", List.map(~f=ProofMark.show, marks)),
            )
          | Some(_)
          | None => ()
          },
      Fixture.all(f),
    );
    goal(last(f.rows));
  };

  /* Each case picks `step` on a node of the hole's goal, checks what the
   * picker writes, then checks that what it wrote selects that node. */
  let generalizing_tests = [
    test_case(
      "a focus writes `$e` for each part of its selection that its match doesn't need",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 1 + 2 + 3 == 6 proof ? in t");
        let row = first(f.rows);
        let goal = Fixture.goal(f, row);
        check(
          string,
          "`$e + $e` would select `1 + 2 + 3`",
          "from $e + 2 do step end",
          eval_written(row, goal, Picking.node(goal, "1 + 2")),
        );
        check(
          string,
          "it selects the `1 + 2`",
          "3 + 3 == 6",
          ends_on(
            "theorem t = 1 + 2 + 3 == 6 proof from $e + 2 do step end in t",
          ),
        );
      },
    ),
    test_case(
      "a focus keeps its ordinal, generalizing only what keeps it",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = (1 + 1) * (1 + 1) == 4 proof ? in t");
        let row = first(f.rows);
        let goal = Fixture.goal(f, row);
        let second =
          switch (Occurrence.nth(parse_exp("1 + 1"), 1, goal.written)) {
          | Some(node) => node
          | None => Alcotest.fail("no second 1 + 1")
          };
        check(
          string,
          "the second `1 + 1`",
          "from ($e + $e) at 2 do step end",
          eval_written(row, goal, second),
        );
        check(
          string,
          "it selects the second `1 + 1`",
          "(1 + 1) * 2 == 4",
          ends_on(
            "theorem t = (1 + 1) * (1 + 1) == 4 proof from ($e + $e) at 2 do step end in t",
          ),
        );
      },
    ),
    test_case(
      "a focus keeps the `_` of a partial application, and reads back",
      `Quick,
      () => {
        let src = "let add = fun (a, b) -> a + b in let g = fun x -> x + 1 in theorem t = g(3) == add(_, 1)(3) proof %% in t";
        let fill = proof =>
          Str.global_replace(Str.regexp_string("%%"), proof, src);
        let f = Fixture.mk(fill("?"));
        let row = first(f.rows);
        let goal = Fixture.goal(f, row);
        let step =
          eval_written(row, goal, Picking.node(goal, "add(_, 1)(3)"));
        check(
          bool,
          step ++ " keeps the `_`",
          true,
          String.contains(step, '_'),
        );
        ends_on(fill(step)) |> ignore;
      },
    ),
    test_case(
      "a focus on a call writes `$e` for the function and its argument",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "let k = fun x -> x + 1 in theorem t = 7 + k(2) == 10 proof ? in t",
          );
        let row = first(f.rows);
        let goal = Fixture.goal(f, row);
        check(
          string,
          "the call",
          "from $e($e) do step end",
          eval_written(row, goal, Picking.node(goal, "k(2)")),
        );
        check(
          string,
          "it selects `k(2)`",
          "7 + (2 + 1) == 10",
          ends_on(
            "let k = fun x -> x + 1 in theorem t = 7 + k(2) == 10 proof from $e($e) do step end in t",
          ),
        );
      },
    ),
  ];

  let tests = [
    test_case(
      "a one-step focus is a line named for its step",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 + 4 == 5 proof from 1 + 4 do step end in t",
          );
        switch (first(f.rows)) {
        | {kind: Step(Leaf), _} as row =>
          check(string, "named for its step", "step", kind(row))
        | row => Alcotest.fail("not a line: " ++ kind(row))
        };
      },
    ),
    test_case(
      "a focus of several steps nests them, and the next row is the lifted goal",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (2 + 3) * 2 == 10 proof from (2 + 3) * 2 do from 2 + 3 do step end; step end; step in t",
          );
        check(
          list(string),
          "outer rows",
          ["focus", "step", "end"],
          kinds(f.rows),
        );
        switch (first(f.rows).kind) {
        | Step(Focus(rows)) =>
          check(
            list(string),
            "its steps",
            ["step", "step", "end"],
            kinds(rows),
          );
          check(
            string,
            "they start from the selected subterm",
            "(2 + 3) * 2",
            goal(first(rows)),
          );
        | _ => Alcotest.fail("not a C")
        };
        check(
          string,
          "the row after it",
          "10 == 10",
          goal(List.nth_exn(f.rows, 1)),
        );
      },
    ),
    test_case(
      "a step taken inside what a focus rewrote is a row of its own",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (2 + 3) * 2 == 10 proof from 2 + 3 do step end in t",
          );
        let row = last(f.rows);
        let goal = Fixture.goal(f, row);
        let pick: Picker.pick = {
          at: Picker.target(goal, Picking.node(goal, "5 * 2")),
          step: Proof.fresh(EvalStep),
        };
        switch (Picker.write(row, pick)) {
        | ProofPatch({replacement, _}) =>
          check(
            string,
            "after the focus",
            "from 2 + 3 do step end; from $e * $e do step end",
            written(replacement),
          )
        | _ => Alcotest.fail("not a proof patch")
        };
      },
    ),
    test_case(
      "a one-step focus underlines the occurrence its ordinal picks",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (1 + 1) * (1 + 1) == 4 proof from 1 + 1 at 2 do step end; step in t",
          );
        let row = first(f.rows);
        check(
          underline,
          "the second 1 + 1",
          ("(1 + 1)", 1),
          underlined(Fixture.goal(f, row), row),
        );
        let next = List.nth_exn(f.rows, 1);
        check(
          underline,
          "the `step` after it, all of its goal",
          ("(1 + 1) * 2 == 4", 0),
          underlined(Fixture.goal(f, next), next),
        );
      },
    ),
    test_case(
      "a focus of several steps underlines its occurrence above the C, and a focus inside it underlines within the C's goal",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (2 + 3) * 2 + (2 + 3) * 2 == 20 proof from (2 + 3) * 2 at 2 do from 2 + 3 do step end; step end; step in t",
          );
        let row = first(f.rows);
        check(
          underline,
          "above the C",
          ("(2 + 3) * 2", 1),
          underlined(Fixture.goal(f, row), row),
        );
        switch (row.kind) {
        | Step(Focus(rows)) =>
          let inner = first(rows);
          check(
            underline,
            "inside the C",
            ("(2 + 3)", 0),
            underlined(Fixture.goal(f, inner), inner),
          );
        | _ => Alcotest.fail("not a C")
        };
      },
    ),
    test_case(
      "the underline lands on a call to a definition",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "let k = fun x -> x + 1 in theorem t = 7 + k(2) == 10 proof from k(2) do step end; step in t",
          );
        let row = first(f.rows);
        check(
          underline,
          "the call",
          ("k(2)", 0),
          underlined(Fixture.goal(f, row), row),
        );
      },
    ),
    test_case(
      "the underline is found in a goal with ids of its own",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (1 + 1) * (1 + 1) == 4 proof from 1 + 1 at 2 do step end; step in t",
          );
        let row = first(f.rows);
        let goal =
          switch (row.goal, Rows.ctx(row)) {
          | (Some(written), Some(ctx)) =>
            Picker.Goal.mk(~settings, ~ctx, Exp.replace_all_ids(written))
          | _ => Alcotest.fail("the row has no goal")
          };
        check(
          underline,
          "the second 1 + 1",
          ("(1 + 1)", 1),
          underlined(goal, row),
        );
      },
    ),
    test_case(
      "a step on the whole goal underlines all of it: eval, axiom, rewrite",
      `Quick,
      () => {
        let whole = (src, named) => {
          let f = Fixture.mk(src);
          let row =
            switch (List.find(~f=r => kind(r) == named, Fixture.all(f))) {
            | Some(row) => row
            | None => Alcotest.fail("no " ++ named ++ " row")
            };
          check(
            underline,
            named,
            (goal(row), 0),
            underlined(Fixture.goal(f, row), row),
          );
        };
        whole("theorem t = 4 == 4 proof step in t", "step");
        whole(
          "theorem t = forall x: Int -> x == x proof forall x => rewrite refl end in t",
          "refl",
        );
        whole(
          "theorem t = 4 / 2 == 2 proof algebra 2 == 2 end; step in t",
          "algebra",
        );
      },
    ),
    test_case(
      "a step that changes the goal's shape underlines nothing",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall x: Int -> x == x proof forall x => rewrite refl end in t",
          );
        let row = first(f.rows);
        check(string, "a forall", "forall", kind(row));
        check(
          bool,
          "nothing underlined",
          true,
          Picker.Goal.acts_on(Fixture.goal(f, row), row) == None,
        );
      },
    ),
    test_case(
      "an occurrence with a hidden ascription underlines what it ascribes",
      `Quick,
      () => {
        /* The checker's goal reads `x :: rev([]) : [Int]`, and the
         * selector matches the ascription first. */
        let f =
          Fixture.mk(
            "let rev = fun xs -> case xs | [] => [] | x :: t => rev(t) @ [x] end in theorem t = forall x: Int -> [x] == x :: rev([]) proof forall x => from rev([]) do step end; ? in t",
          );
        let row =
          switch (
            List.find(
              ~f=
                (r: Rows.t) =>
                  switch (r.step.term) {
                  | Focus(_) => true
                  | _ => false
                  },
              Fixture.all(f),
            )
          ) {
          | Some(row) => row
          | None => Alcotest.fail("no focus row")
          };
        let goal = Fixture.goal(f, row);
        check(
          bool,
          "the checker selected the ascription",
          true,
          switch (row.goal, row.acts_on) {
          | (Some(written), Some(part)) =>
            switch (Exp.find_by_id(part, written)) {
            | Some({term: Asc(_), _}) => true
            | _ => false
            }
          | _ => false
          },
        );
        check(
          option(string),
          "the application, not its ascription",
          Some("application"),
          Picker.Goal.acts_on(goal, row)
          |> Option.bind(_, ~f=id => Exp.find_by_id(id, goal.written))
          |> Option.map(~f=(e: Exp.t) =>
               switch (Exp.term_of(e)) {
               | Ap(_) => "application"
               | _ => print(e)
               }
             ),
        );
      },
    ),
    test_case(
      "an occurrence inside a folded function underlines the fold",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (fun x: Int -> x + 0) == (fun x: Int -> x) proof from x + 0 do algebra x end end; rewrite refl end in t",
          );
        let row = first(f.rows);
        let goal = Fixture.goal(f, row);
        let term = Web.CodeEditable.Model.get_statics(goal.editor).term;
        check(
          option(string),
          "a fold",
          Some("fold"),
          Picker.Goal.acts_on(goal, row)
          |> Option.bind(_, ~f=id => Exp.find_by_id(id, term))
          |> Option.map(~f=(e: Exp.t) =>
               switch (Exp.term_of(e)) {
               | Projector(_) => "fold"
               | _ => print(e)
               }
             ),
        );
      },
    ),
  ];
};

/* The page scrolls to the focused editor's caret after an update that
 * asks it to. A click in the stepper leaves that caret where it was, so
 * its updates mustn't ask, or the page jumps to a caret the user has
 * scrolled away from. */
/* What a write leaves for the next one: focus on the box where the proof
 * goes on, open on the same tab. */
module Writing = {
  /* A theorem's cell, and the zipper of its program, which patches are
   * written into. */
  type t = {
    zipper: Zipper.t,
    f: Fixture.t,
  };

  let check_zipper =
      (~pending=false, ~name="t", zipper: Zipper.t, cell): Fixture.t => {
    let statics =
      Fixture.statics_of_term(
        MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term,
      );
    Fixture.of_cell(
      ~name,
      statics,
      Fixture.calculate(~pending, ~is_edited=true, statics, cell),
    );
  };

  let mk = (src: string): t => {
    let zipper = Insertion.parse_zipper(src);
    {
      zipper,
      f: check_zipper(zipper, Web.EvalResult.Model.init),
    };
  };

  /* Writes `patch` and checks the program again, keeping the steppers'
   * state, as the page does. */
  let write = (~pending=false, w: t, patch): t => {
    let zipper = EditorTransform.apply_patch(w.zipper, patch);
    {
      zipper,
      f: check_zipper(~pending, zipper, w.f.cell),
    };
  };

  /* The row holding a box, the one a write from the box goes on at. */
  let box_row = (f: Fixture.t): Rows.t =>
    switch (
      List.filter(
        ~f=
          (row: Rows.t) =>
            switch (Stepper.Model.find(row.key, f.theorem.stepper)) {
            | Some({box: Some(_), _}) => true
            | _ => false
            },
        Fixture.all(f),
      )
    ) {
    | [row] => row
    | rows =>
      Alcotest.fail(string_of_int(List.length(rows)) ++ " rows with a box")
    };

  /* Takes the ghost at the box, as Tab does, giving the program after and
   * the key focus moves to. */
  let tab = (w: t): (t, Rows.key) => {
    let row = box_row(w.f);
    switch (Stepper.Update.ghost(Fixture.goal(w.f, row))) {
    | Some(ghost) =>
      let written = Picker.focused(ghost.pick);
      let next =
        switch (Stepper.Focus.after_write([written])) {
        | Some(next) => next
        | None => Alcotest.fail("no box to go on at")
        };
      let w = {
        ...w,
        f: Fixture.act(w.f, (row.key, Wrote(next))),
      };
      (write(w, Picker.Patches.write(row, written)), next);
    | None => Alcotest.fail("no ghost")
    };
  };

  let box = (f: Fixture.t): Stepper.Model.box =>
    switch (Fixture.local(f, box_row(f)).box) {
    | Some(box) => box
    | None => Alcotest.fail("no box")
    };

  /* Types `text` into the box and runs its search to the end. */
  let type_ = (f: Fixture.t, text: string): Fixture.t => {
    let key = box_row(f).key;
    let rec run = (f, slices) =>
      slices > 1000 || !Stepper.Model.running(box(f))
        ? f
        : run(
            Fixture.act(f, (key, SearchSlice(box(f).generation))),
            slices + 1,
          );
    run(Fixture.act(f, (key, BoxEditor(Perform(Insert(text))))), 0);
  };

  let tests = [
    test_case(
      "a search that finds nothing says so only once typing has paused",
      `Quick,
      () => {
        let f = mk("theorem t = (1 + 1) * (2 + 0) == 4 proof ? in t").f;
        let key = box_row(f).key;
        let verdict = f => Stepper.View.Box.verdict(box(f));
        let f = type_(f, "2");
        check(
          bool,
          "the search found nothing",
          true,
          Option.map(~f=(s: Web.StepSearch.t) => s.status, box(f).search)
          == Some(Web.StepSearch.NotFound),
        );
        check(
          bool,
          "while typing",
          true,
          verdict(f) == Some(Web.StepSearch.Searching),
        );
        let edits = box(f).edits;
        let f = type_(f, "2");
        let f = Fixture.act(f, (key, Paused(edits)));
        check(
          bool,
          "a pause after an earlier edit",
          true,
          verdict(f) == Some(Web.StepSearch.Searching),
        );
        let f = Fixture.act(f, (key, Paused(box(f).edits)));
        check(
          bool,
          "once paused",
          true,
          verdict(f) == Some(Web.StepSearch.NotFound),
        );
      },
    ),
    test_case(
      "a typo above a theorem keeps its stepper and its state until it is fixed",
      `Quick,
      () => {
        let w = mk("theorem t = (1 + 1) * (2 + 0) == 4 proof ? in t");
        let w = {
          ...w,
          f: type_(w.f, "4"),
        };
        let edit = (actions, w: t) => {
          let zipper = Test_Editing.perform(w.zipper, actions);
          {
            zipper,
            f: check_zipper(zipper, w.f.cell),
          };
        };
        let typo = [
          Action.Move(Start),
          ...List.map(
               ~f=c => Action.Insert(c),
               ["e", "t", " ", "z", " ", "=", " ", "1", " ", "i", "n", " "],
             ),
        ];
        let broken = edit(typo, w);
        check(bool, "stale", true, broken.f.theorem.stale != None);
        check(
          string,
          "the box keeps its text",
          "4",
          Printer.of_zipper(
            ~holes="?",
            box(broken.f).editor.editor.state.zipper,
          ),
        );
        let fixed =
          edit(
            List.init(12, ~f=_ => Action.Destruct(Local(Left, ByChar))),
            broken,
          );
        check(bool, "not stale", true, fixed.f.theorem.stale == None);
        check(
          string,
          "the box still has its text",
          "4",
          Printer.of_zipper(
            ~holes="?",
            box(fixed.f).editor.editor.state.zipper,
          ),
        );
      },
    ),
    test_case(
      "after each Tab, focus goes to the box where the proof goes on",
      `Quick,
      () => {
        let w = mk("theorem t = (1 + 1) * (2 + 0) == 4 proof ? in t");
        let (w, next) = tab(w);
        check(bool, "the first Tab's box", true, box_row(w.f).key == next);
        let (w, next) = tab(w);
        check(bool, "the second Tab's box", true, box_row(w.f).key == next);
        check(string, "its goal", "2 * 2 == 4", goal(box_row(w.f)));
      },
    ),
    test_case(
      "while a write is checked, its rows show the goal above and the box stays",
      `Quick,
      () => {
        let w = mk("theorem t = (1 + 1) * (2 + 0) == 4 proof ? in t");
        let row = box_row(w.f);
        let goal_before = goal(row);
        let written =
          switch (Stepper.Update.ghost(Fixture.goal(w.f, row))) {
          | Some(ghost) => Picker.focused(ghost.pick)
          | None => Alcotest.fail("no ghost")
          };
        let patch = Picker.Patches.write(row, written);
        let w = {
          ...w,
          f:
            Fixture.act(
              w.f,
              (
                row.key,
                Wrote(
                  Option.value_exn(Stepper.Focus.after_write([written])),
                ),
              ),
            ),
        };
        let checking = write(~pending=true, w, patch);
        let row = box_row(checking.f);
        let local = Fixture.local(checking.f, row);
        check(bool, "checking", true, local.checking);
        check(
          string,
          "the goal above",
          goal_before,
          print(local.goal.written),
        );
        check(
          bool,
          "the box offers nothing",
          true,
          Option.bind(local.box, ~f=(box: Stepper.Model.box) => box.ghost)
          == None,
        );
        let checked = write(w, patch);
        let local = Fixture.local(checked.f, box_row(checked.f));
        check(bool, "checked", false, local.checking);
        check(
          string,
          "its own goal",
          "(1 + 1) * 2 == 4",
          goal(box_row(checked.f)),
        );
        check(
          bool,
          "the box offers a step",
          true,
          Option.bind(local.box, ~f=(box: Stepper.Model.box) => box.ghost)
          != None,
        );
      },
    ),
    test_case(
      "a box written from on the Step tab is followed by one on the Step tab",
      `Quick,
      () => {
        let w = mk("theorem t = (1 + 1) * (2 + 0) == 4 proof ? in t");
        let w = {
          ...w,
          f: Fixture.act(w.f, (box_row(w.f).key, Tab(Step))),
        };
        let (w, _) = tab(w);
        switch (Fixture.local(w.f, box_row(w.f)).box) {
        | Some(box) =>
          check(bool, "on Step", true, box.tab == Some(Stepper.Model.Step))
        | None => Alcotest.fail("no box")
        };
      },
    ),
    test_case(
      "a write that leaves holes goes on at the first of them",
      `Quick,
      () => {
        let w = mk("theorem t = forall n : Int -> n == n proof ? in t");
        let row = box_row(w.f);
        let written =
          Proof.fresh(
            Forall(Pat.fresh(Var("n")), Proof.fresh(EmptyHole)),
          );
        let next = Stepper.Focus.after_write([written]);
        let w = write(w, Picker.Patches.write(row, written));
        check(bool, "the body's box", true, Some(box_row(w.f).key) == next);
      },
    ),
  ];
};

module Scrolling = {
  let not_always = (updated: Web.Updated.t(_)) =>
    switch (updated.scroll) {
    | Always => false
    | IfCaretVisible
    | Never => true
    };

  let tests = [
    test_case(
      "the stepper's patch to the cell's program doesn't scroll to its caret",
      `Quick,
      () => {
        let src = "theorem t = 1 + 1 == 2 proof ? in t";
        let z = Insertion.parse_zipper(src);
        let patch =
          EditorTransform.mk_proof_patch(
            ~target_id=Proof.rep_id(Insertion.theorem_proof(z)),
            Proof.fresh(EvalStep),
          );
        check(
          bool,
          "PatchMainEditor",
          true,
          not_always(
            Web.CellEditor.Update.update(
              ~settings=Web.Settings.Model.init,
              PatchMainEditor(patch),
              Web.CellEditor.Model.mk(Editor.Model.mk(~root=Exp, z)),
            ),
          ),
        );
      },
    ),
    test_case(
      "the cell stepper's toggle and patches don't scroll to a caret",
      `Quick,
      () => {
        let statics = Fixture.statics("1 + 2 + 3");
        let cell =
          Fixture.calculate(
            ~is_edited=true,
            statics,
            Web.EvalResult.Model.init,
          );
        let update = (action, cell) =>
          Web.EvalResult.Update.update(
            ~settings=Web.Settings.Model.init,
            action,
            cell,
          );
        let toggled = update(ToggleStepper, cell);
        check(bool, "ToggleStepper", true, not_always(toggled));
        let cell =
          Fixture.calculate(~is_edited=false, statics, toggled.model);
        let stepper =
          switch (cell.display) {
          | Stepper(s) => s
          | Evaluation(_) => Alcotest.fail("the stepper is off")
          };
        let row =
          last(
            Rows.chain(
              ~proof_map=stepper.proof_map,
              Web.EvalResult.CellStepper.proof(stepper),
            ),
          );
        check(
          bool,
          "StepperPatch",
          true,
          not_always(
            update(
              StepperPatch(
                Picker.Patches.write(row, Proof.fresh(EvalStep)),
              ),
              cell,
            ),
          ),
        );
      },
    ),
    test_case(
      "the write box's tabs and list don't scroll to a caret",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 1 + 1 == 2 proof ? in t");
        let row = first(f.rows);
        let update = action =>
          Stepper.Update.update(
            ~settings=Web.Settings.Model.init,
            (row.key, action),
            f.theorem.stepper,
          );
        List.iter(
          ~f=
            ((label, action)) =>
              check(bool, label, true, not_always(update(action))),
          [
            ("Tab", Stepper.Update.Tab(Strategy)),
            ("Choose", Choose(Some(0))),
            ("SearchSlice", SearchSlice(0)),
            ("Complete", Complete("refl")),
          ],
        );
      },
    ),
  ];
};

/* An offer writes a step that survives being printed into the program,
 * read back and checked again, and names definitions as the goal does. */
module Round_trip = {
  let len = "let len = fun xs -> case xs | [] => 0 | x :: t => 1 + len(t) end in ";

  let has_fun = (text: string): bool =>
    Test_ProofMap.contains(text, "fun ")
    || Test_ProofMap.contains(text, "fix ");

  /* `template` with its `%%` filled. */
  let fill = (template: string, text: string): string =>
    switch (String.split(template, ~on='%')) {
    | [before, "", after] => before ++ text ++ after
    | _ => Alcotest.fail("the template needs one %%")
    };

  /* The program with `step` written at `%%`, after checking that it
   * names no function bodies. */
  let write = (template: string, step: Proof.t): string => {
    let text = written(step);
    check(bool, "written without bodies: " ++ text, false, has_fun(text));
    fill(template, text);
  };

  let check_clean = (src: string) => {
    let (pm, proof) = Test_ProofMap.run_named("t", src);
    check(
      option(string),
      "reread and rechecked\n" ++ src ++ "\n" ++ Test_ProofMap.dump(pm, proof),
      None,
      Test_ProofMap.find_marked_sub(pm, proof)
      |> Option.map(~f=((_, marks)) =>
           marks |> List.map(~f=ProofMark.show) |> String.concat(~sep="; ")
         ),
    );
    (pm, proof);
  };

  let tests = [
    test_case(
      "Tab's steps, written in turn, prove the theorem",
      `Quick,
      () => {
        let template =
          len
          ++ "theorem t = forall y: Int -> forall ys: [Int] -> len(y :: ys) == 1 + len(ys) proof %% in t";
        let rec go = (steps, rounds) => {
          let src = fill(template, String.concat(~sep="; ", steps @ ["?"]));
          let f = Fixture.mk(src);
          let goal = Fixture.goal(f, last(f.rows));
          switch (Picker.Ghost.mk(goal, ~selected=None)) {
          | Some(ghost) when rounds > 0 =>
            let text = written(Picker.focused(ghost.pick));
            check(
              bool,
              "written without bodies: " ++ text,
              false,
              has_fun(text),
            );
            go(steps @ [text], rounds - 1);
          | _ => steps
          };
        };
        let steps = go([], 4);
        let src = fill(template, String.concat(~sep="; ", steps));
        let (pm, proof) = check_clean(src);
        check(
          string,
          "proven by " ++ String.concat(~sep="; ", steps),
          "Proven",
          ProofMap.show_full_status(
            ProofMap.full_status_of_proof(pm, proof),
          ),
        );
      },
    ),
  ];
};

/* Rows drawn again only when what they show changes. */
module Redrawing = {
  /* A row's goal editor drawn through Stepper.Drawn, as the stepper draws
   * it, keeping the handlers its node was drawn with. */
  let draw = (~focused=false, row: Rows.t, local: Stepper.Model.local, signal) => {
    let globals = Web.Globals.Model.init();
    let drawn_with = ref(None);
    let _: Virtual_dom.Vdom.Node.t =
      Stepper.Drawn.view(
        row.key,
        {
          editor: {
            editor: local.goal.editor,
            deco: local.deco,
            hints: true,
            replacing: false,
            previewing: false,
          },
          focused,
          settings: globals.settings,
          font_metrics: globals.font_metrics,
          meta_down: false,
          color_highlights: None,
        },
        {
          signal,
          inject: _ => Ui_effect.Ignore,
          keys: _ => None,
        },
        handlers => {
          drawn_with := Some(handlers);
          Virtual_dom.Vdom.Node.none;
        },
      );
    drawn_with^;
  };

  let tests = [
    test_case(
      "a row's goal is prepared again when a function its definitions call changes",
      `Quick,
      () => {
        let src = g =>
          "let g = fun x -> "
          ++ g
          ++ " in let f = fun x -> g(x) in theorem t = f(1) == 2 proof ? in t";
        /* Whether the goal checked with `g` defined as `before` is kept
         * when it is checked again with `g` defined as `after`. */
        let kept = (before, after) => {
          let row = first(Fixture.mk(src(after)).rows);
          switch (row.goal, Rows.ctx(row)) {
          | (Some(written), Some(ctx)) =>
            let f = Fixture.mk(src(before));
            Picker.Goal.is_for(
              Fixture.goal(f, first(f.rows)),
              ~ctx,
              written,
            );
          | _ => Alcotest.fail("the row has no goal")
          };
        };
        check(bool, "g the same", true, kept("x + 1", "x + 1"));
        check(bool, "g changed", false, kept("x + 1", "x + 2"));
      },
    ),
    test_case(
      "rows rebuilt from the same check keep their state's records",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = (1 + 1) * 2 == 4 proof from 1 + 1 do eval end in t",
          );
        let again =
          Stepper.Update.calculate(
            ~settings=Web.Theorems.with_proof_settings(settings),
            f.rows,
            f.theorem.stepper,
          );
        check(
          list(bool),
          "the same record",
          List.map(~f=_ => true, Fixture.all(f)),
          List.map(
            ~f=
              (row: Rows.t) =>
                switch (
                  Stepper.Model.find(row.key, f.theorem.stepper),
                  Stepper.Model.find(row.key, again),
                ) {
                | (Some(before), Some(after)) => phys_equal(before, after)
                | _ => false
                },
            Fixture.all(f),
          ),
        );
      },
    ),
    test_case(
      "a click on a row's reused node acts through the latest render",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = (1 + 1) * (2 + 0) == 4 proof ? in t");
        let row = last(f.rows);
        let local = Fixture.local(f, row);
        let taken = ref([]);
        let take = (render, event: Web.StepperEditor.View.event) =>
          switch (event) {
          | TakeStep(i) =>
            Ui_effect.of_sync_fun(
              () => taken := [(render, i), ...taken^],
              (),
            )
          | MakeActive
          | Refl(_) => Ui_effect.Ignore
          };
        let first =
          switch (draw(row, local, take("first"))) {
          | Some(handlers) => handlers
          | None => Alcotest.fail("the row wasn't drawn")
          };
        check(
          bool,
          "the second render reuses the node",
          true,
          Option.is_none(draw(row, local, take("second"))),
        );
        Virtual_dom.Vdom.Effect.Expert.handle_non_dom_event_exn(
          first.signal(TakeStep(0)),
        );
        check(
          list(pair(string, int)),
          "taken by",
          [("second", 0)],
          taken^,
        );
        check(
          bool,
          "a change of focus draws the row again",
          true,
          Option.is_some(draw(~focused=true, row, local, take("third"))),
        );
      },
    ),
  ];
};

let tests = (
  "StepperView",
  Walk.tests
  @ Scopes.tests
  @ State.tests
  @ Picking.tests
  @ Regressions.tests
  @ Display.tests
  @ Round_trip.tests
  @ Insertion.tests
  @ Structure.tests
  @ Focusing.tests
  @ Focusing.generalizing_tests
  @ Writing.tests
  @ Scrolling.tests
  @ Redrawing.tests,
);
