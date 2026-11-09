open Alcotest;
open Language;
open Util;
open Haz3lcore;

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
      |> List.find_opt(((_, thm: Web.Theorems.Model.theorem)) =>
           thm.name == name
         )
    ) {
    | Some((id, theorem)) => {
        statics,
        cell,
        id,
        theorem,
        rows: Rows.chain(~proof_map=theorems.proof_map, theorem.proof),
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

let kinds = (rows: list(Rows.t)) => List.map(kind, rows);

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
  | Step(Cases(cases)) => List.map(snd, cases)
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
    SemanticCtx.facts(ctx) |> List.map(((name, _, e)) => (name, print(e)))
  | None => Alcotest.fail("the row has no scope")
  };

/* The offered eval steps' displayed nodes, printed. */
let offered = (goal: Picker.Goal.t): list(string) =>
  Picker.Goal.step_ids(goal)
  |> List.filter_map(id => Exp.find_by_id(id, goal.display))
  |> List.map(print);

let labels = (offers: list(Picker.offer)) =>
  List.map((o: Picker.offer) => o.label, offers);

let written = (term: TermBase.Proof.term): string =>
  Proof(Proof.fresh(term))
  |> ExpToSegment.any_to_segment(
       ~settings=ExpToSegment.Settings.editable(~inline=true),
     )
  |> Printer.of_segment(~holes="?", ~refractors=[]);

let write_of = (offers, label): TermBase.Proof.term =>
  switch (List.find_opt((o: Picker.offer) => o.label == label, offers)) {
  | Some({action: Write(term), _}) => term
  | Some({action: Open(_), _}) => Alcotest.fail(label ++ " opens a box")
  | None => Alcotest.fail("no " ++ label ++ " offered")
  };

/* Every tile id in a segment, children and folded projector interiors
 * included. A repeated id makes `Highlight.of_tile` raise a shard
 * mismatch. */
let rec segment_tile_ids = (seg: Segment.t): list(Id.t) =>
  List.concat_map(
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
  let seen = Hashtbl.create(64);
  List.filter(
    id =>
      switch (Hashtbl.find_opt(seen, id)) {
      | Some () => true
      | None =>
        Hashtbl.add(seen, id, ());
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
          Fixture.mk("theorem t = 1 + 4 == 5 proof eval 1 + 4 at 0 end in t");
        check(list(string), "rows", ["eval", "end"], kinds(f.rows));
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
            "theorem t = 1 + 4 == 5 proof eval 1 + 4 at 0 end; ? in t",
          );
        check(list(string), "rows", ["eval", "hole"], kinds(f.rows));
        check(string, "the hole's goal", "5 == 5", goal(last(f.rows)));
      },
    ),
    test_case(
      "a hole mid-chain passes its goal on",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 + 4 == 5 proof ?; eval 1 + 4 at 0 end in t",
          );
        check(
          list(string),
          "rows",
          ["hole", "eval", "end"],
          kinds(f.rows),
        );
        check(
          string,
          "the step's goal",
          "1 + 4 == 5",
          goal(List.nth(f.rows, 1)),
        );
      },
    ),
    test_case(
      "a step inserted before another heads the chain before the reparse",
      `Quick,
      () => {
        let step = s =>
          Proof.fresh(
            EvalStep({
              at_idx: parse_exp("0"),
              at_exp: parse_exp(s),
            }),
          );
        let (a, b, c) = (step("1"), step("2"), step("3"));
        let (head, tail) =
          Rows.split(Proof.fresh(Seq(Proof.fresh(Seq(a, b)), c)));
        check(bool, "the head is the inserted step", true, head === a);
        switch (tail) {
        | Some({term: Seq(b', c'), _}) =>
          check(bool, "then the old chain", true, b' === b && c' === c)
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
            "theorem t = 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end in t",
          );
        check(list(string), "rows", ["refl_eq", "end"], kinds(f.rows));
        check(string, "it proves the goal", "true", goal(last(f.rows)));
      },
    ),
    test_case(
      "an induction row nests each case's rows",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall (b : Bool) -> b || false == b proof induction b | true => ? | false => ? end in t",
          );
        let cases = cases(first(f.rows));
        check(int, "two cases", 2, List.length(cases));
        check(
          list(string),
          "each case's goal has its pattern for the scrutinee",
          ["true || false == true", "false || false == false"],
          List.map(rows => goal(first(rows)), cases),
        );
        check(
          bool,
          "each case's scope has its case equation",
          true,
          List.for_all(
            rows => List.mem_assoc("case_eq", facts(first(rows))),
            cases,
          ),
        );
      },
    ),
    test_case(
      "a step that doesn't apply is broken, and offers its repairs",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = 1 + 4 == 5 proof axiom refl_eq at 0 on 1 + 4 == 5 end in t",
          );
        let row = first(f.rows);
        switch (Rows.Status.of_row(row)) {
        | Broken(RuleDoesNotApply(_)) => ()
        | status =>
          Alcotest.fail(
            "status: "
            ++ (
              Rows.Status.broken_mark(status)
              |> Option.fold(~none="not broken", ~some=ProofMark.show)
            ),
          )
        };
        check(
          bool,
          "it can be flipped",
          true,
          Picker.Patches.flip_direction(row.step) != None,
        );
      },
    ),
  ];
};

/* A form's body, and a have's premises, are chains of their own, with
 * the goal and scope the checker gave them. */
module Scopes = {
  let assume_src = {|theorem t = forall x -> x == 1 proof assume x == 1 => axiom assume at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
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
          ["Assume", "end"],
          kinds(f.rows),
        );
        check(
          list(string),
          "body rows",
          ["assume", "refl_eq", "end"],
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
          List.assoc_opt("assume", facts(first(body(first(f.rows))))),
        );
        check(
          bool,
          "and the assume itself is checked without it",
          false,
          List.mem_assoc("assume", facts(first(f.rows))),
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
        check(string, "the row", "Revert", kind(revert));
        let hole = first(body(revert));
        check(string, "the body's goal", "x == 1 ==> x == 1", goal(hole));
        check(
          bool,
          "the reverted fact is still citable",
          true,
          List.mem_assoc("assume", facts(hole)),
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
          List.mem_assoc("assume", facts(generalize)),
        );
        let hole = first(body(generalize));
        switch (hole.goal |> Option.map(Exp.term_of)) {
        | Some(Forall(_)) => ()
        | _ => Alcotest.fail("the body's goal is not re-quantified")
        };
        check(
          bool,
          "and gone inside it",
          false,
          List.mem_assoc("assume", facts(hole)),
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
          "a picker row: it has state",
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
            {|theorem t = forall x -> x == 1 proof have 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end => ? in t|},
          );
        let (sub, _) = premises(first(f.rows));
        check(
          list(string),
          "the subproof's rows",
          ["refl_eq", "end"],
          kinds(sub),
        );
        check(
          option(string),
          "it reaches true",
          Some("true"),
          Rows.reached(sub) |> Option.map(print),
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
          List.assoc_opt("have", facts(first(body))),
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
          |> List.filter(((name, _)) => name == "have")
          |> List.map(snd);
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
            "let f = fun x where x != 0 -> 100 / x in theorem t = forall y -> f(y) == f(y) proof have y != 0 proof ? => ? in t",
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
module State = {
  let tests = [
    test_case(
      "a recalculation keeps each row's editor while its goal is unchanged",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => eval 1 + 1 at 0 end in t|},
          );
        let f = Fixture.act(f, (first(f.rows).key, Goal(Move(Start))));
        let again =
          Stepper.Update.calculate(~settings, f.rows, f.theorem.stepper);
        List.iter(
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
                before.goal.editor === after.goal.editor,
              )
            | (None, None) => ()
            | _ => Alcotest.fail(kind(row) ++ ": state came or went")
            },
          Fixture.all(f),
        );
      },
    ),
    test_case(
      "a changed goal is prepared again, keeping the row's open box",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 1 + 2 == 3 proof ? in t");
        let row = first(f.rows);
        let f = Fixture.act(f, (row.key, Toggle(Strategies)));
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
          check(bool, "the box is still open", true, local.box == Strategies);
        | None => Alcotest.fail("the row lost its state")
        };
      },
    ),
    test_case(
      "a row's state goes when its step does",
      `Quick,
      () => {
        let f =
          Fixture.mk("theorem t = 1 + 4 == 5 proof eval 1 + 4 at 0 end in t");
        let other = Fixture.mk("theorem t = 1 + 4 == 5 proof ? in t");
        let model =
          Stepper.Update.calculate(~settings, other.rows, f.theorem.stepper);
        check(
          bool,
          "none of the old rows has state",
          true,
          List.for_all(
            (row: Rows.t) => Stepper.Model.find(row.key, model) == None,
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
        check(list(string), "rows", ["Assume", "end"], kinds(f.rows));
        check(bool, "not checked yet", true, first(f.rows).goal == None);
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
          |> List.map(((_, thm: Web.Theorems.Model.theorem)) => thm.name),
        );
      },
    ),
  ];
};

/* What the picker offers, as a function of the goal and the selection. */
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
        goal.display,
      );
    switch (found^) {
    | Some(e) => e
    | None => Alcotest.fail("no " ++ printed ++ " in the goal")
    };
  };

  let strategy_labels = (strategies: list(Picker.strategy)) =>
    List.map((s: Picker.strategy) => s.label, strategies);

  /* The fact names a strategy cites, with the step each writes. */
  let cited = (strategies: list(Picker.strategy), label) =>
    switch (
      List.find_opt((s: Picker.strategy) => s.label == label, strategies)
    ) {
    | Some({picks: Cites(facts), _}) =>
      List.map(((name, term)) => (name, written(term)), facts)
    | Some({picks: Writes(_), _}) => Alcotest.fail(label ++ " cites no fact")
    | None => Alcotest.fail("no " ++ label ++ " strategy")
    };

  let equation = "theorem t = 1 + 2 + 3 == 6 proof ? in t";

  let tests = [
    test_case("with nothing selected, only Strategies", `Quick, () =>
      check(
        list(string),
        "offers",
        ["Strategies ▼"],
        labels(Picker.offers(root_goal(equation), ~selected=None)),
      )
    ),
    test_case(
      "a selection with no step offers Rewrite and Strategies",
      `Quick,
      () => {
        let goal = root_goal(equation);
        check(
          list(string),
          "offers",
          ["Rewrite ▼", "Strategies ▼"],
          labels(Picker.offers(goal, ~selected=Some(node(goal, "6")))),
        );
      },
    ),
    test_case(
      "selecting a redex offers Step, writing the checker's eval step",
      `Quick,
      () => {
        let goal = root_goal(equation);
        let offers =
          Picker.offers(goal, ~selected=Some(node(goal, "1 + 2")));
        check(
          list(string),
          "offers",
          ["Step", "Rewrite ▼", "Strategies ▼"],
          labels(offers),
        );
        check(
          string,
          "the step",
          "eval 1 + 2 at 0 end",
          written(write_of(offers, "Step")),
        );
      },
    ),
    test_case(
      "selecting an equality offers Reflexivity",
      `Quick,
      () => {
        let goal =
          root_goal("theorem t = forall (x : Int) -> x == x proof ? in t");
        let offers = Picker.offers(goal, ~selected=Some(goal.display));
        check(
          string,
          "the step",
          "axiom refl_eq at 0 on x == x end",
          written(write_of(offers, "Reflexivity")),
        );
      },
    ),
    test_case(
      "a function equality offers Funext, named after the lambda's binder",
      `Quick,
      () => {
        let goal =
          root_goal(
            "theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof ? in t",
          );
        let offers = Picker.offers(goal, ~selected=None);
        check(
          string,
          "the step",
          "funext x => ?",
          written(write_of(offers, "Funext")),
        );
      },
    ),
    test_case(
      "only an implication offers Assume, assuming its antecedent",
      `Quick,
      () => {
        let goal =
          root_goal(
            "theorem t = forall x -> x == 2 ==> x + 1 == 3 proof ? in t",
          );
        let offers = Picker.offers(goal, ~selected=None);
        check(
          list(string),
          "offers",
          ["Assume", "Strategies ▼"],
          labels(offers),
        );
        check(
          string,
          "the step",
          "assume x == 2 => ?",
          written(write_of(offers, "Assume")),
        );
      },
    ),
    test_case(
      "every button says what it writes",
      `Quick,
      () => {
        let goal =
          root_goal(
            "theorem t = forall x -> x == 2 ==> x + 1 == 3 proof ? in t",
          );
        let (common, rare) =
          Picker.strategies(goal, ~selected=Some(node(goal, "x + 1")));
        check(
          list(string),
          "without a tooltip",
          [],
          List.map(
            (o: Picker.offer) => (o.label, o.tooltip),
            Picker.offers(goal, ~selected=Some(node(goal, "x + 1"))),
          )
          @ List.map(
              (s: Picker.strategy) => (s.label, s.tooltip),
              common @ rare,
            )
          @ List.map(
              (s: Picker.strategy) => (s.label, s.explanation),
              common @ rare,
            )
          |> List.filter(((_, text)) => text == "")
          |> List.map(fst),
        );
      },
    ),
    test_case(
      "Rewrite lists the rules that rewrite the selection, by source",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall x -> x == 1 ==> x + 0 == 1 proof assume x == 1 => ? in t",
          );
        let goal = Fixture.goal(f, first(body(first(f.rows))));
        let sections = (~filter="", selected) =>
          Picker.rules(goal, ~filter, ~selected)
          |> List.map(({heading, rules}: Picker.section) =>
               (heading, List.map((e: ProofCtx.entry) => e.name, rules))
             );
        let names = (~filter="", selected) =>
          sections(~filter, selected) |> List.concat_map(snd);
        check(
          option(list(string)),
          "the hypothesis rewrites x, under the scope's facts",
          Some(["assume"]),
          List.assoc_opt(
            "Facts and theorems",
            sections(Some(node(goal, "x"))),
          ),
        );
        check(
          bool,
          "but not 0",
          false,
          List.mem("assume", names(Some(node(goal, "0")))),
        );
        check(
          bool,
          "a search lists by name, whatever the selection",
          true,
          List.mem("assume", names(~filter="assu", None)),
        );
        check(
          list(string),
          "the builtin axioms have a section of their own",
          ["Axioms"],
          sections(~filter="refl", None) |> List.map(fst),
        );
      },
    ),
    test_case(
      "Strategies: Contradiction and Revert cite the scope's facts, not the axioms",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "theorem t = forall x -> x == 1 ==> x == 1 proof assume x == 1 => ? in t",
          );
        let goal = Fixture.goal(f, first(body(first(f.rows))));
        let (common, rare) = Picker.strategies(goal, ~selected=None);
        check(
          list(string),
          "strategies",
          ["Have", "Contradiction", "Cases/Induction"],
          strategy_labels(common),
        );
        check(
          list(string),
          "rarely needed",
          ["Revert", "Generalize"],
          strategy_labels(rare),
        );
        check(
          list(pair(string, string)),
          "contradiction",
          [("assume", "contradiction x == 1 end")],
          cited(common, "Contradiction"),
        );
        check(
          list(pair(string, string)),
          "revert",
          [("assume", "revert x == 1 => ?")],
          cited(rare, "Revert"),
        );
      },
    ),
  ];
};

/* A row's selection reaches its picker, and the cell stepper advances. */
module Regressions = {
  /* The id and measured range of the offered step at `printed`. */
  let step_range = (goal: Picker.Goal.t, printed: string) => {
    let id =
      switch (
        Picker.Goal.step_ids(goal)
        |> List.find_opt(id =>
             Option.map(print, Exp.find_by_id(id, goal.display))
             == Some(printed)
           )
      ) {
      | Some(id) => id
      | None => Alcotest.fail("no step offered at " ++ printed)
      };
    let syntax = goal.editor.editor.syntax;
    switch (
      TermData.extreme_measures(
        id,
        syntax.term_data,
        CachedSyntax.measured(syntax),
      )
    ) {
    | Some(range) => (id, range)
    | None => Alcotest.fail("the step has no measured range")
    };
  };

  let tests = [
    test_case(
      "theorem stepper: a redex selected in a row offers Step, a second click takes it, and Algebra shows it",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 1 + 2 + 3 == 6 proof ? in t");
        let row = first(f.rows);
        let (id, range) = step_range(Fixture.goal(f, row), "1 + 2");
        /* The first click on an underline selects it (StepperEditor.deco). */
        let f =
          Fixture.act(f, (row.key, Goal(Select(PointToPoint(range)))));
        let goal = Fixture.goal(f, row);
        let selected = Picker.Goal.selected(goal);
        check(
          option(string),
          "the selection",
          Some("1 + 2"),
          Option.map(print, selected),
        );
        let offers = Picker.offers(goal, ~selected);
        check(
          string,
          "Step",
          "eval 1 + 2 at 0 end",
          written(write_of(offers, "Step")),
        );
        /* The second click takes the step when the underline is the
         * selection. */
        check(
          option(Alcotest.testable(Id.pp, Id.equal)),
          "the underline is the selection",
          Some(id),
          Option.map(Exp.rep_id, selected),
        );
        let f = Fixture.act(f, (row.key, Toggle(Rewrite)));
        check(
          option(string),
          "the Algebra box replaces the selection",
          Some("1 + 2"),
          Picker.Goal.selected(Fixture.goal(f, row)) |> Option.map(print),
        );
      },
    ),
    test_case(
      "cell stepper: taking a step advances to the next goal",
      `Quick,
      () => {
        let statics = Fixture.statics("1 + 2 + 3");
        let cell =
          Fixture.calculate(
            ~is_edited=true,
            statics,
            Web.EvalResult.Model.init,
          );
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
        /* Takes the only step the last row offers. */
        let step = cell => {
          let row = last(rows(cell));
          let goal =
            switch (Stepper.Model.find(row.key, stepper(cell).stepper)) {
            | Some(local) => local.goal
            | None => Alcotest.fail("the last row has no state")
            };
          switch (Picker.take_step(goal, 0)) {
          | Some(term) =>
            update(
              StepperPatch(Picker.Patches.write(row, Proof.fresh(term))),
              cell,
            )
          | None => Alcotest.fail("no step offered")
          };
        };
        let cell = update(ToggleStepper, cell);
        check(
          list(string),
          "first",
          ["1 + 2 + 3"],
          List.map(goal, rows(cell)),
        );
        let cell = step(cell);
        check(
          list(string),
          "after one step",
          ["1 + 2 + 3", "3 + 3"],
          List.map(goal, rows(cell)),
        );
        let cell = step(cell);
        check(
          list(string),
          "after two",
          ["1 + 2 + 3", "3 + 3", "6"],
          List.map(goal, rows(cell)),
        );
      },
    ),
  ];
};

/* What the picker writes into the proof text, and that it reads back. A
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
      i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
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
        let src = "theorem t = 1 + 4 == 5 proof ?; eval 1 + 4 at 0 end in t";
        let z = parse_zipper(src);
        let proof_map = Fixture.mk(src).cell.theorems.proof_map;
        let rows = Rows.chain(~proof_map, theorem_proof(z));
        check(list(string), "rows", ["hole", "eval", "end"], kinds(rows));
        /* The chain after picking `eval 2 + 2 at 0 end` on `row`: each
         * row's hole or eval target. */
        let pick = row => {
          let term =
            Picker.Terms.eval_step(~at_idx=0, ~at_exp=parse_exp("2 + 2"));
          EditorTransform.apply_patch(
            z,
            Picker.Patches.write(row, Proof.fresh(term)),
          )
          |> theorem_proof
          |> Rows.chain(~proof_map=ProofMap.empty)
          |> List.filter_map((row: Rows.t) =>
               switch (row.kind, row.step.term) {
               | (Hole, _) => Some("?")
               | (Step(_), EvalStep({at_exp, _})) => Some(print(at_exp))
               | _ => None
               }
             );
        };
        check(
          list(string),
          "on the hole",
          ["2 + 2", "1 + 4"],
          pick(List.nth(rows, 0)),
        );
        check(
          list(string),
          "on the step",
          ["?", "2 + 2", "1 + 4"],
          pick(List.nth(rows, 1)),
        );
        check(
          list(string),
          "on the end row",
          ["?", "1 + 4", "2 + 2"],
          pick(List.nth(rows, 2)),
        );
      },
    ),
  ];
};

/* Goals as rows show them: env-substituted for display, as written for
 * the eval steps. */
module Display = {
  let sum = {|let sum(xs) = case xs
| [] => 0
| x::xs => x + sum(xs)
end
in
|};

  let rec has = (pred, e: Exp.t): bool =>
    pred(e) || List.exists(has(pred), Occurrence.children(e));

  let is_fun = (e: Exp.t) =>
    switch (e.term) {
    | Fun(_)
    | FixF(_)
    | Closure(_) => true
    | _ => false
    };

  let tests = [
    test_case(
      "the row after a rewrite at an env-inlined index shows the rewritten goal",
      `Quick,
      () => {
        /* `at 2` names the literal `0` of `sum == fold_left(_, sum, 0)`
         * counted in the displayed goal: the two before it are in `sum`'s
         * inlined body. */
        let f =
          Fixture.mk(
            sum
            ++ "theorem t = sum == fold_left(_, sum, 0) proof rewrite 0 with 0 + 0 at 2 end in t",
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
      "a row shows its goal with the env substituted",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "let f = fun x where x != 0 -> 100 / x in theorem t = forall y where y != 0 -> f(y) == f(y) proof ? in t",
          );
        let goal = Fixture.goal(f, first(f.rows));
        check(string, "as written", "f(y) == f(y)", print(goal.written));
        check(
          bool,
          "displayed with f inlined",
          true,
          has(is_fun, goal.display),
        );
      },
    ),
    test_case(
      "an end row shows the outgoing goal with the env substituted",
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
        check(
          bool,
          "displayed with f inlined",
          true,
          has(is_fun, goal.display),
        );
      },
    ),
    test_case(
      "inlined recursive applications still offer eval steps",
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
            ++ "theorem t = sum([]) == fold_left(_, sum, 0)([]) proof eval fold_left(_, sum, 0)([]) at 0 end in t",
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
          duplicated(ids) |> List.map(Id.to_string),
        );
        check(
          list(string),
          "underlined ids that are not unique",
          [],
          Picker.Goal.step_ids(goal)
          |> List.filter(id =>
               List.length(List.filter(Id.equal(id), ids)) > 1
             )
          |> List.map(Id.to_string),
        );
      },
    ),
    test_case(
      "a funext body still offers eval of an applied recursive function",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            sum
            ++ "theorem t = sum == fold_left(_, sum, 0) proof funext xs => ? in t",
          );
        let goal = Fixture.goal(f, first(body(first(f.rows))));
        check(
          bool,
          "the pointwise goal is displayed with sum inlined",
          true,
          has(
            e =>
              switch (e.term) {
              | FixF(_) => true
              | _ => false
              },
            goal.display,
          ),
        );
        check(bool, "steps offered", true, offered(goal) != []);
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
  @ Insertion.tests,
);
