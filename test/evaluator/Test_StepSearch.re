open Alcotest;
open Language;
open Haz3lcore;

module Fixture = Test_StepperView.Fixture;
module Picker = Web.StepperPicker;
module Search = Web.StepSearch;

let first = Test_StepperView.first;
let body = Test_StepperView.body;
let written = Test_StepperView.written;

/* The goal of the theorem's first row, or of its first row inside the
 * first row's body. */
let goal = (~inside=false, src) => {
  let f = Fixture.mk(src);
  let row = first(f.rows);
  Fixture.goal(f, inside ? first(body(row)) : row);
};

/* `text` typed into the row's write box: elaborated in the goal's scope,
 * as the box does. */
let typed = (goal: Picker.Goal.t, text: string): Exp.t =>
  switch (Parser.to_zipper(~root=Exp, text)) {
  | Some(z) =>
    Web.CodeEditable.Model.mk(Editor.Model.mk(z, ~root=Exp))
    |> Web.StepperView.Update.calculate_box(
         ~settings=Test_StepperView.settings,
         goal,
       )
    |> Web.CodeEditable.Model.get_statics
    |> (statics => statics.elaborated)
  | None => Alcotest.fail("can't parse " ++ text)
  };

let no_algebra = (_, _) => false;

let search = (~algebra=no_algebra, goal, text) =>
  Search.run(
    ~continue_=() => true,
    ~algebra,
    goal,
    Search.start(goal, ~wanted=typed(goal, text)),
  );

/* The steps found, each as written, or None if there are none. */
let found = (s: Search.t): option(list(string)) =>
  switch (s.status) {
  | Found(steps) => Some(List.map(written, steps))
  | NotFound => None
  | Searching => Alcotest.fail("still searching")
  };

let steps = option(list(string));

let arithmetic = "theorem t = (1 + 1) * 2 == 4 proof ? in t";

let tests = (
  "StepSearch",
  [
    test_case("a typed negative number is the number a step gives", `Quick, () =>
      check(
        steps,
        "steps",
        Some(["from 0 - 5 do eval end"]),
        found(
          search(
            goal("theorem t = 0 - 5 + 2 == 0 - 3 proof ? in t"),
            "-5 + 2 == 0 - 3",
          ),
        ),
      )
    ),
    test_case(
      "at a hole with a step after it, a write keeps the hole unless that step still checks",
      `Quick,
      () => {
        /* `from 1 + 4 do eval end`, written in the hole. */
        let step =
          Proof.fresh(
            Picker.Terms.focus(
              ~selector=Test_Evaluator_Prelude.parse_exp("1 + 4"),
              ~index=0,
              Proof.fresh(EvalStep),
            ),
          );
        let bridged = src => {
          let f = Fixture.mk(src);
          let row = first(f.rows);
          Search.Bridge.steps(
            ~algebra=no_algebra,
            ~proof=f.theorem.proof,
            Fixture.goal(f, row),
            row,
            [step],
          )
          |> List.map(written);
        };
        check(
          list(string),
          "the step after still checks",
          ["from 1 + 4 do eval end"],
          bridged("theorem t = 1 + 4 == 5 proof ?; eval in t"),
        );
        check(
          list(string),
          "it doesn't, and nothing leads back",
          ["from 1 + 4 do eval end", "?"],
          bridged(
            "theorem t = 1 + 4 == 5 proof ?; from 1 + 4 do eval end in t",
          ),
        );
      },
    ),
    test_case("the algebra check takes a call as an unknown", `Quick, () =>
      check(
        bool,
        "a part with a call is arithmetic",
        true,
        Search.arithmetic(Test_Evaluator_Prelude.parse_exp("0 + len(ys)")),
      )
    ),
    test_case(
      "one eval step reaches the typed goal, written where the checker takes it",
      `Quick,
      () => {
        let g = goal(arithmetic);
        check(
          steps,
          "steps",
          Some(["from (1 + 1) do eval end"]),
          found(search(g, "2 * 2 == 4")),
        );
      },
    ),
    test_case(
      "the fewest steps reaching it, one row each",
      `Quick,
      () => {
        let g = goal(arithmetic);
        check(
          steps,
          "two",
          Some(["from (1 + 1) do eval end", "from 2 * 2 do eval end"]),
          found(search(g, "4 == 4")),
        );
        check(
          steps,
          "to true",
          Some([
            "from (1 + 1) do eval end",
            "from 2 * 2 do eval end",
            "eval",
          ]),
          found(search(g, "true")),
        );
      },
    ),
    test_case(
      "a goal no step reaches is not found, and the goal itself takes no steps",
      `Quick,
      () => {
        let g = goal(arithmetic);
        check(steps, "unreachable", None, found(search(g, "5 == 5")));
        check(
          steps,
          "itself",
          Some([]),
          found(search(g, "(1 + 1) * 2 == 4")),
        );
      },
    ),
    test_case(
      "a fact rewrites where it applies, in either direction",
      `Quick,
      () => {
        let g =
          goal(
            ~inside=true,
            "theorem t = forall x -> x == 1 ==> x + 0 == 1 proof assume x == 1 => ? in t",
          );
        check(
          steps,
          "forward",
          Some(["from x do axiom assume end end"]),
          found(search(g, "1 + 0 == 1")),
        );
        check(
          steps,
          "backward",
          Some(["from 1 do axiomrev assume end end"]),
          found(search(g, "x + 0 == x")),
        );
      },
    ),
    test_case(
      "a rule that only wraps a part, as `axiomrev not_not` turns b into !!b, isn't tried",
      `Quick,
      () =>
      check(
        steps,
        "steps",
        None,
        found(search(goal(arithmetic), "!!((1 + 1) * 2 == 4)")),
      )
    ),
    test_case(
      "a theorem in scope rewrites to the typed goal, whose definitions inline with binders renamed",
      `Quick,
      () => {
        let f =
          Fixture.mk(
            "let rev = fun xs -> case xs | [] => [] | y :: ys => rev(ys) @ [y] end in theorem rev_app = forall (xs : [Int]) -> forall (ys : [Int]) -> rev(xs @ ys) == rev(ys) @ rev(xs) proof ? in theorem t = forall (xs : [Int]) -> forall (ys : [Int]) -> rev(xs @ ys) == rev(ys) @ rev(xs) proof ? in t",
          );
        check(
          steps,
          "steps",
          Some(["from rev(xs @ ys) do axiom rev_app end end"]),
          found(
            search(
              Fixture.goal(f, first(f.rows)),
              "rev(ys) @ rev(xs) == rev(ys) @ rev(xs)",
            ),
          ),
        );
      },
    ),
    test_case(
      "reflexivity closes an equation of equals",
      `Quick,
      () => {
        let g =
          goal(
            "let f = fun x -> x in theorem t = forall (xs : [Int]) -> f(xs) == f(xs) proof ? in t",
          );
        check(
          steps,
          "steps",
          Some(["axiom refl_eq end"]),
          found(search(g, "true")),
        );
      },
    ),
    test_case(
      "an arithmetic part the algebra check equates is rewritten to what was typed",
      `Quick,
      () => {
        let g =
          goal("theorem t = forall (x : Int) -> x + x == 2 * x proof ? in t");
        let algebra = (a, b) =>
          Test_ProofMap.print_exp(a) == "x + x"
          && Test_ProofMap.print_exp(b) == "2 * x";
        check(
          steps,
          "steps",
          Some(["from x + x do rewrite 2 * x end end"]),
          found(search(~algebra, g, "2 * x == 2 * x")),
        );
        check(
          steps,
          "not without it",
          None,
          found(search(g, "2 * x == 2 * x")),
        );
      },
    ),
    test_case(
      "typing into a row's box starts a search, its slices find the steps, and they are written one row each",
      `Quick,
      () => {
        let f = Fixture.mk(arithmetic);
        let row = first(f.rows);
        let f =
          Fixture.act(f, (row.key, BoxEditor(Perform(Paste("4 == 4")))));
        let box = (f: Fixture.t) =>
          switch (Fixture.local(f, row).box) {
          | Some(box) => box
          | None => Alcotest.fail("no box")
          };
        let rec slices = (n, f: Fixture.t) =>
          switch (box(f).search) {
          | Some({status: Searching, _}) when n < 100 =>
            slices(
              n + 1,
              Fixture.act(f, (row.key, SearchSlice(box(f).generation))),
            )
          | Some(search) => search
          | None => Alcotest.fail("no search started")
          };
        let search = slices(0, f);
        check(
          steps,
          "found",
          Some(["from (1 + 1) do eval end", "from 2 * 2 do eval end"]),
          found(search),
        );
        switch (search.status) {
        | Found(found) =>
          switch (Picker.write_steps(row, found)) {
          | Some(ProofPatch({replacement, _})) =>
            check(
              string,
              "in place of the hole",
              "from (1 + 1) do eval end; from 2 * 2 do eval end",
              written(replacement),
            )
          | _ => Alcotest.fail("not a proof patch")
          }
        | Searching
        | NotFound => ()
        };
      },
    ),
    test_case(
      "a rule named in full lists each of its uses, in each direction, with the goal it gives",
      `Quick,
      () => {
        let g =
          goal(
            ~inside=true,
            "theorem t = forall x -> x == 1 ==> x + 0 == 1 proof assume x == 1 => ? in t",
          );
        let suggestions =
          Search.Suggestions.run(
            ~continue_=() => true,
            g,
            Search.Suggestions.start(~rule=Some("assume"), ~at=None),
          );
        check(
          list(pair(string, string)),
          "uses",
          [
            ("from x do axiom assume end end", "1 + 0 == 1"),
            ("from 1 do axiomrev assume end end", "x + 0 == x"),
          ],
          List.map(
            (use: Search.Suggestions.suggestion) =>
              (written(use.step), Test_ProofMap.print_exp(use.result)),
            suggestions.found,
          ),
        );
        check(
          bool,
          "listed",
          true,
          List.length(
            Search.items(g, ~typed_name=Some("assume"), Some(suggestions)),
          )
          == 2,
        );
      },
    ),
    test_case(
      "typing the start of a name lists the names in scope it completes to",
      `Quick,
      () => {
        let g =
          goal(
            ~inside=true,
            "theorem t = forall x -> x == 1 ==> x + 0 == 1 proof assume x == 1 => ? in t",
          );
        let completions = start =>
          Search.items(g, ~typed_name=Some(start), None)
          |> List.filter_map((item: Search.item) =>
               switch (item) {
               | Complete(name) => Some(name)
               | Use(_) => None
               }
             );
        check(list(string), "ass", ["assume"], completions("ass"));
        check(
          list(string),
          "the axioms too",
          ["nonzero_of_neg", "nonzero_of_pos", "nonzero_mul", "not_not"],
          completions("no"),
        );
      },
    ),
    test_case(
      "with part of the goal selected, the box's text is that part's replacement, and the uses listed are inside it",
      `Quick,
      () => {
        let f = Fixture.mk("theorem t = 2 * (1 + 1) == 4 proof ? in t");
        let row = first(f.rows);
        let part =
          Test_StepperView.Picking.node(Fixture.goal(f, row), "(1 + 1)");
        let f =
          Fixture.act(
            f,
            (row.key, Goal(Select(Term(Id(Exp.rep_id(part), Right))))),
          );
        let f = Fixture.act(f, (row.key, BoxEditor(Perform(Paste("2")))));
        let box = (f: Fixture.t) =>
          switch (Fixture.local(f, row).box) {
          | Some(box) => box
          | None => Alcotest.fail("no box")
          };
        let rec slices = (n, f: Fixture.t) =>
          Web.StepperView.Model.running(box(f)) && n < 100
            ? slices(
                n + 1,
                Fixture.act(f, (row.key, SearchSlice(box(f).generation))),
              )
            : box(f);
        let box = slices(0, f);
        check(
          steps,
          "found",
          Some(["from (1 + 1) do eval end"]),
          Option.bind(box.search, s => found(s)),
        );
        check(
          bool,
          "the uses are looked for inside the part",
          true,
          Option.bind(box.suggestions, (s: Search.Suggestions.t) => s.at)
          != None,
        );
      },
    ),
    test_case(
      "run in slices, the search finds what it finds in one go",
      `Quick,
      () => {
        let g = goal(arithmetic);
        let wanted = typed(g, "true");
        /* One candidate per slice. */
        let rec slices = (n, s: Search.t) =>
          switch (s.status) {
          | Searching =>
            let budget = ref(1);
            slices(
              n + 1,
              Search.run(
                ~continue_=
                  () => {
                    budget := budget^ - 1;
                    budget^ >= 0;
                  },
                ~algebra=no_algebra,
                g,
                s,
              ),
            );
          | Found(_)
          | NotFound => (n, s)
          };
        let (n, s) = slices(0, Search.start(g, ~wanted));
        check(bool, "several slices", true, n > 1);
        check(steps, "steps", found(search(g, "true")), found(s));
      },
    ),
  ],
);
