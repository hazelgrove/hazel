open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Sample focus upkeep against real evaluations. A program is loaded under
 * a multi probe on its root (autoprobe's All mode) and evaluated once.
 *
 * Automatic focus moves (ProbeFocus.editor_effects): the edited or newly
 * placed autoprobe line moves the focus only when it would otherwise show
 * ⊖, i.e. to fill a gap. An edit is simulated by putting the caret on a
 * line and running editor_effects with is_edited=true against those
 * dynamics, which is what happens when the edited line's probe survives
 * the edit. */

type editor = {
  z: Haz3lcore.Zipper.t,
  syntax: Haz3lcore.CachedSyntax.t,
  info_map: Statics.Map.t,
  dynamics: Dynamics.Map.t,
};

/* The autoprobe line on a (0-based) row. */
let ephemeral_on_row =
    (z: Haz3lcore.Zipper.t, syntax: Haz3lcore.CachedSyntax.t, row: int): Id.t =>
  switch (
    Id.Map.bindings(z.refractors.multis.ephemerals)
    |> List.map(fst)
    |> List.filter(id =>
         switch (
           Haz3lcore.TermData.extreme_measures(
             id,
             syntax.term_data,
             syntax.measured,
           )
         ) {
         | Some((_, end_pt)) => end_pt.row == row
         | None => false
         }
       )
  ) {
  | [id] => id
  | ids =>
    fail(
      Printf.sprintf(
        "expected one probe on row %d, found %d",
        row,
        List.length(ids),
      ),
    )
  };

/* ~unprobed_rows: lines whose probe is suppressed before evaluating. */
let load = (~unprobed_rows: list(int)=[], code: string): editor => {
  let z =
    switch (Haz3lcore.Parser.to_zipper(~root=Exp, code)) {
    | Some(z) => z
    | None => fail("Failed to parse: " ++ code)
    };
  let term = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let (info_map, elaborated) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let syntax = Haz3lcore.CachedSyntax.mk(z, ~info_map, ~dyn_map=Id.Map.empty);
  let seg = Haz3lcore.Zipper.unselect_and_zip(z);
  let root_id = Haz3lcore.Segment.root_id(Haz3lcore.Segment.skel(seg), seg);
  let z =
    Haz3lcore.ProbePerform.add_multi(
      root_id,
      ~drill=false,
      ~set_pending_cursor=false,
      ~syntax,
      ~info_map,
      z,
    );
  let syntax = Haz3lcore.CachedSyntax.mk(z, ~info_map, ~dyn_map=Id.Map.empty);
  let z =
    z
    |> Haz3lcore.ProbePerform.add_suppression(
         List.map(ephemeral_on_row(z, syntax), unprobed_rows),
       )
    |> Haz3lcore.ProbePerform.add_ids_from_multi_term(~syntax, ~info_map)
    /* start from a settled editor: no request in flight */
    |> Haz3lcore.ProbeFocus.clear_pending_probe_cursor;
  let (_, state) =
    Evaluator.evaluate(
      ~eval_info=EvalInfo.of_targets(targets_of_zipper(z, info_map)),
      ~env=Builtins.env_init,
      elaborated,
    );
  {
    z,
    syntax,
    info_map,
    dynamics: Sample.Map.finalize(EvaluatorState.get_probes(state)),
  };
};

let probe_on_row = (ed: editor, row: int): Id.t =>
  ephemeral_on_row(ed.z, ed.syntax, row);

let samples_of = (ed: editor, id: Id.t): list(Sample.t) =>
  Dynamics.Map.lookup(id, ed.dynamics) |> Option.value(~default=[]);

let ap_id_of = (ed: editor, id: Id.t): option(Id.t) =>
  Statics.Map.lookup(id, ed.info_map)
  |> Option.map(Sample.Focus.cur_var_ap)
  |> Option.join;

/* What a row shows in One mode (as ProbeProj selects it): its aligned
 * sample's value, ⍟ when a pin hides every sample, or ⊖. */
let shown = (ed: editor, z: Haz3lcore.Zipper.t, row: int): string => {
  let id = probe_on_row(ed, row);
  let ap_id = ap_id_of(ed, id);
  let sample_focus = z.refractors.sample_focus;
  let samples =
    Sample.Selection.filter_by_pin(
      ~ap_id,
      ~pinned=sample_focus.pinned_stack,
      ~pinned_interval=
        Haz3lcore.ProjectorInfo.pinned_interval(~sample_focus, ed.dynamics),
      samples_of(ed, id),
    );
  switch (
    Sample.Selection.most_aligned_index(
      ~ap_id,
      ~below=
        Sample.Selection.below_of(
          ~cursor=sample_focus,
          ~probe_id=id,
          ed.dynamics,
        ),
      sample_focus,
      samples,
    )
  ) {
  | Some(i) =>
    Test_Evaluator_Probes.format_sample_value(List.nth(samples, i).value)
  | None => samples == [] ? "⍟" : "⊖"
  };
};

/* Click the k-th sample (evaluation order) of a row's probe. */
let click =
    (ed: editor, z: Haz3lcore.Zipper.t, row: int, k: int): Haz3lcore.Zipper.t => {
  let id = probe_on_row(ed, row);
  Haz3lcore.SampleFocusPerform.capture(
    z,
    Sample.capture_of_sample(List.nth(samples_of(ed, id), k)),
    ap_id_of(ed, id),
  );
};

let caret_on_row =
    (ed: editor, z: Haz3lcore.Zipper.t, row: int): Haz3lcore.Zipper.t =>
  switch (Haz3lcore.Move.jump_to_id_indicated(z, probe_on_row(ed, row))) {
  | Some(z) => z
  | None => fail(Printf.sprintf("could not put the caret on row %d", row))
  };

let edit_on_row =
    (ed: editor, z: Haz3lcore.Zipper.t, row: int): Haz3lcore.Zipper.t =>
  caret_on_row(ed, z, row)
  |> Haz3lcore.ProbeFocus.editor_effects(
       ~is_edited=true,
       ~syntax=ed.syntax,
       ~info_map=ed.info_map,
       ~dynamics=ed.dynamics,
     );

let resolve = (ed: editor, z: Haz3lcore.Zipper.t): Haz3lcore.Zipper.t =>
  Haz3lcore.ProbeFocus.resolve_pending_probe_cursor(
    ~dynamics=ed.dynamics,
    ~syntax=ed.syntax,
    ~info_map=ed.info_map,
    z,
  );

let focus_testable =
  testable(Sample.Focus.pp, (a, b) => Sample.Focus.equal(a, b));

let shows = (ed, z, row, expected) =>
  check(
    string,
    Printf.sprintf("row %d shows", row),
    expected,
    shown(ed, z, row),
  );

/* rows: 0 row, 1 n, 2 n == 1, 3 "one", 4 "other", 5 inner map, 6 outer map */
let nested_map = {|map([[1, 2], [3, 4]], fun row ->
  map(row, fun n ->
    if n == 1
    then "one"
    else "other"
  )
)|};

/* rows: 0 minutes, 1 minutes > 60, 2 then, 3 else, 5 times, 6 t,
 * 7 format(t), 8 the map call (its closing paren on its own line, as after
 * pressing Return before it), 10 schedule call */
let timer = {|let format(minutes: Int): String =
  if minutes > 60
  then string_of_int(minutes / 60) ++ "h"
  else string_of_int(minutes) ++ "m"
in
let schedule(times: [Int]): [String] =
  map(times, fun t ->
    format(t)
  )
in
schedule([20, 60, 100])|};

/* rows: 0 x, 1 x > 100, 2 100, 3 x < 0, 4 0, 5 x, 7 to 9 the calls */
let clamp = {|let clamp(x: Int): Int =
  if x > 100
  then 100
  else if x < 0
  then 0
  else x
in
clamp(150);
clamp(-45);
clamp(92)|};

/* A nested map, n * 10. rows: 0 row, 1 n, 2 n * 10, 3 inner map, 4 outer
 * map. map is a recursive library function, so the path runs through map
 * frames for both maps and each iteration: at 40 it reads map ❯ map ❯ λ ❯
 * map ❯ map ❯ λ (focus bar indices 0 to 5). */
let nested_map_times_ten = {|map([[1, 2], [3, 4]], fun row ->
  map(row, fun n ->
    n * 10
  )
)|};

/* rows: 0 x, 1 case x (its scrutinee), 2 1, 3 x * fact(x - 1), 4 the case,
 * 6 fact(5). x's samples: 5, 4, 3, 2, 1, 0. */
let fact = {|let fact = fun x ->
  case x
  | 0 => 1
  | _ => x * fact(x - 1)
  end
in
fact(5)|};

/* The recursive call on its own line. rows: 0 x, 4 fact(x - 1), 5 x * rest,
 * 8 fact(4). The call line's samples, in evaluation order: 1, 1, 2, 6 (6 is
 * fact(4)'s call fact(3)). */
let fact_let = {|let fact = fun x ->
  case x
  | 0 => 1
  | _ =>
    let rest = fact(x - 1) in
    x * rest
  end
in
fact(4)|};

/* The stack focus bar's click on its i-th entry from the outer end. */
let bar = (z: Haz3lcore.Zipper.t, i: int): Haz3lcore.Zipper.t =>
  Haz3lcore.SampleFocusPerform.set_index(z, i);

let tests = (
  "Evaluator.ProbeFocus",
  [
    test_case(
      "Nested map: focusing the outer map's result keeps the selected iteration",
      `Quick,
      () => {
        let ed = load(nested_map_times_ten);
        /* click 10, press Right twice: 30, in the row [3, 4] */
        let z = click(ed, click(ed, ed.z, 2, 0), 2, 2);
        shows(ed, z, 2, "30");
        shows(ed, z, 1, "3");
        shows(ed, z, 0, "[3, 4]");
        /* the outer result: the focus moves to the top level, and the
           calls below stay on the path (map's own recursion does not cut
           it: map has no probed lines) */
        let z' = click(ed, z, 4, 0);
        check(
          int,
          "the path is kept",
          List.length(z.refractors.sample_focus.call_stack),
          List.length(z'.refractors.sample_focus.call_stack),
        );
        shows(ed, z', 2, "30");
        shows(ed, z', 1, "3");
        shows(ed, z', 0, "[3, 4]");
      },
    ),
    test_case(
      "Nested map: moving up the path from 40 keeps 40",
      `Quick,
      () => {
        let ed = load(nested_map_times_ten);
        let z = click(ed, ed.z, 2, 3);
        shows(ed, z, 2, "40");
        shows(ed, z, 1, "4");
        let keeps_40 = (what, z') => {
          check(string, what ++ ": n * 10", "40", shown(ed, z', 2));
          check(string, what ++ ": n", "4", shown(ed, z', 1));
          check(string, what ++ ": row", "[3, 4]", shown(ed, z', 0));
        };
        /* the row line's sample [3, 4] */
        keeps_40("row clicked", click(ed, z, 0, 1));
        /* the inner map's result [30, 40] */
        keeps_40("inner result clicked", click(ed, z, 3, 1));
        /* the focus bar's outer λ (map ❯ map ❯ λ) */
        keeps_40("bar's outer λ", bar(z, 2));
        /* and the top level */
        keeps_40("bar's top level", bar(z, -1));
      },
    ),
    test_case(
      "Recursion: the focus bar moves every line of the function, the clicked one too",
      `Quick,
      () => {
        let ed = load(fact);
        /* click x's 5, press Right twice: fact(3) */
        let z = click(ed, click(ed, ed.z, 0, 0), 0, 2);
        shows(ed, z, 0, "3");
        shows(ed, z, 1, "3");
        shows(ed, z, 4, "6");
        /* the bar's outermost fact: fact(5) on every line, x included */
        let z = bar(z, 0);
        shows(ed, z, 0, "5");
        shows(ed, z, 1, "5");
        shows(ed, z, 3, "120");
        shows(ed, z, 4, "120");
        /* click case x's 5, then the bar's third fact: fact(3) on every
           line, case x included */
        let z = click(ed, z, 1, 0);
        shows(ed, z, 1, "5");
        let z = bar(z, 2);
        shows(ed, z, 1, "3");
        shows(ed, z, 0, "3");
        shows(ed, z, 3, "6");
        shows(ed, z, 4, "6");
      },
    ),
    test_case(
      "Recursion: clicking the call's result shows that call, not a deeper one",
      `Quick,
      () => {
        let ed = load(fact);
        let z = click(ed, click(ed, ed.z, 0, 0), 0, 2);
        shows(ed, z, 0, "3");
        /* fact(5)'s result where it is called */
        let z = click(ed, z, 6, 0);
        shows(ed, z, 6, "120");
        shows(ed, z, 0, "5");
        shows(ed, z, 1, "5");
        shows(ed, z, 4, "120");
        shows(ed, z, 2, "⊖");
        /* the deeper calls stay on the bar (as ghosts): its third fact
           entry goes back down to fact(3) */
        let z = bar(z, 2);
        shows(ed, z, 0, "3");
        shows(ed, z, 4, "6");
      },
    ),
    test_case(
      "Recursion: clicking the recursive call's line keeps the call you are in",
      `Quick,
      () => {
        let ed = load(fact_let);
        /* click x's 4, press Right twice (fact(2)), then the bar's
           outermost fact: fact(4) */
        let z = bar(click(ed, click(ed, ed.z, 0, 0), 0, 2), 0);
        shows(ed, z, 0, "4");
        shows(ed, z, 4, "6");
        /* the call line's 6, fact(4)'s call fact(3): the line keeps the
           clicked sample, and the function's lines stay on fact(4) */
        let z = click(ed, z, 4, 3);
        shows(ed, z, 4, "6");
        shows(ed, z, 0, "4");
        shows(ed, z, 5, "24");
      },
    ),
    test_case(
      "Editing a line that shows a sample leaves an enclosing-call focus alone",
      `Quick,
      () => {
        let ed = load(nested_map);
        /* Focus the [1, 2] row's call: no n is chosen, so the lines below
           show its first inner call, n = 1, together; the else branch did
           not run there. */
        let z = click(ed, ed.z, 0, 0);
        shows(ed, z, 1, "1");
        shows(ed, z, 3, "\"one\"");
        shows(ed, z, 4, "⊖");
        let z' = edit_on_row(ed, z, 3);
        check(
          focus_testable,
          "focus unchanged",
          z.refractors.sample_focus,
          z'.refractors.sample_focus,
        );
        shows(ed, z', 1, "1");
        shows(ed, z', 3, "\"one\"");
        check(
          bool,
          "request resolved",
          true,
          z'.refractors.pending_probe_cursor == None,
        );
      },
    ),
    test_case(
      "Editing a line that shows ⊖ moves the focus to the first call reaching it",
      `Quick,
      () => {
        let ed = load(nested_map);
        /* Focus the n = 1 call, which never reaches the else branch. */
        let z = click(ed, ed.z, 1, 0);
        shows(ed, z, 4, "⊖");
        let z' = edit_on_row(ed, z, 4);
        shows(ed, z', 4, "\"other\"");
        shows(ed, z', 1, "2");
        shows(ed, z', 3, "⊖");
      },
    ),
    test_case(
      "With no focus yet, an automatic request sets one",
      `Quick,
      () => {
        /* Nothing selected: no line shows ⊖, but each would show its own
           first sample, and those come from different calls. */
        let ed = load(nested_map);
        let z' = edit_on_row(ed, ed.z, 4);
        check(
          bool,
          "focus set",
          true,
          z'.refractors.sample_focus.anchor != None,
        );
        /* the else line's first sample, n = 2, is now the focus */
        shows(ed, z', 1, "2");
        shows(ed, z', 3, "⊖");
      },
    ),
    test_case(
      "With a focus on the top level, an automatic request still only fills gaps",
      `Quick,
      () => {
        let ed = load(nested_map);
        /* the whole program's sample: a focus with an empty path */
        let z = click(ed, ed.z, 6, 0);
        check(
          bool,
          "a focus is set",
          true,
          z.refractors.sample_focus.anchor != None,
        );
        shows(ed, z, 3, "\"one\"");
        let z' = edit_on_row(ed, z, 3);
        check(
          focus_testable,
          "focus unchanged",
          z.refractors.sample_focus,
          z'.refractors.sample_focus,
        );
      },
    ),
    test_case(
      "Return before a call's closing paren keeps a selected iteration",
      `Quick,
      () => {
        /* The study's line-break case: an iteration of the map callback is
           selected, and the caret's row now holds the enclosing map call,
           whose sample lies on the focus path. */
        let ed = load(timer);
        let z = click(ed, ed.z, 6, 1);
        shows(ed, z, 6, "60");
        shows(ed, z, 7, "\"60m\"");
        let z' = edit_on_row(ed, z, 8);
        check(
          focus_testable,
          "focus unchanged (path, level and focal sample)",
          z.refractors.sample_focus,
          z'.refractors.sample_focus,
        );
        shows(ed, z', 6, "60");
      },
    ),
    test_case(
      "New autoprobe lines move the focus only into a gap",
      `Quick,
      () => {
        let ed = load(nested_map);
        /* the new line is the then line, with the caret on it */
        let pend = (row, z) =>
          Haz3lcore.ProbePerform.set_pending_probe(
            ~only_if_not_aligned=true,
            [probe_on_row(ed, row)],
            caret_on_row(ed, z, row),
          );
        let z = click(ed, ed.z, 0, 0);
        /* the then line shows "one" under the [1, 2] row: nothing moves */
        let z' = resolve(ed, pend(3, z));
        check(
          focus_testable,
          "aligned: focus unchanged",
          z.refractors.sample_focus,
          z'.refractors.sample_focus,
        );
        /* under the [3, 4] row it shows ⊖: the focus moves to n = 1 */
        let z = click(ed, ed.z, 0, 1);
        shows(ed, z, 3, "⊖");
        let z' = resolve(ed, pend(3, z));
        shows(ed, z', 3, "\"one\"");
        shows(ed, z', 1, "1");
      },
    ),
    test_case(
      "Explicit requests capture even when the probe shows a sample",
      `Quick,
      () => {
        let ed = load(nested_map);
        let z = caret_on_row(ed, click(ed, ed.z, 0, 0), 4);
        let z' =
          resolve(
            ed,
            Haz3lcore.ProbePerform.set_pending_probe(
              [probe_on_row(ed, 4)],
              z,
            ),
          );
        /* the else line's sample is captured: the focus narrows to n = 2 */
        shows(ed, z', 1, "2");
        shows(ed, z', 3, "⊖");
      },
    ),
    test_case(
      "Showing a hidden autoprobe line again captures, as adding a probe does",
      `Quick,
      () => {
        let ed = load(nested_map);
        let id = probe_on_row(ed, 4);
        let toggle = z =>
          Haz3lcore.ProbePerform.toggle_probe(
            ~syntax=ed.syntax,
            id,
            ~info_map=ed.info_map,
            z,
          );
        /* the calculate after each action rebuilds the autoprobe lines */
        let recalc =
          Haz3lcore.ProbePerform.add_ids_from_multi_term(
            ~syntax=ed.syntax,
            ~info_map=ed.info_map,
          );
        /* Cmd+E on the else line hides it; again shows it */
        let z = click(ed, ed.z, 0, 0) |> toggle |> recalc |> toggle |> recalc;
        check(
          bool,
          "explicit request for the line",
          true,
          switch (z.refractors.pending_probe_cursor) {
          | Some({ids, only_if_not_aligned: false}) => List.mem(id, ids)
          | _ => false
          },
        );
        shows(ed, resolve(ed, z), 1, "2");
      },
    ),
    test_case(
      "An automatic request never replaces a pending explicit one",
      `Quick,
      () => {
        let ed = load(nested_map);
        let id3 = probe_on_row(ed, 3);
        let id4 = probe_on_row(ed, 4);
        let z =
          ed.z
          |> Haz3lcore.ProbePerform.set_pending_probe([id3])
          |> Haz3lcore.ProbePerform.set_pending_probe(
               ~only_if_not_aligned=true,
               [id4],
             );
        check(
          bool,
          "explicit request kept",
          true,
          z.refractors.pending_probe_cursor
          == Some({
               ids: [id3],
               only_if_not_aligned: false,
             }),
        );
      },
    ),
    test_case(
      "The caret's probe injected into an explicit request only fills a gap",
      `Quick,
      () => {
        let ed = load(nested_map);
        /* [1, 2] row focused, caret on the n line (which shows a sample),
           explicit request for the else line */
        let z = caret_on_row(ed, click(ed, ed.z, 0, 0), 1);
        let z' =
          resolve(
            ed,
            Haz3lcore.ProbePerform.set_pending_probe(
              [probe_on_row(ed, 4)],
              z,
            ),
          );
        /* the else line is captured (n = 2), not the caret's n line */
        shows(ed, z', 1, "2");
        shows(ed, z', 4, "\"other\"");
        shows(ed, z', 3, "⊖");
      },
    ),
    test_case(
      "Pin enclosing call pins the whole call, probed or not", `Quick, () => {
      /* With the calls probed, the pin's span is clamp(150)'s own sample;
         without, it filters by stack. */
      List.iter(
        unprobed_rows => {
          let ed = load(~unprobed_rows, clamp);
          /* P on the x > 100 sample of clamp(150) */
          let z = click(ed, ed.z, 1, 0);
          let pin =
            switch (
              Haz3lcore.ProbeProj.enclosing_call_pin(
                List.hd(samples_of(ed, probe_on_row(ed, 1))),
              )
            ) {
            | Some(pin) => pin
            | None => fail("no enclosing call")
            };
          let z = Haz3lcore.SampleFocusPerform.go(z, pin);
          shows(ed, z, 1, "true");
          shows(ed, z, 2, "100");
          /* lines only the other calls reach are hidden by the pin */
          shows(ed, z, 3, "⍟");
          shows(ed, z, 5, "⍟");
        },
        [[], [7, 8, 9]],
      )
    }),
    test_case(
      "Under a pin, a request for a line the pinned call never runs leaves the focus",
      `Quick,
      () => {
        let ed = load(clamp);
        /* pin clamp(150), which takes the x > 100 branch only */
        let z = click(ed, ed.z, 1, 0);
        let pin =
          switch (
            Haz3lcore.ProbeProj.enclosing_call_pin(
              List.hd(samples_of(ed, probe_on_row(ed, 1))),
            )
          ) {
          | Some(pin) => pin
          | None => fail("no enclosing call")
          };
        let z = Haz3lcore.SampleFocusPerform.go(z, pin);
        shows(ed, z, 2, "100");
        /* an explicit request for the else-if branch (row 4), as adding a
           probe there makes: its samples all come from the other calls */
        let z' =
          resolve(
            ed,
            Haz3lcore.ProbePerform.set_pending_probe(
              [probe_on_row(ed, 4)],
              z,
            ),
          );
        check(
          focus_testable,
          "focus unchanged",
          z.refractors.sample_focus,
          z'.refractors.sample_focus,
        );
        shows(ed, z', 1, "true");
        shows(ed, z', 2, "100");
        check(
          bool,
          "request resolved",
          true,
          z'.refractors.pending_probe_cursor == None,
        );
      },
    ),
  ],
);
