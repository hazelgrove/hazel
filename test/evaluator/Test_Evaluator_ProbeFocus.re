open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Sample focus upkeep against real evaluations. A program is loaded under
 * a multi probe on its root (autoprobe's All mode) and evaluated once. */

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
    |> Haz3lcore.ProbePerform.add_ids_from_multi_term(~syntax, ~info_map);
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
  switch (Sample.Selection.most_aligned_index(~ap_id, sample_focus, samples)) {
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

let shows = (ed, z, row, expected) =>
  check(
    string,
    Printf.sprintf("row %d shows", row),
    expected,
    shown(ed, z, row),
  );

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

let tests = (
  "Evaluator.ProbeFocus",
  [
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
  ],
);
