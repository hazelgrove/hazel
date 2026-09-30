open Alcotest;
open Haz3lcore;
open Language;

/* A sample-focus capture at an un-refractored anchor (as made by the
 * canvas focus strip) must survive Editor.calculate's probe housekeeping
 * when the editor has NO probe refractors. Regression: rm_manual([]) in
 * remove_colliding_probes applied its unconditional no-probes cursor
 * reset every calculate, wiping such captures on the next frame. */

let prog = {|let f = fun x -> x + 1 in f(1) + f(2)|};

let tests = [
  test_case(
    "capture at un-refractored anchor survives editor_effects", `Quick, () => {
    switch (Parser.to_zipper(~root=Exp, prog)) {
    | None => fail("parse failed")
    | Some(z) =>
      let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
      let settings = {
        ...CoreSettings.on,
        probe_all: true,
      };
      let (info_map, elaborated) =
        Statics.mk(settings, Builtins.ctx_init(Some(Int)), term);
      let targets =
        CachedStatics.compute_targets(
          ~settings,
          ~info_map,
          ~probe_ids=Id.Map.empty,
        );
      let (_, state) =
        Evaluator.evaluate(
          ~eval_info=EvalInfo.of_targets(targets),
          ~env=Builtins.env_init,
          elaborated,
        );
      let dynamics = EvaluatorState.get_probes(state);
      /* capture the first sampled site, like a canvas well click */
      let (anchor_id, sample) =
        switch (Id.Map.bindings(dynamics)) {
        | [(id, [s, ..._]), ..._] => (id, s)
        | _ => fail("no samples collected")
        };
      let z =
        SampleFocusPerform.go(
          z,
          Capture(Sample.capture_of_sample(sample), None),
        );
      check(
        bool,
        "capture recorded",
        true,
        z.refractors.sample_focus.anchor != None,
      );
      let syntax =
        CachedSyntax.mk(
          ~info_map,
          ~dyn_map=dynamics,
          ~elaborated=Some(elaborated),
          z,
        );
      let z =
        ProbeFocus.editor_effects(
          ~is_edited=false,
          ~syntax,
          ~info_map,
          ~dynamics,
          z,
        );
      switch (z.refractors.sample_focus.anchor) {
      | Some(a) =>
        check(
          bool,
          "anchor survives and still names the captured site",
          true,
          Id.equal(a.probe_id, anchor_id) || Id.Map.mem(a.probe_id, dynamics),
        )
      | None => fail("editor_effects reset the captured sample focus")
      };
    }
  }),
];
