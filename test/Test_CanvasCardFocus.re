open Alcotest;
open Haz3lcore;
open Language;
module F = Web.CanvasFocus;

let source =
  List.assoc("Type Cards / Tic-Tac-Toe", Livelitdemos.Slides.all_slides).
    backup_text;

let evaluate = moves => {
  let source =
    Str.global_replace(
      Str.regexp_string("[At(4), At(0), At(8), At(2), At(1)]"),
      moves,
      source,
    );
  let z =
    PersistentZipper.parse_text(~source="card regression", ~root=Exp, source)
    |> Option.get;
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let settings = {
    ...CoreSettings.on,
    probe_all: true,
  };
  let statics =
    CachedStatics.init_from_term(~settings, ~is_dynamic_term=false, term);
  check(int, "valid demo", 0, List.length(statics.error_ids));
  let (_, state) =
    Evaluator.evaluate(
      ~eval_info=EvalInfo.of_targets(statics.targets),
      ~env=Builtins.env_init,
      statics.elaborated,
    );
  let dynamics = EvaluatorState.get_probes(state);
  let syntax =
    CachedSyntax.mk(
      ~info_map=statics.info_map,
      ~dyn_map=dynamics,
      ~elaborated=Some(statics.elaborated),
      z,
    );
  (z, statics, dynamics, syntax, Web.CanvasGraph.extract(statics));
};

let compact = s => Str.global_replace(Str.regexp("[ \n\t\r]+"), "", s);
let value = (s: Sample.t) => compact(F.print_value(s.value));
let latest = samples =>
  List.fold_left(
    (best: Sample.t, s: Sample.t) => s.seq > best.seq ? s : best,
    List.hd(samples),
    samples,
  );

let tests = [
  test_case("live cards agree on the final game state", `Quick, () => {
    List.iter(
      ((moves, player, outcome)) => {
        let (_, st, dynamics, syntax, graph) = evaluate(moves);
        List.iter(
          ((key, expected)) => {
            let id =
              F.value_site(
                ~dynamics,
                ~info_map=st.info_map,
                ~syntax,
                ~graph,
                key,
              )
              |> Option.get;
            let s = latest(Id.Map.find(id, dynamics));
            check(string, key ++ " after " ++ moves, expected, value(s));
            check(int, "outer program result", 0, List.length(s.call_stack));
          },
          [("Player", player), ("Outcome", outcome)],
        );
      },
      [
        ("[]", "X", "Playing"),
        ("[At(4), At(0), At(8), At(2), At(1)]", "O", "Playing"),
        ("[At(4), At(0), At(8), At(2), At(1), At(3)]", "X", "Playing"),
        ("[At(4), At(0), At(8), At(2), At(1), At(3), At(7)]", "O", "Won(X)"),
      ],
    )
  }),
  test_case(
    "history keeps its anchor; reset returns to the current result",
    `Quick,
    () => {
      let (z, st, dynamics, syntax, graph) =
        evaluate("[At(4), At(0), At(8), At(2), At(1)]");
      let edge =
        List.find(
          (e: Web.CanvasGraph.edge) => e.e_name == "advance",
          graph.edges,
        );
      let within =
        TermData.extreme_measures(
          edge.e_id,
          syntax.term_data,
          syntax.measured,
        )
        |> Option.get;
      let board_site =
        Id.Map.bindings(dynamics)
        |> List.find_map(((id, samples)) =>
             switch (TermData.segment(id, syntax.term_data), samples) {
             | (Some(seg), [_, ..._])
                 when compact(Printer.of_segment(~holes="?", seg)) == "next" =>
               Some((id, List.hd(samples)))
             | _ => None
             }
           )
        |> Option.get;
      let (id, sample) = board_site;
      let focused =
        SampleFocusPerform.capture(
          z,
          Sample.capture_of_sample(sample),
          None,
        );
      let focus = focused.refractors.sample_focus;
      let choose = (~focus, key) =>
        F.value_site(
          ~dynamics,
          ~info_map=st.info_map,
          ~syntax,
          ~graph,
          ~within,
          ~focus,
          key,
        )
        |> Option.get;
      check(
        bool,
        "keep the chosen Board site",
        true,
        Id.equal(id, choose(~focus, "Board")),
      );
      let player_id = choose(~focus, "Player");
      check(
        bool,
        "Player follows the inspected call",
        true,
        List.exists(
          (s: Sample.t) =>
            Sample.Focus.relation(~trimmed=true, ~ap_id=None, focus, s).
              is_call_cursor,
          Id.Map.find(player_id, dynamics),
        ),
      );
      let reset = SampleFocusPerform.reset(focused).refractors.sample_focus;
      List.iter(
        key => {
          let chosen = choose(~focus=reset, key);
          check(
            int,
            "live " ++ key ++ " ignores an open function panel",
            0,
            List.length(latest(Id.Map.find(chosen, dynamics)).call_stack),
          );
        },
        ["Board", "Player", "Outcome"],
      );
    },
  ),
  test_case(
    "presentation cache tracks focus-only changes and reset",
    `Quick,
    () => {
      let (z, statics, dynamics, _, graph) =
        evaluate("[At(4), At(0), At(8), At(2), At(1)]");
      let seg = Zipper.unselect_and_zip(z);
      let edge =
        List.find(
          (e: Web.CanvasGraph.edge) => e.e_name == "advance",
          graph.edges,
        );
      let editors: Web.Editors.Model.t =
        Scratch({
          current: 0,
          scratchpads: [],
          focus:
            Some({
              /* Reuse the identical source segment to exercise the cache hit;
                 edits to open cells take the separate rebuild path. */
              f_entries: [],
              f_master_seg: seg,
            }),
        });
      let original =
        Web.CodeWithStatics.Model.mk(
          ~statics,
          ~dynamics,
          Editor.Model.mk(~root=Exp, z),
        );
      let shown = fallback =>
        Web.Page.live_presentation_editor(
          ~settings=Web.Settings.Model.init,
          editors,
          fallback,
        );
      Web.Page.presentation_master := None;
      let initial = shown(original);
      let samples = Id.Map.find(List.hd(edge.e_arg_ids), dynamics);
      let with_zipper = zipper =>
        Web.CodeWithStatics.Model.{
          ...original,
          editor: {
            ...original.editor,
            state: {
              ...original.editor.state,
              zipper,
            },
          },
        };
      List.iter(
        (sample: Sample.t) => {
          let captured =
            SampleFocusPerform.capture(
              z,
              Sample.capture_of_sample(sample),
              None,
            );
          let current = shown(with_zipper(captured));
          check(
            bool,
            "cached view carries the new capture",
            true,
            current.editor.state.zipper.refractors.sample_focus
            == captured.refractors.sample_focus,
          );
          check(
            bool,
            "unchanged syntax stays cached",
            true,
            initial.editor.syntax === current.editor.syntax,
          );
          let reset =
            shown(with_zipper(SampleFocusPerform.reset(captured)));
          check(
            bool,
            "reset is immediately visible",
            true,
            reset.editor.state.zipper.refractors.sample_focus.anchor == None,
          );
          check(
            bool,
            "subsequent idle render does not revive a capture",
            true,
            shown(with_zipper(z)).editor.state.zipper.refractors.sample_focus.
              anchor
            == None,
          );
        },
        Util.ListUtil.take(2, samples),
      );
      Web.Page.presentation_master := None;
    },
  ),
  test_case(
    "cards keep a complete evaluation while new samples stream",
    `Quick,
    () => {
      let (_, statics, previous, _, _) = evaluate("[At(4), At(0)]");
      let (_, fresh_statics, fresh, _, _) =
        evaluate("[At(4), At(0), At(8)]");
      let mixed = Id.Map.union((_, a, _) => Some(a), fresh, previous);
      let streaming: Web.EvalResult.Model.t = {
        ...Web.EvalResult.Model.init,
        completed_dynamics: Some(previous),
        dynamics:
          Util.Calc.Calculated(
            Some({
              probe_map: mixed,
              test_results: TestResults.mk_results([]),
              theorems: [],
            }),
          ),
      };
      check(
        bool,
        "ordinary probes can see streamed samples",
        true,
        Web.EvalResult.Model.dynamics(streaming) === mixed,
      );
      check(
        bool,
        "cards do not combine runs",
        true,
        Web.EvalResult.Model.card_dynamics(streaming) === previous,
      );
      let completed =
        Web.EvalResult.Update.calculate(
          ~settings={
            ...CoreSettings.on,
            probe_all: true,
          },
          ~queue_worker=None,
          ~is_edited=true,
          fresh_statics,
          streaming,
        );
      check(
        bool,
        "fresh result finished",
        false,
        Web.EvalResult.Model.eval_is_pending(completed),
      );
      check(
        bool,
        "cards adopt the entire new result",
        true,
        Web.EvalResult.Model.card_dynamics(completed)
        === Web.EvalResult.Model.dynamics(completed),
      );
      check(
        bool,
        "previous snapshot is released",
        false,
        Web.EvalResult.Model.card_dynamics(completed) === previous,
      );
      ignore(statics);
    },
  ),
];
