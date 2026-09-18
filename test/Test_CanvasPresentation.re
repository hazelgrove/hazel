open Alcotest;
open Haz3lcore;
module B = Web.CanvasBuffer;
let model = text => {
  let z = Test_StackFocus.parse(text) |> Zipper.unzip;
  let statics =
    CachedStatics.init_compositional(
      ~settings=Language.CoreSettings.on,
      ~stitch=x => x,
      ~root=Exp,
      z,
    );
  Web.CodeWithStatics.Model.mk(~statics, Editor.Model.mk(~root=Exp, z));
};
let watch = live =>
  B.observe(~enabled=true, ~stage=() => (), ~schedule_tick=_ => (), live);
let tests = (
  "Canvas presentation",
  [
    test_case(
      "identity-only repairs do not create a beat for every definition",
      `Quick,
      () => {
        B.reset();
        let src = "let m = {\n  let x = 1;\n  let y = 2\n} in\n\nlet a = 3 in\n\nlet b = 4 in a + b";
        /* Separately parsed models have equal content and entirely fresh ids,
           including newline ids. Publish the exact accepted model once. */
        let before = model(src)
        and after = model(src);
        Web.CanvasPresentation.capture(
          ~settings=Language.CoreSettings.on,
          ~label="update_body",
          ~avatar=None,
          before,
          after,
        );
        check(int, "one synchronization beat", 1, List.length(B.queue^));
        check(
          bool,
          "accepted identities are installed",
          true,
          B.same_program(Lazy.force(List.hd(B.queue^).b_model), after),
        );
        B.reset();
      },
    ),
    test_case(
      "one accepted tool creates a lossless definition queue",
      `Quick,
      () => {
        B.reset();
        let before = model("0");
        let src =
          String.concat(
            " ",
            List.init(12, i => "type T" ++ string_of_int(i) ++ " = Int in"),
          )
          ++ " 0";
        let after = model(src);
        Web.CanvasPresentation.capture(
          ~settings=Language.CoreSettings.on,
          ~label="insert_before",
          ~avatar=None,
          before,
          after,
        );
        check(
          bool,
          "more than the old eight-beat limit",
          true,
          List.length(B.queue^) >= 12,
        );
        let planned = List.length(B.queue^);
        let seen = ref([]);
        for (_i in 1 to planned) {
          B.last_beat := 0.;
          let shown = watch(after);
          seen := [Printer.of_zipper(shown.editor.state.zipper), ...seen^];
        };
        check(int, "every operation released", 0, List.length(B.queue^));
        check(
          int,
          "every step distinct",
          planned,
          List.length(List.sort_uniq(compare, seen^)),
        );
        check(
          bool,
          "caught up to exact accepted program",
          true,
          B.same_program(Option.get(B.shown^), after),
        );
        B.reset();
      },
    ),
    test_case(
      "uncalculated accepted edit never presents an empty graph",
      `Quick,
      () => {
        B.reset();
        let before =
          model(
            "type Model = (Int, Bool) in let view : Model -> String = ? in 0",
          );
        let accepted =
          model(
            "type Model = (Int, Bool) in let view : Model -> String = fun m -> \"ok\" in 0",
          );
        let after = {
          ...accepted,
          statics: CachedStatics.empty,
        };
        Web.CanvasPresentation.capture(
          ~settings=Language.CoreSettings.on,
          ~label="update_definition",
          ~avatar=None,
          before,
          after,
        );
        List.iter(
          beat => {
            let shown = Lazy.force(beat.Web.CanvasBuffer.b_model);
            check(
              bool,
              "statics are populated before the beat is released",
              false,
              Id.Map.is_empty(shown.statics.info_map),
            );
            let graph = Web.CanvasGraph.extract(shown.statics);
            check(
              bool,
              "Model remains visible",
              true,
              List.exists(
                (n: Web.CanvasGraph.tynode) => n.key == "Model",
                graph.nodes,
              ),
            );
          },
          B.queue^,
        );
        check(
          bool,
          "final syntax still matches the accepted edit",
          true,
          B.same_program(
            Lazy.force(List.hd(List.rev(B.queue^)).b_model),
            after,
          ),
        );
        B.last_beat := 0.;
        let presented = watch(after);
        check(
          bool,
          "pending live calculate cannot replace a prepared beat",
          false,
          Id.Map.is_empty(presented.statics.info_map),
        );
        B.reset();
      },
    ),
    test_case(
      "hold freezes a syntax change invisible to graph weighting",
      `Quick,
      () => {
        B.reset();
        let before = model("let x = 1 in x")
        and after = model("let x = 2 in x");
        B.seed(before);
        B.push_snapshot(after);
        B.last_beat := 0.;
        B.held := true;
        check(
          bool,
          "held code is unchanged",
          true,
          B.same_program(watch(after), before),
        );
        check(int, "held step retained", 1, List.length(B.queue^));
        B.held := false;
        check(
          bool,
          "release shows the new value even with zero graph delta",
          true,
          B.same_program(watch(after), after),
        );
        B.reset();
      },
    ),
    test_case(
      "presentation failure preserves accepted edits",
      `Quick,
      () => {
        B.reset();
        let before = model("0")
        and after = model("type A = Int in 0");
        B.seed(before);
        B.push_lazy(lazy(failwith("deliberate snapshot failure")));
        B.push_snapshot(after);
        B.last_beat := 0.;
        check(
          bool,
          "accepted program remains available",
          true,
          B.same_program(watch(after), after),
        );
        check(int, "invalid history discarded", 0, List.length(B.queue^));
        B.reset();
      },
    ),
    test_case(
      "catch-up discards history and subsequent ticks stay current",
      `Quick,
      () => {
        B.reset();
        let before = model("type A = Int in 0")
        and after = model("0");
        B.seed(before);
        B.push_snapshot(after);
        B.held := true;
        B.reset();
        check(
          bool,
          "blank graph is the real final state",
          true,
          B.same_program(watch(after), after),
        );
        B.tick_fired();
        check(
          bool,
          "stale repaint cannot resurrect old program",
          true,
          B.same_program(watch(after), after),
        );
        check(bool, "hold cleared", false, B.held^);
        B.reset();
      },
    ),
  ],
);
