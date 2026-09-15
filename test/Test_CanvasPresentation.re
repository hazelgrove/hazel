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
    test_case("presentation failure preserves accepted edits", `Quick, () => {
      B.reset();
      let before = model("0") and after = model("type A = Int in 0");
      B.seed(before);
      B.push_lazy(lazy(failwith("deliberate snapshot failure")));
      B.push_snapshot(after);
      B.last_beat := 0.;
      check(bool,"accepted program remains available",true,B.same_program(watch(after),after));
      check(int,"invalid history discarded",0,List.length(B.queue^));
      B.reset();
    }),
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
