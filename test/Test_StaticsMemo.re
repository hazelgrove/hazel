open Alcotest;
open Haz3lcore;
open Language;

/* the default context is one record per mode, so analyzing the same term
   again hits Statics.mk's memo */
let repeat_hits = () => {
  let term =
    switch (FastParse.of_text(~root=Exp, "let x = 1 in\nx + 1")) {
    | Some(seg) => MakeTerm.go(seg).term
    | None => fail("unparseable")
    };
  let run = () =>
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  let first = run();
  check(bool, "the cached result comes back", true, run() === first);
};

/* a frame that only brings values (a worker result) reuses the program's
   statics. With probe-all on, the sampling targets hold every expression,
   not just the probes, and judging probes by them re-ran statics on every
   result */
let result_frame_reuses = () => {
  let z =
    switch (FastParse.of_text(~root=Exp, "let a = 1 in\na + 2")) {
    | Some(seg) => Zipper.unzip(seg)
    | None => fail("unparseable")
    };
  let settings = {
    ...CoreSettings.on,
    probe_all: true,
  };
  let calc = (~is_edited, m) =>
    Web.CodeWithStatics.Update.calculate(
      ~settings,
      ~is_edited,
      ~stitch=x => x,
      ~dynamics=Dynamics.Map.empty,
      ~is_dynamic_term=false,
      m,
    );
  let outcome = () =>
    switch (Web.PerfMetrics.history^) {
    | [f, ..._] => f.statics_outcome
    | [] => None
    };
  Web.PerfMetrics.sync(~enabled=true);
  let m =
    Web.PerfMetrics.time_frame(() =>
      calc(
        ~is_edited=true,
        Web.CodeWithStatics.Model.mk(Editor.Model.mk(~root=Exp, z)),
      )
    );
  check(
    bool,
    "an edit computes",
    true,
    outcome() == Some(Web.PerfMetrics.Recomputed),
  );
  let _ = Web.PerfMetrics.time_frame(() => calc(~is_edited=false, m));
  let reused = outcome() == Some(Web.PerfMetrics.Cached);
  Web.PerfMetrics.sync(~enabled=false);
  check(bool, "a result reuses", true, reused);
};

let tests = (
  "StaticsMemo",
  [
    test_case("a repeat analysis hits the memo", `Quick, repeat_hits),
    test_case("a result frame reuses statics", `Quick, result_frame_reuses),
  ],
);
