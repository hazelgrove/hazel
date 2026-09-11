open Alcotest;
open Haz3lcore;
open Language;

/* Every sampled value must render through the canvas well pipeline (chip) without raising. Regression: abbreviated list
 * values ([…, …]) crashed Skel via ExpToSegment's table projection until
 * CanvasValue disabled project_tables. */

let prog = {|type Todo = (String, Bool) in
type Model = ([Todo], Int) in
type Msg = Add(String) + Clear in
let init : Model = ([], 1) in
let update(msg: Msg, model: Model): Model =
let (todos, next_id) = model in
case msg
| Add(t) => ((t, false) :: todos, next_id + 1)
| Clear => ([], next_id)
end in
let m1 = update(Add("milk and honey"), init) in
let m2 = update(Add("eggs benedict"), m1) in
let m3 = update(Add("sourdough bread"), m2) in
let m4 = update(Add("orange marmalade"), m3) in
update(Clear, m4)|};

let fm: Web.FontMetrics.t = {
  row_height: 10.,
  col_width: 5.,
};

let tests = [
  test_case("chips render for all sampled values", `Quick, () => {
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
      let n = ref(0);
      Sample.Map.fold(
        (_, samples, ()) =>
          List.iter(
            (s: Sample.t) => {
              incr(n);
              let str =
                try({
                  let _ =
                    Web.CanvasValue.chip(
                      ~font_metrics=fm,
                      ~available=24,
                      s.value,
                    );
                  "ok";
                }) {
                | e => "EXN " ++ Printexc.to_string(e)
                };
              check(string, Printf.sprintf("sample %d", n^), "ok", str);
            },
            samples,
          ),
        EvaluatorState.get_probes(state),
        (),
      );
    }
  }),
];
