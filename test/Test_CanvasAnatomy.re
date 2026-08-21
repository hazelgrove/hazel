open Alcotest;
open Haz3lcore;
open Language;

/* Canvas focus-strip sample anchors must be ids the evaluator actually
 * samples under probe-all. Covers both definition shapes: explicit-`fun`
 * rhs and funlet (params in the let pattern's Ap argument), plus
 * let-headed bodies (the output anchor must descend to the result term,
 * since a `let` header never carries a sample of its own). */

let prog_funlet = {|type Model = ([String], Int) in
type Msg = Add(String) + Clear in
let init : Model = ([], 1) in
let update(msg: Msg, model: Model): Model =
let (todos, next_id) = model in
case msg
| Add(t) => (todos @ [t], next_id + 1)
| Clear => ([], next_id)
end in
let m1 = update(Add("milk"), init) in
update(Clear, m1)|};

let prog_fun = {|type Model = ([String], Int) in
type Msg = Add(String) + Clear in
let init : Model = ([], 1) in
let update : (Msg, Model) -> Model =
fun (msg, model) ->
let (todos, next_id) = model in
case msg
| Add(t) => (todos @ [t], next_id + 1)
| Clear => ([], next_id)
end in
let m1 = update((Add("milk"), init)) in
update((Clear, m1))|};

/* Parse, evaluate under probe-all, and return update's anatomy ids
 * alongside the collected sample map. */
let run = (code: string): ((list(Id.t), option(Id.t)), Sample.Map.t) => {
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("failed to parse program")
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
    let anatomy =
      Web.CanvasGraph.spine(term)
      |> List.filter_map(
           fun
           | Web.CanvasGraph.ILet(_, pat, def)
               when List.mem("update", Web.CanvasGraph.pat_names(pat)) =>
             Some(Web.CanvasGraph.fun_anatomy(~pat, def))
           | _ => None,
         )
      |> (
        fun
        | [a] => a
        | _ => fail("expected exactly one `update` binding")
      );
    (anatomy, EvaluatorState.get_probes(state));
  };
};

let samples_at = (probes: Sample.Map.t, id: Id.t): int =>
  Sample.Map.lookup(id, probes) |> Option.value(~default=[]) |> List.length;

let check_anatomy_sampled = (name: string, code: string) =>
  test_case(
    name,
    `Quick,
    () => {
      let ((arg_ids, out_id), probes) = run(code);
      check(int, "two input anchors", 2, List.length(arg_ids));
      List.iteri(
        (i, id) =>
          check(
            bool,
            Printf.sprintf("input anchor %d has samples", i),
            true,
            samples_at(probes, id) > 0,
          ),
        arg_ids,
      );
      switch (out_id) {
      | None => fail("no output anchor")
      | Some(id) =>
        check(
          bool,
          "output anchor has samples",
          true,
          samples_at(probes, id) > 0,
        )
      };
    },
  );

let tests = [
  check_anatomy_sampled(
    "funlet-form update: anchors are sampled",
    prog_funlet,
  ),
  check_anatomy_sampled("fun-form update: anchors are sampled", prog_fun),
];
