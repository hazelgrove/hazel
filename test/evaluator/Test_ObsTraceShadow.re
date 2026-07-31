open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Live/batch parity for the observation trace
 * (plans/observation-trace.md): state.probes is maintained incrementally
 * by ObsTrace.fold_step as events are recorded, and replaying the same
 * events through the batch ObsTrace.assemble must reproduce it exactly
 * (modulo mint-time metadata: seq counter and wall-clock time — sample
 * `id` is a content hash and must agree). One transition, two drivers.
 *
 * Fresh evaluations only: incremental reuse replays cached SAMPLES via
 * append but not their events (cache entries are cleared transient), so
 * fold parity under ~prev is a slice-3 concern (samples-as-data compose
 * in the same accumulator there). */

let eval_both = (code: string): (Sample.Map.t, Sample.Map.t) => {
  let (_term, elaborated, _info_map, targets) = parse_with_probes(code);
  let (_, state) =
    Evaluator.evaluate(
      ~eval_info=EvalInfo.of_targets(targets),
      ~env=Builtins.env_init,
      elaborated,
    );
  let inline = EvaluatorState.get_probes(state);
  let folded = ObsTrace.assemble(List.rev(state.obs_trace));
  (inline, folded);
};

let frame_key = (f: option(CallStack.frame)) =>
  Option.map((f: CallStack.frame) => (f.id, f.fn_def_id), f);

let sample_eq = (a: Sample.t, b: Sample.t): bool =>
  a.syntax_id == b.syntax_id
  && DHExp.fast_equal(a.value, b.value)
  && CallStack.ids_of_stack(a.call_stack)
  == CallStack.ids_of_stack(b.call_stack)
  && a.args == b.args
  && frame_key(a.frame) == frame_key(b.frame)
  && a.step_start == b.step_start
  && a.step_end == b.step_end
  && a.origin == b.origin;

let describe = (s: Sample.t): string =>
  Printf.sprintf(
    "{id=%s depth=%d steps=[%d,%d] args=%b frame=%b}",
    Id.to_string(s.syntax_id),
    List.length(s.call_stack),
    s.step_start,
    s.step_end,
    Option.is_some(s.args),
    Option.is_some(s.frame),
  );

let check_parity = (label: string, code: string) => {
  let (inline, folded) = eval_both(code);
  let keys = m => m |> Id.Map.bindings |> List.map(fst);
  check(
    int,
    label ++ ": same probe ids",
    List.length(keys(inline)),
    List.length(keys(folded)),
  );
  List.iter(
    id => {
      let get = m => Id.Map.find_opt(id, m) |> Option.value(~default=[]);
      let (si, sf) = (get(inline), get(folded));
      if (List.length(si) != List.length(sf)
          || !List.for_all2(sample_eq, si, sf)) {
        fail(
          label
          ++ ": mismatch at probe "
          ++ Id.to_string(id)
          ++ "\n  inline: "
          ++ String.concat(" ", List.map(describe, si))
          ++ "\n  folded: "
          ++ String.concat(" ", List.map(describe, sf)),
        );
      };
    },
    keys(inline),
  );
};

let parity = (label, code) =>
  test_case(label, `Quick, () => check_parity(label, code));

let tests = (
  "ObsTraceShadow",
  [
    parity("simple call", {|let f = fun x -> x + 1
in ^^probe(f(5))|}),
    parity(
      "recursion, two probes",
      {|let fact = fun x -> if x < 2 then 1 else ^^probe(x) * ^^probe(fact(x - 1))
in fact(4)|},
    ),
    parity(
      "cast-distributed call (delegated span)",
      {|let add = fun (a, b) -> a + b in
let mk: Int -> (Int -> Int) = fun a -> add(a, _) in
let apply = fun (f, x) -> ^^probe(f(x)) in
apply(mk(1), 5)|},
    ),
    parity(
      "partial application through annotation",
      {|let setCell: (Int, Int) -> Int = fun (g, x) -> g + x in
let updateGrove = fun (m, f) -> ^^probe(f(m)) in
updateGrove(10, setCell(_, 5))|},
    ),
    parity(
      "higher-order, multiple calls",
      {|let apply = fun (f, x) -> ^^probe(f(x))
in [apply(fun a -> a + 1, 1), apply(fun a -> a * 2, 2)]|},
    ),
    parity(
      "nested probes",
      {|let f = fun x -> ^^probe(x + 1)
in ^^probe(f(^^probe(f(1))))|},
    ),
  ],
);
