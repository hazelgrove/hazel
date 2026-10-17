/* Byte budgets: what crosses to the eval worker and back, measured in the
   bytes the page actually posts (WorkerServer's Marshal encoding) and held
   to a budget, as Test_PerfBudgets holds the evaluator's steps.

   Bytes, like steps, are the same on every machine, and from run to run
   differ by a few dozen bytes at most (ids are minted afresh), so a budget can
   fail CI without false alarms. They matter because both sides
   of the boundary pay for them, once to encode and once to decode, on
   every change: before #2606 an answer carried the whole incremental
   cache, which the page only sent back, and before #2608 every node of a
   program carried its source's whitespace and comments.

   Each budget is a ratchet. Over it is a regression: find it, or raise the
   budget with the reason in the commit. Well under it (by more than a
   quarter) means the budget is stale: lower it in the same change. */
open Alcotest;
open Language;

let program = (path: string): Exp.t => {
  let root =
    Sys.file_exists("hazel-programs")
      ? "hazel-programs" : "../hazel-programs";
  let full = Filename.concat(root, path);
  let ic = open_in_bin(full);
  let src = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  switch (
    Haz3lcore.PersistentZipper.parse_text(
      ~source="byte-budgets",
      ~root=Exp,
      src,
    )
  ) {
  | Some(z) => Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term
  | None => fail(full ++ ": did not parse")
  };
};

/* A request as the page builds one for the worker: the elaboration, its
   evaluation info, and no `prev` (the worker keeps the cache, #2606). */
let request_value = (exp: Exp.t): WorkerServer.Request.value => {
  let (info_map, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
  WorkerServer.Request.{
    expr: elab,
    eval_info_map:
      EvalInfo.of_info_map(
        ~probe_all=CoreSettings.on.probe_all,
        ~targets=Id.Map.empty,
        info_map,
      ),
    prev: IncrEval.empty,
    use_held_prev: true,
    doc: "byte-budgets",
  };
};

let request_bytes = (v: WorkerServer.Request.value): int =>
  WorkerServer.MarshalEncoding.(
    encode_request(
      WorkerServer.ClientMessage.Evaluate(
        WorkerServer.Request.{
          request_id: 1,
          batch: [("", v)],
        },
      ),
    )
    |> size_request
    |> Core.Byte_units.bytes_int_exn
  );

/* The worker's answer to it: evaluated against the cache it kept, the
   cache kept again and left out of the answer. */
let response_bytes = (v: WorkerServer.Request.value): int => {
  let v = WorkerServer.Held.resolve("", v);
  let response =
    WorkerServer.Held.keep(~doc=v.doc, "", WorkerServer.evaluate_sync(v));
  WorkerServer.MarshalEncoding.(
    encode_response(
      WorkerServer.ServerMessage.Result(
        WorkerServer.ServerMessage.{
          request_id: 1,
          response: [("", response)],
          eval_time: Core.Time_ns.Span.zero,
        },
      ),
    )
    |> size_response
    |> Core.Byte_units.bytes_int_exn
  );
};

let within_budget = (name: string, ~budget: int, bytes: int): unit => {
  Printf.printf("BYTES %s: %d (budget %d)\n", name, bytes, budget);
  if (bytes > budget) {
    failf(
      "%s: %d bytes, over its budget of %d. A regression: find it, or raise the budget, with the reason in the commit.",
      name,
      bytes,
      budget,
    );
  } else if (bytes * 4 < budget * 3) {
    failf(
      "%s: %d bytes, more than a quarter under its budget of %d. An improvement: lower the budget to about %d.",
      name,
      bytes,
      budget,
      bytes * 11 / 10,
    );
  };
};

/* Budgets, about a tenth over what dev with #2606 and #2608 sends today
   (2026-10-15): probes.hz a request of 25,180 bytes and an answer of
   3,617; the leftjoin program 60,628 and 21,258. */
let cases = [
  ("docs/reference/probes.hz", 27_700, 4_000),
  ("docs/b2t2/table-api-constructors-leftjoin.hz", 66_700, 23_400),
];

let case = ((path, req_budget, resp_budget)) =>
  test_case(
    path,
    `Quick,
    () => {
      WorkerServer.Held.clear();
      let v = request_value(program(path));
      within_budget(
        path ++ ": the request",
        ~budget=req_budget,
        request_bytes(v),
      );
      /* the first answer, then a second run of the same program: the worker
         answers both without the cache */
      let first = response_bytes(v);
      within_budget(path ++ ": the answer", ~budget=resp_budget, first);
      let again = response_bytes(v);
      check(
        bool,
        Printf.sprintf(
          "a second answer (%d bytes) is no bigger than the first (%d)",
          again,
          first,
        ),
        true,
        again <= first,
      );
    },
  );

let tests = ("ByteBudgets", List.map(case, cases));
