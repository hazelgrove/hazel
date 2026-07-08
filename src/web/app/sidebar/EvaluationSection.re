open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* The "Evaluation" debug sidebar section: the eval Web Worker round trip. One
   row per recent request — latency (postMessage→onmessage), outcome, and the
   encoded request/response byte lengths (cheap; the active encoding already
   computes them). Read from EvalMetrics, populated in WorkerClient while this
   panel is open. Implements DebugSection.S. */

let title = "Evaluation";

let status = (s: EvalMetrics.status): (string, string) =>
  switch (s) {
  | Pending => ("pending", "perf-pending")
  | Ok => ({|ok|}, "perf-ok")
  | Fail => ("fail", "perf-fail")
  | Timeout => ("timeout", "perf-fail")
  };

let row = (~max: Core.Time_ns.Span.t, r: EvalMetrics.record): Node.t => {
  let (label, cls) = status(r.status);
  Node.tr([
    Node.td([text(Printf.sprintf("#%d", r.id))]),
    Node.td([text(string_of_int(r.entries))]),
    Node.td(~attrs=[clss([cls])], [text(label)]),
    PerfFormat.heat_cell(~max, ~cls=["perf-total"], r.latency),
    Node.td([text(PerfFormat.bytes(r.req_bytes))]),
    Node.td([text(PerfFormat.bytes_str(r.resp_bytes))]),
  ]);
};

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (EvalMetrics.history^) {
  | [] => [
      PerfFormat.empty("No evaluations recorded yet — evaluate a program."),
    ]
  | records =>
    let max =
      PerfFormat.max_span(
        List.map((r: EvalMetrics.record) => r.latency, records),
      );
    [
      /* Round-trip = time from posting the request until its result arrives on
         the main thread: worker queue + evaluation + result serialization +
         transfer. It is dominated by evaluation but is not the worker's
         isolated evaluate() call. */
      PerfFormat.note(
        Printf.sprintf(
          "round-trip = post → result (queue + eval + serialize); restarts: %d",
          EvalMetrics.restarts^,
        ),
      ),
      PerfFormat.table([
        PerfFormat.head_row([
          (
            "id",
            "Request id, shared with the Worker Messaging panel so rows correlate.",
          ),
          ("cells", "Number of cells (request entries) evaluated together."),
          (
            "status",
            "Outcome: pending (awaiting response), ok, fail (evaluator error), or timeout.",
          ),
          (
            "round-trip",
            "Wall-clock from posting the request to receiving its result: worker queue + evaluation + result serialization + transfer. Dominated by evaluation, but not the worker's isolated evaluate() call.",
          ),
          (
            "req",
            "Encoded request size — bytes of the Marshal payload posted to the worker.",
          ),
          (
            "resp",
            "Encoded response size — bytes of the Marshal payload received back.",
          ),
        ]),
        ...List.map(row(~max), records),
      ]),
    ];
  };
