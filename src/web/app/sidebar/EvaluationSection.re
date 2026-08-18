open Virtual_dom.Vdom;

/* The "Evaluation" debug sidebar section: the eval Web Worker round trip. One
   row per recent request — the worker's own evaluation time, the round trip the
   main thread sees, the outcome, and the encoded request/response byte lengths
   (cheap; the active encoding already computes them). Read from EvalMetrics,
   populated in WorkerClient while this panel is open. Implements
   DebugSection.S. */

let title = "Evaluation";

/* Label and color class for an outcome. */
let status = (s: EvalMetrics.status): (string, string) =>
  switch (s) {
  | Pending => ("pending", "perf-pending")
  | Success => ({|ok|}, "perf-ok")
  | Failure => ("fail", "perf-fail")
  | Timeout => ("timeout", "perf-fail")
  };

let columns =
    (~max: Core.Time_ns.Span.t): list(PerfFormat.column(EvalMetrics.record)) => [
  {
    label: "id",
    tooltip: "Request id, shared with the Worker Messaging panel so rows correlate.",
    cell: r => PerfFormat.text_cell(Printf.sprintf("#%d", r.id)),
  },
  {
    label: "cells",
    tooltip: "Number of cells (request entries) evaluated together.",
    cell: r => PerfFormat.int_cell(r.entries),
  },
  {
    label: "status",
    tooltip: "Outcome: pending (awaiting response), ok, fail (evaluator error), or timeout.",
    cell: r => {
      let (label, cls) = status(r.status);
      PerfFormat.status_cell(~cls, label);
    },
  },
  {
    label: "eval",
    tooltip: "The worker's own time inside the evaluator for this batch, as reported back in the result. The gap to round-trip is queue + result serialization + transfer.",
    cell: r => PerfFormat.heat_cell(~max, r.eval),
  },
  {
    label: "round-trip",
    tooltip: "Wall-clock from posting the request to receiving its result: worker queue + evaluation + result serialization + transfer. This is the latency the user feels.",
    cell: r => PerfFormat.heat_cell(~max, ~total=true, r.latency),
  },
  {
    label: "req",
    tooltip: "Encoded request size — bytes of the Marshal payload posted to the worker.",
    cell: r => PerfFormat.text_cell(PerfFormat.fmt_bytes(r.req_bytes)),
  },
  {
    label: "resp",
    tooltip: "Encoded response size — bytes of the Marshal payload received back.",
    cell: r =>
      PerfFormat.text_cell(
        PerfFormat.fmt_opt(PerfFormat.fmt_bytes, r.resp_bytes),
      ),
  },
];

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (EvalMetrics.history^) {
  | [] => [
      PerfFormat.empty("No evaluations recorded yet — evaluate a program."),
    ]
  | records =>
    /* One scale for both duration columns: the peak round trip, which bounds
       the eval time inside it. */
    let max =
      records
      |> List.to_seq
      |> Seq.map((r: EvalMetrics.record) => r.latency)
      |> PerfFormat.max_span;
    [
      PerfFormat.note(
        Printf.sprintf(
          "round-trip − eval = queue + serialize + transfer; restarts: %d",
          EvalMetrics.restarts^,
        ),
      ),
      PerfFormat.table(
        ~columns=columns(~max),
        List.map(r => PerfFormat.Row(r), records),
      ),
    ];
  };
