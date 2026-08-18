open Virtual_dom.Vdom;

/* The "Evaluation" debug sidebar section: the eval Web Worker round trip. One
   row per recent request — the worker's own evaluation time, the round trip the
   main thread sees, the outcome, and the encoded request/response byte lengths
   (cheap; the active encoding already computes them). Read from EvalMetrics,
   populated in WorkerClient while this panel is open. Implements
   DebugSection.S. */

let title = "Evaluation";

/* How each outcome reads, and how it should read — the class it maps to is
   PerfFormat's business. */
let status = (s: EvalMetrics.status): (string, PerfFormat.outcome) =>
  switch (s) {
  | Pending => ("pending", PerfFormat.Waiting)
  | Success => ({|ok|}, PerfFormat.Good)
  | Failure => ("fail", PerfFormat.Bad)
  | Timeout => ("timeout", PerfFormat.Bad)
  };

let columns: list(PerfFormat.column(EvalMetrics.record)) = [
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
      let (label, outcome) = status(r.status);
      PerfFormat.status_cell(~outcome, label);
    },
  },
  {
    label: "eval",
    tooltip: "The worker's own time inside the evaluator for this batch, as reported back in the result. The gap to round-trip is queue + result serialization + transfer.",
    cell: r => PerfFormat.heat_cell(r.eval),
  },
  {
    label: "round-trip",
    tooltip: "Wall-clock from posting the request to receiving its result: worker queue + evaluation + result serialization + transfer. This is the latency the user feels.",
    cell: r => PerfFormat.total_cell(r.latency),
  },
  {
    label: "req",
    tooltip: "Encoded request size — bytes of the Marshal payload posted to the worker.",
    cell: r => PerfFormat.bytes_cell(r.req_bytes),
  },
  {
    label: "resp",
    tooltip: "Encoded response size — bytes of the Marshal payload received back.",
    cell: r =>
      PerfFormat.opt_cell(Option.map(PerfFormat.bytes_cell, r.resp_bytes)),
  },
];

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (EvalMetrics.history^) {
  | [] => [
      PerfFormat.empty("No evaluations recorded yet — evaluate a program."),
    ]
  | records => [
      PerfFormat.note(
        Printf.sprintf(
          "round-trip − eval = queue + serialize + transfer; restarts: %d",
          EvalMetrics.restarts^,
        ),
      ),
      PerfFormat.table(~columns, List.map(r => PerfFormat.Row(r), records)),
    ]
  };
