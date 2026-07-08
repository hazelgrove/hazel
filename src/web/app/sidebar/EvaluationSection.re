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

let row = (r: EvalMetrics.record): Node.t => {
  let (label, cls) = status(r.status);
  Node.tr([
    Node.td([text(Printf.sprintf("#%d", r.id))]),
    Node.td([text(string_of_int(r.entries))]),
    Node.td(~attrs=[clss([cls])], [text(label)]),
    Node.td(
      ~attrs=[clss(["perf-total"])],
      [text(PerfFormat.span_str(r.latency))],
    ),
    Node.td([text(PerfFormat.bytes(r.req_bytes))]),
    Node.td([text(PerfFormat.bytes_str(r.resp_bytes))]),
  ]);
};

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (EvalMetrics.history^) {
  | [] => [
      PerfFormat.empty("No evaluations recorded yet — evaluate a program."),
    ]
  | records => [
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
          "id",
          "cells",
          "status",
          "round-trip",
          "req",
          "resp",
        ]),
        ...List.map(row, records),
      ]),
    ]
  };
