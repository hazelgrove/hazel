open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* The "Worker Messaging" debug sidebar section: how the main thread talks to
   the eval Web Worker. Per-request benchmarks of the candidate encodings
   (WorkerMetrics) that pack payloads crossing the boundary, populated only
   while the panel is open. All requests share one table so columns line up;
   each request is a bold `#N` group row plus a lighter `response` sub-row, one
   row per encoding under each. Shares PerfFormat's table, formatting, and
   heat-map with the profiling sections. Implements DebugSection.S. */

let title = "Worker Messaging";

/* encode + clone + decode, and only once every stage completed: a partial sum
   would read as artificially fast next to the complete ones. */
let total_of = (m: WorkerMetrics.dir_metric): option(Core.Time_ns.Span.t) =>
  switch (m.encode, m.clone, m.decode) {
  | (Some(a), Some(b), Some(c)) => Some(Core.Time_ns.Span.(a + b + c))
  | _ => None
  };

/* One scale for the whole table: the peak per-encoding total across every
   request and response row. A stage as red as its own total dominates that
   encoding's cost, and the slower encodings read redder than the rest. */
let max_total = (records: list(WorkerMetrics.record)): Core.Time_ns.Span.t =>
  PerfFormat.max_span(
    List.concat_map(
      (r: WorkerMetrics.record) =>
        List.map(total_of, r.request @ r.response),
      records,
    ),
  );

let head_row: Node.t =
  PerfFormat.head_row([
    (
      "encoding",
      "Candidate wire encoding benchmarked for this payload. Only enabled encodings (chips above) are measured.",
    ),
    ("enc", "Time to pack the payload into this encoding."),
    (
      "clone",
      "Time for the structuredClone the browser performs when the payload crosses the worker boundary.",
    ),
    ("dec", "Time to unpack the payload back into OCaml values."),
    (
      "total",
      "encode + structuredClone + decode for this encoding — the cost of using it for this payload.",
    ),
    ("size", "Encoded payload size (approximate)."),
    (
      "ok?",
      "Whether the round trip succeeded; hover a ✕ for the failure message.",
    ),
  ]);

/* A full-width label row separating request groups within the shared table. */
let wm_group_row = (~cls: string, label: string): Node.t =>
  Node.tr([
    Node.td(
      ~attrs=[Attr.create("colspan", "7"), clss(["wm-group", cls])],
      [text(label)],
    ),
  ]);

let wm_metric_row =
    (~max: Core.Time_ns.Span.t, m: WorkerMetrics.dir_metric): Node.t => {
  /* Compact glyph keeps the column narrow; any failure message is on the
     cell's tooltip. */
  let (glyph, cls, tooltip) =
    switch (m.error) {
    | None => ({|✓|}, "perf-ok", "ok")
    | Some(e) => ({|✕|}, "perf-fail", e)
    };
  Node.tr([
    Node.td(
      ~attrs=[clss(["wm-wire"])],
      [text(WorkerServer.show_encoding(m.encoding))],
    ),
    PerfFormat.heat_cell(~max, m.encode),
    PerfFormat.heat_cell(~max, m.clone),
    PerfFormat.heat_cell(~max, m.decode),
    PerfFormat.heat_cell(~max, ~cls=["perf-total"], total_of(m)),
    Node.td([text(PerfFormat.bytes_str(m.size))]),
    Node.td(~attrs=[clss([cls]), Attr.title(tooltip)], [text(glyph)]),
  ]);
};

/* The rows contributed by one request: a request group header (which also
   starts the visual separation from the previous request), its encoding rows,
   then a lighter response sub-header and its rows. */
let wm_record_rows =
    (~max: Core.Time_ns.Span.t, r: WorkerMetrics.record): list(Node.t) => {
  let req_label =
    Printf.sprintf(
      "#%d · %d %s · request",
      r.id,
      r.entries,
      r.entries == 1 ? "entry" : "entries",
    );
  let response_rows =
    switch (r.response) {
    | [] => [wm_group_row(~cls="wm-note", "response pending / timed out")]
    | rows => [
        wm_group_row(~cls="wm-resp", "response"),
        ...List.map(wm_metric_row(~max), rows),
      ]
    };
  [
    wm_group_row(~cls="wm-req", req_label),
    ...List.map(wm_metric_row(~max), r.request),
  ]
  @ response_rows;
};

/* Per-encoding on/off chip. Only enabled encodings are benchmarked (so a slow
   one like Sexp can be skipped); state persists via sidebar settings. */
let encoding_toggle = (~globals: Globals.t, e: WorkerServer.encoding): Node.t => {
  let on =
    SidebarModel.Settings.is_encoding_enabled(e, globals.settings.sidebar);
  let name = WorkerServer.show_encoding(e);
  div(
    ~attrs=[
      clss(["wm-toggle", on ? "on" : "off"]),
      Attr.title(
        (on ? "Disable" : "Enable")
        ++ " benchmarking of the "
        ++ name
        ++ " encoding",
      ),
      Attr.on_click(_ =>
        globals.inject_global(Set(Sidebar(ToggleWorkerEncoding(e))))
      ),
    ],
    [text((on ? {|☑|} : {|☐|}) ++ " " ++ name)],
  );
};

let encoding_toggles = (~globals): Node.t =>
  div(
    ~attrs=[clss(["wm-toggles"])],
    List.map(encoding_toggle(~globals), WorkerServer.all_of_encoding),
  );

let view = (~globals: Globals.t): list(Node.t) =>
  [encoding_toggles(~globals)]
  @ (
    switch (WorkerMetrics.history^) {
    | [] => [
        PerfFormat.empty("No requests recorded yet — evaluate a program."),
      ]
    | records =>
      let max = max_total(records);
      [
        /* Column meanings live on the header tooltips; this just anchors the
           heat scale the way the other profiling sections do. */
        PerfFormat.note(
          "max total: " ++ PerfFormat.span(max) ++ " · redder = slower",
        ),
        PerfFormat.table([
          head_row,
          ...List.concat_map(wm_record_rows(~max), records),
        ]),
      ];
    }
  );
