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

let columns: list(PerfFormat.column(WorkerMetrics.dir_metric)) = [
  {
    label: "encoding",
    tooltip: "Candidate wire encoding benchmarked for this payload. Only enabled encodings (chips above) are measured.",
    cell: m => PerfFormat.name_cell(WorkerServer.show_encoding(m.encoding)),
  },
  {
    label: "enc",
    tooltip: "Time to pack the payload into this encoding.",
    cell: m => PerfFormat.heat_cell(m.encode),
  },
  {
    label: "clone",
    tooltip: "Time for the structuredClone the browser performs when the payload crosses the worker boundary.",
    cell: m => PerfFormat.heat_cell(m.clone),
  },
  {
    label: "dec",
    tooltip: "Time to unpack the payload back into OCaml values.",
    cell: m => PerfFormat.heat_cell(m.decode),
  },
  {
    label: "total",
    tooltip: "encode + structuredClone + decode for this encoding — the cost of using it for this payload.",
    cell: m => PerfFormat.total_cell(total_of(m)),
  },
  {
    label: "size",
    tooltip: "Encoded payload size (approximate).",
    cell: m =>
      PerfFormat.text_cell(PerfFormat.fmt_opt(PerfFormat.fmt_bytes, m.size)),
  },
  {
    /* Compact glyph keeps the column narrow; any failure message is on the
       cell's tooltip. */
    label: "ok?",
    tooltip: "Whether the round trip succeeded; hover a ✕ for the failure message.",
    cell: m =>
      switch (m.error) {
      | None =>
        PerfFormat.status_cell(
          ~outcome=PerfFormat.Good,
          ~tooltip=Some("ok"),
          {|✓|},
        )
      | Some(e) =>
        PerfFormat.status_cell(
          ~outcome=PerfFormat.Bad,
          ~tooltip=Some(e),
          {|✕|},
        )
      },
  },
];

/* The rows contributed by one request: a request group header (which also
   starts the visual separation from the previous request), its encoding rows,
   then a lighter response sub-header and its rows. */
let rows_of_record =
    (r: WorkerMetrics.record)
    : list(PerfFormat.row(WorkerMetrics.dir_metric)) => {
  let req_label =
    Printf.sprintf(
      "#%d · %d %s · request",
      r.id,
      r.entries,
      r.entries == 1 ? "entry" : "entries",
    );
  let response_rows =
    switch (r.response) {
    | [] => [
        PerfFormat.Group({
          kind: PerfFormat.Absent,
          label: "response pending / timed out",
        }),
      ]
    | rows => [
        PerfFormat.Group({
          kind: PerfFormat.Secondary,
          label: "response",
        }),
        ...List.map(m => PerfFormat.Row(m), rows),
      ]
    };
  [
    PerfFormat.Group({
      kind: PerfFormat.Primary,
      label: req_label,
    }),
    ...List.map(m => PerfFormat.Row(m), r.request),
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
      let rows = List.concat_map(rows_of_record, records);
      [
        /* Column meanings live on the header tooltips; this just names the heat
           scale the way the other profiling sections do. */
        PerfFormat.note(
          "max total: "
          ++ PerfFormat.fmt_span(PerfFormat.scale(~columns, rows))
          ++ " · redder = slower",
        ),
        PerfFormat.table(~columns, rows),
      ];
    }
  );
