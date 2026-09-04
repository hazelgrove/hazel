open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;

/* The "Worker Messaging" debug sidebar section: how the main thread talks to
   the eval Web Worker. Per-request benchmarks of the candidate encodings
   (WorkerMetrics) that pack payloads crossing the boundary, populated only
   while the panel is open. All requests share one table so columns line up;
   each request is a bold `#N` group row plus a lighter `response` sub-row, one
   row per encoding under each. Implements DebugSection.S. */

let title = "Worker Messaging";

/* Format an optional metric, showing an em dash for a stage that didn't
   complete rather than a misleading 0. Durations are Core.Time_ns.Span and
   sizes Core.Byte_units, each carrying its own unit via to_string_hum. */
let opt_str = (to_string: 'a => string, x: option('a)): string =>
  switch (x) {
  | None => {|—|}
  | Some(x) => to_string(x)
  };

let span_str = opt_str(s => Core.Time_ns.Span.to_string_hum(~decimals=2, s));
let bytes_str = opt_str(b => Core.Byte_units.to_string_hum(b));

let wm_head = (label: string): Node.t =>
  Node.td(~attrs=[clss(["wm-head"])], [text(label)]);

/* A full-width label row separating request groups within the shared table. */
let wm_group_row = (~cls: string, label: string): Node.t =>
  Node.tr([
    Node.td(
      ~attrs=[Attr.create("colspan", "7"), clss(["wm-group", cls])],
      [text(label)],
    ),
  ]);

let wm_metric_row = (m: WorkerMetrics.dir_metric): Node.t => {
  let total =
    switch (m.encode, m.clone, m.decode) {
    | (Some(a), Some(b), Some(c)) => Some(Core.Time_ns.Span.(a + b + c))
    | _ => None
    };
  /* Compact glyph keeps the column narrow; any failure message is on the
     cell's tooltip. */
  let (glyph, cls, tooltip) =
    switch (m.error) {
    | None => ({|✓|}, "wm-ok", "ok")
    | Some(e) => ({|✕|}, "wm-fail", e)
    };
  Node.tr([
    Node.td(
      ~attrs=[clss(["wm-wire"])],
      [text(WorkerServer.show_encoding(m.encoding))],
    ),
    Node.td([text(span_str(m.encode))]),
    Node.td([text(span_str(m.clone))]),
    Node.td([text(span_str(m.decode))]),
    Node.td(~attrs=[clss(["wm-total"])], [text(span_str(total))]),
    Node.td([text(bytes_str(m.size))]),
    Node.td(~attrs=[clss([cls]), Attr.title(tooltip)], [text(glyph)]),
  ]);
};

/* The rows contributed by one request: a request group header (which also
   starts the visual separation from the previous request), its encoding rows,
   then a lighter response sub-header and its rows. */
let wm_record_rows = (r: WorkerMetrics.record): list(Node.t) => {
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
        ...List.map(wm_metric_row, rows),
      ]
    };
  [
    wm_group_row(~cls="wm-req", req_label),
    ...List.map(wm_metric_row, r.request),
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

/* Column legend: expand the abbreviations. The durations and size carry their
   own units via Span / Byte_units formatting. */
let wm_legend: Node.t =
  div(
    ~attrs=[clss(["wm-legend"])],
    [text("encode / structuredClone / decode / total; size approximate.")],
  );

let view = (~globals: Globals.t): list(Node.t) =>
  [encoding_toggles(~globals)]
  @ (
    switch (WorkerMetrics.history^) {
    | [] => [
        div(
          ~attrs=[clss(["wm-empty"])],
          [text("No requests recorded yet — evaluate a program.")],
        ),
      ]
    | records => [
        wm_legend,
        div(
          ~attrs=[clss(["wm-scroll"])],
          [
            Node.table(
              ~attrs=[clss(["wire-metrics-table"])],
              [
                Node.tr([
                  wm_head("encoding"),
                  wm_head("enc"),
                  wm_head("clone"),
                  wm_head("dec"),
                  wm_head("total"),
                  wm_head("size"),
                  wm_head("ok?"),
                ]),
                ...List.concat_map(wm_record_rows, records),
              ],
            ),
          ],
        ),
      ]
    }
  );
