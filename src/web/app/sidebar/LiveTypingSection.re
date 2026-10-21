open Virtual_dom.Vdom;

/* The "Live Typing" debug sidebar section: a per-frame history of the live
   typing pass — a second Statics.mk run against the evaluated dynamics. Shows
   how long each pass took (`—` when it was skipped, with the reason in the
   `outcome` column) and what it produced: the size of the live info map and
   how many errors it reports that the static pass does not. Read from
   PerfMetrics.history, populated in CodeWithStatics.calculate while this panel
   is open. Implements DebugSection.S. */

let title = "Live Typing";

let columns: list(PerfFormat.column(PerfMetrics.frame)) = [
  PerfFormat.action_column((f: PerfMetrics.frame) =>
    Option.map(fst, f.perform)
  ),
  {
    label: "time",
    tooltip: "Time to re-run statics against the dynamics (sample filter, Statics.mk, error ids). — when the pass was skipped this frame.",
    cell: f => PerfFormat.total_cell(f.live_typing),
  },
  {
    label: "entries",
    tooltip: "Number of entries in the live-typing info map (one per expression id).",
    cell: f => PerfFormat.int_cell(f.live_typing_entries),
  },
  {
    label: "live err",
    tooltip: "Number of errors live typing reports that the static pass does not.",
    cell: f => PerfFormat.int_cell(f.live_typing_errors),
  },
  {
    label: "outcome",
    tooltip: "What became of the live pass this frame: ran, throttled (a stream slice arrived inside the throttle window), reused (nothing new to type against), or off (live typing disabled).",
    cell: f =>
      PerfFormat.opt_cell(
        Option.map(
          o =>
            PerfFormat.text_cell(
              String.lowercase_ascii(
                PerfMetrics.show_live_typing_outcome(o),
              ),
            ),
          f.live_typing_outcome,
        ),
      ),
  },
];

let view = (~globals as _: Globals.t): list(Node.t) =>
  PerfFormat.view(
    ~columns,
    ~empty="No live typing recorded yet — type in the editor.",
    List.map(f => PerfFormat.Row(f), PerfMetrics.history^),
  );
