open Js_of_ocaml;

/* CanvasLog — a copy-pasteable journal of canvas pacing events (beats,
   queue depth, arrivals, auto-fit, avatar moves), for correlating a
   watcher's subjective impressions ("that felt jarring", "too much at
   once") with what actually fired and when.

   From the devtools console:
     copy(__constellationLogText())   put the log on the clipboard
     __constellationLogClear()        start fresh

   Always on (entries only fire on canvas/agent events, so an idle or
   human-editing session stays quiet); ~10s lulls get a gap marker so
   bursts read as separate stanzas. */

let cap = 1200;
/* burst counter, shown in the UI clock and prefixed to log lines so a
   watcher can report "on turn 7 I saw X" and land on the right stanza */
let turn: ref(int) = ref(0);
let next_turn = (): int => {
  turn := turn^ + 1;
  turn^;
};
let turn_no = (): int => turn^;
let entries: ref(list(string)) = ref([]); /* newest first */
let count: ref(int) = ref(0);
let t0: ref(float) = ref(0.);
let last_t: ref(float) = ref(0.);
/* unsaved journal/trace changes pending a localStorage flush */
let dirty: ref(bool) = ref(false);

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let iso_now = (): string => {
  let d = Js.Unsafe.new_obj(Js.Unsafe.get(Js.Unsafe.global, "Date"), [||]);
  Js.to_string(Js.Unsafe.meth_call(d, "toISOString", [||]));
};

/* layout trace: a separate ring of node-position lines (grid coords),
   recorded by CanvasSidebar whenever the layout meaningfully changes.
   Console: copy(__constellationLayoutTrace()) — paste alongside the
   journal when a layout event needs diagnosing. */
let layouts: ref(list(string)) = ref([]); /* newest first */
let layouts_count: ref(int) = ref(0);
let layout_cap = 300;
let record_layout = (line: string): unit => {
  dirty := true;
  layouts := [line, ...layouts^];
  layouts_count := layouts_count^ + 1;
  if (layouts_count^ > layout_cap) {
    layouts := List.filteri((i, _) => i < layout_cap, layouts^);
    layouts_count := layout_cap;
  };
};

/* "T7 132.4s" — the same coordinates the journal uses */
let stamp = (): string => {
  let el = t0^ == 0. ? 0. : (now() -. t0^) /. 1000.;
  Printf.sprintf("T%d %.1fs", turn^, el);
};

let push = (line: string): unit => {
  dirty := true;
  entries := [line, ...entries^];
  count := count^ + 1;
  if (count^ > cap) {
    entries := List.filteri((i, _) => i < cap, entries^);
    count := cap;
  };
};

/* the header clock ("T7 · 2:13"): imperative textContent updates on an
   interval, so ticking never re-renders the vdom */
/* ---- persistence: the journal + layout trace survive a dead tab ----
   Flushed to localStorage on a debounce; the PREVIOUS session's copies
   are captured at startup and stay readable via
   __constellationLogPrevious() / __constellationLayoutTracePrevious()
   even after this session starts overwriting the keys. */
let ls_key_log = "constellation:log";
let ls_key_trace = "constellation:trace";
let prev_log: ref(string) = ref("");
let prev_trace: ref(string) = ref("");

let ls_get = (k: string): string =>
  switch (
    Js.Opt.to_option(
      Js.Unsafe.meth_call(
        Js.Unsafe.get(Js.Unsafe.global, "localStorage"),
        "getItem",
        [|Js.Unsafe.inject(Js.string(k))|],
      ),
    )
  ) {
  | Some(v) => Js.to_string(v)
  | None => ""
  | exception _ => ""
  };

let ls_set = (k: string, v: string): unit =>
  switch (
    Js.Unsafe.meth_call(
      Js.Unsafe.get(Js.Unsafe.global, "localStorage"),
      "setItem",
      [|Js.Unsafe.inject(Js.string(k)), Js.Unsafe.inject(Js.string(v))|],
    )
  ) {
  | _ => ()
  | exception _ => ()
  };

let flush = (): unit =>
  if (dirty^) {
    dirty := false;
    ls_set(ls_key_log, String.concat("\n", List.rev(entries^)));
    ls_set(ls_key_trace, String.concat("\n", List.rev(layouts^)));
  };

/* ---- perf heartbeat: main-thread stalls via the longtask observer ----
   Accumulated per window; the interval writes a journal line only when
   stalls actually happened, so healthy sessions stay quiet. */
let lt_count: ref(int) = ref(0);
let lt_max: ref(float) = ref(0.);
let lt_total: ref(float) = ref(0.);

let install_longtask_observer = (): unit =>
  if (Js.Optdef.test(Js.Unsafe.get(Js.Unsafe.global, "PerformanceObserver"))) {
    switch (
      {
        let cb =
          Js.Unsafe.callback((list: Js.t(Js.Unsafe.any)) => {
            let entries: array(Js.t(Js.Unsafe.any)) =
              Js.to_array(Js.Unsafe.meth_call(list, "getEntries", [||]));
            Array.iter(
              e => {
                let d: float = Js.Unsafe.coerce(e)##.duration;
                lt_count := lt_count^ + 1;
                lt_max := max(lt_max^, d);
                lt_total := lt_total^ +. d;
              },
              entries,
            );
          });
        let obs =
          Js.Unsafe.new_obj(
            Js.Unsafe.get(Js.Unsafe.global, "PerformanceObserver"),
            [|Js.Unsafe.inject(cb)|],
          );
        Js.Unsafe.meth_call(
          obs,
          "observe",
          [|
            Js.Unsafe.inject(
              Js.Unsafe.obj([|
                (
                  "entryTypes",
                  Js.Unsafe.inject(Js.array([|Js.string("longtask")|])),
                ),
              |]),
            ),
          |],
        );
      }
    ) {
    | _ => ()
    | exception _ => ()
    };
  };

let ticks: ref(int) = ref(0);
/* forward ref: the heartbeat writes through log(), defined below */
let heartbeat: ref(unit => unit) = ref(() => ());

let update_clock = (): unit =>
  switch (Util.JsUtil.get_elem_by_id_opt("canvas-clock")) {
  | None => ()
  | Some(el) =>
    let txt =
      if (t0^ == 0.) {
        "";
      } else {
        let secs = int_of_float((now() -. t0^) /. 1000.);
        turn^ == 0
          ? Printf.sprintf("%d:%02d", secs / 60, secs mod 60)
          : Printf.sprintf("T%d  %d:%02d", turn^, secs / 60, secs mod 60);
      };
    Js.Unsafe.set(el, "textContent", Js.string(txt));
  };

let installed: ref(bool) = ref(false);
let install = (): unit =>
  if (! installed^) {
    installed := true;
    prev_log := ls_get(ls_key_log);
    prev_trace := ls_get(ls_key_trace);
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__constellationLogPrevious",
      Js.Unsafe.callback(() => Js.string(prev_log^)),
    );
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__constellationLayoutTracePrevious",
      Js.Unsafe.callback(() => Js.string(prev_trace^)),
    );
    /* browser only: under node (the test runner) a live interval keeps
       the event loop alive forever, so the process never exits */
    if (Js.Optdef.test(Js.Unsafe.get(Js.Unsafe.global, "document"))) {
      install_longtask_observer();
      ignore(
        Js.Unsafe.meth_call(
          Js.Unsafe.global,
          "setInterval",
          [|
            Js.Unsafe.inject(
              Js.Unsafe.callback(() => {
                ticks := ticks^ + 1;
                update_clock();
                if (ticks^ mod 4 == 0) {
                  flush();
                };
                if (ticks^ mod 10 == 0 && lt_count^ > 0) {
                  heartbeat^();
                };
              }),
            ),
            Js.Unsafe.inject(500),
          |],
        ),
      );
    };
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__constellationLogText",
      Js.Unsafe.callback(() =>
        Js.string(String.concat("\n", List.rev(entries^)))
      ),
    );
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__constellationLayoutTrace",
      Js.Unsafe.callback(() =>
        Js.string(String.concat("\n", List.rev(layouts^)))
      ),
    );
    Js.Unsafe.set(
      Js.Unsafe.global,
      "__constellationLogClear",
      Js.Unsafe.callback(() => {
        entries := [];
        count := 0;
        t0 := 0.;
        last_t := 0.;
        layouts := [];
        layouts_count := 0;
        dirty := true;
      }),
    );
  };

/* set by CanvasTrajectory: the trace time while a replay runs */
let trace_clock: ref(unit => option(string)) = ref(() => None);

let log = (msg: string): unit => {
  install();
  let t = now();
  if (t0^ == 0.) {
    t0 := t;
    push("log start " ++ iso_now());
  };
  if (last_t^ > 0. && t -. last_t^ > 10000.) {
    push(
      Printf.sprintf("          ~  quiet %.0fs  ~", (t -. last_t^) /. 1000.),
    );
  };
  last_t := t;
  let tlabel = turn^ == 0 ? "  " : Printf.sprintf("T%d", turn^);
  /* while a replay runs every line also carries the TRACE time, so an
     observation "at 38.2" lands on the same clock as the score */
  let msg =
    switch (trace_clock^()) {
    | Some(tc) => "[R " ++ tc ++ "] " ++ msg
    | None => msg
    };
  push(Printf.sprintf("%8.2fs %-3s %s", (t -. t0^) /. 1000., tlabel, msg));
};

heartbeat :=
  (
    () => {
      let phases = Util.PerfTimer.take_summary();
      log(
        Printf.sprintf(
          "perf: %d long task(s), max %.0fms, total %.1fs (last 5s)%s",
          lt_count^,
          lt_max^,
          lt_total^ /. 1000.,
          phases == "" ? "" : " — " ++ phases,
        ),
      );
      lt_count := 0;
      lt_max := 0.;
      lt_total := 0.;
    }
  );
