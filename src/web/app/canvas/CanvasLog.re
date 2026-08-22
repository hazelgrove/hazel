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
let entries: ref(list(string)) = ref([]); /* newest first */
let count: ref(int) = ref(0);
let t0: ref(float) = ref(0.);
let last_t: ref(float) = ref(0.);

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let iso_now = (): string => {
  let d = Js.Unsafe.new_obj(Js.Unsafe.get(Js.Unsafe.global, "Date"), [||]);
  Js.to_string(Js.Unsafe.meth_call(d, "toISOString", [||]));
};

let push = (line: string): unit => {
  entries := [line, ...entries^];
  count := count^ + 1;
  if (count^ > cap) {
    entries := List.filteri((i, _) => i < cap, entries^);
    count := cap;
  };
};

/* the header clock ("T7 · 2:13"): imperative textContent updates on an
   interval, so ticking never re-renders the vdom */
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
    /* browser only: under node (the test runner) a live interval keeps
       the event loop alive forever, so the process never exits */
    if (Js.Optdef.test(Js.Unsafe.get(Js.Unsafe.global, "document"))) {
      ignore(
        Js.Unsafe.meth_call(
          Js.Unsafe.global,
          "setInterval",
          [|
            Js.Unsafe.inject(Js.Unsafe.callback(update_clock)),
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
      "__constellationLogClear",
      Js.Unsafe.callback(() => {
        entries := [];
        count := 0;
        t0 := 0.;
        last_t := 0.;
      }),
    );
  };

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
  push(Printf.sprintf("%8.2fs %-3s %s", (t -. t0^) /. 1000., tlabel, msg));
};
