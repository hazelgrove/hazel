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

let installed: ref(bool) = ref(false);
let install = (): unit =>
  if (! installed^) {
    installed := true;
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
  push(Printf.sprintf("%8.2fs  %s", (t -. t0^) /. 1000., msg));
};
