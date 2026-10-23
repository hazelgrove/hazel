/* Wall-clock phase timers for the agent tool path and the canvas: each
   `time(label, f)` adds f's duration to its label; `summary()` renders
   the totals with counts and resets. Dependency-free (Date.now), so any
   layer can report; the constellation journal prints the summary on its
   5-second perf line and after each tool. */
open Js_of_ocaml;

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let totals: Hashtbl.t(string, (float, int)) =
  Hashtbl.create(~size=16, (module String));

let record = (label: string, ms: float): unit => {
  let (t, n) = Option.value(Hashtbl.find(totals, label), ~default=(0., 0));
  Hashtbl.set(totals, ~key=label, ~data=(t +. ms, n + 1));
};

let time = (label: string, f: unit => 'a): 'a => {
  let t0 = now();
  let r = f();
  record(label, now() -. t0);
  r;
};

let reset = (): unit => Hashtbl.clear(totals);

/* "statics 312ms×2 | node-map 40ms×2 | edit 205ms×1", biggest first */
let summary = (): string => {
  let rows =
    Hashtbl.fold(totals, ~init=[], ~f=(~key as k, ~data as (t, n), acc) =>
      [(k, t, n), ...acc]
    )
    |> List.sort(~compare=((_, a, _), (_, b, _)) => Float.compare(b, a));
  String.concat(
    ~sep=" | ",
    List.map(rows, ~f=((k, t, n)) => sprintf("%s %.0fms×%d", k, t, n)),
  );
};

let take_summary = (): string => {
  let s = summary();
  reset();
  s;
};
