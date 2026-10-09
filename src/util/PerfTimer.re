/* Wall-clock phase timers for the agent tool path and the canvas: each
   `time(label, f)` adds f's duration to its label; `summary()` renders
   the totals with counts and resets. Dependency-free (Date.now), so any
   layer can report; the constellation journal prints the summary on its
   5-second perf line and after each tool. */
open Js_of_ocaml;

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let totals: Stdlib.Hashtbl.t(string, (float, int)) =
  Stdlib.Hashtbl.create(16);

let record = (label: string, ms: float): unit => {
  let (t, n) =
    Option.value(Stdlib.Hashtbl.find_opt(totals, label), ~default=(0., 0));
  Stdlib.Hashtbl.replace(totals, label, (t +. ms, n + 1));
};

let time = (label: string, f: unit => 'a): 'a => {
  let t0 = now();
  let r = f();
  record(label, now() -. t0);
  r;
};

let reset = (): unit => Stdlib.Hashtbl.reset(totals);

/* "statics 312ms×2 | node-map 40ms×2 | edit 205ms×1", biggest first */
let summary = (): string => {
  let rows =
    Stdlib.Hashtbl.fold(
      (k, (t, n), acc) => [(k, t, n), ...acc],
      totals,
      [],
    )
    |> List.sort(~compare=((_, a, _), (_, b, _)) => Float.compare(b, a));
  String.concat(
    ~sep=" | ",
    List.map(rows, ~f=((k, t, n)) =>
      Stdlib.Printf.sprintf("%s %.0fms×%d", k, t, n)
    ),
  );
};

let take_summary = (): string => {
  let s = summary();
  reset();
  s;
};
