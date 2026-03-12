/* Phase timing instrumentation for benchmarking.
 *
 * When enabled, records wall-clock time for labeled code sections.
 * When disabled (default), passes through with minimal overhead
 * (a ref check and closure call).
 *
 * Usage:
 *   PhaseTiming.enabled := true;
 *   let result = PhaseTiming.record("statics/Elaborate", () => elaborate(x));
 *   let timings = PhaseTiming.get_and_clear(); */

let enabled = ref(false);

/* Accumulated (label, nanoseconds) pairs, most recent first. */
let recordings: ref(list((string, float))) = ref([]);

let now_ms = (): float => {
  let perf =
    Js_of_ocaml.Js.Unsafe.get(Js_of_ocaml.Js.Unsafe.global, "performance");
  Js_of_ocaml.Js.Unsafe.meth_call(perf, "now", [||]);
};

let record = (label: string, f: unit => 'a): 'a =>
  if (enabled^) {
    let t0 = now_ms();
    let result = f();
    let t1 = now_ms();
    recordings := [(label, (t1 -. t0) *. 1e6), ...recordings^];
    result;
  } else {
    f();
  };

/* Returns recordings in chronological order and clears the buffer. */
let get_and_clear = (): list((string, float)) => {
  let r = List.rev(recordings^);
  recordings := [];
  r;
};
