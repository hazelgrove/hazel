/* AgentPulse — the dependency-free "is the agent actively editing"
   signal. CanvasBuffer stamps it on every applied tool call; low-level
   consumers (statics targets, the evaluator's sampling flag) read it
   without depending on the canvas machinery.

   Ambient all-sites sampling (probe_all) is masked while a burst is
   live: nobody can read those samples mid-burst (each edit discards
   them ~a second later), they dominate eval cost and memory on large
   programs, and the wells repopulate from one full-sampling refresh
   when the burst settles. Explicit probes keep sampling throughout —
   they are the agent's own feedback channel. */

let burst_window_ms = 5000.;

let last_action: ref(float) = ref(-1.e12);

let now = (): float =>
  Js_of_ocaml.Js.Unsafe.coerce(Js_of_ocaml.Js.Unsafe.global)##._Date##now();

let note_action = (): unit => last_action := now();

let in_burst = (): bool => now() -. last_action^ < burst_window_ms;

/* The user asked for samples NOW (probe-all toggled by hand): end the
   mask immediately rather than waiting the window out. */
let release = (): unit => last_action := (-1.e12);

/* ms until the current burst settles (0 when not in a burst) */
let ms_to_settle = (): float =>
  Float.max(0., last_action^ +. burst_window_ms -. now());

/* A projector just committed new syntax (a livelit gesture, a slider,
   a checkbox): a discrete edit, not a keystroke in a burst of typing,
   so the statics debounce that smooths typing only adds latency to the
   interaction. Set by ProjectorPerform, consumed by the debounce. */
let projector_commit: ref(bool) = ref(false);
