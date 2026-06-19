open Js_of_ocaml;
open Haz3lcore;

/* WASM Z3 backend for test input generation (browser + node).
 *
 * The native opam `z3` bindings can't link under js_of_ocaml, so in the web
 * frontend we call the official `z3-solver` WebAssembly package instead.
 * The async Promise/WASM-init plumbing is wrapped in JS (see
 * src/web/www/prebundle.js, which installs `window.hazelZ3Solve`); here we
 * just hand it the SMT-LIB2 script and a callback that receives the raw
 * solver output text. That text is parsed by the shared, web-free
 * TestGen.parse_model, so model parsing is identical to the native backend.
 *
 * `solve` is asynchronous: it returns immediately and invokes [k] once the
 * solver resolves. Callers in the projector view schedule a SetResult action
 * from [k] via Bonsai.Effect.Expert.handle. */

let is_available = (): bool =>
  Js.Optdef.test(Js.Unsafe.global##.hazelZ3Solve);

/* SMT-LIB2 scripts solve deterministically, so a script's outcome never
 * changes: cache script -> outcome and skip the solver (and the serialization
 * queue) on a hit. This makes re-solving after edits cheap — only conditions
 * that actually changed hit Z3 — and lets every member of a merge group reuse
 * one group solve. Errors are not cached (they may be transient, e.g. the
 * solver not yet loaded). */
let cache: Hashtbl.t(string, TestGen.outcome) = Hashtbl.create(256);

let solve = (~k: TestGen.outcome => unit, script: string): unit =>
  switch (Hashtbl.find_opt(cache, script)) {
  | Some(outcome) => k(outcome)
  | None =>
    if (!is_available()) {
      k(
        TestGen.Error(
          "Z3 solver unavailable (z3-solver WASM not loaded). Run `npm install` and rebuild.",
        ),
      );
    } else {
      let cb =
        Js.wrap_callback((result: Js.t(Js.js_string)) => {
          let outcome = TestGen.parse_model(Js.to_string(result));
          switch (outcome) {
          | TestGen.Error(_) => ()
          | _ => Hashtbl.replace(cache, script, outcome)
          };
          k(outcome);
        });
      ignore(
        Js.Unsafe.fun_call(
          Js.Unsafe.global##.hazelZ3Solve,
          [|Js.Unsafe.inject(Js.string(script)), Js.Unsafe.inject(cb)|],
        ),
      );
    }
  };
