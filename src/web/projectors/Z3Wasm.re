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

let solve = (~k: TestGen.outcome => unit, script: string): unit =>
  if (!is_available()) {
    k(
      TestGen.Error(
        "Z3 solver unavailable (z3-solver WASM not loaded). Run `npm install` and rebuild.",
      ),
    );
  } else {
    let cb =
      Js.wrap_callback((result: Js.t(Js.js_string)) =>
        k(TestGen.parse_model(Js.to_string(result)))
      );
    ignore(
      Js.Unsafe.fun_call(
        Js.Unsafe.global##.hazelZ3Solve,
        [|Js.Unsafe.inject(Js.string(script)), Js.Unsafe.inject(cb)|],
      ),
    );
  };
