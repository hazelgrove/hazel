/* Hazel as a JavaScript library (a draft): window.Hazel. The working part,
   and what the values look like, is HazelLibCore. */
open Js_of_ocaml;
open HazelLibCore;

let () =
  Js.Unsafe.set(
    Js.Unsafe.global,
    "Hazel",
    Js.Unsafe.obj([|
      ("version", js_string(version)),
      ("build", js_string(build)),
      (
        "analyze",
        Js.Unsafe.inject(Js.wrap_callback(t => analyze(Js.to_string(t)))),
      ),
      (
        "run",
        Js.Unsafe.inject(Js.wrap_callback(t => run(Js.to_string(t)))),
      ),
      (
        "analyzeProgram",
        Js.Unsafe.inject(Js.wrap_callback(analyze_program)),
      ),
      ("runProgram", Js.Unsafe.inject(Js.wrap_callback(run_program))),
      /* the evaluator's passes since the last read, then back to 0: a debug
         hook for what a call costs (^ticket(552)) */
      (
        "passes",
        Js.Unsafe.inject(
          Js.wrap_callback(() => {
            let n = Language.Evaluator.passes^;
            Language.Evaluator.passes := 0;
            Js.number_of_float(float_of_int(n));
          }),
        ),
      ),
    |]),
  );
