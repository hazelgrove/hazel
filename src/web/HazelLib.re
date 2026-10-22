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
      /* MEASURING ONLY (^ticket(540)): evaluate calls and steps by kind since
         the last read, then reset */
      (
        "stepCounts",
        Js.Unsafe.inject(
          Js.wrap_callback(() => {
            let fields =
              Hashtbl.fold(
                (k, v, acc) =>
                  [
                    (
                      k,
                      Js.Unsafe.inject(Js.number_of_float(float_of_int(v))),
                    ),
                    ...acc,
                  ],
                Language.Evaluator.step_counts,
                [],
              );
            Hashtbl.reset(Language.Evaluator.step_counts);
            Js.Unsafe.obj(Array.of_list(fields));
          }),
        ),
      ),
    |]),
  );
