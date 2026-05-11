/* Smoke tests for the inference pipeline.

   The branch's whole purpose is type-hole inference: Statics emits
   unification constraints, Inference.go consumes them and produces a
   TypSolutionMap. After the elastatics merge, the constraint flow was
   re-threaded through the new combined Statics. These tests check that
   inference is actually being driven end-to-end on representative
   programs — they don't check specific solutions, only that the
   inference map gets populated when constraints exist. */

open Alcotest;
open Language;

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s, ~root=Haz3lcore.Sort.Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let run_inference = (src: string): Inference.TypSolutionMap.t => {
  let term = parse_exp(src);
  let (_info_map, _elab, inference_map) =
    Statics.mk_with_inference(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      term,
    );
  inference_map;
};

let tests = (
  "Inference",
  [
    test_case("inference produces non-empty map for hole over int", `Quick, () => {
      /* `(? : ? -> ?)(1)` — applying an unknown function to an Int. The
         constraint between the function's arrow type and the call site
         should give the function input parameter an Int solution. */
      let inference = run_inference({|(? : ? -> ?)(1)|});
      let card = Inference.TypSolutionMap.cardinal(inference);
      check(
        bool,
        "inference map should have at least one solution",
        true,
        card > 0,
      );
    }),
    test_case("inference handles a fully-typed program", `Quick, () => {
      /* `1 + 2` has no unknowns and no inference work to do; the map can
         legitimately be empty but shouldn't crash. */
      let _inference = run_inference({|1 + 2|});
      check(bool, "didn't crash", true, true);
    }),
    test_case("inference handles let with annotation", `Quick, () => {
      let _inference = run_inference({|let x : Int = 1 in x + 2|});
      check(bool, "didn't crash", true, true);
    }),
  ],
);
