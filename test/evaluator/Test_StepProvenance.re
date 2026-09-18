open Alcotest;
open Language;
open Transition;

/* Slice 1 of the observation-trace design (plans/observation-trace.md):
 * step provenance is declared, inertly, by classification over step_kind.
 * These tests pin the classification — in particular the kinds whose
 * rules can rebuild a redex under its own id (rewrap / Ascriptions'
 * fast_copy) and thereby CONTINUE an observation span (the delegation
 * law in Evaluator.eval_3_record_probe_sample). */

let is_proper = (k: step_kind): bool =>
  switch (provenance_of_kind(k)) {
  | Proper => true
  | Administrative(_) => false
  };

let may_delegate = (k: step_kind): bool =>
  switch (provenance_of_kind(k)) {
  | Administrative({may_delegate}) => may_delegate
  | Proper => false
  };

let tests = (
  "StepProvenance",
  [
    test_case("Real computation steps are Proper", `Quick, () => {
      List.iter(
        k =>
          check(bool, show_step_kind(k) ++ " proper", true, is_proper(k)),
        [
          FunAp,
          DeferredAp,
          TypFunAp,
          BuiltinAp("string_length"),
          LetBind("x"),
          CaseApply,
          Conditional(true),
          BinOp(Operators.Int(Operators.Plus)),
          Seq,
          VarLookup,
          FixUnwrap,
        ],
      )
    }),
    test_case(
      "Bookkeeping steps are Administrative and non-delegating", `Quick, () => {
      List.iter(
        k => {
          check(
            bool,
            show_step_kind(k) ++ " administrative",
            false,
            is_proper(k),
          );
          check(
            bool,
            show_step_kind(k) ++ " non-delegating",
            false,
            may_delegate(k),
          );
        },
        [
          WrapClosure,
          FixClosure,
          CompleteClosure,
          CompleteFilter,
          BuiltinWrap,
          RemoveParens,
        ],
      )
    }),
    test_case(
      "Cast/ascription steps are the delegating class",
      `Quick,
      () => {
        List.iter(
          k =>
            check(
              bool,
              show_step_kind(k) ++ " may delegate",
              true,
              may_delegate(k),
            ),
          [Ascription, AscriptionAp, AscriptionTypAp],
        );
        /* The delegating class is exactly the ascription family: any rule
         * that starts rebuilding redexes under preserved ids outside it
         * must either join the class or stop preserving ids. */
        check(
          bool,
          "no delegating kinds outside the ascription family",
          false,
          List.exists(
            may_delegate,
            [
              FunAp,
              DeferredAp,
              WrapClosure,
              FixClosure,
              FixUnwrap,
              CompleteClosure,
              RemoveParens,
              CaseApply,
              VarLookup,
            ],
          ),
        );
      },
    ),
  ],
);
