open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* `==` is the equality the checker rewrites with, so it has to tell apart
 * any two floats a program can: `0.0` and `-0.0` differ (`1.0 /. x`
 * tells them apart). Comparing terms, matching float patterns and
 * checking their coverage have to agree with it. In an expression
 * `-0.0` is a negation, so a `-0.0` literal only reaches a goal through a
 * `step`; in a pattern it is a literal. */

let not_an_implication_goal: ProofMark.t => bool =
  fun
  | ExpectedImplicationGoal(_) => true
  | _ => false;

let check_assume_refused = src => {
  let (pm, proof) = run_named("bad", src);
  Alcotest.check(
    Alcotest.bool,
    "the assume is refused\n" ++ dump(pm, proof),
    true,
    has_mark_kind(pm, proof, not_an_implication_goal),
  );
};

/* `case x | 0.0 => 1 | _ => 2 end` at `x = -0.0`. */
let zero_pattern_at_negative_zero = "case 0.0 *. -1.0 | 0.0 => 1 | _ => 2 end";

/* The coverage marks the statics put on `src`, sorted. A term with
 * several ids (`-0.0` is two tokens) is stored under each, so each info
 * is counted once, under its own id. */
let coverage_marks = (src): list(string) => {
  let (statics, _) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      parse_exp(src),
    );
  Id.Map.fold(
    (id, info, acc) =>
      !Id.equal(id, Info.id_of(info))
        ? acc
        : List.filter_map(
            ~f=
              fun
              | Mark.Redundant => Some("redundant")
              | InexhaustiveMatch(_) => Some("inexhaustive")
              | _ => None,
            Info.marks_of(info),
          )
          @ acc,
    statics,
    [],
  )
  |> List.sort(~compare=String.compare);
};

let check_coverage = (~expected, src) =>
  Alcotest.check(
    Alcotest.(list(string)),
    src,
    expected,
    coverage_marks(src),
  );

let tests = (
  "FloatEquality",
  [
    Alcotest.test_case(
      "a proof that rewrites with `x == 0.0` holds at -0.0",
      `Quick,
      () => {
        let (pm, proof) =
          run_named(
            "t",
            "theorem t = forall x: Float -> x == 0.0 ==> 1.0 /. x == 1.0 /. 0.0 proof assume h = x == 0.0 => from x do rewrite h end end; rewrite refl end in t",
          );
        check_proven("t", pm, proof);
        evaluation_test(
          "t's statement at -0.0",
          IdTagged.FreshGrammar.Exp.bool(true),
          elaborate(
            parse_exp("let x = -0.0 in x == 0.0 ==> 1.0 /. x == 1.0 /. 0.0"),
          ),
        );
      },
    ),
    Alcotest.test_case(
      "assume doesn't take `x == 0.0` for the antecedent `x == -0.0`",
      `Quick,
      () =>
      check_assume_refused(
        "theorem bad = forall x: Float -> x == -0.0 ==> 1.0 /. x == 1.0 /. 0.0 proof from -0.0 do step end; assume h = x == 0.0 => from x do rewrite h end end; rewrite refl end in bad",
      )
    ),
    Alcotest.test_case(
      "assume doesn't take a definition that is 0.0 for -0.0", `Quick, () =>
      check_assume_refused(
        "let z = 0.0 in theorem bad = forall x: Float -> x == -0.0 ==> 1.0 /. x == 1.0 /. z proof from -0.0 do step end; assume h = x == z => from x do rewrite h end end; rewrite refl end in bad",
      )
    ),
    Alcotest.test_case(
      "a float pattern matches the values `==` equates with it",
      `Quick,
      () => {
        let evaluates_to = (expected, src) =>
          evaluation_test(
            src,
            IdTagged.FreshGrammar.Exp.int(expected),
            elaborate(parse_exp(src)),
          );
        evaluates_to(2, zero_pattern_at_negative_zero);
        evaluates_to(2, "case 0.0 | -0.0 => 1 | _ => 2 end");
        evaluates_to(1, "case 0.0 *. -1.0 | -0.0 => 1 | _ => 2 end");
        evaluates_to(1, "case 0.0 | 0.0 => 1 | _ => 2 end");
      },
    ),
    Alcotest.test_case("a NaN pattern matches every NaN", `Quick, () => {
      IdTagged.FreshGrammar.(
        evaluation_test(
          "case nan | -nan => 1 | _ => 2 end",
          Exp.int(1),
          Exp.match(
            Exp.float(Float.nan),
            [
              (Pat.float(Float.neg(Float.nan)), Exp.int(1)),
              (Pat.wild(), Exp.int(2)),
            ],
          ),
        )
      )
    }),
    Alcotest.test_case(
      "the stepper's case step matches float patterns as `==` compares",
      `Quick,
      () =>
      Alcotest.check(
        step_limited,
        zero_pattern_at_negative_zero,
        LimitedCompleted((
          IdTagged.FreshGrammar.Exp.int(2),
          EvaluatorState.empty,
        )),
        full_small_step_reduction(
          elaborate(parse_exp(zero_pattern_at_negative_zero)),
        ),
      )
    ),
    Alcotest.test_case(
      "coverage: a -0.0 arm after a 0.0 arm is not redundant", `Quick, () =>
      check_coverage(
        ~expected=[],
        "let x: Float = ? in case x | 0.0 => 1 | -0.0 => 2 | _ => 3 end",
      )
    ),
    Alcotest.test_case(
      "coverage: a repeated -0.0 arm is redundant", `Quick, () =>
      check_coverage(
        ~expected=["redundant"],
        "let x: Float = ? in case x | -0.0 => 1 | -0.0 => 2 | _ => 3 end",
      )
    ),
    Alcotest.test_case(
      "coverage: a catch-all row covers -0.0 as well as 0.0", `Quick, () =>
      check_coverage(
        ~expected=[],
        "let x: (Float, Bool) = ? in case x | (0.0, true) => 1 | (-0.0, true) => 2 | _ => 3 end",
      )
    ),
    Alcotest.test_case(
      "coverage: floats that agree to 12 digits are different arms", `Quick, () =>
      check_coverage(
        ~expected=[],
        "let x: (Float, Bool) = ? in case x | (_, true) => 1 | (1.0000000000001, false) => 2 | (1.0000000000002, false) => 3 | _ => 4 end",
      )
    ),
  ],
);
