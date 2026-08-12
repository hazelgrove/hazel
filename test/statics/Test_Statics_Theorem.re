open Alcotest;
open Language;
open Test_Statics_Prelude;
open FTemp;
open Typ;

let all_marks = serialized =>
  parse_exp(serialized) |> statics |> errors |> List.concat_map(snd);

let has_free = (name, marks) =>
  List.exists(
    fun
    | Mark.Free(v) => v == name
    | _ => false,
    marks,
  );

let theorem_ctx = serialized => {
  let theorem = parse_exp(serialized);
  statics(theorem)
  |> Statics.Map.ctx_of(Language.Exp.rep_id(theorem), _)
  |> Option.value(~default=Ctx.empty);
};

let tests = (
  "Statics.Theorem",
  [
    fully_consistent_typecheck(
      "Theorem assumes a consistently typed free variable",
      "theorem identity = x == x in 0",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "Real theorem assumptions resolve polymorphic equality",
      "theorem real_identity = use Real in (x ** 2 == x ** 2) in 0",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "Completing-square statement infers its Real variable",
      "theorem completing_the_square = use Real in (x ** 2 + 6 * x + 5 == (x + 3) ** 2 - 4) in 0",
      Some(int()),
    ),
    fully_consistent_typecheck(
      "Multiple Real variables are independently assumed",
      "theorem real_pair = use Real in (x + y == y + x) in 0",
      Some(int()),
    ),
    test_case(
      "Theorem root context exposes inferred assumptions to its stepper",
      `Quick,
      () => {
        let ctx =
          theorem_ctx(
            "theorem completing_the_square = use Real in (x ** 2 + 6 * x + 5 == (x + 3) ** 2 - 4) in 0",
          );
        check(
          Alcotest.bool,
          "x is available to generated theorem rows",
          true,
          Ctx.lookup_var(ctx, "x") |> Option.is_some,
        );
      },
    ),
    test_case(
      "Reflexivity remains applicable to a Real theorem endpoint",
      `Quick,
      () => {
        let wrapped =
          parse_exp("use Real in ((x + 3) ** 2 - 4 == (x + 3) ** 2 - 4)");
        let equality =
          switch (wrapped.term) {
          | Use(_, equality) => equality
          | _ => Alcotest.fail("Expected a use expression")
          };
        let rule =
          ProofCtx.lookup_rule("Reflexive(==)", Axioms.v) |> Option.get;
        let (forward, backward) =
          ProofRule.can_eq(
            ~info_map=Statics.Map.empty,
            ~env=Environment.empty,
            rule,
            equality,
          );
        check(
          Alcotest.bool,
          "reflexivity rewrite is active",
          true,
          Option.is_some(forward) || Option.is_some(backward),
        );
      },
    ),
    test_case(
      "Theorem assumptions do not escape the statement",
      `Quick,
      () => {
        let marks = all_marks("theorem identity = x == x in y");
        check(
          Alcotest.bool,
          "x is locally assumed",
          false,
          has_free("x", marks),
        );
        check(Alcotest.bool, "y remains free", true, has_free("y", marks));
      },
    ),
    inconsistent_typecheck(
      "Conflicting uses of an implicit theorem variable remain invalid",
      parse_exp("theorem bad = (x + 1 == 2) && x in 0"),
    ),
    inconsistent_typecheck(
      "An existing binding is not replaced by a theorem assumption",
      parse_exp(
        "let x = true in theorem bad = use Real in (x + 1 == 2) in 0",
      ),
    ),
  ],
);
