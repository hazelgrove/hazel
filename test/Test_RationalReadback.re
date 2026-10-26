open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Rational readback: a Rational value or operator that a program write
   copies from an evaluated term into source text must reparse to the
   same value, type and operator kind. There is no rational literal
   syntax; the fraction that result views display (`1/2`) reparses as Int
   division, so source writes must not use it. A visible type error after
   the write is a failure here, not a success. */

let rat_value = (n: int, d: int): Exp.t =>
  switch (Util.Rational.make(Bigint.of_int(n), Bigint.of_int(d))) {
  | Some(r) => IdTagged.FreshGrammar.Exp.basic(Rat(r))
  | None => Alcotest.fail("zero denominator")
  };

let int_value = (n: int): Exp.t => IdTagged.FreshGrammar.Exp.int(n);
let bin = (op, a, b) => IdTagged.FreshGrammar.Exp.bin_op(op, a, b);
let un = (op, a) => IdTagged.FreshGrammar.Exp.un_op(op, a);

/* The program writer's own printer (whole-program fallback). */
let write = (e: Exp.t): string =>
  EditingPrelude.print_seg(Haz3lcore.EditorTransform.exp_to_segment(e));

let static_errors = (info_map: Statics.Map.t): list(Mark.t) =>
  Test_Statics_Prelude.collect_errors(info_map)
  |> Id.Map.bindings
  |> List.concat_map(snd);

/* Parse, check and elaborate source text; fails on any static error. */
let reread = (text: string): Exp.t => {
  let u = parse_exp(text);
  let (info_map, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), u);
  switch (static_errors(info_map)) {
  | [] => elab
  | marks =>
    Alcotest.fail(
      "static errors after rereading `"
      ++ text
      ++ "`: "
      ++ String.concat("; ", List.map(Mark.show, marks)),
    )
  };
};

let rec strip = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e) => strip(e)
  | _ => e
  };

let value = (e: Exp.t): Exp.t => strip(evaluate(e));

let same_value = (a: Exp.t, b: Exp.t): bool =>
  switch (value(a).term, value(b).term) {
  | (Atom(x), Atom(y)) => Atom.equal(x, y)
  | _ => Exp.fast_equal(value(a), value(b))
  };

let whole_nonneg = (r: Util.Rational.t): bool =>
  Bigint.equal(Util.Rational.den(r), Bigint.one)
  && Bigint.compare(Util.Rational.num(r), Bigint.zero) >= 0;

/* `got` (reread and elaborated) is faithful to `src`: every operator keeps
   its kind, every atom keeps its value and class, and an atom that has a
   source spelling of its own (a whole, non-negative number) comes back
   as exactly that atom. Returns the first discrepancy. */
let rec unfaithful = (src: Exp.t, got: Exp.t): option(string) => {
  let got = strip(got);
  let mismatch = what =>
    Some(what ++ ": wrote " ++ Exp.show(src) ++ ", got " ++ Exp.show(got));
  switch (src.term, got.term) {
  | (Atom(Rat(r)), _) =>
    switch (value(got).term) {
    | Atom(Rat(r')) when Util.Rational.equal(r, r') =>
      whole_nonneg(r) && !Exp.fast_equal(src, got)
        ? mismatch("whole Rational not reread as the same atom") : None
    | _ => mismatch("Rational value")
    }
  | (Atom(Int(n)), _) =>
    switch (value(got).term) {
    | Atom(Int(n')) when Bigint.equal(n, n') => None
    | _ => mismatch("Int value")
    }
  | (Atom(a), Atom(b)) => Atom.equal(a, b) ? None : mismatch("atom")
  | (BinOp(op, a, b), BinOp(op', a', b')) =>
    Operators.equal_op_bin(op, op')
      ? switch (unfaithful(a, a')) {
        | Some(m) => Some(m)
        | None => unfaithful(b, b')
        }
      : mismatch("binary operator kind")
  | (UnOp(op, a), UnOp(op', a')) =>
    Operators.equal_op_un(op, op')
      ? unfaithful(a, a') : mismatch("unary operator kind")
  | (Tuple(xs), Tuple(ys))
  | (ListLit(xs), ListLit(ys)) =>
    List.length(xs) == List.length(ys)
      ? List.fold_left2(
          (acc, x, y) => acc == None ? unfaithful(x, y) : acc,
          None,
          xs,
          ys,
        )
      : mismatch("arity")
  | (Ap(_, _, x), Ap(_, _, y)) => unfaithful(x, y)
  | (BinOp(_) | UnOp(_) | Tuple(_) | ListLit(_) | Ap(_), _) =>
    mismatch("form")
  | _ => None
  };
};

/* Write an evaluated term as source, reread it, and require the reread
   term to be faithful and to evaluate to the same value. */
let roundtrip = (name: string, src: Exp.t) =>
  test_case(name, `Quick, () => {
    let text = write(src);
    let got = reread(text);
    switch (unfaithful(src, got)) {
    | Some(m) => Alcotest.fail(m ++ " (text `" ++ text ++ "`)")
    | None =>
      check(bool, "same value (text `" ++ text ++ "`)", true, same_value(src, got))
    };
  });

/* ---- Program writes through EditorTransform.apply_patch ---- */

let first_hole_id = (e: Exp.t): Id.t => {
  let found = ref(None);
  let _ =
    TermBase.Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) =>
          switch (e.term) {
          | EmptyHole when found^ == None =>
            found := Some(Exp.rep_id(e));
            e;
          | _ => continue(e)
          },
      e,
    );
  switch (found^) {
  | Some(id) => id
  | None => Alcotest.fail("no expression hole")
  };
};

/* Fill the first expression hole of `program` with `payload`, then reread
   the whole written program, which must evaluate to `expected`. */
let patch_program = (name: string, program: string, payload: Exp.t, expected) =>
  test_case(name, `Quick, () => {
    let z = Test_EditorTransform.parse_zipper(program);
    let target_id = first_hole_id(Test_EditorTransform.zipper_term(z));
    let z =
      Haz3lcore.EditorTransform.apply_patch(
        z,
        Haz3lcore.EditorTransform.mk_patch(~target_id, payload),
      );
    let text = Test_EditorTransform.serialize(z);
    check(
      bool,
      "same value (text `" ++ text ++ "`)",
      true,
      same_value(expected, reread(text)),
    );
  });

/* Fill the second proof hole (after a first `eval` step) of `program` with
   an `eval <at_exp> at 0` step, then reread and check the written proof:
   no static error, and the step's `at_exp` finds its target in the goal
   left by the first step. */
let patch_eval_step = (name: string, program: string, at_exp: Exp.t) =>
  test_case(name, `Quick, () => {
    let z = Test_EditorTransform.parse_zipper(program);
    let is_hole = (p: Proof.t) => p.term == EmptyHole;
    let step =
      Proof.fresh(
        EvalStep({
          at_idx: int_value(0),
          at_exp,
        }),
      );
    let z =
      Test_EditorTransform.patch_proof_zipper(
        ~select=
          e =>
            switch (Test_EditorTransform.find_theorem_proof(e)) {
            | Some(p) =>
              switch (Test_EditorTransform.find_proofs(is_hole, p)) {
              | [hole, ..._] => Some(hole)
              | [] => None
              }
            | None => None
            },
        z,
        step,
      );
    let text = Test_EditorTransform.serialize(z);
    let _ = reread(text);
    let (state, _, elab) =
      Test_ProofMap.eval_with_proof(parse_exp(text));
    let pm = EvaluatorState.get_proof_map(state);
    check(
      bool,
      "the written at_exp is found in the goal (text `" ++ text ++ "`)",
      false,
      Test_ProofMap.has_mark_kind(
        pm,
        Test_ProofMap.proof_of(elab),
        fun
        | ProofMark.PatternNotFound(_) => true
        | _ => false,
      ),
    );
  });

let tests = (
  "RationalReadback",
  [
    /* The type token, in both parsers */
    test_case("Rational type: structure editor parse and print", `Quick, () => {
      let src = "let x : Rational = rat(1, 2) in x";
      let e = reread(src);
      check(bool, "value", true, same_value(rat_value(1, 2), e));
      check(
        bool,
        "printed back with the type token",
        true,
        Test_EditorTransform.contains_substring(
          write(parse_exp(src)),
          "Rational",
        ),
      );
    }),
    test_case("Rational type: menhir parse and AST", `Quick, () => {
      let src = "let x : Rational = rat(1, 2) in x";
      let (u, _) =
        MenhirParser.Conversion.Exp.get_indicated_ids(
          MenhirParser.Conversion.Exp.of_menhir_ast(
            MenhirParser.Interface.parse_program(src),
          ),
        );
      let (info_map, elab) =
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), u);
      check(int, "no static errors", 0, List.length(static_errors(info_map)));
      check(bool, "value", true, same_value(rat_value(1, 2), elab));
      check(
        bool,
        "core Rational converts to the menhir Rational type",
        true,
        MenhirParser.Conversion.Typ.of_core(
          MenhirParser.Conversion.IndicatedG.Typ.rational(),
        )
        == MenhirParser.AST.RationalType,
      );
    }),
    /* Values */
    roundtrip("value 1/2", rat_value(1, 2)),
    roundtrip("negative value -3/4", rat_value(-3, 4)),
    roundtrip("whole value 5 stays a Rational atom", rat_value(5, 1)),
    roundtrip("zero stays a Rational atom", rat_value(0, 1)),
    roundtrip("negative whole value -5", rat_value(-5, 1)),
    roundtrip(
      "values nested in a list",
      IdTagged.FreshGrammar.Exp.list_lit([rat_value(1, 2), rat_value(2, 3)]),
    ),
    roundtrip(
      "value beside an Int in a tuple",
      IdTagged.FreshGrammar.Exp.tuple([rat_value(1, 3), int_value(1)]),
    ),
    /* Operators outside any use */
    roundtrip("Rational +", bin(Rat(Plus), rat_value(1, 2), rat_value(1, 3))),
    roundtrip("Rational *", bin(Rat(Times), rat_value(1, 2), rat_value(2, 1))),
    roundtrip("Rational /", bin(Rat(Divide), rat_value(1, 2), rat_value(3, 1))),
    roundtrip("Rational unary -", un(Rat(Minus), rat_value(1, 2))),
    roundtrip(
      "Rational < is Bool",
      bin(Rat(LessThan), rat_value(1, 3), rat_value(1, 2)),
    ),
    roundtrip(
      "Rational ** with a positive Int exponent",
      bin(Rat(Power), rat_value(2, 3), int_value(2)),
    ),
    roundtrip(
      "Rational ** with a negative Int exponent",
      bin(Rat(Power), rat_value(2, 3), int_value(-2)),
    ),
    roundtrip(
      "Rational ** with an Int-operator exponent",
      bin(
        Rat(Power),
        rat_value(1, 2),
        bin(Int(Plus), int_value(1), int_value(1)),
      ),
    ),
    roundtrip(
      "Int operator beside a Rational operator in a tuple",
      IdTagged.FreshGrammar.Exp.tuple([
        bin(Rat(Plus), rat_value(1, 2), rat_value(1, 2)),
        bin(Int(Divide), int_value(7), int_value(2)),
      ]),
    ),
    /* Written into a program */
    patch_program(
      "a user `rat` binding does not capture the written value",
      "let rat = fun (a, b) -> 0 in ?",
      rat_value(1, 2),
      rat_value(1, 2),
    ),
    patch_program(
      "an enclosing use Nat does not re-kind the written value",
      "use Nat in ?",
      rat_value(1, 2),
      rat_value(1, 2),
    ),
    patch_program(
      "an enclosing use Nat does not re-kind a written Rational operator",
      "use Nat in ?",
      bin(Rat(Plus), rat_value(1, 2), rat_value(1, 3)),
      rat_value(5, 6),
    ),
    /* Proof steps: the written at_exp must locate the evaluated goal term */
    patch_eval_step(
      "proof at_exp names an evaluated whole Rational",
      "theorem t = (use Rational in rat(2, 1) * 2) == (use Rational in 4) proof eval rat(2, 1) at 0 end; ? in 1",
      bin(Rat(Times), rat_value(2, 1), rat_value(2, 1)),
    ),
    patch_eval_step(
      "proof at_exp names an evaluated non-whole Rational",
      "theorem t = (use Rational in rat(1, 2) * 2) == (use Rational in 1) proof eval rat(1, 2) at 0 end; ? in 1",
      bin(Rat(Times), rat_value(1, 2), rat_value(2, 1)),
    ),
  ],
);
