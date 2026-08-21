open Haz3lcore;
open Language;
open ExplainThisForm;
open Example;

/* ExplainThis documentation for the Proof sort — the prover's step forms
 * (docs/prover-obligations.md). Distilled from that document; nothing here
 * states semantics it does not.
 *
 * House rule for this file: every message names the citable hypotheses the
 * step brings into scope (`where`, `assume`, `case_eq`, `ih`, `have`) and
 * says whether the step incurs an obligation, because that is what a user
 * sitting on the step needs in order to write the next one. */

/* A child slot, padded so the tile's shards read as delimiters. */
let slot = (p: Piece.t): Segment.t => [space(), p, space()];

let proof_body = () => proof("proof");

/* --- degenerate forms ---------------------------------------------------- */

let empty_hole: Simple.t = {
  group_id: ProofEmptyHole,
  form_id: ProofEmptyHole,
  abstract: ([proof("?")], []),
  explanation: "An empty proof. The goal it was handed is passed through unproven, so the theorem is at most partially proven. The stepper's step picker at this position offers the steps that apply to this goal.",
  examples: [],
};

let multi_hole: Simple.t = {
  group_id: ProofMultiHole,
  form_id: ProofMultiHole,
  abstract: ([proof("?")], []),
  explanation: "This proof is incomplete syntax: the pieces here do not yet form a proof step, so nothing is checked and the goal passes through unproven.",
  examples: [],
};

let invalid: Simple.t = {
  group_id: ProofInvalid,
  form_id: ProofInvalid,
  abstract: ([proof("?")], []),
  explanation: "Not a valid proof step: this text does not form any of the proof forms, so nothing is checked here and the goal passes through unproven. The stepper's step picker lists the steps that apply at this goal.",
  examples: [],
};

/* --- sequencing ---------------------------------------------------------- */

let seq = (~fst_id: Id.t, ~snd_id: Id.t): Simple.t => {
  let (p1, p2) = (proof("step"), proof("proof"));
  {
    group_id: ProofSeqStep,
    form_id: ProofSeqStep,
    abstract: (
      [p1, space(), proof_seq(), space(), p2],
      [(Piece.id(p1), fst_id), (Piece.id(p2), snd_id)],
    ),
    explanation:
      Printf.sprintf(
        "Runs the [*first step*](%s) on the current goal, then hands the goal it produces to the [*rest of the proof*](%s). A proof is a chain of such steps, each rewriting the goal; the theorem is proven when the last one leaves the literal `true`.",
        fst_id |> Id.to_string,
        snd_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofSeq1,
        term:
          mk_example(
            "theorem t = 1 + 4 == 5 proof eval 1 + 4 at 0 end; eval 5 == 5 at 0 end in 0",
          ),
        message: "The first step evaluates `1 + 4`, leaving the goal `5 == 5`; the second evaluates that to `true`.",
      },
    ],
  };
};

/* --- citation: axiom / axiomrev, with and without `with` ----------------- */

let axiom = (~name_id: Id.t, ~idx_id: Id.t, ~target_id: Id.t): Simple.t => {
  let (nm, ix, tg) = (exp("fact"), exp("i"), exp("target"));
  {
    group_id: ProofAxiomStep,
    form_id: ProofAxiomStep,
    abstract: (
      [mk_axiom([slot(nm), slot(ix), slot(tg)])],
      [
        (Piece.id(nm), name_id),
        (Piece.id(ix), idx_id),
        (Piece.id(tg), target_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Cites the [*fact*](%s) — a theorem, a built-in axiom, or a hypothesis in scope such as `where`, `assume`, `case_eq`, `ih` or `have` — and applies it left-to-right as a rewrite on the goal. The [*target*](%s) says which term to rewrite, and may use `$e` to match any expression and `$v` to match any value; the [*index*](%s) picks which occurrence matching that target, counting from 0. A cited fact whose statement is a bare boolean proposition `P` reads as `P == true`, so citing it rewrites `P` to `true`. A bare name resolves to the innermost introduction still in scope, so at depth `case_eq` and `ih` mean this split's; a fact that a nearer name hides is still listed, greyed out, until an `alias` gives it a name of its own. Instantiating a quantified or restricted fact incurs its restrictions here as obligations, and add `with x = e` when the match cannot recover a binder on its own.",
        name_id |> Id.to_string,
        target_id |> Id.to_string,
        idx_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAxiom1,
        term:
          mk_example(
            "theorem t = (true && false) == (false && true) proof axiom and_comm at 0 on true && false end; axiom refl_eq at 0 on (false && true) == (false && true) end in 0",
          ),
        message: "`and_comm` is one of the built-in Kleene laws for the boolean connectives. Citing it on the first occurrence of `true && false` rewrites that side of the equation, and `refl_eq` then closes the goal.",
      },
    ],
  };
};

let axiom_with =
    (
      ~name_id: Id.t,
      ~var_id: Id.t,
      ~inst_id: Id.t,
      ~idx_id: Id.t,
      ~target_id: Id.t,
    )
    : Simple.t => {
  let (nm, vr, it, ix, tg) = (
    exp("fact"),
    exp("x"),
    exp("e"),
    exp("i"),
    exp("target"),
  );
  {
    group_id: ProofAxiomStep,
    form_id: ProofAxiomStep,
    abstract: (
      [
        mk_axiom_with([slot(nm), slot(vr), slot(it), slot(ix), slot(tg)]),
      ],
      [
        (Piece.id(nm), name_id),
        (Piece.id(vr), var_id),
        (Piece.id(it), inst_id),
        (Piece.id(ix), idx_id),
        (Piece.id(tg), target_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Cites the [*fact*](%s) and applies it left-to-right as a rewrite on the [*target*](%s), at the occurrence given by the [*index*](%s), counting from 0. The `with` clause instantiates the fact's quantified [*variable*](%s) at the [*witness*](%s) explicitly, before matching: this is what to write when the variable appears only in the fact's antecedent, so matching the conclusion cannot determine it. The witness is checked like any other instantiation — it must be visibly terminating, and its domain conditions (`e != 0`, `i >= 0`) become obligations on this step.",
        name_id |> Id.to_string,
        target_id |> Id.to_string,
        idx_id |> Id.to_string,
        var_id |> Id.to_string,
        inst_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAxiomWith1,
        term:
          mk_example(
            "let f = fun x -> x + 1 in\ntheorem lem = forall n: Int -> n == 1 ==> f(2) == 3 proof ? in\ntheorem g = f(2) == 3 proof axiom lem with n = 1 at 0 on f(2) end; eval 3 == 3 at 0 end in 0",
          ),
        message: "`lem`'s binder `n` occurs only in its antecedent, so matching `f(2) == 3` leaves `n` undetermined and the citation is refused without a `with` clause. `with n = 1` supplies it; the antecedent `1 == 1` is then the obligation this step incurs.",
      },
    ],
  };
};

let axiomrev = (~name_id: Id.t, ~idx_id: Id.t, ~target_id: Id.t): Simple.t => {
  let (nm, ix, tg) = (exp("fact"), exp("i"), exp("target"));
  {
    group_id: ProofAxiomRevStep,
    form_id: ProofAxiomRevStep,
    abstract: (
      [mk_axiomrev([slot(nm), slot(ix), slot(tg)])],
      [
        (Piece.id(nm), name_id),
        (Piece.id(ix), idx_id),
        (Piece.id(tg), target_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "The reverse direction of `axiom`: cites the [*fact*](%s) and applies its equation right-to-left, replacing the [*target*](%s) — the occurrence given by the [*index*](%s), counting from 0 — with the equation's left-hand side. Sound for the same reason as the forward direction: the two sides denote the same value at every instantiation. On a fact whose statement is a bare boolean proposition `P`, the reverse direction rewrites a `true` in the goal into `P`; because `true` occurs everywhere, that reading is only ever applied to a fact you cite by name, and never offered by rule discovery.",
        name_id |> Id.to_string,
        target_id |> Id.to_string,
        idx_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAxiomRev1,
        term:
          mk_example(
            "theorem t = (true && false) == (false && true) proof axiomrev and_comm at 0 on false && true end; axiom refl_eq at 0 on (true && false) == (true && false) end in 0",
          ),
        message: "`and_comm` reads `$e && $f == $f && $e`. Applied in reverse at `false && true`, it rewrites that occurrence into `true && false`, making both sides of the equation identical.",
      },
    ],
  };
};

let axiomrev_with =
    (
      ~name_id: Id.t,
      ~var_id: Id.t,
      ~inst_id: Id.t,
      ~idx_id: Id.t,
      ~target_id: Id.t,
    )
    : Simple.t => {
  let (nm, vr, it, ix, tg) = (
    exp("fact"),
    exp("x"),
    exp("e"),
    exp("i"),
    exp("target"),
  );
  {
    group_id: ProofAxiomRevStep,
    form_id: ProofAxiomRevStep,
    abstract: (
      [
        mk_axiomrev_with([
          slot(nm),
          slot(vr),
          slot(it),
          slot(ix),
          slot(tg),
        ]),
      ],
      [
        (Piece.id(nm), name_id),
        (Piece.id(vr), var_id),
        (Piece.id(it), inst_id),
        (Piece.id(ix), idx_id),
        (Piece.id(tg), target_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Applies the [*fact*](%s) right-to-left at the [*target*](%s), on the occurrence given by the [*index*](%s), counting from 0, with its quantified [*variable*](%s) instantiated explicitly at the [*witness*](%s). Reverse application matches the equation's right-hand side, which often determines fewer binders than the left, so this is the direction that most often needs a `with` clause. The witness goes through the same totality and domain checks as a matched one; nothing is skipped by choosing it yourself.",
        name_id |> Id.to_string,
        target_id |> Id.to_string,
        idx_id |> Id.to_string,
        var_id |> Id.to_string,
        inst_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAxiomRevWith1,
        term:
          mk_example(
            "let f = fun x -> x + 1 in\ntheorem lem = forall n: Int -> n == 1 ==> f(2) == 3 proof ? in\ntheorem g = 3 == 3 proof axiomrev lem with n = 1 at 0 on 3 end in 0",
          ),
        message: "Used in reverse, `lem` rewrites the `3` in the goal back into `f(2)`. Its binder `n` is invisible in the conclusion either way, so the `with` clause is what makes the citation resolvable.",
      },
    ],
  };
};

/* --- rewrite / eval ------------------------------------------------------ */

let rewrite = (~target_id: Id.t, ~with_id: Id.t, ~idx_id: Id.t): Simple.t => {
  let (tg, wi, ix) = (exp("target"), exp("e"), exp("i"));
  {
    group_id: ProofRewriteStep,
    form_id: ProofRewriteStep,
    abstract: (
      [mk_rewrite_step([slot(tg), slot(wi), slot(ix)])],
      [
        (Piece.id(tg), target_id),
        (Piece.id(wi), with_id),
        (Piece.id(ix), idx_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "An algebraic rewrite: replaces the [*target*](%s) with the [*replacement*](%s) at the occurrence given by the [*index*](%s), counting from 0, and checks the two against a computer-algebra system. Because the CAS reasons in a field, the step incurs a domain obligation for every partial operation the rewrite moves across (a denominator's `e != 0`, an exponent's `i >= 0`), and it is refused outright on float-typed terms, where the field laws are false of IEEE arithmetic.",
        target_id |> Id.to_string,
        with_id |> Id.to_string,
        idx_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofRewrite1,
        term:
          mk_example(
            "theorem t = 1 + 1 == 2 proof rewrite 1 + 1 with 2 at 0 end in 0",
          ),
        message: "Rewrites the first occurrence of `1 + 1` to `2`, leaving the goal `2 == 2`.",
      },
    ],
  };
};

let eval_step = (~target_id: Id.t, ~idx_id: Id.t): Simple.t => {
  let (tg, ix) = (exp("target"), exp("i"));
  {
    group_id: ProofEvalStep,
    form_id: ProofEvalStep,
    abstract: (
      [mk_eval_step([slot(tg), slot(ix)])],
      [(Piece.id(tg), target_id), (Piece.id(ix), idx_id)],
    ),
    explanation:
      Printf.sprintf(
        "Takes a single evaluation step on the [*target*](%s) — the occurrence given by the [*index*](%s), counting from 0 — and substitutes the result back into the goal. The target may use `$e` to match any expression and `$v` to match any value. Evaluation preserves what the goal denotes, so this is the one step that is always sound and never incurs an obligation: unfolding a definition, applying a function, taking a branch.",
        target_id |> Id.to_string,
        idx_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofEval1,
        term:
          mk_example(
            "theorem t = (1 + 2) + (3 + 4) == 10 proof eval $v + $v at 1 end in 0",
          ),
        message: "`$v + $v` matches an addition of two values; `at 1` picks the second such occurrence, so this step evaluates `3 + 4` and leaves the goal `(1 + 2) + 7 == 10`.",
      },
    ],
  };
};

/* --- induction / case analysis ------------------------------------------- */

let induction = (~scrut_id: Id.t): Simple.t => {
  let (sc, pt, bd) = (exp("e"), pat("p"), proof("proof"));
  {
    group_id: ProofInductionStep,
    form_id: ProofInductionStep,
    abstract: (
      [
        mk_induction([
          [space(), sc, space(), mk_proof_rule([slot(pt)]), space(), bd],
        ]),
      ],
      [(Piece.id(sc), scrut_id)],
    ),
    explanation:
      Printf.sprintf(
        "Splits the goal into one case per pattern of the [*scrutinee*](%s). On a variable of an algebraic data type this is structural induction: the cases must cover the type exhaustively, and inside each case the pattern's equation is citable as `case_eq`, together with an inductive hypothesis `ih` for every sub-term of the scrutinee's own type. On a computed scrutinee, or a boolean one, it is case analysis instead — no `ih`, the split must still be exhaustive, the scrutinee's type must be known, and the scrutinee must be visibly terminating. Both names are fixed and shadow the enclosing ones, so inside a nested split they mean this split's: write `induction <e> as <name>` to keep an outer equation citable at depth, and `alias` to reach a hypothesis a nearer `ih` hides. Write `generalize` before the induction to get forall-quantified hypotheses, which can then be cited at other instantiations.",
        scrut_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofInduction1,
        term:
          mk_example(
            "type Nt = +Z+S(Nt) in\nlet pos = fun e -> case e | Z => true | S(b) => true end in\ntheorem t = forall e: Nt -> pos(e) proof induction e\n| Z => eval pos(Z) at 0 end\n| S(b) => revert pos(b) => axiom ih at 0 on pos(b) end; eval true ==> pos(S(b)) at 0 end; eval pos(S(b)) at 0 end\nend in 0",
          ),
        message: "The two cases cover `Nt` exhaustively. The base case evaluates outright; the step case cites the inductive hypothesis `ih`, which states the goal for the sub-term `b`.",
      },
    ],
  };
};

/* --- binder manipulation ------------------------------------------------- */

let forall_step = (~pat_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (pt, bd) = (pat("x"), proof_body());
  {
    group_id: ProofForallStep,
    form_id: ProofForallStep,
    abstract: (
      [mk_proof_forall([slot(pt)]), space(), bd],
      [(Piece.id(pt), pat_id), (Piece.id(bd), body_id)],
    ),
    explanation:
      Printf.sprintf(
        "Peels one quantifier off the goal, naming the quantified [*variable*](%s); the [*rest of the proof*](%s) then proves the body for that one arbitrary value. If the peeled binder carried a `where` restriction, the restriction becomes a hypothesis inside under the fixed name `where`, which a more deeply peeled restricted binder shadows in turn.",
        pat_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofForall1,
        term:
          mk_example(
            "theorem t = forall x -> x == x proof forall x => axiom refl_eq at 0 on x == x end in 0",
          ),
        message: "Peeling `forall x` leaves the goal `x == x` for an arbitrary `x`, which reflexivity closes.",
      },
    ],
  };
};

let assume = (~exp_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (e, bd) = (exp("e"), proof_body());
  {
    group_id: ProofAssumeStep,
    form_id: ProofAssumeStep,
    abstract: (
      [mk_assume([slot(e)]), space(), bd],
      [(Piece.id(e), exp_id), (Piece.id(bd), body_id)],
    ),
    explanation:
      Printf.sprintf(
        "Hypothesizes the [*proposition*](%s) for the [*rest of the proof*](%s), where it is citable as `assume`. When it is exactly the antecedent of an `==>` goal, this is implication introduction: the antecedent is stripped from the goal and nothing is owed. Otherwise the assumption incurs an obligation, which you can settle later — prove it here, float it onto an enclosing binder as a `where` restriction, or split on it. The name is fixed: a nested `assume` installs `assume` again and shadows this one, so write `assume <e> as <name>` when both have to stay citable.",
        exp_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAssume1,
        term:
          mk_example(
            "theorem t = forall n: Int -> n == 1 ==> n == 1 proof assume n == 1 => axiom assume at 0 on n == 1 end in 0",
          ),
        message: "The assumption is the goal's own antecedent, so this is implication introduction and incurs nothing. The hypothesis is then cited by its name, `assume`, to rewrite the remaining goal to `true`.",
      },
    ],
  };
};

let generalize = (~exp_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (e, bd) = (exp("x"), proof_body());
  {
    group_id: ProofGeneralizeStep,
    form_id: ProofGeneralizeStep,
    abstract: (
      [mk_generalize([slot(e)]), space(), bd],
      [(Piece.id(e), exp_id), (Piece.id(bd), body_id)],
    ),
    explanation:
      Printf.sprintf(
        "Re-quantifies an already-peeled [*variable*](%s), so the [*rest of the proof*](%s) proves `forall x -> goal` instead of the goal at this one value. Written before `induction`, this is what makes the inductive hypotheses forall-quantified, so they can be cited at instantiations other than the one in hand. The price is capture: every fact mentioning the variable — assumptions, `case_eq`, earlier `ih`s — is unavailable inside. A `where` restriction on the variable travels back onto the new binder, and is a citable `where` again once you re-peel it.",
        exp_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofGeneralize1,
        term:
          mk_example(
            "theorem inv = forall w where w != 0 -> w / w == 1 proof ? in\ntheorem t = forall n where n != 0 -> n / n == 1 proof generalize n => forall n => axiom inv at 0 on n / n end; axiom refl_eq at 0 on 1 == 1 end in 0",
          ),
        message: "`generalize n` re-quantifies the peeled `n`, carrying its `where` restriction back onto the new binder; re-peeling with `forall n` makes the restriction a citable `where` fact again, which discharges the obligation that citing `inv` incurs.",
      },
    ],
  };
};

/* --- revert -------------------------------------------------------------- */

let revert = (~exp_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (e, bd) = (exp("fact"), proof_body());
  {
    group_id: ProofRevertStep,
    form_id: ProofRevertStep,
    abstract: (
      [mk_revert([slot(e)]), space(), bd],
      [(Piece.id(e), exp_id), (Piece.id(bd), body_id)],
    ),
    explanation:
      Printf.sprintf(
        "Cashes the in-scope [*fact*](%s) — named by its hypothesis name, or spelled out — back into the goal: the [*rest of the proof*](%s) proves `fact ==> goal`. Nothing is owed, and the fact is not consumed: it holds here, so the implication denotes exactly what the goal denotes. The point is to move a hypothesis to where the evaluation and rewriting machinery can compute with it — rewrite the antecedent until it is `false`, and the implication collapses to `true`.",
        exp_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofRevert1,
        term:
          mk_example(
            "type Nt = +Z+S(Nt) in\nlet pos = fun e -> case e | Z => true | S(b) => true end in\ntheorem t = forall e: Nt -> pos(e) proof induction e\n| Z => eval pos(Z) at 0 end\n| S(b) => revert pos(b) => axiom ih at 0 on pos(b) end; eval true ==> pos(S(b)) at 0 end; eval pos(S(b)) at 0 end\nend in 0",
          ),
        message: "In the step case the inductive hypothesis `pos(b)` is reverted into the goal, giving `pos(b) ==> pos(S(b))`. Citing `ih` rewrites the antecedent to `true`, and evaluation takes the implication away.",
      },
    ],
  };
};

let revert_with =
    (~exp_id: Id.t, ~var_id: Id.t, ~inst_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (e, vr, it, bd) = (exp("fact"), exp("x"), exp("e"), proof_body());
  {
    group_id: ProofRevertStep,
    form_id: ProofRevertStep,
    abstract: (
      [mk_revert_with([slot(e), slot(vr), slot(it)]), space(), bd],
      [
        (Piece.id(e), exp_id),
        (Piece.id(vr), var_id),
        (Piece.id(it), inst_id),
        (Piece.id(bd), body_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Cashes the in-scope [*fact*](%s) into the goal at one chosen instance: the fact must be quantified over the [*variable*](%s), which is eliminated at the [*witness*](%s), and it is that instance the [*rest of the proof*](%s) gets as an antecedent. A `where` restriction on the eliminated binder survives as a further antecedent — dropping it would be unsound. The step itself owes nothing, but the witness passes the ordinary gates: it must be visibly terminating, and its domain conditions become obligations here. This is how a quantified inductive hypothesis is used at a value the split produced rather than the one it was generated at.",
        exp_id |> Id.to_string,
        var_id |> Id.to_string,
        inst_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofRevertWith1,
        term:
          mk_example(
            "let f = fun x -> x + 1 in\ntheorem p = forall n: Int -> n + 0 == n proof ? in\ntheorem g = f(2) == 3 proof revert p with n = 5 => eval 5 + 0 at 0 end; eval 5 == 5 at 0 end; eval true ==> f(2) == 3 at 0 end; eval f(2) at 0 end; eval 2 + 1 at 0 end; eval 3 == 3 at 0 end in 0",
          ),
        message: "`revert p with n = 5` cashes the instance `5 + 0 == 5` into the goal, not the quantified statement. Evaluating that antecedent to `true` lets the implication be stepped away, leaving the original goal.",
      },
    ],
  };
};

/* --- contradiction ------------------------------------------------------- */

let contradiction = (~exp_id: Id.t): Simple.t => {
  let e = exp("fact");
  {
    group_id: ProofContradictionStep,
    form_id: ProofContradictionStep,
    abstract: ([mk_contradiction([slot(e)])], [(Piece.id(e), exp_id)]),
    explanation:
      Printf.sprintf(
        "Closes ANY goal by showing that the in-scope [*fact*](%s) — named by its hypothesis name, or spelled out — evaluates to `false`. If the hypotheses of this branch are jointly unsatisfiable then the branch is vacuous, and concluding it is the ex falso reading. This is the one-step way to dismiss the impossible case of a split, in place of reverting the fact and rewriting it to `false` by hand. It is terminal — there is no sub-proof — and it incurs no obligation. If the fact gets stuck instead of reaching `false`, the step is marked rather than believed.",
        exp_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofContradiction1,
        term:
          mk_example(
            "theorem t = forall n: Int -> false ==> n == n proof assume false => contradiction false end in 0",
          ),
        message: "`assume false` introduces the antecedent as a hypothesis; citing it evaluates to `false`, so the branch is vacuous and the goal closes.",
      },
    ],
  };
};

let contradiction_with =
    (~exp_id: Id.t, ~var_id: Id.t, ~inst_id: Id.t): Simple.t => {
  let (e, vr, it) = (exp("fact"), exp("x"), exp("e"));
  {
    group_id: ProofContradictionStep,
    form_id: ProofContradictionStep,
    abstract: (
      [mk_contradiction_with([slot(e), slot(vr), slot(it)])],
      [
        (Piece.id(e), exp_id),
        (Piece.id(vr), var_id),
        (Piece.id(it), inst_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Closes ANY goal by showing the in-scope [*fact*](%s) is `false` in this branch, after exactly one rewrite: the [*variable*](%s) is replaced by the [*expression*](%s) before the fact is evaluated. The rewrite must be licensed — `x == e` (in either orientation) has to be a fact in scope, typically the `case_eq` of the split that made this branch — and the checker refuses anything it cannot verify. Only the rewrite written here is applied; nothing is searched for, so the step's reasoning is what you can read.",
        exp_id |> Id.to_string,
        var_id |> Id.to_string,
        inst_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofContradictionWith1,
        term:
          mk_example(
            "theorem t = forall n: Int -> n == 1 ==> n == 2 ==> false proof assume n == 1 => assume n == 2 => contradiction n == 1 with n = 2 end in 0",
          ),
        message: "Both antecedents are hypotheses here. Rewriting `n` to `2` in the first — licensed by the second — makes it `2 == 1`, which evaluates to `false`, so the branch is vacuous.",
      },
    ],
  };
};

/* --- have ---------------------------------------------------------------- */

let have = (~exp_id: Id.t, ~sub_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (e, sub, bd) = (exp("e"), proof("subproof"), proof_body());
  {
    group_id: ProofHaveStep,
    form_id: ProofHaveStep,
    abstract: (
      [mk_have([slot(e), slot(sub)]), space(), bd],
      [
        (Piece.id(e), exp_id),
        (Piece.id(sub), sub_id),
        (Piece.id(bd), body_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Forward reasoning: proves the [*proposition*](%s) once in the [*sub-proof*](%s), and then makes it available to the [*rest of the proof*](%s) as a hypothesis citable as `have`, with nothing owed. It is `assume` with the proof attached — and while the sub-proof is unfinished the proposition rides along as a pending obligation instead, so the theorem is proven modulo it.",
        exp_id |> Id.to_string,
        sub_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofHave1,
        term:
          mk_example(
            "theorem t = forall x -> x == 1 proof have 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end => ? in 0",
          ),
        message: "The sub-proof closes `1 == 1` by reflexivity, so `1 == 1` is a hypothesis named `have` in the body. Leaving the sub-proof a hole instead would keep `1 == 1` as a pending obligation.",
      },
    ],
  };
};

/* --- hypothesis naming: the `as` variants and `alias` -------------------- */

let induction_as = (~scrut_id: Id.t, ~name_id: Id.t): Simple.t => {
  let (sc, nm, pt, bd) = (exp("e"), exp("h"), pat("p"), proof("proof"));
  {
    group_id: ProofInductionAsStep,
    form_id: ProofInductionAsStep,
    abstract: (
      [
        mk_induction_as([
          slot(sc),
          [space(), nm, space(), mk_proof_rule([slot(pt)]), space(), bd],
        ]),
      ],
      [(Piece.id(sc), scrut_id), (Piece.id(nm), name_id)],
    ),
    explanation:
      Printf.sprintf(
        "Splits the goal into one case per pattern of the [*scrutinee*](%s), and gives this split's case equation the [*name*](%s) in place of the fixed `case_eq`. One name covers the whole split: the equation still differs case by case, only the name is shared. That is the point of writing it — a deeper split installs `case_eq` again and shadows the bare name, but it cannot shadow this one, so the equation stays citable from any leaf underneath. Everything else is as an unnamed `induction`: the cases must be exhaustive, each recursive sub-term still contributes a hypothesis under the fixed name `ih`, and a computed or boolean scrutinee makes this case analysis rather than induction. The name lives in the theorem namespace, never the variable one, and is visible only inside this form.",
        scrut_id |> Id.to_string,
        name_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofInductionAs1,
        term:
          mk_example(
            "theorem t = forall n: Int -> forall m: Int -> n == n proof induction n > 0 as hn\n| true => induction m > 0\n  | true => revert hn => ?\n  | false => ? end\n| false => ? end in 0",
          ),
        message: "The inner split installs its own `case_eq`, which would hide the outer one. Naming the outer split `hn` keeps `n > 0` citable in the inner leaf, with no primes to count.",
      },
    ],
  };
};

let assume_as = (~exp_id: Id.t, ~name_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (e, nm, bd) = (exp("e"), pat("h"), proof_body());
  {
    group_id: ProofAssumeAsStep,
    form_id: ProofAssumeAsStep,
    abstract: (
      [mk_assume_as([slot(e), slot(nm)]), space(), bd],
      [
        (Piece.id(e), exp_id),
        (Piece.id(nm), name_id),
        (Piece.id(bd), body_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Hypothesizes the [*proposition*](%s) for the [*rest of the proof*](%s) under the [*name*](%s), in place of the fixed `assume`. The logic is unchanged: this is implication introduction, owing nothing, when the proposition is exactly the antecedent of an `==>` goal, and an obligation otherwise. What the name buys is reach — a nested `assume` installs `assume` again and shadows the bare name, but not this one, so the hypothesis stays citable however deep the proof goes. The name lives in the theorem namespace, never the variable one (a hypothesis is a judgment, not a value), and is visible only inside this form.",
        exp_id |> Id.to_string,
        body_id |> Id.to_string,
        name_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAssumeAs1,
        term:
          mk_example(
            "theorem t = forall x: Int -> forall y: Int -> x == 1 ==> y == 2 ==> x == x proof assume x == 1 => assume y == 2 as hy => revert hy => ? in 0",
          ),
        message: "Naming the inner assumption `hy` leaves the outer one reachable as the bare `assume`, and `hy` denotes the assumption it is attached to. Unnamed, the inner one would answer to `assume` and hide the outer.",
      },
    ],
  };
};

let alias = (~name_id: Id.t, ~exp_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (nm, e, bd) = (pat("h"), exp("fact"), proof_body());
  {
    group_id: ProofAliasStep,
    form_id: ProofAliasStep,
    abstract: (
      [mk_alias([slot(nm), slot(e)]), space(), bd],
      [
        (Piece.id(nm), name_id),
        (Piece.id(e), exp_id),
        (Piece.id(bd), body_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Installs a fact that is ALREADY in scope under a second [*name*](%s), for the [*rest of the proof*](%s). The [*fact*](%s) is resolved exactly as `revert` resolves its argument — by its bare name, or by spelling the proposition out — and nothing else happens: no obligation, and no change to the goal, because nothing is assumed that was not already known. It is pure renaming, and it is the escape hatch for shadowing: take an alias of an outer `case_eq`, `assume` or `ih` just before the split or assumption that will install that name again, and the outer fact stays citable underneath. Spelling the proposition out is the only way to reach a fact that is already hidden where you are standing — which is how the second `ih` of a case with two recursive sub-terms is reached, since both are `ih`.",
        name_id |> Id.to_string,
        body_id |> Id.to_string,
        exp_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAlias1,
        term:
          mk_example(
            "theorem t = forall n: Int -> forall m: Int -> n == n proof induction n > 0\n| true => alias hn = case_eq => induction m > 0\n  | true => revert hn => ?\n  | false => ? end\n| false => ? end in 0",
          ),
        message: "The alias is taken before the inner split reuses `case_eq`, so `hn` still denotes the outer equation `n > 0` in the inner leaf. Nothing is proven here — the goal passes straight through.",
      },
    ],
  };
};

/* --- dispatch ------------------------------------------------------------ */

/* Total over the Proof sort: the compiler is the completeness check that
   no prover form falls back to a generic message. */
let single = (p: Proof.t): Simple.t =>
  switch (p.term) {
  | EmptyHole => empty_hole
  | MultiHole(_) => multi_hole
  | Invalid(_) => invalid
  | Seq(p1, p2) => seq(~fst_id=Proof.rep_id(p1), ~snd_id=Proof.rep_id(p2))
  | AxiomStep({at_idx, at_exp, direction, equality, instantiation}) =>
    let (name_id, idx_id, target_id) = (
      Exp.rep_id(equality),
      Exp.rep_id(at_idx),
      Exp.rep_id(at_exp),
    );
    switch (direction, instantiation) {
    | (Right, None) => axiom(~name_id, ~idx_id, ~target_id)
    | (Left, None) => axiomrev(~name_id, ~idx_id, ~target_id)
    | (Right, Some((v, i))) =>
      axiom_with(
        ~name_id,
        ~var_id=Exp.rep_id(v),
        ~inst_id=Exp.rep_id(i),
        ~idx_id,
        ~target_id,
      )
    | (Left, Some((v, i))) =>
      axiomrev_with(
        ~name_id,
        ~var_id=Exp.rep_id(v),
        ~inst_id=Exp.rep_id(i),
        ~idx_id,
        ~target_id,
      )
    };
  | AlgebriteStep({at_idx, at_exp, with_exp}) =>
    rewrite(
      ~target_id=Exp.rep_id(at_exp),
      ~with_id=Exp.rep_id(with_exp),
      ~idx_id=Exp.rep_id(at_idx),
    )
  | EvalStep({at_idx, at_exp}) =>
    eval_step(~target_id=Exp.rep_id(at_exp), ~idx_id=Exp.rep_id(at_idx))
  | Induction(scrut, None, _cases) => induction(~scrut_id=Exp.rep_id(scrut))
  | Induction(scrut, Some(h), _cases) =>
    induction_as(~scrut_id=Exp.rep_id(scrut), ~name_id=Pat.rep_id(h))
  | Forall(pat, body) =>
    forall_step(~pat_id=Pat.rep_id(pat), ~body_id=Proof.rep_id(body))
  | Assume(e, None, body) =>
    assume(~exp_id=Exp.rep_id(e), ~body_id=Proof.rep_id(body))
  | Assume(e, Some(h), body) =>
    assume_as(
      ~exp_id=Exp.rep_id(e),
      ~name_id=Pat.rep_id(h),
      ~body_id=Proof.rep_id(body),
    )
  | Alias(h, e, body) =>
    alias(
      ~name_id=Pat.rep_id(h),
      ~exp_id=Exp.rep_id(e),
      ~body_id=Proof.rep_id(body),
    )
  | Generalize(e, body) =>
    generalize(~exp_id=Exp.rep_id(e), ~body_id=Proof.rep_id(body))
  | Revert(e, None, body) =>
    revert(~exp_id=Exp.rep_id(e), ~body_id=Proof.rep_id(body))
  | Revert(e, Some((v, i)), body) =>
    revert_with(
      ~exp_id=Exp.rep_id(e),
      ~var_id=Exp.rep_id(v),
      ~inst_id=Exp.rep_id(i),
      ~body_id=Proof.rep_id(body),
    )
  | Contradiction(e, None) => contradiction(~exp_id=Exp.rep_id(e))
  | Contradiction(e, Some((v, i))) =>
    contradiction_with(
      ~exp_id=Exp.rep_id(e),
      ~var_id=Exp.rep_id(v),
      ~inst_id=Exp.rep_id(i),
    )
  | Have(e, sub, body) =>
    have(
      ~exp_id=Exp.rep_id(e),
      ~sub_id=Proof.rep_id(sub),
      ~body_id=Proof.rep_id(body),
    )
  };
