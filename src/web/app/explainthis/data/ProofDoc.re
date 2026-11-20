open Haz3lcore;
open Language;
open ExplainThisForm;
open Example;

/* ExplainThis documentation for the Proof sort — the prover's step forms.
 *
 * House rule for this file: every message names the facts the step brings
 * into scope (`assume`, `case_eq`, `ih_t`, `have`) and says whether the
 * step adds a side condition, in the words the Prover slides use, because
 * that is what a user sitting on the step needs to write the next one. */

/* A child slot, padded so the tile's shards read as delimiters. */
let slot = (p: Piece.t): Segment.t => [space(), p, space()];

let proof_body = () => proof("proof");

// Degenerate forms

let empty_hole: Simple.t = {
  group_id: ProofEmptyHole,
  form_id: ProofEmptyHole,
  abstract: ([proof("?")], []),
  explanation: "An unfinished proof: the goal it is handed stays open, so the theorem reads incomplete. In the stepper, the write box here offers the steps that apply to this goal.",
  examples: [],
};

let multi_hole: Simple.t = {
  group_id: ProofMultiHole,
  form_id: ProofMultiHole,
  abstract: ([proof("?")], []),
  explanation: "These pieces don't form a proof step yet, so nothing is checked here and the goal stays open.",
  examples: [],
};

let invalid: Simple.t = {
  group_id: ProofInvalid,
  form_id: ProofInvalid,
  abstract: ([proof("?")], []),
  explanation: "Not a proof step: this text isn't any of the step forms, so nothing is checked here and the goal stays open. In the stepper, the write box here offers the steps that apply to this goal.",
  examples: [],
};

// Sequencing

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
      sprintf(
        "Runs the [*first step*](%s) on the goal, then gives the goal it leaves to the [*rest of the proof*](%s). A proof is a chain of steps, each replacing the goal with one that is enough to prove it, until the goal is `true`.",
        fst_id |> Id.to_string,
        snd_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofSeq1,
        term:
          mk_example(
            "theorem t = 1 + 4 == 5 proof from 1 + 4 do step end; step in 0",
          ),
        message: "The first step evaluates `1 + 4`, leaving the goal `5 == 5`; the second evaluates that to `true`.",
      },
    ],
  };
};

// Citation: rewrite / rewrite <-, with and without `with`

let axiom = (~name_id: Id.t): Simple.t => {
  let nm = exp("fact");
  {
    group_id: ProofAxiomStep,
    form_id: ProofAxiomStep,
    abstract: ([mk_axiom([slot(nm)])], [(Piece.id(nm), name_id)]),
    explanation:
      sprintf(
        "Uses the [*fact*](%s) — a theorem, a builtin rule, or a fact in scope such as `assume`, `case_eq`, `ih_t` or `have` — as a rule: on a goal that matches its left side, replaces the goal with its right side; inside a `from`, rewrites just the focused part. A fact that is a proposition `P` rather than an equation reads as `P == true`, so it rewrites `P` to `true`. A name means the nearest fact with that name; `alias` reaches one that a nearer fact hides. A rule with `==>` conditions adds them as side conditions of this step. Add `with x = e` when matching can't determine one of the rule's variables.",
        name_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAxiom1,
        term:
          mk_example(
            "theorem t = (true && false) == (false && true) proof from true && false do rewrite and_comm end end; rewrite refl end in 0",
          ),
        message: "`and_comm` is a builtin rule, `a && b == b && a`. Rewriting the focused `true && false` with it makes the two sides equal, and `refl` closes the goal.",
      },
    ],
  };
};

let axiom_with = (~name_id: Id.t, ~var_id: Id.t, ~inst_id: Id.t): Simple.t => {
  let (nm, vr, it) = (exp("fact"), exp("x"), exp("e"));
  {
    group_id: ProofAxiomStep,
    form_id: ProofAxiomStep,
    abstract: (
      [mk_axiom_with([slot(nm), slot(vr), slot(it)])],
      [
        (Piece.id(nm), name_id),
        (Piece.id(vr), var_id),
        (Piece.id(it), inst_id),
      ],
    ),
    explanation:
      sprintf(
        "Uses the [*fact*](%s) as `rewrite` does, with its [*variable*](%s) set to the [*value*](%s) before matching. Write it when the variable appears only in the rule's conditions, so matching the goal can't determine it. The value must visibly terminate, and its own side conditions, such as `e != 0` for a divisor, are added to this step.",
        name_id |> Id.to_string,
        var_id |> Id.to_string,
        inst_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAxiomWith1,
        term:
          mk_example(
            "let f = fun x -> x + 1 in\ntheorem lem = forall n: Int -> n == 1 ==> f(2) == 3 proof assume n == 1 => from f(2) do step end; from 2 + 1 do step end; step in\ntheorem g = f(2) == 3 proof from f(2) do rewrite lem with n = 1 end end; step in 0",
          ),
        message: "`lem`'s variable `n` appears only in its condition, so matching `f(2)` can't determine it, and without `with` the step is refused. `with n = 1` sets it; the condition `1 == 1` is this step's side condition, which evaluation closes.",
      },
    ],
  };
};

let axiomrev = (~name_id: Id.t): Simple.t => {
  let nm = exp("fact");
  {
    group_id: ProofAxiomRevStep,
    form_id: ProofAxiomRevStep,
    abstract: (
      [mk_axiom([[space(), mk_rewrite_back(), ...slot(nm)]])],
      [(Piece.id(nm), name_id)],
    ),
    explanation:
      sprintf(
        "`rewrite` right to left: on a goal that matches the [*fact*](%s)'s right side, replaces the goal with its left side; inside a `from`, rewrites just the focused part. On a fact that is a proposition `P`, it rewrites `true` to `P`; since `true` is everywhere, that is only done for a fact you name.",
        name_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAxiomRev1,
        term:
          mk_example(
            "theorem t = (true && false) == (false && true) proof from false && true do rewrite <- and_comm end end; rewrite refl end in 0",
          ),
        message: "`and_comm` states `a && b == b && a`. Right to left on the focused `false && true`, it gives `true && false`, so the two sides are equal and `refl` closes the goal.",
      },
    ],
  };
};

let axiomrev_with = (~name_id: Id.t, ~var_id: Id.t, ~inst_id: Id.t): Simple.t => {
  let (nm, vr, it) = (exp("fact"), exp("x"), exp("e"));
  {
    group_id: ProofAxiomRevStep,
    form_id: ProofAxiomRevStep,
    abstract: (
      [
        mk_axiom_with([
          [space(), mk_rewrite_back(), ...slot(nm)],
          slot(vr),
          slot(it),
        ]),
      ],
      [
        (Piece.id(nm), name_id),
        (Piece.id(vr), var_id),
        (Piece.id(it), inst_id),
      ],
    ),
    explanation:
      sprintf(
        "`rewrite <-` with the [*fact*](%s)'s [*variable*](%s) set to the [*value*](%s) before matching. Matching a rule's right side often leaves more of its variables undetermined than matching its left, so this direction needs `with` more often. The value is checked as for `rewrite ... with`.",
        name_id |> Id.to_string,
        var_id |> Id.to_string,
        inst_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAxiomRevWith1,
        term:
          mk_example(
            "let f = fun x -> x + 1 in\ntheorem lem = forall n: Int -> n == 1 ==> f(2) == 3 proof assume n == 1 => from f(2) do step end; from 2 + 1 do step end; step in\ntheorem g = 3 == f(2) proof from 3 do rewrite <- lem with n = 1 end end; rewrite refl end in 0",
          ),
        message: "Right to left, `lem` turns the `3` into `f(2)`, so the two sides are equal. `lem`'s `n` appears in neither side, so `with n = 1` is what lets it apply; its condition `1 == 1` is this step's side condition.",
      },
    ],
  };
};

// Algebra / step / focus

let algebra = (~with_id: Id.t): Simple.t => {
  let wi = exp("e");
  {
    group_id: ProofRewriteStep,
    form_id: ProofRewriteStep,
    abstract: (
      [mk_algebra_step([slot(wi)])],
      [(Piece.id(wi), with_id)],
    ),
    explanation:
      sprintf(
        "Replaces the goal, or the focused part inside a `from`, with the [*replacement*](%s), and checks with computer algebra that the two are equal. The algebra treats numbers as a field, so every division or power the rewrite moves across adds a side condition (a divisor `e != 0`, an exponent `i >= 0`). On Float terms the step is refused: the field laws don't hold for floating point.",
        with_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofRewrite1,
        term:
          mk_example(
            "theorem t = 1 + 1 == 2 proof from 1 + 1 do algebra 2 end end; step in 0",
          ),
        message: "Rewrites the focused `1 + 1` to `2`, leaving `2 == 2`, which `step` evaluates to `true`.",
      },
    ],
  };
};

let eval_step: Simple.t = {
  group_id: ProofEvalStep,
  form_id: ProofEvalStep,
  abstract: ([mk_eval_step([])], []),
  explanation: "Takes one evaluation step at the root of the goal, or of the focused part inside a `from`: unfolds a definition, applies a function, or takes a branch. A function or `let` there takes its argument as it is, value or not, so `(fun x -> x + 1)(2 + 3)` becomes `2 + 3 + 1`. Otherwise, if the root can't step yet, usually because its parts aren't values, the step is refused: focus on a part that can. The step adds no side condition.",
  examples: [
    {
      sub_id: ProofEval1,
      term:
        mk_example(
          "theorem t = 2 + 3 == 5 proof from 2 + 3 do step end; step in 0",
        ),
      message: "The root `2 + 3 == 5` can't step while `2 + 3` isn't a value, so the first step focuses on `2 + 3` and evaluates it to `5`. The second evaluates `5 == 5` to `true`.",
    },
  ],
};

let focus = (~selector_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (sl, bd) = (exp("selector"), proof("steps"));
  {
    group_id: ProofFocusStep,
    form_id: ProofFocusStep,
    abstract: (
      [mk_focus([slot(sl), slot(bd)])],
      [(Piece.id(sl), selector_id), (Piece.id(bd), body_id)],
    ),
    explanation:
      sprintf(
        "Focuses on the first part of the goal that matches the [*selector*](%s), or the nth with `at n`, counting from 1 left to right, and runs the [*steps*](%s) on that part alone. Their result goes back into the goal in its place. In the selector, `$e` matches any expression, `$v` any value and `$x` any variable. It can match under a `fun`, a `forall`, a `let` body or a case arm; there the variables bound around it are fixed, unknown values, as under `forall x =>`, so facts about an outer variable of the same name don't apply. The steps' side conditions are their own.",
        selector_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofFocus1,
        term:
          mk_example(
            "theorem t = (1 + 2) + (3 + 4) == 10 proof from $v + $v at 2 do step end; from $v + $v do step end; from 3 + 7 do step end; step in 0",
          ),
        message: "`$v + $v` matches an addition of two values. `at 2` picks the second, so the first focus evaluates `3 + 4`, leaving `(1 + 2) + 7 == 10`; the second evaluates `1 + 2`. Then `3 + 7` evaluates to `10`, and `10 == 10` to `true`.",
      },
    ],
  };
};

// Case analysis and induction

let cases = (~scrut_id: Id.t): Simple.t => {
  let (sc, pt, bd) = (exp("e"), pat("p"), proof("proof"));
  {
    group_id: ProofCasesStep,
    form_id: ProofCasesStep,
    abstract: (
      [
        mk_cases([
          [space(), sc, space(), mk_proof_rule([slot(pt)]), space(), bd],
        ]),
      ],
      [(Piece.id(sc), scrut_id)],
    ),
    explanation:
      sprintf(
        "Splits the goal into one case per pattern of the [*expression*](%s), which can be any expression. In each case the expression is replaced by the pattern in the goal, and the fact `case_eq` states `expression == pattern`. The patterns must cover the expression's type, its type must be known, and a computed expression must visibly terminate; a split with no cases covers only a type with no values. Cases give no `ih_` facts: for those, use `induction`. A nested split's `case_eq` hides the outer one: write `alias h = case_eq =>` at the top of a case to keep its equation available as `h` in splits nested inside.",
        scrut_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofCases1,
        term:
          mk_example(
            "let sign = fun n -> if n > 0 then 1 else 0 in\ntheorem t = forall n: Int -> sign(n) <= 1 proof from sign(n) do step end; cases n > 0\n| true => from if true then 1 else 0 do step end; step\n| false => from if false then 1 else 0 do step end; step\nend in 0",
          ),
        message: "Unfolding `sign(n)` leaves an `if` on `n > 0`. In the `true` case that test is `true` in the goal, so the `if` takes its first branch; in the `false` case, its second. Either way the result is at most `1`.",
      },
    ],
  };
};

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
      sprintf(
        "Splits the goal into one case per pattern of the [*expression*](%s), as `cases` does, and in each case adds a fact for every variable of the expression's own type, named after it: `ih_t` for a tail `t`. That fact is the goal with the variable in place of the expression. The patterns must cover the type. A computed expression is first replaced by a fresh variable, so there is no `case_eq`, and facts that mention the expression leave scope in the cases: `revert` a fact before the induction to keep it. Write `generalize` first to get `ih_` facts that are `forall`s, which can be used at other values. An `ih_` fact hides an outer one of the same name; `alias` keeps the outer one.",
        scrut_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofInduction1,
        term:
          mk_example(
            "type Nt = +Z+S(Nt) in\nlet pos = fun e -> case e | Z => true | S(b) => true end in\ntheorem t = forall e: Nt -> pos(e) proof induction e\n| Z => from pos(Z) do step end\n| S(b) => revert pos(b) => from pos(b) do rewrite ih_b end end; from true ==> pos(S(b)) do step end; from pos(S(b)) do step end\nend in 0",
          ),
        message: "The two cases cover `Nt`. The base case evaluates outright; the step case uses the fact `ih_b`, which states the goal for `b`.",
      },
    ],
  };
};

// Binder manipulation

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
      sprintf(
        "Names the [*variable*](%s) of a `forall` goal; the [*rest of the proof*](%s) proves the body for it, an unknown value. A theorem's outer `forall`s are named for you, so this is for a `forall` a later goal starts with, such as after `assume` or `generalize`.",
        pat_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofForall1,
        term:
          mk_example(
            "theorem t = true ==> (forall x: Int -> x == x) proof assume true => forall x => rewrite refl end in 0",
          ),
        message: "`assume true` leaves the goal `forall x: Int -> x == x`; `forall x` names its variable, and `refl` closes `x == x`.",
      },
    ],
  };
};

/* `funext x => <proof>`: functional extensionality as an intro rule
   rather than an axiom (an axiom is a rewrite equality, and funext as
   one would quantify over functions at unknown types). */
let funext_step = (~pat_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (pt, bd) = (pat("x"), proof_body());
  {
    group_id: ProofFunextStep,
    form_id: ProofFunextStep,
    abstract: (
      [mk_proof_funext([slot(pt)]), space(), bd],
      [(Piece.id(pt), pat_id), (Piece.id(bd), body_id)],
    ),
    explanation:
      sprintf(
        "Turns a goal `f == g` between two functions into the goal that they give equal results for the named [*argument*](%s), an unknown value: `fun x -> x + 1` and `fun x -> 1 + x` give `x + 1 == 1 + x`. The [*rest of the proof*](%s) proves that. One argument per `funext`: a function of two arguments needs two.",
        pat_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofFunext1,
        term:
          mk_example(
            "theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof funext x => from x + 1 do algebra 1 + x end end; from 1 + x == 1 + x do rewrite refl end end in 0",
          ),
        message: "`funext x` turns the equation between functions into `x + 1 == 1 + x`, which `algebra` and `refl` close.",
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
      sprintf(
        "On a goal `P ==> Q` whose `P` is the [*proposition*](%s), removes `P` from the goal and makes it a fact, named `assume`, for the [*rest of the proof*](%s), which proves `Q`. On any other goal the step is refused; `have` adds a fact to any goal. A nested `assume` takes the name `assume` again and hides this one: write `assume <name> = <e>` to keep both available.",
        exp_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAssume1,
        term:
          mk_example(
            "theorem t = forall n: Int -> n == 1 ==> n == 1 proof assume n == 1 => from n do rewrite assume end end; rewrite refl end in 0",
          ),
        message: "The proposition is the goal's `P`, so the goal becomes `n == 1`. The fact `assume` rewrites `n` to `1`, and `refl` closes `1 == 1`.",
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
      sprintf(
        "Turns the named [*variable*](%s) back into a `forall`: the [*rest of the proof*](%s) proves `forall x -> goal`. Before `induction`, this makes the `ih_` facts `forall`s, so they can be used at values other than the one in hand. Facts that mention the variable (`assume`, `case_eq`, earlier `ih_` facts) leave scope inside, since they are about the old value.",
        exp_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofGeneralize1,
        term:
          mk_example(
            "theorem t = forall n: Int -> n + 0 == n proof generalize n => forall n => from n + 0 do algebra n end end; rewrite refl end in 0",
          ),
        message: "`generalize n` turns the goal back into `forall n -> n + 0 == n`, and `forall n` names the variable again.",
      },
    ],
  };
};

// Revert

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
      sprintf(
        "Moves the [*fact*](%s), given by name or written out, back into the goal: the [*rest of the proof*](%s) proves `fact ==> goal`. It adds no side condition, and the fact stays in scope. It lets steps work on the fact: rewrite the antecedent to `true` and drop it, or to `false`, and the implication is `true`.",
        exp_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofRevert1,
        term:
          mk_example(
            "type Nt = +Z+S(Nt) in\nlet pos = fun e -> case e | Z => true | S(b) => true end in\ntheorem t = forall e: Nt -> pos(e) proof induction e\n| Z => from pos(Z) do step end\n| S(b) => revert pos(b) => from pos(b) do rewrite ih_b end end; from true ==> pos(S(b)) do step end; from pos(S(b)) do step end\nend in 0",
          ),
        message: "In the step case the fact `pos(b)` goes into the goal, giving `pos(b) ==> pos(S(b))`. Rewriting it with `ih_b` makes the antecedent `true`, and evaluation drops it.",
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
      sprintf(
        "Moves one instance of the [*fact*](%s) into the goal: the fact must be a `forall` over the [*variable*](%s), which is set to the [*value*](%s), and the [*rest of the proof*](%s) gets that instance as an antecedent. The value must visibly terminate, and its side conditions are added here. This is how a `forall` fact, such as an `ih_` fact after `generalize`, is used at a value other than its own.",
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
            "let f = fun x -> x + 1 in\ntheorem p = forall n: Int -> n + 0 == n proof from n + 0 do algebra n end end; rewrite refl end in\ntheorem g = f(2) == 3 proof revert p with n = 5 => from 5 + 0 do step end; from 5 == 5 do step end; from true ==> f(2) == 3 do step end; from f(2) do step end; from 2 + 1 do step end; from 3 == 3 do step end in 0",
          ),
        message: "`revert p with n = 5` puts the instance `5 + 0 == 5`, not the `forall`, into the goal. Evaluating it to `true` lets `step` drop the implication, leaving the original goal.",
      },
    ],
  };
};

// Contradiction

let contradiction = (~exp_id: Id.t): Simple.t => {
  let e = exp("fact");
  {
    group_id: ProofContradictionStep,
    form_id: ProofContradictionStep,
    abstract: ([mk_contradiction([slot(e)])], [(Piece.id(e), exp_id)]),
    explanation:
      sprintf(
        "Closes any goal by showing that the in-scope [*fact*](%s) — named by its hypothesis name, or spelled out — evaluates to `false`. If the hypotheses of this branch are jointly unsatisfiable then the branch is vacuous, and concluding it is the ex falso reading. This is the one-step way to dismiss the impossible case of a split, in place of reverting the fact and rewriting it to `false` by hand. It is terminal — there is no sub-proof — and it incurs no obligation. If the fact gets stuck instead of reaching `false`, the step is marked rather than believed.",
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
      sprintf(
        "Closes any goal by showing the in-scope [*fact*](%s) is `false` in this branch, after exactly one rewrite: the [*variable*](%s) is replaced by the [*expression*](%s) before the fact is evaluated. The rewrite must be licensed — `x == e` (in either orientation) has to be a fact in scope, typically the `case_eq` of the split that made this branch — and the checker refuses anything it cannot verify. Only the rewrite written here is applied; nothing is searched for, so the step's reasoning is what you can read.",
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

// Have

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
      sprintf(
        "Proves the [*proposition*](%s) in the [*subproof*](%s) and makes it a fact, named `have`, for the [*rest of the proof*](%s). It works on any goal. While the subproof is unfinished, the proposition is a side condition instead, and the theorem isn't proven until it is closed. Write `have <name> = <e> proof ...` to give the fact a name.",
        exp_id |> Id.to_string,
        sub_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofHave1,
        term:
          mk_example(
            "theorem t = forall z: Int -> z > 0 ==> z / z == 1 proof assume z > 0 => have z != 0 proof rewrite nonzero_of_pos end => from z / z do algebra 1 end end; rewrite refl end in 0",
          ),
        message: "The subproof derives `z != 0` from the assumption with the builtin rule `nonzero_of_pos`. That fact closes the side condition `z != 0` of `algebra 1` on `z / z`.",
      },
    ],
  };
};

// Hypothesis naming: the named forms and `alias`

let have_named =
    (~name_id: Id.t, ~exp_id: Id.t, ~sub_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (nm, e, sub, bd) = (
    pat("h"),
    exp("e"),
    proof("subproof"),
    proof_body(),
  );
  {
    group_id: ProofHaveNamedStep,
    form_id: ProofHaveNamedStep,
    abstract: (
      [mk_have_named([slot(nm), slot(e), slot(sub)]), space(), bd],
      [
        (Piece.id(nm), name_id),
        (Piece.id(e), exp_id),
        (Piece.id(sub), sub_id),
        (Piece.id(bd), body_id),
      ],
    ),
    explanation:
      sprintf(
        "Proves the [*proposition*](%s) in the [*subproof*](%s) and makes it a fact for the [*rest of the proof*](%s) under the [*name*](%s) instead of `have`. Otherwise it is an unnamed `have`. A nested `have` takes the name `have` again and hides an outer one, but not this one. The name is only for the proof: it names a fact, not a value.",
        exp_id |> Id.to_string,
        sub_id |> Id.to_string,
        body_id |> Id.to_string,
        name_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofHaveNamed1,
        term:
          mk_example(
            "theorem t = forall z: Int -> z > 0 ==> 8 / z == 8 / z proof assume z > 0 => have nz = z != 0 proof rewrite nonzero_of_pos end => from 8 / z == 8 / z do rewrite refl end end in 0",
          ),
        message: "The subproof derives `z != 0` from the assumption, and the rest of the proof can use it as `nz`. That fact closes the division's side condition `z != 0`.",
      },
    ],
  };
};

let assume_named = (~name_id: Id.t, ~exp_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (nm, e, bd) = (pat("h"), exp("e"), proof_body());
  {
    group_id: ProofAssumeNamedStep,
    form_id: ProofAssumeNamedStep,
    abstract: (
      [mk_assume_named([slot(nm), slot(e)]), space(), bd],
      [
        (Piece.id(nm), name_id),
        (Piece.id(e), exp_id),
        (Piece.id(bd), body_id),
      ],
    ),
    explanation:
      sprintf(
        "Makes the [*proposition*](%s) a fact for the [*rest of the proof*](%s) under the [*name*](%s) instead of `assume`. As for `assume`, the goal must be `P ==> Q` with the proposition as `P`. A nested `assume` takes the name `assume` again and hides an outer one, but not this one, so the fact stays available however deep the proof goes. The name is only for the proof: it names a fact, not a value.",
        exp_id |> Id.to_string,
        body_id |> Id.to_string,
        name_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAssumeNamed1,
        term:
          mk_example(
            "theorem t = forall x: Int -> forall y: Int -> x == 1 ==> y == 2 ==> x + y == 3 proof assume x == 1 => assume hy = y == 2 => from x do rewrite assume end end; from y do rewrite hy end end; from 1 + 2 do step end; step in 0",
          ),
        message: "Naming the second assumption `hy` leaves the first available as `assume`. Unnamed, the second would take that name and hide the first.",
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
      sprintf(
        "Gives a fact in scope a second [*name*](%s) for the [*rest of the proof*](%s). The [*fact*](%s) is given as for `revert`: by name, or written out. Nothing else changes: no side condition, and the goal stays the same. Its use is to keep a fact available when a later step will hide its name: alias an outer `case_eq` at the top of a case before a nested split, or an `assume` before another `assume`. Writing the proposition out is the only way to reach a fact that is already hidden.",
        name_id |> Id.to_string,
        body_id |> Id.to_string,
        exp_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ProofAlias1,
        term:
          mk_example(
            "theorem t = forall x: Int -> forall y: Int -> x == 1 ==> y == 2 ==> x + y == 3 proof assume x == 1 => alias hx = assume => assume y == 2 => from x do rewrite hx end end; from y do rewrite assume end end; from 1 + 2 do step end; step in 0",
          ),
        message: "The second `assume` takes the name `assume` and hides the first. Aliasing the first as `hx` beforehand keeps it available.",
      },
    ],
  };
};

// Dispatch

/* Total over the Proof sort: the compiler is the completeness check that
   no prover form falls back to a generic message. */
let rec single = (p: Proof.t): Simple.t =>
  switch (p.term) {
  | EmptyHole => empty_hole
  | MultiHole(_) => multi_hole
  | Invalid(_) => invalid
  /* Grouping only: explain the form inside the parens. */
  | Parens(inner) => single(inner)
  | Seq(p1, p2) => seq(~fst_id=Proof.rep_id(p1), ~snd_id=Proof.rep_id(p2))
  | AxiomStep({direction, equality, instantiation}) =>
    let name_id = Exp.rep_id(equality);
    switch (direction, instantiation) {
    | (Right, None) => axiom(~name_id)
    | (Left, None) => axiomrev(~name_id)
    | (Right, Some((v, i))) =>
      axiom_with(~name_id, ~var_id=Exp.rep_id(v), ~inst_id=Exp.rep_id(i))
    | (Left, Some((v, i))) =>
      axiomrev_with(~name_id, ~var_id=Exp.rep_id(v), ~inst_id=Exp.rep_id(i))
    };
  | AlgebriteStep(e) => algebra(~with_id=Exp.rep_id(e))
  | EvalStep => eval_step
  | Focus({selector, body, _}) =>
    focus(~selector_id=Exp.rep_id(selector), ~body_id=Proof.rep_id(body))
  | Cases(scrut, _cases) => cases(~scrut_id=Exp.rep_id(scrut))
  | Induction(scrut, _cases) => induction(~scrut_id=Exp.rep_id(scrut))
  | Forall(pat, body) =>
    forall_step(~pat_id=Pat.rep_id(pat), ~body_id=Proof.rep_id(body))
  | Funext(pat, body) =>
    funext_step(~pat_id=Pat.rep_id(pat), ~body_id=Proof.rep_id(body))
  | Assume(e, None, body) =>
    assume(~exp_id=Exp.rep_id(e), ~body_id=Proof.rep_id(body))
  | Assume(e, Some(h), body) =>
    assume_named(
      ~name_id=Pat.rep_id(h),
      ~exp_id=Exp.rep_id(e),
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
  | Have(e, None, sub, body) =>
    have(
      ~exp_id=Exp.rep_id(e),
      ~sub_id=Proof.rep_id(sub),
      ~body_id=Proof.rep_id(body),
    )
  | Have(e, Some(h), sub, body) =>
    have_named(
      ~name_id=Pat.rep_id(h),
      ~exp_id=Exp.rep_id(e),
      ~sub_id=Proof.rep_id(sub),
      ~body_id=Proof.rep_id(body),
    )
  };
