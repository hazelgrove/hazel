/* The proof grammar as a model is told it, with worked examples. Each
 * example is a complete program whose theorem the checker proves
 * (Test_AgentFill checks them), so a change to the proof syntax that
 * isn't made here too fails that test. */

let examples: list(string) = [
  "theorem t = 1 + 4 == 5 proof from 1 + 4 do step end; step in 0",
  "theorem t = (1 + 2) + (3 + 4) == 10 proof from $v + $v at 2 do step end; from $v + $v do step end; from $v + $v do step end; step in 0",
  "theorem t = (if 1 < 2 then 3 else 4) == 3 proof from if $e then $e else $e do from 1 < 2 do step end; step end; step in 0",
  "theorem t = (true && false) == (false && true) proof from true && false do rewrite and_comm end end; rewrite refl end in 0",
  "let double = fun x -> x + x in\ntheorem t = forall n: Int -> double(n) == 2 * n proof from double(n) do step end; from n + n do algebra 2 * n end end; rewrite refl end in 0",
  "theorem t = forall n: Int -> n == 1 ==> n == 1 proof assume n == 1 => from n do rewrite assume end end; step in 0",
  "type Nt = +Z+S(Nt) in\nlet pos = fun e -> case e | Z => true | S(b) => true end in\ntheorem t = forall e: Nt -> pos(e) proof induction e\n| Z => from pos(Z) do step end\n| S(b) => revert pos(b) => from pos(b) do rewrite ih_b end end; from true ==> pos(S(b)) do step end; from pos(S(b)) do step end\nend in 0",
  "type Nt = +Z+S(Nt) in\nlet plus = fun (a, b) -> case a | Z => b | S(p) => S(plus((p, b))) end in\ntheorem t = forall n: Nt -> plus((n, Z)) == n proof\n  induction n\n  | Z => from plus((Z, Z)) do step end; rewrite refl end\n  | S(p) => from plus((S(p), Z)) do step end; from plus((p, Z)) do rewrite ih_p end end; rewrite refl end\n  end\nin 0",
  "type Nt = +Z+S(Nt) in\nlet to_int = fun n -> case n | Z => 0 | S(p) => 1 + to_int(p) end in\nlet count = fun (n, acc) -> case n | Z => acc | S(p) => count((p, acc + 1)) end in\ntheorem t = forall n: Nt -> forall acc: Int -> acc + to_int(n) == count((n, acc)) proof\n  generalize acc => induction n\n  | Z => forall acc => from to_int(Z) do step end; from count((Z, acc)) do step end; from acc + 0 do algebra acc end end; rewrite refl end\n  | S(p) => forall acc => from to_int(S(p)) do step end; from count((S(p), acc)) do step end;\n    from acc + (1 + to_int(p)) do algebra (acc + 1) + to_int(p) end end; from count((p, acc + 1)) do rewrite <- ih_p end end; rewrite refl end\n  end\nin 0",
];

let self: list(string) =
  [
    "## Proof syntax",
    "",
    "A theorem is written `theorem <name> = <statement> proof <proof> in <body>`. The checker hands each step a goal, a boolean expression, and the step rewrites it into a new goal. A proof is finished when its goal is the literal `true`. `?` is an unfinished proof. The statement's outer `forall`s are introduced before the proof starts: their variables are in scope, and the goal is the body.",
    "",
    "Steps are chained with `;`: each takes the goal the previous one left. A rewriting step acts on the root of its goal:",
    "",
    "- `step`: one evaluation step at the root: unfold a definition, apply a function, do arithmetic, take a branch, compare values. Refused when the root can't step yet, typically because its operands aren't values: focus on the part that can.",
    "- `rewrite <fact> end`: rewrite the root with the equation `<fact>`, left to right. `rewrite <- <fact> end` rewrites right to left. An equation `a == b` rewrites `a` to `b`, so to use `n == 1` on the goal `n == 1`, focus on `n`. A fact that isn't an equation, `P`, reads as `P == true`. A fact is a builtin axiom, an earlier theorem, or a hypothesis in scope. A refuted theorem, whose proof reaches `false`, can't be cited; one whose proof is unfinished can, and the proof then rests on its statement. `rewrite <fact> with <x> = <e> end` instantiates the fact's variable `x` when matching can't find it; it takes one binding, and matching finds the other variables.",
    "- `rewrite refl end` closes `a == a`. Its two sides must be the same term, not just print the same: a function the checker has unfolded on one side differs from its name on the other until you `step` that one too, and `x :: []` differs from `[x]` until `step` turns it into `[x]`.",
    "- `algebra <replacement> end`: an algebraic rewrite of the root, checked by a computer-algebra system.",
    "",
    "To act on a subterm, focus on it: `from <selector> do <steps> end` runs the steps on the first occurrence of the selector in the goal, counting left to right, and puts the result back. `from <selector> at 2 do ... end` picks the second occurrence, and so on. A selector may use `$e` to match any expression, `$v` any value and `$x` any variable, and is otherwise an ordinary expression: `if c then a else b` has no `end` (`case` does), so `from if $e then $e else $e do step end` focuses on an `if`; an `end` after its `else` branch would close the `from`. Inside the focus, the steps act on the root of the focused subterm. `rewrite` and `algebra` close with their own `end`, so a focus around one ends `end end`: `from n + n do algebra 2 * n end end; rewrite refl end`.",
    "",
    "These forms run the rest of the chain, after `=>`, in a new scope:",
    "",
    "- `forall x => <proof>`: for a goal `forall x -> P` (inside the statement), proves `P` for an arbitrary `x`.",
    "- `assume <P> => <proof>`: for a goal `P ==> Q`, proves `Q` with the hypothesis `assume` (`P`). `assume <name> = <P> => ...` names it. Only an implication's antecedent can be assumed.",
    "- `cases <e> | <pat> => <proof> | ... end`: case analysis on any expression, one case per pattern; the cases must be exhaustive. In each case `<e>` is replaced by the pattern in the goal, and the hypothesis `case_eq` is the equation `<e> == <pat>`. To keep it under another name, `alias <name> = case_eq => ...` at the start of the case. Use `cases` when a case needs to know which pattern `<e>` matched.",
    "- `induction <e> | <pat> => <proof> | ... end`: structural induction, with cases as for `cases`. Each case has `ih_<x>`, the goal for each recursive sub-term `x`, named after it: the case `S(b)` has `ih_b`, and `x :: t` has `ih_t`. Use `induction` when a case needs the claim for a smaller value. It gives no `case_eq`, and in each case the facts that mention `<e>` are out of scope. To keep such a fact, `revert` it before the induction (`revert h => induction xs | ...`), so it is part of the goal and of each `ih_<x>`.",
    "- `revert <fact> => <proof>`: turns the goal `G` into `<fact> ==> G`. `revert <fact> with <x> = <e> => ...` instantiates a quantified fact.",
    "- `have <P> proof <proof> => <proof>`: proves `P`, then adds it as the hypothesis `have`. `have <name> = <P> proof ...` names it.",
    "- `funext x => <proof>`: for a goal `f == g` between functions, proves `f(x) == g(x)`.",
    "- `generalize x => <proof>`: puts `forall x ->` back on the goal, so a later inductive hypothesis is quantified. When a variable such as an accumulator changes in the recursive call, write `generalize acc => induction xs | ... => forall acc => ...`: `generalize` before `induction`, and `forall acc =>` at the start of each case. The inductive hypothesis (`ih_t` for `x :: t`) then holds for every `acc`.",
    "- `alias <name> = <fact> => <proof>`: gives a fact in scope a second name.",
    "",
    "A form's body extends to the end of the chain: in `forall x => a; b`, both `a` and `b` are in the `forall`. An implication `true ==> P` evaluates to `P`, and `false ==> P` to `true`.",
    "",
    "Examples:",
    "",
  ]
  @ List.map(~f=e => "```\n" ++ e ++ "\n```", examples);

/* The builtin axioms, citable by name in every proof. */
let axioms: list(string) =
  ["", "## Builtin axioms", "", "Citable by name in every proof:", ""]
  @ List.map(
      ~f=
        ((name, stmt)) =>
          "- `"
          ++ name
          ++ "`: `"
          ++ (
            stmt
            |> ExpToSegment.exp_to_segment(
                 ~settings=ExpToSegment.Settings.editable(~inline=true),
               )
            |> Printer.of_segment(~holes="?", ~refractors=[])
          )
          ++ "`",
      Language.Axioms.axioms,
    );
