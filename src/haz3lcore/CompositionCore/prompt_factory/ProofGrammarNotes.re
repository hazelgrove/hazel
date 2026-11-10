/* The proof grammar as a model is told it, with worked examples. Each
 * example is a complete program whose theorem the checker proves
 * (Test_AgentFill checks them), so a change to the proof syntax that
 * isn't made here too fails that test. */

let examples: list(string) = [
  "theorem t = 1 + 4 == 5 proof from 1 + 4 do eval end; eval in 0",
  "theorem t = (1 + 2) + (3 + 4) == 10 proof from 2nd $v + $v do eval end; from $v + $v do eval end; from $v + $v do eval end; eval in 0",
  "theorem t = (true && false) == (false && true) proof from true && false do axiom and_comm end end; axiom refl_eq end in 0",
  "let double = fun x -> x + x in\ntheorem t = forall n: Int -> double(n) == 2 * n proof from double(n) do eval end; from n + n do rewrite 2 * n end end; axiom refl_eq end in 0",
  "theorem t = forall n: Int -> n == 1 ==> n == 1 proof assume n == 1 => from n do axiom assume end end; eval in 0",
  "type Nt = +Z+S(Nt) in\nlet pos = fun e -> case e | Z => true | S(b) => true end in\ntheorem t = forall e: Nt -> pos(e) proof induction e\n| Z => from pos(Z) do eval end\n| S(b) => revert pos(b) => from pos(b) do axiom ih end end; from true ==> pos(S(b)) do eval end; from pos(S(b)) do eval end\nend in 0",
];

let self: list(string) =
  [
    "## Proof syntax",
    "",
    "A theorem is written `theorem <name> = <statement> proof <proof> in <body>`. The checker hands each step a goal, a boolean expression, and the step rewrites it into a new goal. A proof is finished when its goal is the literal `true`. `?` is an unfinished proof. The statement's outer `forall`s are introduced before the proof starts: their variables are in scope, and the goal is the body.",
    "",
    "Steps are chained with `;`: each takes the goal the previous one left. A rewriting step acts on the root of its goal:",
    "",
    "- `eval`: one evaluation step at the root: unfold a definition, apply a function, do arithmetic, take a branch, compare values. Refused when the root can't step yet, typically because its operands aren't values: focus on the part that can.",
    "- `axiom <fact> end`: rewrite the root with the equation `<fact>`, left to right. `axiomrev <fact> end` rewrites right to left. An equation `a == b` rewrites `a` to `b`, so to use `n == 1` on the goal `n == 1`, focus on `n`. A fact that isn't an equation, `P`, reads as `P == true`. A fact is a builtin axiom, an earlier theorem, or a hypothesis in scope. `axiom <fact> with <x> = <e> end` instantiates the fact's variable `x` when matching can't find it.",
    "- `rewrite <replacement> end`: an algebraic rewrite of the root, checked by a computer-algebra system.",
    "- `contradiction <fact> end`: closes any goal when the fact in scope evaluates to `false`. `contradiction <fact> with <x> = <e> end` first replaces `x` by `e`, which needs `x == e` in scope.",
    "",
    "To act on a subterm, focus on it: `from <selector> do <steps> end` runs the steps on the first occurrence of the selector in the goal, counting left to right, and puts the result back. `from 2nd <selector> do ... end` picks the second occurrence, and so on (`3rd`, `4th`). A selector may use `$e` to match any expression, `$v` any value and `$x` any variable. Inside the focus, the steps act on the root of the focused subterm. `axiom` and `rewrite` close with their own `end`, so a focus around one ends `end end`: `from n + n do rewrite 2 * n end end; axiom refl_eq end`.",
    "",
    "These forms run the rest of the chain, after `=>`, in a new scope:",
    "",
    "- `forall x => <proof>`: for a goal `forall x -> P` (inside the statement), proves `P` for an arbitrary `x`. A `where` restriction on `x` becomes the hypothesis `where`.",
    "- `assume <P> => <proof>`: for a goal `P ==> Q`, proves `Q` with the hypothesis `assume` (`P`). `assume <P> as <name> => ...` names it.",
    "- `induction <e> | <pat> => <proof> | ... end`: one case per pattern; the cases must be exhaustive. In each case `case_eq` is the equation `<e> == <pat>`, and `ih` is the goal for each recursive sub-term (structural induction on a variable of a recursive type). `induction <e> as <name> | ...` names the case equation.",
    "- `revert <fact> => <proof>`: turns the goal `G` into `<fact> ==> G`. `revert <fact> with <x> = <e> => ...` instantiates a quantified fact.",
    "- `have <P> proof <proof> => <proof>`: proves `P`, then adds it as the hypothesis `have`.",
    "- `funext x => <proof>`: for a goal `f == g` between functions, proves `f(x) == g(x)`.",
    "- `generalize x => <proof>`: puts `forall x ->` back on the goal, so a later `ih` is quantified.",
    "- `alias <name> = <fact> => <proof>`: gives a fact in scope a second name.",
    "",
    "A form's body extends to the end of the chain: in `forall x => a; b`, both `a` and `b` are in the `forall`. An implication `true ==> P` evaluates to `P`, and `false ==> P` to `true`.",
    "",
    "Examples:",
    "",
  ]
  @ List.map(e => "```\n" ++ e ++ "\n```", examples);
