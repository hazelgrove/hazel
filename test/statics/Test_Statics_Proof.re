open Alcotest;
open Test_Statics_Prelude;
open Language;

/* Helpers to check that the statics result for a given program
   contains a mark matching a supplied predicate, or that it contains
   no errors of interest. */
let has_mark = (pred: Mark.t => bool, map: Statics.Map.t): bool =>
  Id.Map.exists(
    (_, info: Info.t) => List.exists(pred, Info.marks_of(info)),
    map,
  );

let no_mark = (pred: Mark.t => bool, map: Statics.Map.t): bool =>
  !has_mark(pred, map);

let expects_mark = (name: string, source: string, pred: Mark.t => bool) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(source);
      let s = statics(exp);
      Alcotest.check(bool, name, true, has_mark(pred, s));
    },
  );

let expects_no_mark = (name: string, source: string, pred: Mark.t => bool) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(source);
      let s = statics(exp);
      Alcotest.check(bool, name, true, no_mark(pred, s));
    },
  );

let is_free_hyp =
  fun
  | Mark.FreeHypothesis(_) => true
  | _ => false;

let is_axiom_slot_error =
  fun
  | Mark.AxiomSlotNotHypothesis(_) => true
  | _ => false;

let is_free_var =
  fun
  | Mark.Free(_) => true
  | _ => false;

let is_expectation_mismatch =
  fun
  | Mark.ExpectationMismatch(_) => true
  | _ => false;

let is_redundant =
  fun
  | Mark.Redundant => true
  | _ => false;

let is_inexhaustive =
  fun
  | Mark.InexhaustiveMatch(_) => true
  | _ => false;

/* Hovering `name` in the program: its info has no marks and
   its scope has `name` as a fact. */
let cites_fact_in_scope = (name: string, source: string) =>
  test_case(
    "hovering `" ++ name ++ "` resolves it as a fact in scope",
    `Quick,
    () => {
      let m = statics(parse_exp(source));
      let hovered =
        Id.Map.fold(
          (_, info: Info.t, found) =>
            switch (found, info) {
            | (None, InfoExp({user_term: {term: Var(x), _}, _} as e))
                when x == name =>
              Some(e)
            | _ => found
            },
          m,
          None,
        );
      switch (hovered) {
      | None => Alcotest.fail("no `" ++ name ++ "` in the program")
      | Some(e) =>
        Alcotest.(check(list(string)))(
          "no marks",
          [],
          List.map(Mark.show, e.marks),
        );
        Alcotest.check(
          bool,
          "a fact in scope",
          true,
          Ctx.lookup_theorem(e.ctx, name) != None,
        );
      };
    },
  );

/* A list induction whose cons case cites `fact`. */
let snoc_citing = fact =>
  {|theorem t = forall xs: [Int] -> forall x: Int -> xs @ [x] == xs @ [x] proof induction xs | [] => ? | y :: rest => from rest @ [x] do rewrite |}
  ++ fact
  ++ {| end end end in t|};

let tests = (
  "Statics.Proof",
  [
    /* Built-in axiom `refl` is a hypothesis, so an axiom step
       referring to it should not produce a FreeHypothesis mark. */
    expects_no_mark(
      "rewrite refl is a known hypothesis",
      {|theorem t = true proof from 1 do rewrite refl end end in t|},
      is_free_hyp,
    ),
    /* A completely unknown name in the axiom equality slot should
       trigger FreeHypothesis. */
    expects_mark(
      "rewrite bogus is a free hypothesis",
      {|theorem t = true proof from 1 do rewrite bogus end end in t|},
      is_free_hyp,
    ),
    /* `forall x => ...`: `x` should be visible as a variable,
       not as a hypothesis. */
    expects_no_mark(
      "forall binds a variable visible in body",
      {|theorem t = true proof forall x => from x do rewrite refl end end in 0|},
      is_free_var,
    ),
    /* The theorem introduces the goal's outer `forall`-bound variables
       into the proof scope, so referencing `x` in the proof of
       `forall x -> x == x` is not a free variable. */
    expects_no_mark(
      "theorem goal forall var is visible in proof",
      {|theorem t = forall x -> x == x proof from x == x do rewrite refl end end in 0|},
      is_free_var,
    ),
    /* Proof induction must be exhaustive: a list induction with only the
       empty-list case is inexhaustive, even when the scrutinee is an
       unannotated forall variable (type Unknown refined from the patterns). */
    expects_mark(
      "induction on unannotated forall var flags inexhaustive",
      {|theorem t = forall xs -> xs == xs proof induction xs | [] => ? end in t|},
      is_inexhaustive,
    ),
    /* Covering both list constructors is exhaustive (no mark). */
    expects_no_mark(
      "induction covering [] and :: is exhaustive",
      {|theorem t = forall xs -> xs == xs proof induction xs | [] => ? | y :: ys => ? end in t|},
      is_inexhaustive,
    ),
    /* A proof induction with no cases proves nothing -> inexhaustive. */
    expects_mark(
      "induction with no cases flags inexhaustive",
      {|theorem t = forall xs -> xs == xs proof induction xs end in t|},
      is_inexhaustive,
    ),
    /* Induction with two identical patterns should flag redundancy. */
    expects_mark(
      "induction flags redundant rows",
      {|theorem t = true proof induction 1 | 1 => ? | 1 => ? end in t|},
      is_redundant,
    ),
    /* Induction with a non-exhaustive match should flag inexhaustiveness. */
    expects_mark(
      "induction flags inexhaustive match",
      {|theorem t = true proof induction 1 | 0 => ? end in t|},
      is_inexhaustive,
    ),
    /* `assume` introduces a hypothesis citable in its body: an axiom step
       naming the generated hypothesis (`assume`) is not a free
       hypothesis. */
    expects_no_mark(
      "assume body sees the hypothesis",
      {|theorem t = 1 == 1 proof assume 2 == 2 => from 2 do rewrite assume end end in t|},
      is_free_hyp,
    ),
    /* The assumption analyzes against Bool: a non-bool assumption is a
       static type error. */
    expects_mark(
      "non-bool assumption flags a type error",
      {|theorem t = 1 == 1 proof assume 5 => ? in t|},
      is_expectation_mismatch,
    ),
    /* ...and a boolean assumption is not. */
    expects_no_mark(
      "bool assumption has no type error",
      {|theorem t = 1 == 1 proof assume 2 == 2 => ? in t|},
      is_expectation_mismatch,
    ),
    /* --- Implication and restrictions -------------------------------- */
    /* `==>` types Bool × Bool → Bool: boolean operands are fine... */
    expects_no_mark(
      "==> accepts boolean operands",
      {|theorem t = true ==> true proof ? in t|},
      is_expectation_mismatch,
    ),
    /* ...non-boolean ones are a type error. */
    expects_mark(
      "==> rejects non-boolean operands",
      {|theorem t = 5 ==> true proof ? in t|},
      is_expectation_mismatch,
    ),
    /* Restricted binder: the binder is in scope for both the guard and
       the body, and both analyze against Bool. */
    expects_no_mark(
      "forall-where binder scopes over guard and body",
      {|theorem t = forall x where x == 1 -> x == 1 proof ? in 0|},
      is_free_var,
    ),
    expects_mark(
      "forall-where guard analyzes against Bool",
      {|theorem t = forall x where 5 -> x == x proof ? in t|},
      is_expectation_mismatch,
    ),
    /* Function contract: the parameter is in scope for both
       the guard and the body... */
    expects_no_mark(
      "fun-where binder scopes over guard and body",
      {|let f = fun x where x != 0 -> 100 / x in f(1) == f(1)|},
      is_free_var,
    ),
    /* ...and the guard analyzes against Bool. */
    expects_mark(
      "fun-where guard analyzes against Bool",
      {|let f = fun x where 5 -> x in f(1)|},
      is_expectation_mismatch,
    ),
    expects_no_mark(
      "fun-where bool guard has no type error",
      {|let f = fun x where x != 0 -> 100 / x in f(1)|},
      is_expectation_mismatch,
    ),
    /* Peeling a restricted binder installs the guard as a hypothesis
       (base name "where"): citing it in the proof is not a free
       hypothesis. */
    expects_no_mark(
      "forall-where restriction is citable as `where`",
      {|theorem t = forall x where x == 1 -> x == 1 proof from x do rewrite where end end in 0|},
      is_free_hyp,
    ),
    /* --- Proofs are not values ---------------------------------------
       Theorems, axioms and hypotheses live in their own context
       namespace, like type aliases. */
    /* (a) A theorem name is not a value, so naming it in expression
       position is a free variable. */
    expects_mark(
      "a theorem name in expression position is free",
      {|theorem t = true proof ? in t|},
      is_free_var,
    ),
    /* (b) ...so a `let` and a `theorem` of the same name don't collide:
       `t` in the body is the value 5, not the theorem. */
    expects_no_mark(
      "let-bound name and theorem name do not collide",
      {|let t = 5 in theorem t = true proof ? in t|},
      is_free_var,
    ),
    /* (c) ...and a later `let` of the same name doesn't hide the theorem
       from a `rewrite` slot, which looks the name up in the theorem
       namespace. */
    expects_no_mark(
      "citation finds a theorem shadowed by a later value binding",
      {|theorem lem = 1 == 1 proof ? in let lem = 5 in theorem g = true proof from 1 do rewrite lem end end in lem|},
      is_free_hyp,
    ),
    /* (d) Hypotheses are invisible to expressions for the same reason:
       the auto-named `assume` hypothesis is citable in a proof but is
       not a variable of the program. */
    expects_mark(
      "an assume hypothesis is not an expression variable",
      {|theorem t = 1 == 1 proof assume 2 == 2 => ? in assume|},
      is_free_var,
    ),
    /* --- Hypothesis naming, at edit time ------------------------------
       The statics installs an `as` name in the theorem namespace, so a
       citation of it resolves while the user types, not only when the
       checker runs. */
    expects_no_mark(
      "an induction `as` name is a known hypothesis at edit time",
      {|theorem t = forall n -> n == n proof induction n > 0 as h | true => from n do rewrite h end end | false => ? end in t|},
      is_free_hyp,
    ),
    /* ...and it is visible in every case, not just the first. */
    expects_no_mark(
      "an induction `as` name is visible in every case",
      {|theorem t = forall n -> n == n proof induction n > 0 as h | true => ? | false => from n do rewrite h end end end in t|},
      is_free_hyp,
    ),
    /* An `as` name is scoped to its own form. */
    expects_mark(
      "an induction `as` name does not escape its form",
      {|theorem t = forall n -> n == n proof induction n > 0 as h | true => ? | false => ? end; from n do rewrite h end end in t|},
      is_free_hyp,
    ),
    /* A duplicate `as` name shadows rather than erroring: the inner
       binding hides the outer, and the citation still resolves. */
    expects_no_mark(
      "a duplicate `as` name shadows without erroring",
      {|theorem t = forall n -> forall m -> n == n proof induction n > 0 as h | true => induction m > 0 as h | true => from m do rewrite h end end | false => ? end | false => ? end in t|},
      is_free_hyp,
    ),
    /* `assume <e> as <h>` likewise. */
    expects_no_mark(
      "an assume `as` name is a known hypothesis at edit time",
      {|theorem t = forall n -> n == n proof assume n == 1 as h => from n do rewrite h end end in t|},
      is_free_hyp,
    ),
    /* An `as`/`alias` name binds only in the theorem namespace: like the
       auto-named ones, it is not a variable of the program. */
    expects_mark(
      "an `as` name is not an expression variable",
      {|theorem t = 1 == 1 proof assume 2 == 2 as h => ? in h|},
      is_free_var,
    ),
    /* `alias` re-binds an in-scope fact under a second name, and the
       body sees it. */
    expects_no_mark(
      "an alias name is a known hypothesis at edit time",
      {|theorem t = forall n -> n == n proof assume n == 1 => alias h = assume => from n do rewrite h end end in t|},
      is_free_hyp,
    ),
    /* The alias's fact slot is a fact reference, not a program variable:
       a bare name there must not read as free. (The body is `0`, not
       `t`: a theorem's own name is free in expression position, as
       tested above, and would mask this.) */
    expects_no_mark(
      "an alias's fact slot resolves a hypothesis name",
      {|theorem t = forall n -> n == n proof assume n == 1 => alias h = assume => ? in 0|},
      is_free_var,
    ),
    /* ...and aliasing a name that is not a fact does mark. */
    expects_mark(
      "aliasing an unknown name marks at edit time",
      {|theorem t = forall n -> n == n proof alias h = nosuchfact => ? in t|},
      is_free_hyp,
    ),
    /* --- The facts an induction case installs ------------------------
       A case body sees what `ProofCheck.check_induction` gives it: an
       `ih_<var>` per recursive position, the case equation and the arm's
       `where` facts. */
    expects_no_mark(
      "`ih_rest` is in scope in a recursive case",
      snoc_citing("ih_rest"),
      is_free_hyp,
    ),
    cites_fact_in_scope("ih_rest", snoc_citing("ih_rest")),
    expects_mark(
      "`ih_rest` is not in scope in a base case",
      {|theorem t = forall xs: [Int] -> xs == xs proof induction xs | [] => from xs do rewrite ih_rest end end | y :: rest => ? end in t|},
      is_free_hyp,
    ),
    expects_no_mark(
      "`case_eq` is in scope in a case",
      snoc_citing("case_eq"),
      is_free_hyp,
    ),
    cites_fact_in_scope("case_eq", snoc_citing("case_eq")),
    expects_mark(
      "an `as` name replaces `case_eq`",
      {|theorem t = forall xs: [Int] -> xs == xs proof induction xs as h | [] => from xs do rewrite case_eq end end | y :: rest => ? end in t|},
      is_free_hyp,
    ),
    /* The case equation `xs == y :: xs` would mention the pattern's own
       `xs`, not the scrutinee, so the checker doesn't install it. */
    expects_mark(
      "no `case_eq` when the pattern rebinds the scrutinee",
      {|theorem t = forall xs: [Int] -> xs == xs proof induction xs | [] => ? | y :: xs => from xs do rewrite case_eq end end end in t|},
      is_free_hyp,
    ),
    expects_mark(
      "an unknown name in a case still errors",
      snoc_citing("nosuchfact"),
      is_free_hyp,
    ),
    expects_no_mark(
      "a case arm's guard is citable as `where`",
      {|theorem t = forall xs: [Int] -> xs == xs proof induction xs | [] => ? | y :: rest where y > 0 => from xs do rewrite where end end | y :: rest => ? end in t|},
      is_free_hyp,
    ),
    /* The last arm is reached only when the guarded arm above failed. */
    expects_no_mark(
      "an earlier overlapping arm's negated guard is citable as `where`",
      {|theorem t = forall xs: [Int] -> xs == xs proof induction xs | [] => ? | y :: rest where y > 0 => ? | y :: rest => from xs do rewrite where end end end in t|},
      is_free_hyp,
    ),
    expects_mark(
      "a constructor-disjoint earlier arm gives no `where`",
      {|theorem t = forall xs: [Int] -> xs == xs proof induction xs | y :: rest where y > 0 => ? | y :: rest => ? | [] => from xs do rewrite where end end end in t|},
      is_free_hyp,
    ),
    /* --- Other forms that extend a body's scope ---------------------- */
    expects_no_mark(
      "a `have` body sees the hypothesis",
      {|theorem t = 1 == 1 proof have 2 == 2 proof ? => from 1 do rewrite have end end in t|},
      is_free_hyp,
    ),
    /* The subproof proves the `have`; it can't cite it. */
    expects_mark(
      "a `have` subproof doesn't see the hypothesis",
      {|theorem t = 1 == 1 proof have 2 == 2 proof from 2 do rewrite have end end => ? in t|},
      is_free_hyp,
    ),
    /* `x` is bound by the goal's `fun x ->` around the focused `x + 0`,
       which the checker brings into the focus body's scope. */
    expects_no_mark(
      "a focus body sees a binder its selector names",
      {|theorem t = (fun x -> x + 0) == (fun x -> x) proof from x + 0 do algebra x end end; ? in 0|},
      is_free_var,
    ),
    expects_mark(
      "a focus body's binder names don't escape it",
      {|theorem t = (fun x -> x + 0) == (fun x -> x) proof from x + 0 do algebra x end end; algebra x in 0|},
      is_free_var,
    ),
  ],
);
