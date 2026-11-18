open Alcotest;
open Test_Statics_Prelude;
open Language;

/* Helpers to check that the statics result for a given program
   contains a mark matching a supplied predicate, or that it contains
   no errors of interest. */
let has_mark = (pred: Mark.t => bool, map: Statics.Map.t): bool =>
  Id.Map.exists(
    (_, info: Info.t) => List.exists(~f=pred, Info.marks_of(info)),
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
                when String.equal(x, name) =>
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
          List.map(~f=Mark.show, e.marks),
        );
        Alcotest.check(
          bool,
          "a fact in scope",
          true,
          Option.is_some(Ctx.lookup_theorem(e.ctx, name)),
        );
      };
    },
  );

/* A list split, `induction` or `cases`, whose cons case cites `fact`. */
let snoc_citing = (~split="induction", fact) =>
  {|theorem t = forall xs: [Int] -> forall x: Int -> xs @ [x] == xs @ [x] proof |}
  ++ split
  ++ {| xs | [] => ? | y :: rest => from rest @ [x] do rewrite |}
  ++ fact
  ++ {| end end end in t|};

let tests = (
  "Statics.Proof",
  [
    expects_no_mark(
      "!! of a function is a Bool",
      {|let b : Bool = !!(fun (x : Int) -> 1 / x) in b|},
      is_expectation_mismatch,
    ),
    expects_mark(
      "!! is not an Int",
      {|let n : Int = !!3 in n|},
      is_expectation_mismatch,
    ),
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
    expects_mark(
      "cases flags inexhaustive match",
      {|theorem t = true proof cases 1 | 0 => ? end in t|},
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
    /* --- Implication ------------------------------------------------- */
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
       The statics installs a fact's name in the theorem namespace, so a
       citation of it resolves while the user types, not only when the
       checker runs. */
    expects_no_mark(
      "an assume's name is a known hypothesis at edit time",
      {|theorem t = forall n -> n == n proof assume h = n == 1 => from n do rewrite h end end in t|},
      is_free_hyp,
    ),
    /* A fact's name binds only in the theorem namespace: like the
       auto-named ones, it is not a variable of the program. */
    expects_mark(
      "a fact's name is not an expression variable",
      {|theorem t = 1 == 1 proof assume h = 2 == 2 => ? in h|},
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
    /* --- The facts a split's case installs ---------------------------
       A case body sees what `ProofCheck.check_split` gives it: in an
       `induction`, an `ih_<var>` per recursive position, and in a
       `cases`, the case equation. */
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
      "`case_eq` is in scope in a `cases` case",
      snoc_citing(~split="cases", "case_eq"),
      is_free_hyp,
    ),
    cites_fact_in_scope("case_eq", snoc_citing(~split="cases", "case_eq")),
    expects_mark(
      "`case_eq` is not in scope in an `induction` case",
      snoc_citing("case_eq"),
      is_free_hyp,
    ),
    expects_mark(
      "`ih_rest` is not in scope in a `cases` case",
      snoc_citing(~split="cases", "ih_rest"),
      is_free_hyp,
    ),
    /* An induction case is about a fresh value in the scrutinee's place,
       so a fact about the scrutinee is out of scope there. */
    expects_mark(
      "an `induction` case can't cite a fact about the scrutinee",
      {|theorem t = forall xs: [Int] -> xs == [] ==> xs == xs proof assume xs == [] => induction xs | [] => from xs do rewrite assume end end | y :: rest => ? end in t|},
      is_free_hyp,
    ),
    expects_no_mark(
      "a `cases` case can cite a fact about the scrutinee",
      {|theorem t = forall xs: [Int] -> xs == [] ==> xs == xs proof assume xs == [] => cases xs | [] => from xs do rewrite assume end end | y :: rest => ? end in t|},
      is_free_hyp,
    ),
    expects_no_mark(
      "an `induction` case can cite a fact that doesn't mention the scrutinee",
      {|theorem t = forall xs: [Int] -> forall n: Int -> n == 1 ==> xs == xs proof assume n == 1 => induction xs | [] => from n do rewrite assume end end | y :: rest => ? end in t|},
      is_free_hyp,
    ),
    /* The case equation `xs == y :: xs` would mention the pattern's own
       `xs`, not the scrutinee, so the checker doesn't install it. */
    expects_mark(
      "no `case_eq` when the pattern rebinds the scrutinee",
      {|theorem t = forall xs: [Int] -> xs == xs proof cases xs | [] => ? | y :: xs => from xs do rewrite case_eq end end end in t|},
      is_free_hyp,
    ),
    /* `generalize x` makes `x` arbitrary again, so the facts about the
       old `x` leave scope, as in the checker. */
    expects_mark(
      "generalize hides the facts about its variable",
      {|theorem t = forall x: Int -> x > 0 ==> x + 0 == x proof assume x > 0 => generalize x => revert assume => ? in t|},
      is_free_hyp,
    ),
    expects_mark(
      "an unknown name in a case still errors",
      snoc_citing("nosuchfact"),
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
