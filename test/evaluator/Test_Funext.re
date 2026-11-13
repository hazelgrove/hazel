open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* `funext x => <proof>`: functional extensionality as a proof form, not
 * an axiom. An axiom is a rewrite equality; stating funext as one would
 * have to quantify over functions at unknown/hole types. Funext is an
 * intro rule, like `forall x =>` and `assume`.
 *
 * Rule: with incoming goal `f == g` at an arrow type, the body's incoming
 * goal is the pointwise `f(x) == g(x)` and `x` is bound at the domain
 * type. Outgoing is `true` when the body closes that; otherwise the outer
 * `f == g` passes through (same convention as Forall). */

let full_status = (src: string): ProofMap.full_status => {
  let (pm, proof) = run(src);
  ProofMap.full_status_of_proof(pm, proof);
};

let check_status = (msg: string, expected: ProofMap.full_status, src: string) => {
  let actual = full_status(src);
  Alcotest.check(
    Alcotest.bool,
    msg ++ " (got: " ++ ProofMap.show_full_status(actual) ++ ")",
    true,
    actual == expected,
  );
};

/* The immediate body of a `funext` node — the sub-proof that sees the
 * pointwise goal. */
let funext_body = (p: Proof.t): Proof.t =>
  switch (p.term) {
  | Funext(_, body) => body
  | _ => Alcotest.fail("expected the proof to be a funext node")
  };

/* --- 1. Happy path ---------------------------------------------------- */

/* The commuting pointwise-as-functions theorem. `funext x` turns the
 * function equality into `(fun x -> x + 1)(x) == (fun x -> 1 + x)(x)`,
 * the rewrite commutes the left body, and refl closes it. Fully Proven. */
let test_happy_path_proven = () =>
  check_status(
    "pointwise commute proves as a function equality",
    ProofMap.Proven,
    {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof funext x => from x + 1 do rewrite 1 + x end end; from 1 + x == 1 + x do axiom refl_eq end end in t|},
  );

/* The body's incoming goal is the pointwise one. When a side is a
 * literal lambda, the application is a beta-redex whose contraction is
 * definitional, so the goal the body sees is the contracted `x + 1 == 1 + x`
 * — which is what makes a plain pointwise `rewrite` reach it. */
let test_body_goal_is_pointwise = () => {
  let src = {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof funext x => ? in t|};
  let (pm, proof) = run(src);
  let goal = incoming_of(pm, funext_body(proof));
  check_exp(
    "the body sees the contracted pointwise goal (got: "
    ++ print_exp(goal)
    ++ ")",
    "x + 1 == 1 + x",
    goal,
  );
};

/* A named function stays an application: unfolding a definition is
 * `eval`'s job, not funext's. This keeps
 * `refl_eq` on `inv(x) == inv(x)` a claim about `inv` applied to an
 * arbitrary argument rather than a claim about `inv`'s body. */
let test_named_function_stays_applied = () => {
  let src = {|let inv = fun x -> 1 / x in theorem t = inv == inv proof funext x => ? in t|};
  let (pm, proof) = run(src);
  let goal = incoming_of(pm, funext_body(proof));
  let is_ap = (e: Exp.t) =>
    switch (e |> Exp.term_of) {
    | Ap(_, _, _) => true
    | _ => false
    };
  switch (goal |> Exp.term_of) {
  | BinOp(Poly(Equals), l, r) =>
    Alcotest.check(
      Alcotest.bool,
      "both sides stay applications for a named function (got: "
      ++ print_exp(goal)
      ++ ")",
      true,
      is_ap(l) && is_ap(r),
    )
  | _ =>
    Alcotest.fail("expected an `==` body goal, got: " ++ print_exp(goal))
  };
};

/* And that pointwise goal closes by reflexivity without ever applying
 * `inv`, which is a different claim from `inv == (fun x -> 1 / x)` (see
 * the partiality test below). */
let test_named_function_refl_proves = () =>
  check_status(
    "`inv == inv` closes pointwise by refl without applying inv",
    ProofMap.Proven,
    {|let inv = fun x -> 1 / x in theorem t = inv == inv proof funext x => from inv(x) == inv(x) do axiom refl_eq end end in t|},
  );

/* --- 2. Control: refl at identical functions, without funext ---------- */

/* `f == f` between two syntactically identical lambdas closes by
 * reflexivity with no funext. */
let test_refl_identical_functions_no_funext = () =>
  check_status(
    "refl closes identical function literals without funext",
    ProofMap.Proven,
    {|theorem t = (fun x -> x + 1) == (fun x -> x + 1) proof from (fun x -> x + 1) == (fun x -> x + 1) do axiom refl_eq end end in t|},
  );

/* --- 3. Malformed: funext on a non-function equality ------------------ */

/* `1 == 1` is an equality, but not at an arrow type: there is no
 * pointwise goal to form. Mark and pass the goal through, the same
 * recovery `forall` uses for `ExpectedForallGoal`. */
let test_non_arrow_equality_marked = () => {
  let src = {|theorem t = 1 == 1 proof funext x => ? in t|};
  let (pm, proof) = run(src);
  Alcotest.check(
    Alcotest.bool,
    "ExpectedFunEqGoal is emitted on a non-arrow `==`",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.ExpectedFunEqGoal => true
      | _ => false,
    ),
  );
  Alcotest.check(
    Alcotest.bool,
    "a marked funext does not prove the theorem",
    false,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- 4. Control: the hole, with no funext ----------------------------- */

/* A bare hole against a function equality is Incomplete. If this flips
 * to Proven, function `==` has started to compute, which would be
 * wrong. */
let test_hole_without_funext_incomplete = () =>
  check_status(
    "a hole against a function equality stays Incomplete",
    ProofMap.Incomplete,
    {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof ? in t|},
  );

/* A `funext` whose body is still a hole is Incomplete too: the form
 * itself proves nothing. */
let test_funext_hole_body_incomplete = () =>
  check_status(
    "funext with a hole body is Incomplete",
    ProofMap.Incomplete,
    {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof funext x => ? in t|},
  );

/* --- 5. Currying: one arrow per funext -------------------------------- */

/* `a -> b -> c` peels once to `f(x) == g(x)`, which is still an arrow; a
 * second `funext y =>` peels again. */
let test_two_arrow_nested_funext = () => {
  let src = {|theorem t = (fun x -> fun y -> x + y) == (fun x -> fun y -> y + x) proof funext x => funext y => from x + y do rewrite y + x end end; from y + x == y + x do axiom refl_eq end end in t|};
  let (pm, proof) = run(src);
  let status = ProofMap.full_status_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.bool,
    "two nested funext close the curried commute (got: "
    ++ ProofMap.show_full_status(status)
    ++ ")",
    true,
    status == ProofMap.Proven,
  );
};

/* The inner funext's goal is the twice-applied pointwise equality. */
let test_two_arrow_inner_goal = () => {
  let src = {|theorem t = (fun x -> fun y -> x + y) == (fun x -> fun y -> y + x) proof funext x => funext y => ? in t|};
  let (pm, proof) = run(src);
  let inner = funext_body(proof);
  let inner_goal = incoming_of(pm, funext_body(inner));
  Alcotest.check(
    Alcotest.bool,
    "the inner body goal is an equality (got: "
    ++ print_exp(inner_goal)
    ++ ")",
    true,
    switch (inner_goal |> Exp.term_of) {
    | BinOp(Poly(Equals), _, _) => true
    | _ => false
    },
  );
};

/* --- 6. Partiality: no automatic `where` on the body goal -------------- */

/* Funext doesn't wrap the body goal in a domain restriction: partiality
 * shows up when a later step applies `f(x)`. So proving
 * `inv == (fun x -> 1 / x)` by evaluating the application is not a
 * clean Proven. */
let test_partial_function_not_cleanly_proven = () => {
  let src = {|let inv = fun x -> 1 / x in theorem t = inv == (fun x -> 1 / x) proof funext x => from inv(x) == inv(x) do axiom refl_eq end end in t|};
  let (pm, proof) = run(src);
  let status = ProofMap.full_status_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.bool,
    "a partial function equality is not a clean Proven (got: "
    ++ ProofMap.show_full_status(status)
    ++ ")",
    false,
    status == ProofMap.Proven,
  );
};

/* --- 7. Contraction keeps let-bound names ----------------------------- */

/* A literal lambda whose body mentions a let-bound name:
 *
 *   let f : Int -> Int = fun x -> 1 / x in
 *   theorem t = (fun x -> f(f(x))) == (fun x : Int -> x) proof
 *   funext y => from f do eval end in t
 *
 * Contracting the redex substitutes only `x |-> y`, so the pointwise goal
 * names `f` as the lambda did, and later steps find it by name. */

let funext_repro = (body: string): string =>
  "let f : Int -> Int = fun x -> 1 / x in theorem t = (fun x -> f(f(x))) == (fun x : Int -> x) proof "
  ++ body
  ++ " in t";

let test_letbound_name_is_kept = () => {
  let (pm, proof) = run(funext_repro("funext y => ?"));
  Alcotest.check(
    Alcotest.string,
    "the pointwise goal",
    "f(f(y)) == y",
    print_exp(incoming_of(pm, funext_body(proof))),
  );
};

/* And so `eval f at 0` inside the funext body must not be a
 * PatternNotFound. Whether that eval then has anything to step is a
 * separate question (`f`'s definition is already a
 * value); the assertion here is only that the pattern is found. */
let test_eval_in_funext_body_finds_pattern = () => {
  let src = funext_repro("funext y => from f do eval end");
  let (pm, proof) = run(src);
  Alcotest.check(
    Alcotest.bool,
    "`eval f` inside funext finds its pattern (status: "
    ++ ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof))
    ++ ")",
    false,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.PatternNotFound(_) => true
      | _ => false,
    ),
  );
};

/* Same, for an eval of the application rather than the bare name, the
 * form that has a redex to reduce. */
let test_eval_application_in_funext_body_finds_pattern = () => {
  let src = funext_repro("funext y => from f(f(y)) do eval end");
  let (pm, proof) = run(src);
  Alcotest.check(
    Alcotest.bool,
    "`eval f(f(y))` inside funext finds its pattern (status: "
    ++ ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof))
    ++ ")",
    false,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.PatternNotFound(_) => true
      | _ => false,
    ),
  );
};

/* --- Insert-time binder recovery ---------------------------------------
 *
 * `ProofCheck.Funext.binder` is what the step picker's Funext button
 * uses to open `funext x => ?` instead of `funext ? => ?`. The rule
 * itself reads whatever binder is written, so these tests read the
 * pattern it hands back, not a checking result. */

let print_pat = (p: Pat.t): string =>
  (Pat(p): Any.t)
  |> Haz3lcore.ExpToSegment.any_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

let binder_of = (~env=Environment.empty, src: string): option(string) => {
  let goal = parse_exp(src);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), goal);
  ProofCheck.Funext.binder(~info_map, ~env, goal) |> Option.map(print_pat);
};

let check_binder = (msg: string, expected: option(string), actual) =>
  Alcotest.check(
    Alcotest.(option(string)),
    msg,
    expected,
    actual |> Option.map(String.trim),
  );

/* The LHS lambda names its parameter, so the step opens with that name;
 * the RHS's `y` is not consulted. */
let test_binder_from_lhs_lambda = () =>
  check_binder(
    "literal lambda gives its parameter",
    Some("x"),
    binder_of("(fun x -> x) == (fun y -> y)"),
  );

/* Nothing to read off either side: keep the hole. */
let test_binder_opaque_is_hole = () =>
  check_binder(
    "opaque function equality gives no binder",
    None,
    binder_of("f == g"),
  );

/* The whole pattern is copied: `fun (x, y) ->` has to be bound by
 * `funext (x, y) =>`, since the binder binds everything the parameter
 * binds. */
let test_binder_tuple_pattern = () =>
  check_binder(
    "tuple parameter copies in full",
    Some("(x, y)"),
    binder_of("(fun (x, y) -> x) == (fun (a, b) -> a)"),
  );

/* A named function: the binder comes from the definition the env holds,
 * so `inv == inv` still opens as `funext x =>`. */
let test_binder_named_function = () => {
  let env = Environment.of_bindings([("inv", parse_exp("fun x -> 1 / x"))]);
  check_binder(
    "named function reads its definition's parameter",
    Some("x"),
    binder_of(~env, "inv == inv"),
  );
};

/* --- id uniqueness of the synthesized pointwise goal ------------------ */

/* The body's incoming goal is synthesized: `f(x) == g(x)` is assembled
 * from the incoming goal's own two sides plus one argument tree lifted
 * out of the funext binder, each carrying its ids from the source
 * program, and the argument is written into both sides. Without
 * freshening, the goal claims the binder's tiles twice and re-claims
 * tiles still live in the funext row and the main editor. Duplicate ids
 * in one expression make `Highlight.of_tile` fail on a shard-count
 * mismatch (double-click in the inner stepper), and ids shared with the
 * outside park the caret in the wrong place on a single click.
 *
 * Mirrors `Test_EditorTransform.duplicate_tile_ids`: collect, count,
 * assert zero repeats. */
let all_ids = (e: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) => {
      acc := x.annotation.ids @ acc^;
      continue(x);
    };
  let _: Exp.t =
    e |> Exp.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f);
  acc^ |> List.filter(id => id != Id.invalid);
};

let duplicates = (ids: list(Id.t)): list(Id.t) => {
  let seen = Hashtbl.create(64);
  List.filter(
    id =>
      switch (Hashtbl.find_opt(seen, id)) {
      | Some () => true
      | None =>
        Hashtbl.add(seen, id, ());
        false;
      },
    ids,
  );
};

let check_no_duplicate_ids = (msg: string, e: Exp.t) => {
  let dups = duplicates(all_ids(e));
  Alcotest.check(
    Alcotest.int,
    msg
    ++ " — duplicated ids: "
    ++ String.concat(", ", List.map(Id.to_string, dups))
    ++ "\nin: "
    ++ print_exp(e),
    0,
    List.length(dups),
  );
};

/* The minimal shape: one binder, written into both sides. */
let test_body_goal_ids_unique_minimal = () => {
  let src = {|theorem t = (fun x -> x) == (fun x -> x) proof funext x => ? in t|};
  let (pm, proof) = run(src);
  check_no_duplicate_ids(
    "the pointwise body goal has no duplicate ids",
    incoming_of(pm, funext_body(proof)),
  );
};

/* Named functions on both sides, so neither application contracts and
 * the shared `xs` argument appears in the goal twice. */
let matt_repro = {|let plus = fun (x,y) -> x + y in
let sum(xs) = case xs
| [] => 0
| x::xs => x + sum(xs)
end
in

theorem t =
    sum == fold_left(_, sum, 0)
proof
    funext xs => ?
in t|};

let test_body_goal_ids_unique_named = () => {
  let (pm, proof) = run(matt_repro);
  check_no_duplicate_ids(
    "the pointwise goal of a named-function funext has no duplicate ids",
    incoming_of(pm, funext_body(proof)),
  );
};

/* And the goal must not reuse the outer `f == g`'s ids either: those
 * tiles belong to the theorem's own editor, and sharing them makes a
 * click inside the inner stepper jump out to the parent row. */
let test_body_goal_ids_disjoint_from_outer = () => {
  let src = {|theorem t = (fun x -> x) == (fun x -> x) proof funext x => ? in t|};
  let (pm, proof) = run(src);
  let outer = all_ids(incoming_of(pm, proof));
  let body = all_ids(incoming_of(pm, funext_body(proof)));
  let seen = Hashtbl.create(64);
  List.iter(id => Hashtbl.replace(seen, id, ()), outer);
  let shared = List.filter(id => Hashtbl.mem(seen, id), body);
  Alcotest.check(
    Alcotest.int,
    "the pointwise goal shares no ids with the outer function equality "
    ++ "— shared: "
    ++ String.concat(", ", List.map(Id.to_string, shared)),
    0,
    List.length(shared),
  );
};

/* --- Grouping parens around a proof ----------------------------------- */

/* `( funext x => ... )` is a ParensProof tile: a Proof-sorted paren whose
 * child is the wrapping form, rather than an Exp paren around a
 * sort-mismatched `funext`. */
let test_parens_parse_shape = () => {
  let src = {|theorem t = (fun x -> x) == (fun x -> x) proof ( funext x => ) in t|};
  let proof = find_theorem_proof(parse_exp(src));
  switch (proof) {
  | Some({term: Parens({term: Funext(_, {term: EmptyHole, _}), _}), _}) =>
    ()
  | Some(p) =>
    Alcotest.fail(
      "expected Parens(Funext(_, EmptyHole)), got " ++ Proof.show(p),
    )
  | None => Alcotest.fail("no theorem found")
  };
};

/* A program that needs ParensProof. Checked as a parse test only: whether `sum == fold_left(_, sum, 0)` type-checks is
 * a separate question from whether `( funext xs =>)` groups. */
let test_parens_motivating_program = () => {
  let src = {|let plus = fun (x,y) -> x + y in
let sum(xs) = case xs
| [] => 0
| x::xs => x + sum(xs)
end
in

theorem t =
    sum == fold_left(_, sum, 0)
proof
    ( funext xs =>)
in
|};
  switch (find_theorem_proof(parse_exp(src))) {
  | Some({term: Parens({term: Funext(_, _), _}), _}) => ()
  | Some(p) =>
    Alcotest.fail(
      "expected the proof to be Parens(Funext(...)), got " ++ Proof.show(p),
    )
  | None => Alcotest.fail("no theorem found")
  };
};

/* The checker sees straight through the parens: a parenthesized proof
 * that would prove the theorem still proves it. */
let test_parens_transparent_to_checker = () =>
  check_status(
    "a parenthesized funext proof still proves",
    ProofMap.Proven,
    {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof ( funext x => from x + 1 do rewrite 1 + x end end; from 1 + x == 1 + x do axiom refl_eq end end ) in t|},
  );

/* The stepper sees through them too: the parens are unwrapped before
 * looking for a `;` chain, so the row is the funext's, with its body. */
let test_parens_transparent_to_stepper = () => {
  let inner =
    Proof.fresh(Funext(Pat.fresh(Var("x")), Proof.fresh(EmptyHole)));
  let (head, tail) = Web.StepperRows.split(Proof.fresh(Parens(inner)));
  Alcotest.check(
    Alcotest.bool,
    "a parenthesized leaf step has no tail",
    true,
    tail == None,
  );
  switch (head.term) {
  | Funext(_, _) => ()
  | _ => Alcotest.fail("the parens were not unwrapped to the funext")
  };
  switch (
    Web.StepperRows.chain(
      ~proof_map=ProofMap.empty,
      Proof.fresh(Parens(inner)),
    )
  ) {
  | [{kind: Step(Body(_)), step: {term: Funext(_), _}, _}, _] => ()
  | _ => Alcotest.fail("a parenthesized funext is not a funext row")
  };
};

let tests = (
  "Funext",
  [
    test_case(
      "grouping parens: `( funext x => )` parses as Parens(Funext(...))",
      `Quick,
      test_parens_parse_shape,
    ),
    test_case(
      "grouping parens: the motivating program parses",
      `Quick,
      test_parens_motivating_program,
    ),
    test_case(
      "grouping parens: the checker sees through them",
      `Quick,
      test_parens_transparent_to_checker,
    ),
    test_case(
      "grouping parens: the stepper's proof splitting sees through them",
      `Quick,
      test_parens_transparent_to_stepper,
    ),
    test_case(
      "happy path: pointwise commute proves as `f == g`",
      `Quick,
      test_happy_path_proven,
    ),
    test_case(
      "the body's goal is the contracted pointwise equality",
      `Quick,
      test_body_goal_is_pointwise,
    ),
    test_case(
      "a named function stays applied in the pointwise goal",
      `Quick,
      test_named_function_stays_applied,
    ),
    test_case(
      "`inv == inv` closes pointwise by refl, without applying inv",
      `Quick,
      test_named_function_refl_proves,
    ),
    test_case(
      "control: refl still closes identical functions without funext",
      `Quick,
      test_refl_identical_functions_no_funext,
    ),
    test_case(
      "funext on a non-arrow `==` is marked ExpectedFunEqGoal",
      `Quick,
      test_non_arrow_equality_marked,
    ),
    test_case(
      "control: a hole against a function equality stays Incomplete",
      `Quick,
      test_hole_without_funext_incomplete,
    ),
    test_case(
      "funext with a hole body is Incomplete",
      `Quick,
      test_funext_hole_body_incomplete,
    ),
    test_case(
      "currying: two nested funext peel two arrows",
      `Quick,
      test_two_arrow_nested_funext,
    ),
    test_case(
      "currying: the inner body goal is an equality",
      `Quick,
      test_two_arrow_inner_goal,
    ),
    test_case(
      "partiality: no automatic `where`, so `inv == ...` is not clean",
      `Quick,
      test_partial_function_not_cleanly_proven,
    ),
    test_case(
      "a let-bound name is kept when funext contracts the redex",
      `Quick,
      test_letbound_name_is_kept,
    ),
    test_case(
      "`eval f` inside a funext body is not a PatternNotFound",
      `Quick,
      test_eval_in_funext_body_finds_pattern,
    ),
    test_case(
      "`eval f(f(y))` inside a funext body is not a PatternNotFound",
      `Quick,
      test_eval_application_in_funext_body_finds_pattern,
    ),
    test_case(
      "insert: binder comes from the LHS lambda's parameter",
      `Quick,
      test_binder_from_lhs_lambda,
    ),
    test_case(
      "insert: an opaque function equality keeps the hole binder",
      `Quick,
      test_binder_opaque_is_hole,
    ),
    test_case(
      "insert: a tuple parameter copies whole",
      `Quick,
      test_binder_tuple_pattern,
    ),
    test_case(
      "insert: a named function's binder comes from its definition",
      `Quick,
      test_binder_named_function,
    ),
    test_case(
      "ids: the pointwise body goal has no duplicate ids",
      `Quick,
      test_body_goal_ids_unique_minimal,
    ),
    test_case(
      "ids: a named-function pointwise goal has no duplicate ids",
      `Quick,
      test_body_goal_ids_unique_named,
    ),
    test_case(
      "ids: the pointwise goal is disjoint from the outer goal",
      `Quick,
      test_body_goal_ids_disjoint_from_outer,
    ),
  ],
);
