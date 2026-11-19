open Util;

/* The conservative domain scan.
 *
 * `scan` collects the boolean domain conditions of every
 * partial-primitive application syntactically present in an expression:
 * the conditions under which no subterm evaluates to a domain error.
 * The gates in ProofCheck incur one obligation per condition.
 *
 * The scan is path-insensitive: in `if b == 0 then 0 else a / b` the
 * guarded division still emits `b != 0`. The user repairs an over-strong
 * condition by splitting on the guard and eval-stepping the conditional
 * away in each branch, where the branch's `case_eq` fact discharges the
 * condition by lookup. A weakest-precondition pass would avoid that;
 * conservative is always sound.
 *
 * Coverage:
 *   - `a / b` (Int / SInt / Nat) → `b != 0`. Float division is total
 *     under IEEE: `1.0 /. 0.0` is `infinity`, so it emits nothing.
 *   - `int_mod` / `sint_mod` / `nat_mod` / `float_mod` → `b != 0`
 *     (float mod errors on 0.0, unlike float division).
 *   - `a ** e` (Int / SInt) → `e >= 0` (they error on NegativeExponent;
 *     Nat power is total). Skipped when the exponent is a non-negative
 *     literal.
 *   - `int_of_float` / `sint_of_float` / `nat_of_float` →
 *     `is_finite(arg)` (they error on nan and inf).
 *
 * The scan doesn't descend into `Fun` bodies: a lambda's body doesn't
 * run until applied, and its conditions are phrased in the lambda's own
 * bound variables, which mean nothing to the caller.
 *
 * Not covered:
 *   - the `int_of_string` family: the language has no boolean predicate
 *     for parseability.
 *   - `string_sub` / `string_search` index bounds.
 *   - `nat_of_float` negativity (`arg >= 0.0`) and `Int → SInt/Nat`
 *     conversion errors (IntegerTooBig / NegativeNat): only finiteness
 *     is emitted.
 *   - mod/conversion builtins applied to a computed (non-literal)
 *     tuple: the denominator can't be named.
 *
 * Identical conditions are deduplicated (Exp.fast_equal). */

/* Deduplicate by Exp.fast_equal, preserving order. */
let dedup = (conditions: list(Exp.t)): list(Exp.t) =>
  List.fold_left(
    ~f=
      (acc, c) => List.exists(~f=Exp.fast_equal(c), acc) ? acc : acc @ [c],
    ~init=[],
    conditions,
  );

// Building conditions

/* Conditions use the operators user code parses to, so that a condition
 * `Exp.fast_equal`s the same guard written by hand: `!=` is polymorphic
 * inequality, and `>=` is the operand class's comparison. The closure
 * lemmas in Axioms are stated with these builders so that they conclude
 * exactly these conditions. */
let int_zero = (): Exp.t => Exp.fresh(Atom(Int(Bigint.zero)));
let nat_zero = (): Exp.t => Exp.fresh(Atom(Nat(Bigint.zero)));
let sint_zero = (): Exp.t => Exp.fresh(Atom(SInt(0)));

/* `b != 0`, at the class of `zero`. */
let neq_zero = (zero: Exp.t, b: Exp.t): Exp.t =>
  Exp.fresh(BinOp(Poly(NotEquals), b, zero));

let geq_zero_int = (e: Exp.t): Exp.t =>
  Exp.fresh(BinOp(Int(GreaterThanOrEqual), e, int_zero()));

/* Conditions of one builtin application, if it is a partial builtin
 * the scan covers. */
let builtin_conditions = (name: string, arg: Exp.t): list(Exp.t) => {
  /* The second component of a syntactically visible pair argument (mod
   * and the other two-argument builtins take a 2-tuple). */
  let pair_snd = (arg: Exp.t): option(Exp.t) =>
    switch (ProofSyntax.unwrap(arg) |> Exp.term_of) {
    | Tuple([_, b]) => Some(b)
    | _ => None
    };
  /* `is_finite(arg)`, with a Var head so that it matches a user-written
   * `is_finite(x)` fact, and env-substitutes to the builtin for closed
   * evaluation. */
  let is_finite = (arg: Exp.t): Exp.t =>
    Exp.fresh(Ap(Operators.Forward, Exp.fresh(Var("is_finite")), arg));
  switch (name) {
  | "int_mod" =>
    pair_snd(arg) |> Option.map(~f=neq_zero(int_zero())) |> Option.to_list
  | "sint_mod" =>
    pair_snd(arg) |> Option.map(~f=neq_zero(sint_zero())) |> Option.to_list
  | "nat_mod" =>
    pair_snd(arg) |> Option.map(~f=neq_zero(nat_zero())) |> Option.to_list
  /* Float mod's condition uses IEEE float inequality `!=.`. */
  | "float_mod" =>
    pair_snd(arg)
    |> Option.map(~f=b =>
         Exp.fresh(
           BinOp(Float(NotEquals), b, Exp.fresh(Atom(Float(0.0)))),
         )
       )
    |> Option.to_list
  | "int_of_float"
  | "sint_of_float"
  | "nat_of_float" => [is_finite(arg)]
  | _ => []
  };
};

// The scan

/* The domain conditions of every partial-primitive application in
 * `exp`, in traversal (pre-)order, deduplicated. */
let scan = (exp: Exp.t): list(Exp.t) => {
  /* A manifestly non-negative exponent literal needs no condition. */
  let nonneg_literal = (e: Exp.t): bool =>
    switch (ProofSyntax.unwrap(e) |> Exp.term_of) {
    | Atom(Int(n))
    | Atom(Nat(n)) => Bigint.(>=)(n, Bigint.zero)
    | Atom(SInt(i)) => i >= 0
    | _ => false
    };
  let conditions = ref([]);
  let emit = (c: Exp.t) => conditions := [c, ...conditions^];
  let visit = (e: Exp.t): unit =>
    switch (e |> Exp.term_of) {
    /* Integral division errors on 0. Float division is IEEE-total. */
    | BinOp(Int(Divide), _, b) => emit(neq_zero(int_zero(), b))
    | BinOp(SInt(Divide), _, b) => emit(neq_zero(sint_zero(), b))
    | BinOp(Nat(Divide), _, b) => emit(neq_zero(nat_zero(), b))
    /* Int/SInt power error on a negative exponent (Operators.re
     * int_power/sint_power); Nat power is total. */
    | BinOp(Int(Power), _, e2) when !nonneg_literal(e2) =>
      emit(geq_zero_int(e2))
    | BinOp(SInt(Power), _, e2) when !nonneg_literal(e2) =>
      emit(Exp.fresh(BinOp(SInt(GreaterThanOrEqual), e2, sint_zero())))
    | Ap(_, fn, arg) =>
      switch (ProofSyntax.unwrap(fn) |> Exp.term_of) {
      | Var(name)
      | BuiltinFun(name) =>
        List.iter(~f=emit, builtin_conditions(name, arg))
      | _ => ()
      }
    | _ => ()
    };
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          visit(e);
          switch (e |> Exp.term_of) {
          /* Function bodies are skipped (module header). */
          | Fun(_) => e
          | _ => continue(e)
          };
        },
      exp,
    );
  conditions^ |> List.rev |> dedup;
};
