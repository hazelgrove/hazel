open Util;

/* DOMAINCONDITIONS.re — the conservative domain scan (docs/
 * prover-obligations.md §4.1).
 *
 * `scan` collects the boolean domain conditions of every
 * partial-primitive application syntactically present in an expression:
 * the conditions under which no subterm evaluates to a domain error
 * (`err`, §1.1). The gates (ProofCheck) incur one obligation per
 * condition, run through the ordinary discharge channels (§4.2).
 *
 * The scan is deliberately path-INsensitive: in
 * `if b == 0 then 0 else a / b` the guarded division still emits
 * `b != 0`. Over-strong conditions are repaired manually — split on the
 * guard (bool case analysis), then eval-step the conditional away in
 * each branch, where the branch's `case_eq` fact discharges the
 * condition by lookup (§3.3). A weakest-precondition pass is a later,
 * purely-ergonomic upgrade; conservative is always sound.
 *
 * v1 coverage:
 *   - `a / b` (Int / SInt / Nat) → `b != 0`. Float division is
 *     IEEE-TOTAL (§1.5): `1.0 /. 0.0` is the value `infinity` — emits
 *     NOTHING.
 *   - `int_mod` / `sint_mod` / `nat_mod` / `float_mod` builtins →
 *     `b != 0` (float mod DOES error on 0.0, unlike float division —
 *     Phase 0 audit).
 *   - `a ** e` (Int / SInt) → `e >= 0` (they error on
 *     NegativeExponent, Operators.re; Nat power is total). Skipped when
 *     the exponent is a manifestly non-negative literal.
 *   - `int_of_float` / `sint_of_float` / `nat_of_float` →
 *     `is_finite(arg)` (they error on nan/inf — Phase 0 fix).
 *
 * v1 limitations (documented, deliberately skipped):
 *   - `int_of_string`-family: no clean boolean predicate for
 *     parseability exists in the language — skipped.
 *   - `string_sub` / `string_search` index bounds: skipped.
 *   - `nat_of_float` negativity (`arg >= 0.0`), `Int → SInt/Nat`
 *     conversion errors (IntegerTooBig / NegativeNat): only the
 *     finiteness condition is emitted.
 *   - mod/conversion builtins applied to a computed (non-literal)
 *     tuple: the denominator cannot be named — skipped.
 *
 * Identical conditions are deduplicated (Exp.fast_equal). */

/* Peel transparent wrappers to read an application's callee. */
let rec unwrap = (e: Exp.t): Exp.t =>
  switch (e |> Exp.term_of) {
  | Parens(inner)
  | Projector(_, inner)
  | Asc(inner, _)
  | Closure(_, inner) => unwrap(inner)
  | _ => e
  };

let callee_name = (fn: Exp.t): option(string) =>
  switch (unwrap(fn) |> Exp.term_of) {
  | Var(name)
  | BuiltinFun(name) => Some(name)
  | _ => None
  };

let int_zero: unit => Exp.t = () => Exp.fresh(Atom(Int(Bigint.zero)));
let nat_zero: unit => Exp.t = () => Exp.fresh(Atom(Nat(Bigint.zero)));
let sint_zero: unit => Exp.t = () => Exp.fresh(Atom(SInt(0)));
let float_zero: unit => Exp.t = () => Exp.fresh(Atom(Float(0.0)));

/* `b != 0` at the operand's class. Int/SInt/Nat go through polymorphic
 * inequality (what a user-written guard parses to); float mod's
 * condition uses IEEE float inequality `!=.`. */
let neq_zero = (zero: Exp.t, b: Exp.t): Exp.t =>
  Exp.fresh(BinOp(Poly(NotEquals), b, zero));
let neq_zero_float = (b: Exp.t): Exp.t =>
  Exp.fresh(BinOp(Float(NotEquals), b, float_zero()));

/* `e >= 0` for a power's exponent, at the exponent's class. */
let geq_zero_int = (e: Exp.t): Exp.t =>
  Exp.fresh(BinOp(Int(GreaterThanOrEqual), e, int_zero()));
let geq_zero_sint = (e: Exp.t): Exp.t =>
  Exp.fresh(BinOp(SInt(GreaterThanOrEqual), e, sint_zero()));

/* `is_finite(arg)` for float→integral conversions. The builtin exists
 * (BuiltinsBase); constructed with a Var head so it matches a
 * user-written `is_finite(x)` fact under Exp.fast_equal, and
 * env-substitutes to the BuiltinFun for closed evaluation. */
let is_finite = (arg: Exp.t): Exp.t =>
  Exp.fresh(Ap(Operators.Forward, Exp.fresh(Var("is_finite")), arg));

/* A manifestly non-negative exponent literal needs no condition. */
let nonneg_literal = (e: Exp.t): bool =>
  switch (unwrap(e) |> Exp.term_of) {
  | Atom(Int(n))
  | Atom(Nat(n)) => Bigint.(>=)(n, Bigint.zero)
  | Atom(SInt(i)) => i >= 0
  | _ => false
  };

/* The second component of a syntactically-visible pair argument (mod
 * and other TwoFun-style builtins take a 2-tuple). */
let pair_snd = (arg: Exp.t): option(Exp.t) =>
  switch (unwrap(arg) |> Exp.term_of) {
  | Tuple([_, b]) => Some(b)
  | _ => None
  };

/* Conditions of one builtin application, if it is a partial builtin we
 * cover in v1. */
let builtin_conditions = (name: string, arg: Exp.t): list(Exp.t) =>
  switch (name) {
  | "int_mod" =>
    pair_snd(arg) |> Option.map(neq_zero(int_zero())) |> Option.to_list
  | "sint_mod" =>
    pair_snd(arg) |> Option.map(neq_zero(sint_zero())) |> Option.to_list
  | "nat_mod" =>
    pair_snd(arg) |> Option.map(neq_zero(nat_zero())) |> Option.to_list
  | "float_mod" =>
    pair_snd(arg) |> Option.map(neq_zero_float) |> Option.to_list
  | "int_of_float"
  | "sint_of_float"
  | "nat_of_float" => [is_finite(arg)]
  /* int_of_string family / string index bounds: skipped in v1 (see
   * module header). */
  | _ => []
  };

/* Collect the domain conditions of every partial-primitive application
 * in `exp`, in traversal (pre-)order, deduplicated. */
let scan = (exp: Exp.t): list(Exp.t) => {
  let conditions = ref([]);
  let emit = (c: Exp.t) => conditions := [c, ...conditions^];
  let visit = (e: Exp.t): unit =>
    switch (e |> Exp.term_of) {
    /* Integral division errors on 0. Float(Divide) is IEEE-total
     * (§1.5): deliberately no case — emits nothing. */
    | BinOp(Int(Divide), _, b) => emit(neq_zero(int_zero(), b))
    | BinOp(SInt(Divide), _, b) => emit(neq_zero(sint_zero(), b))
    | BinOp(Nat(Divide), _, b) => emit(neq_zero(nat_zero(), b))
    /* Int/SInt power error on a negative exponent (Operators.re
     * int_power/sint_power); Nat power is total. */
    | BinOp(Int(Power), _, e2) when !nonneg_literal(e2) =>
      emit(geq_zero_int(e2))
    | BinOp(SInt(Power), _, e2) when !nonneg_literal(e2) =>
      emit(geq_zero_sint(e2))
    | Ap(_, fn, arg) =>
      switch (callee_name(fn)) {
      | Some(name) => List.iter(emit, builtin_conditions(name, arg))
      | None => ()
      }
    | _ => ()
    };
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          visit(e);
          continue(e);
        },
      exp,
    );
  let dedup =
    List.fold_left(
      (acc, c) => List.exists(Exp.fast_equal(c), acc) ? acc : acc @ [c],
      [],
    );
  conditions^ |> List.rev |> dedup;
};
