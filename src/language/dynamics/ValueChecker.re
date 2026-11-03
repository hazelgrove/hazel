open Transition;

type t =
  | Value
  | Indet
  | Expr;

module ValueCheckerEVMode: {
  include EV_MODE with type inner_result = t and type result = t;
} = {
  type result = t;
  type inner_result = result;

  type requirement('a) = ('a, result);
  type requirements('a, 'b) = ('a, result);

  let combine = (r1, r2) =>
    switch (r1, r2) {
    | (Expr, _) => Expr
    | (_, Expr) => Expr
    | (Indet, _) => Indet
    | (_, Indet) => Indet
    | (Value, Value) => Value
    };

  let req_final = (vc, _, d) => (d, vc(d));
  let req_all_final = (vc, _, ds) =>
    List.fold_right(
      ((v1, r1), (v2, r2)) => ([v1, ...v2], combine(r1, r2)),
      List.map(req_final(vc, x => x), ds),
      ([], Value),
    );

  let otherwise = (_, _) => ((), Value);

  let (let.) = ((v, r), rule) =>
    switch (r, rule(v)) {
    | (_, Constructor) => r
    | (Expr, Indet) => Expr
    | (_, Indet) => Indet
    | (_, Value) => Value
    | (_, Step(_)) => Expr
    };

  let (and.) = ((v1, r1), (v2, r2)) => {
    ((v1, v2), combine(r1, r2));
  };
};

module CV = Transition(ValueCheckerEVMode);

let rec check_value = (env, d) =>
  CV.transition(check_value, ~mode=`Environment, env, d);

/* Classifies a form in substitution mode, which answers for the form itself
   rather than re-wrapping it: `Fun` answers `Value` there, and anything
   finished-but-not-a-value answers `Value` too once wrapped, which is what
   hid stuck results. */
let rec check_value_unwrapped = (env, d) =>
  switch (Exp.term_of(d)) {
  /* A nested closure carries its own environment around a finished form, so
     it is judged exactly like the sample's own closure. The transition's
     `Closure` rule answers `Constructor` — and thus `Value` — for any
     closure, so without this case `[if ? then 1 else 2]`, which is really
     `Closure(env, ListLit([Closure(env, If ? then 1 else 2)]))`, passes as a
     value and the probe loses its indet styling. A closure's form is final
     either way, so a non-value form is `Indet`, not `Expr`. */
  | Closure(env', body) => is_value_form(env', body) ? Value : Indet
  | _ => CV.transition(check_value_unwrapped, ~mode=`Substitution, env, d)
  }

/* A closure packages a form with the environment it needs:
     - a function is a value;
     - a recursive binding holds a variable naming its own fixpoint, whose
       body is what decides (`fun -> fix` is a value, `fix -> fun` is not);
     - anything else in a closure is a stuck result, not a value.
   Environment entries are, by invariant, either finished forms or
   fixpoints, so resolving a variable terminates at one or the other; a
   chain this long means that invariant is broken, so say so rather than
   guess. */
and is_value_form = (~rec_counter=0, env: Environment.t(Exp.t), d: Exp.t) =>
  if (rec_counter > 100) {
    failwith(
      "ValueChecker.is_value_form: over 100 variable/fixpoint lookups; "
      ++ "an environment entry is neither a finished form nor a fixpoint",
    );
  } else {
    switch (Exp.term_of(d)) {
    | Var(x) =>
      switch (Environment.lookup(env, x)) {
      /* A variable bound to itself names a bound variable rather than a
         form; the transition answers `Indet` for it, so it is not a value.
         (Not an invariant break: fixpoint environments legitimately hold
         these.) */
      | Some({term: Var(y), _}) when x == y => false
      | Some(d') => is_value_form(~rec_counter=rec_counter + 1, env, d')
      | None => false
      }
    | FixF(_, body, _) =>
      is_value_form(~rec_counter=rec_counter + 1, env, body)
    | Fun(_)
    | TypFun(_) => true
    | _ => check_value_unwrapped(env, d) == Value
    };
  };

/* Check if an expression is a fully-evaluated value.
   A probe sample may arrive closure-wrapped or, when it is a container whose
   children were wrapped, as a bare form holding nested closures, so ask in
   substitution mode where each closure is judged by the form it wraps. */
let is_value = (exp: Exp.t): bool =>
  check_value_unwrapped(Environment.empty, exp) == Value;
