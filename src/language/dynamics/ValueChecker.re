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

/* Classifies the form inside a closure, which the environment mode would
   re-wrap: `Fun` answers `Value` there, and anything finished-but-not-a-value
   answers `Value` too once wrapped, which is what hid stuck results. */
let rec check_value_unwrapped = (env, d) =>
  CV.transition(check_value_unwrapped, ~mode=`Substitution, env, d);

/* A closure packages a form with the environment it needs:
     - a function is a value;
     - a recursive binding holds a variable naming its own fixpoint, whose
       body is what decides (`fun -> fix` is a value, `fix -> fun` is not);
     - anything else in a closure is a stuck result, not a value.
   Environment entries are, by invariant, either finished forms or
   fixpoints, so resolving a variable terminates at one or the other. */
let rec is_value_form = (~rec_counter=0, env: Environment.t(Exp.t), d: Exp.t) =>
  if (rec_counter > 100) {
    false;
  } else {
    switch (Exp.term_of(d)) {
    | Var(x) =>
      switch (Environment.lookup(env, x)) {
      | Some(d') =>
        is_value_form(~rec_counter=rec_counter + 1, env, d')
      | None => false
      }
    | FixF(_, body, _) =>
      is_value_form(~rec_counter=rec_counter + 1, env, body)
    | Fun(_)
    | TypFun(_) => true
    | _ => check_value_unwrapped(env, d) == Value
    };
  };

/* Check if an expression is a fully-evaluated value. A probe sample is
   closure-wrapped, so ask about the form inside it. */
let is_value = (exp: Exp.t): bool =>
  switch (Exp.term_of(exp)) {
  | Closure(env, body) => is_value_form(env, body)
  | _ => check_value(Environment.empty, exp) == Value
  };
