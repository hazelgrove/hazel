open Transition;

type t =
  | Value
  | Indet
  | Expr;

module ValueCheckerEVMode: {
  include EV_MODE with type result = t and type state = unit;
} = {
  type state = unit;
  type result = t;

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

  let update_test = (_, _, _) => ();
};

module CV = Transition(ValueCheckerEVMode);

let rec check_value = (state, env, d) =>
  CV.transition(check_value, state, env, d);

let rec check_value_mod_ctx = ((), env, d) =>
  switch (DHExp.term_of(d)) {
  | Var(x) =>
    switch (ClosureEnvironment.lookup(env, x)) {
    | Some(v) => check_value_mod_ctx((), env, v)
    | None => CV.transition(check_value_mod_ctx, (), env, d)
    }
  | _ => CV.transition(check_value_mod_ctx, (), env, d)
  };
