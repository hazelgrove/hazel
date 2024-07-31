open DHExp;
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

  type requirement('a) = ('a, (result, bool));
  type requirements('a, 'b) = ('a, (result, bool));

  let combine = ((r1, b1), (r2, b2)) => (
    switch (r1, r2) {
    | (Expr, _) => Expr
    | (_, Expr) => Expr
    | (Indet, _) => Indet
    | (_, Indet) => Indet
    | (Value, Value) => Value
    },
    b1 && b2,
  );

  let req_value = (vc, _, d) => (
    d,
    switch (vc(d)) {
    | Value => (Value, true)
    | Indet => (Indet, false)
    | Expr => (Expr, false)
    },
  );
  let req_all_value = (vc, _, ds) =>
    List.fold_right(
      ((v1, r1), (v2, r2)) => ([v1, ...v2], combine(r1, r2)),
      List.map(req_value(vc, x => x), ds),
      ([], (Value, true)),
    );
  let req_final = (vc, _, d) => (
    d,
    switch (vc(d)) {
    | Value => (Value, true)
    | Indet => (Indet, true)
    | Expr => (Expr, false)
    },
  );
  let req_all_final = (vc, _, ds) =>
    List.fold_right(
      ((v1, r1), (v2, r2)) => ([v1, ...v2], combine(r1, r2)),
      List.map(req_final(vc, x => x), ds),
      ([], (Value, true)),
    );

  let req_final_or_value = (vc, _, d) =>
    switch (vc(d)) {
    | Value => ((d, true), (Value, true))
    | Indet => ((d, false), (Value, true))
    | Expr => ((d, false), (Value, false))
    };

  let otherwise = (_, _) => ((), (Value, true));

  let (let.) = ((v, (r, b)), rule) =>
    switch (b, r, rule(v)) {
    | (_, _, Constructor) => r
    | (_, Expr, Indet) => Expr
    | (_, _, Indet) => Indet
    | (true, _, Step(_)) => Expr
    | (false, _, Step(_)) => r
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
