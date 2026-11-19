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

let rec check_value = (~in_closure=?, env, d) =>
  CV.transition(check_value, ~mode=`Environment, ~in_closure?, env, d);

/* (^report(51)) Plain data: literals, labels and constructors, and lists,
   tuples, labelled entries and constructor applications of plain data. By
   the step rules above each is a Value exactly when its parts are (Atom,
   Label and Constructor are final; ListLit, Tuple, TupLabel and a
   constructor's Ap combine their parts' answers), so plain data is a Value
   with no step rules run. A probe's sample came across as data
   (FirstOrder) and is plain data; checked by the step rules, the Witch's
   picture took 9 to 31 ms at each answer (run 42761046647). */
let rec is_data = (exp: Exp.t): bool =>
  switch (exp.term) {
  | Atom(_)
  | Label(_)
  | Constructor(_) => true
  | ListLit(xs)
  | Tuple(xs) => List.for_all(is_data, xs)
  | TupLabel(_, v) => is_data(v)
  | Ap(Forward, {term: Constructor(_), _}, arg) => is_data(arg)
  | _ => false
  };

/* Check if an expression is a fully-evaluated value */
let is_value = (exp: Exp.t): bool =>
  is_data(exp) || check_value(Environment.empty, exp) == Value;
