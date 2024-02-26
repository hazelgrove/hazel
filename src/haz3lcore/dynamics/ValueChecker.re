open DHExp;
open Transition;
open Util;

type t =
  | Value
  | Indet
  | Expr;

module ValueCheckerEVMode: {
  include EV_MODE with type result = t and type state = Statics.Map.t;
} = {
  type state = Statics.Map.t;
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
  let get_info_map = (info_map: state) => info_map;
};

module CV = Transition(ValueCheckerEVMode);

let rec check_value = (state, env, d) =>
  CV.transition(check_value, state, env, d);

let rec check_value_mod_ctx = (info_map: Statics.Map.t, env, d) =>
  switch (DHExp.term_of(d)) {
  | Var(x) =>
    check_value_mod_ctx(
      info_map,
      env,
      ClosureEnvironment.lookup(env, x)
      |> OptUtil.get(() => {
           print_endline("FreeInvalidVar:" ++ x);
           raise(EvaluatorError.Exception(FreeInvalidVar(x)));
         }),
    )
  | _ => CV.transition(check_value_mod_ctx, info_map, env, d)
  };
