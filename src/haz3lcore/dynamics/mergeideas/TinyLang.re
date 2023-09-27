type tinylang =
  | Hole
  | Const(int)
  | Add(tinylang, tinylang)
  | Seq(tinylang, tinylang)
  | Var(string)
  | Fun(string, tinylang)
  | Closure(env, tinylang)
  | Ap(tinylang, tinylang)
and env = list((string, tinylang));

let add_env = (e, k, v) => [(k, v), ...e];
let rec get_env = (e, k) =>
  switch (e) {
  | [(k', v'), ..._] when k == k' => v'
  | [_, ...e'] => get_env(e', k)
  | [] => failwith("Not in environment")
  };

type kind =
  | Value
  | Expr;

type rule =
  | Step(unit => tinylang, unit => tinylang, kind)
  | Constructor(unit => tinylang)
  | Indet(unit => tinylang);

module type EV_MODE = {
  type result('a);

  /*
   There are three types of requirement:
   a - must be fully evaluated before the syntactic form it's a part of
   b - ideally fully evaluated before the syntactic form it's a part of
   c - could be evaluated before the syntactic form it's a part of
   */
  let req_a:
    (tinylang => result(tinylang), tinylang) => (result(tinylang), bool);
  let req_b:
    (tinylang => result(tinylang), tinylang) => (result(tinylang), bool);
  let req_c:
    (tinylang => result(tinylang), tinylang) => (result(tinylang), bool);

  let bind: ((result('a), bool), 'a => rule) => result(tinylang);
  let combine:
    ((result('a), bool), (result('b), bool)) => (result(('a, 'b)), bool);
  let no_req: rule => result(tinylang);
};

module Transition = (EV: EV_MODE) => {
  let (let.) = EV.bind;
  let (and.) = EV.combine;

  let transition = (continue, env: list((string, tinylang))) =>
    fun
    | Hole => EV.no_req(Indet(() => Hole))
    | Const(x) => EV.no_req(Constructor(() => Const(x)))
    | Add(x, y) => {
        let. x' = EV.req_a(continue(env), x)
        and. y' = EV.req_a(continue(env), y);
        Step(
          () => Add(x', y'),
          () =>
            switch (x', y') {
            | (Const(a), Const(b)) => Const(a + b)
            | _ => failwith("invalid addition")
            },
          Value,
        );
      }
    | Seq(x, y) => {
        let. x' = EV.req_b(continue(env), x)
        and. y' = EV.req_c(continue(env), y);
        Step(() => Seq(x', y'), () => y', Expr);
      }
    | Var(x) =>
      EV.no_req(
        Step(
          () => Var(x),
          () => get_env(env, x),
          Value // Should be Value on Evaluator :/
        ),
      )
    | Fun(s, Closure(env', x)) =>
      EV.no_req(Constructor(() => Fun(s, Closure(env', x))))
    | Fun(s, x) => EV.no_req(Constructor(() => Fun(s, Closure(env, x))))
    | Closure(env', x) => {
        let. x' = EV.req_a(continue(env), x);
        Step(() => Closure(env', x), () => x', Value);
      }
    | Ap(x, y) => {
        let. x' = EV.req_a(continue(env), x)
        and. y' = EV.req_a(continue(env), y); // I would love for this to be req_b...
        Step(
          () => Ap(x', y'),
          () =>
            switch (x') {
            | Ap(Fun(s, Closure(env', d1)), d2) =>
              Closure(add_env(env', s, d2), d1)
            | _ => failwith("Invalid application")
            },
          Expr,
        );
      };
};

module Evaluator: {
  include EV_MODE;
  let unfinished: result('a) => option('a);
} = {
  type result('a) =
    | BoxedValue('a)
    | Indet('a)
    | Uneval('a);

  let unfinished =
    fun
    | BoxedValue(_)
    | Indet(_) => None
    | Uneval(x) => Some(x);

  let req_a = (f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (BoxedValue(x), true)
    | Uneval(x) => (Uneval(x), false)
    | Indet(x) => (Indet(x), false)
    };

  let req_b = (f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (BoxedValue(x), true)
    | Uneval(x) => (Uneval(x), true)
    | Indet(x) => (Indet(x), true)
    };

  let req_c = (_, x) => (Uneval(x), true);

  let apply_rule =
    fun
    | Step(_, f, Value) => BoxedValue(f())
    | Step(_, f, Expr) => Uneval(f())
    | Constructor(f) => BoxedValue(f())
    | Indet(f) => Indet(f());

  let retreat =
    fun
    | Step(g, _, _) => Indet(g())
    | Constructor(f) => Indet(f())
    | Indet(f) => Indet(f());

  let unbox =
    fun
    | BoxedValue(x) => x
    | Indet(x) => x
    | Uneval(x) => x;

  let combine = ((x1, b1), (x2, b2)) => (
    BoxedValue((unbox(x1), unbox(x2))),
    b1 && b2,
  );

  let bind = ((x, b), rl) => {
    let x = unbox(x);
    if (b) {
      apply_rule(rl(x));
    } else {
      retreat(rl(x));
    };
  };

  let no_req = apply_rule;
};

module Eval = Transition(Evaluator);

let rec evaluate = (env, d) => {
  let u = Eval.transition(evaluate, env, d);
  switch (Evaluator.unfinished(u)) {
  | None => u
  | Some(x) => evaluate(env, x)
  };
};

/*
 This module takes an expression, and creates a list of expressions that
 it could step to - since execution order doesn't matter.
 */

module Deconstructor: EV_MODE = {
  type result('a) =
    | BoxedValue('a)
    | Indet('a)
    | PossibleSteps('a, list('a));

  let req_a = (f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (BoxedValue(x), true)
    | Indet(x) => (Indet(x), false)
    | PossibleSteps(x, y) => (PossibleSteps(x, y), false)
    };

  let req_b = (f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (BoxedValue(x), true)
    | Indet(x) => (Indet(x), true)
    | PossibleSteps(x, y) => (PossibleSteps(x, y), false)
    };

  let req_c = (f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (BoxedValue(x), true)
    | Indet(x) => (Indet(x), true)
    | PossibleSteps(x, y) => (PossibleSteps(x, y), false) // Perhaps this should be true
    };

  let retreat =
    fun
    | Step(g, _, _) => g()
    | Constructor(f) => f()
    | Indet(f) => f();

  let apply_rule =
    fun
    | Step(g, f, _) => PossibleSteps(g(), [f()])
    | Constructor(f) => BoxedValue(f())
    | Indet(f) => Indet(f());

  let no_req = apply_rule;

  let bind = ((x, b), rl) =>
    if (b) {
      switch (x) {
      | BoxedValue(d) => apply_rule(rl(d))
      | Indet(d) => apply_rule(rl(d))
      | PossibleSteps(_) => failwith("[TODO: Refactor Away]")
      };
    } else {
      switch (x) {
      | BoxedValue(_) => failwith("[TODO: Refactor Away]")
      | Indet(u) => Indet(retreat(rl(u)))
      | PossibleSteps(x, y) =>
        PossibleSteps(retreat(rl(x)), List.map(u => retreat(rl(u)), y))
      };
    };

  let map = List.map;

  let combine = ((u1, b1), (u2, b2)) => (
    switch (u1, u2) {
    // If everything is a value, the result is a value
    | (BoxedValue(x), BoxedValue(y)) => BoxedValue((x, y))
    // If anything is indeterminable, the result is indeterminable
    | (Indet(x), BoxedValue(y))
    | (BoxedValue(x), Indet(y))
    | (Indet(x), Indet(y)) => Indet((x, y))
    // If anything can be stepped create a list of steps
    | (BoxedValue(x), PossibleSteps(y, z))
    | (Indet(x), PossibleSteps(y, z)) =>
      PossibleSteps((x, y), map(u => (x, u), z))
    | (PossibleSteps(x, y), BoxedValue(z))
    | (PossibleSteps(x, y), Indet(z)) =>
      PossibleSteps((x, z), map(u => (u, z), y))
    // Combine two list of steps by stepping one or the other
    | (PossibleSteps(x1, y1), PossibleSteps(x2, y2)) =>
      PossibleSteps(
        (x1, x2),
        map(u => (x1, u), y2) @ map(u => (u, x2), y1),
      )
    },
    b1 && b2,
  );
};

module Dec = Transition(Deconstructor);
let rec deconstruct = d => {
  Dec.transition(deconstruct, d);
};
