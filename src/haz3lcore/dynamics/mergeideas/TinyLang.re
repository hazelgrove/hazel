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

type location = list(int);

let add_env = (e, k, v) => [(k, v), ...e];
let rec get_env = (e, k) =>
  switch (e) {
  | [(k', v'), ..._] when k == k' => v'
  | [_, ...e'] => get_env(e', k)
  | [] => failwith("Not in environment")
  };

type rule =
  | Step({
      undo: tinylang,
      apply: tinylang,
      final: bool,
    })
  | Constructor(tinylang)
  | Indet(tinylang);

module type EV_MODE = {
  type result;
  type intermediate('a);

  let req_value:
    (int, tinylang => result, tinylang) => intermediate(tinylang);
  let req_finished:
    (int, tinylang => result, tinylang) => intermediate(tinylang);
  let do_not_req:
    (int, tinylang => result, tinylang) => intermediate(tinylang);

  let (let.): (intermediate('a), 'a => rule) => result;
  let (and.):
    (intermediate('a), intermediate('b)) => intermediate(('a, 'b));

  let no_req: rule => result;
};

module Transition = (EV: EV_MODE) => {
  open EV;

  let transition = (continue, env: list((string, tinylang))) =>
    fun
    | Hole => no_req(Indet(Hole))
    | Const(x) => no_req(Constructor(Const(x)))
    | Add(x, y) => {
        let. x' = req_value(0, continue(env), x)
        and. y' = req_value(1, continue(env), y);
        Step({
          undo: Add(x', y'),
          apply:
            switch (x', y') {
            | (Const(a), Const(b)) => Const(a + b)
            | _ => failwith("invalid addition")
            },
          final: true,
        });
      }
    | Seq(x, y) => {
        let. x' = EV.req_finished(0, continue(env), x)
        and. y' = EV.do_not_req(1, continue(env), y);
        Step({undo: Seq(x', y'), apply: y', final: false});
      }
    | Var(x) =>
      EV.no_req(Step({undo: Var(x), apply: get_env(env, x), final: true}))
    | Fun(s, Closure(env', x)) =>
      EV.no_req(Constructor(Fun(s, Closure(env', x))))
    | Fun(s, x) => EV.no_req(Constructor(Fun(s, Closure(env, x))))
    | Closure(env', x) => {
        let. x' = EV.req_value(0, continue(env'), x);
        Step({undo: Closure(env', x), apply: x', final: true});
      }
    | Ap(x, y) => {
        let. x' = EV.req_value(0, continue(env), x)
        and. y' = EV.req_value(1, continue(env), y); // I would love for this to not be required on stepper...
        Step({
          undo: Ap(x', y'),
          apply:
            switch (x') {
            | Ap(Fun(s, Closure(env', d1)), d2) =>
              Closure(add_env(env', s, d2), d1)
            | _ => failwith("Invalid application")
            },
          final: false,
        });
      };
};

module Evaluator: {
  include EV_MODE;
  let unfinished: result => option(tinylang);
  let box_value: tinylang => result;
} = {
  type intermediate('a) = ('a, bool);

  type result =
    | BoxedValue(tinylang)
    | Indet(tinylang)
    | Uneval(tinylang);

  let box_value = x => BoxedValue(x);

  let unfinished =
    fun
    | BoxedValue(_)
    | Indet(_) => None
    | Uneval(x) => Some(x);

  let req_value = (_, f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (x, true)
    | Uneval(x) => (x, false)
    | Indet(x) => (x, false)
    };

  let req_finished = (_, f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (x, true)
    | Uneval(x) => (x, true)
    | Indet(x) => (x, true)
    };

  let do_not_req = (_, _, x) => (x, true);

  let apply_rule =
    fun
    | Step(s) =>
      if (s.final) {
        BoxedValue(s.apply);
      } else {
        Uneval(s.apply);
      }
    | Constructor(v) => BoxedValue(v)
    | Indet(v) => Indet(v);

  let retreat =
    fun
    | Step(s) => Indet(s.undo)
    | Constructor(v) => Indet(v)
    | Indet(v) => Indet(v);

  let (and.) = ((x1, b1), (x2, b2)) => ((x1, x2), b1 && b2);

  let (let.) = ((x, b), rl) =>
    if (b) {
      apply_rule(rl(x));
    } else {
      retreat(rl(x));
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

let rec pausable_eval = (pause_oracle, env, d) =>
  if (pause_oracle(d)) {
    Evaluator.box_value(d);
  } else {
    let u = Eval.transition(pausable_eval(pause_oracle), env, d);
    switch (Evaluator.unfinished(u)) {
    | None => u
    | Some(x) => pausable_eval(pause_oracle, env, x)
    };
  };

/*
 This module takes an expression, and creates a list of expressions that
 it could step to - since execution order doesn't matter.
 */

module Deconstructor: EV_MODE = {
  type intermediate('a) =
    | BoxedValue('a)
    | Indet('a, bool) // This bool specifies whether the indet was required to be evaluated
    | PossibleSteps('a, list((location, tinylang)));

  type result =
    | BoxedValue(tinylang)
    | Indet(tinylang)
    | PossibleSteps(tinylang, list((location, tinylang)));

  let add_loc = i => List.map(((j, x)) => ([i, ...j], x));

  let req_value = (i, f, x): intermediate(tinylang) =>
    switch (f(x)) {
    | BoxedValue(x) => BoxedValue(x)
    | Indet(x) => Indet(x, true)
    | PossibleSteps(x, y) => PossibleSteps(x, add_loc(i, y))
    };

  let req_finished = (i, f, x): intermediate(tinylang) =>
    switch (f(x)) {
    | BoxedValue(x) => BoxedValue(x)
    | Indet(x) => Indet(x, false)
    | PossibleSteps(x, y) => PossibleSteps(x, add_loc(i, y))
    };

  let do_not_req = (i, f, x): intermediate(tinylang) =>
    switch (f(x)) {
    | BoxedValue(x) => BoxedValue(x)
    | Indet(x) => Indet(x, false)
    | PossibleSteps(x, y) => PossibleSteps(x, add_loc(i, y)) // Perhaps this should be true
    };

  let retreat =
    fun
    | Step(s) => s.undo
    | Constructor(v) => v
    | Indet(v) => v;

  let apply_rule =
    fun
    | Step(s) => PossibleSteps(s.undo, [([], s.apply)])
    | Constructor(v) => BoxedValue(v)
    | Indet(v) => Indet(v);

  let no_req = apply_rule;

  let (let.) = (x: intermediate('a), rl): result => {
    switch (x) {
    | BoxedValue(d) => apply_rule(rl(d))
    | Indet(d, false) => apply_rule(rl(d))
    | Indet(d, true) => Indet(retreat(rl(d)))
    | PossibleSteps(x, y) => PossibleSteps(retreat(rl(x)), y)
    };
  };

  let (and.) =
      (u1: intermediate('a), u2: intermediate('b)): intermediate(('a, 'b)) =>
    switch (u1, u2) {
    // If everything is a value, the result is a value
    | (BoxedValue(x), BoxedValue(y)) => BoxedValue((x, y))
    // If anything is indeterminable, the result is indeterminable
    | (Indet(x, b), BoxedValue(y))
    | (BoxedValue(x), Indet(y, b)) => Indet((x, y), b)
    | (Indet(x, b1), Indet(y, b2)) => Indet((x, y), b1 || b2)
    // If anything can be stepped create a list of steps
    | (BoxedValue(x), PossibleSteps(y, z))
    | (Indet(x, _), PossibleSteps(y, z)) => PossibleSteps((x, y), z)
    | (PossibleSteps(x, y), BoxedValue(z))
    | (PossibleSteps(x, y), Indet(z, _)) => PossibleSteps((x, z), y)
    // Combine two list of steps by stepping one or the other
    | (PossibleSteps(x1, y1), PossibleSteps(x2, y2)) =>
      PossibleSteps((x1, x2), y1 @ y2)
    };
};

module Dec = Transition(Deconstructor);
let rec deconstruct = d => {
  Dec.transition(deconstruct, d);
};

module SymbolicDeconstructor: EV_MODE = {
  include Deconstructor;
};
