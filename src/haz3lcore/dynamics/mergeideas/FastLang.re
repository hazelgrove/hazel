type tinylang =
  | Hole
  | Const(int)
  | Bool(bool)
  | Add(tinylang, tinylang)
  | Leq(tinylang, tinylang)
  | Seq(tinylang, tinylang)
  | Var(string)
  | Fun(string, tinylang)
  | Closure(env, tinylang)
  | Ap(tinylang, tinylang)
  | If(tinylang, tinylang, tinylang)
  | Fix(tinylang)
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

type result('a) =
  | BoxedValue('a)
  | Indet('a);

let rec evaluate: (list((string, tinylang)), tinylang) => result(tinylang) =
  (env: list((string, tinylang))) =>
    fun
    | Hole => Indet(Hole)
    | Const(x) => BoxedValue(Const(x))
    | Bool(b) => BoxedValue(Bool(b))
    | Add(x, y) => {
        let x' = evaluate(env, x);
        let y' = evaluate(env, y);
        switch (x', y') {
        | (BoxedValue(Const(a)), BoxedValue(Const(b))) =>
          BoxedValue(Const(a + b))
        | (BoxedValue(_), BoxedValue(_)) => failwith("invalid addition")
        | (Indet(x), BoxedValue(y))
        | (BoxedValue(x), Indet(y))
        | (Indet(x), Indet(y)) => Indet(Add(x, y))
        };
      }
    | Leq(x, y) => {
        let x' = evaluate(env, x);
        let y' = evaluate(env, y);
        switch (x', y') {
        | (BoxedValue(Const(a)), BoxedValue(Const(b))) =>
          BoxedValue(Bool(a < b))
        | (BoxedValue(_), BoxedValue(_)) => failwith("invalid addition")
        | (Indet(x), BoxedValue(y))
        | (BoxedValue(x), Indet(y))
        | (Indet(x), Indet(y)) => Indet(Leq(x, y))
        };
      }
    | Seq(x, y) => {
        let _ = evaluate(env, x);
        evaluate(env, y);
      }
    | Var(x) => BoxedValue(get_env(env, x))
    | Fun(_, Closure(_, _)) as u => BoxedValue(u)
    | Fun(s, x) => BoxedValue(Fun(s, Closure(env, x)))
    | Fix(Fun(f, Closure(x, y))) =>
      BoxedValue(Fix(Fun(f, Closure(x, y))))
    | Fix(Fun(f, x)) => BoxedValue(Fix(Fun(f, Closure(env, x))))
    | Fix(_) => failwith("invalid fixpoint")
    | Closure(env', x) => evaluate(env', x)
    | Ap(x, y) => {
        let x' = evaluate(env, x);
        let y' = evaluate(env, y);
        switch (x', y') {
        | (BoxedValue(Fun(s, Closure(env', x))), BoxedValue(u)) =>
          evaluate(add_env(env', s, u), x)
        | (
            BoxedValue(Fix(Fun(f, Closure(env', Fun(x, y)))) as v),
            BoxedValue(u),
          ) =>
          evaluate(add_env(add_env(env', x, u), f, v), y)
        | (BoxedValue(_), BoxedValue(_)) => failwith("invalid application")
        | (Indet(x), BoxedValue(y))
        | (BoxedValue(x), Indet(y))
        | (Indet(x), Indet(y)) => Indet(Ap(x, y))
        };
      }
    | If(c, x, y) => {
        switch (evaluate(env, c)) {
        | Indet(c') => Indet(If(c', x, y))
        | BoxedValue(Bool(true)) => evaluate(env, x)
        | BoxedValue(Bool(false)) => evaluate(env, y)
        | BoxedValue(_) => failwith("invalid boolean")
        };
      };

let ack =
  Fix(
    Fun(
      "f",
      Fun(
        "m",
        Fun(
          "n",
          If(
            Leq(Var("m"), Const(0)),
            Add(Var("n"), Const(1)),
            If(
              Leq(Var("n"), Const(0)),
              Ap(Ap(Var("f"), Add(Var("m"), Const(-1))), Const(1)),
              Ap(
                Ap(Var("f"), Var("m")),
                Ap(Ap(Var("f"), Add(Var("m"), Const(1))), Var("n")),
              ),
            ),
          ),
        ),
      ),
    ),
  );
