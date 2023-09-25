type tinylang =
  | Hole
  | Const(int)
  | Add(tinylang, tinylang)
  | Seq(tinylang, tinylang);

type kind =
  | Value
  | Expr;

type rule =
  | Step(unit => tinylang, kind, unit => tinylang)
  | Constructor(unit => tinylang)
  | Indet(unit => tinylang);

module type EV_MODE = {
  type t('a);
  type r;

  let req_a: (tinylang => r, tinylang) => t(tinylang);
  let req_b: (tinylang => r, tinylang) => t(tinylang);
  let req_c: (tinylang => r, tinylang) => t(tinylang);

  let bind: (t('a), 'a => rule) => r;
  let combine: (t('a), t('b)) => t(('a, 'b));
  let no_req: rule => r;
};

module Transition = (EV: EV_MODE) => {
  let (let.) = EV.bind;
  let (and.) = EV.combine;

  let transition = continue =>
    fun
    | Hole => EV.no_req(Indet(() => Hole))
    | Const(x) => EV.no_req(Constructor(() => Const(x)))
    | Add(x, y) => {
        let. x' = EV.req_a(continue, x)
        and. y' = EV.req_a(continue, y);
        Step(
          () =>
            switch (x', y') {
            | (Const(a), Const(b)) => Const(a + b)
            | _ => failwith("invalid addition")
            },
          Value,
          () => Add(x', y'),
        );
      }
    | Seq(x, y) => {
        let. x' = EV.req_b(continue, x)
        and. y' = EV.req_c(continue, y);
        Step(() => y', Expr, () => Seq(x', y'));
      };
};

module Evaluator: EV_MODE = {
  type r =
    | BoxedValue(tinylang)
    | Indet(tinylang)
    | Uneval(tinylang);

  type t('a) = ('a, bool);

  let req_a = (f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (x, true)
    | Uneval(x) => (x, false)
    | Indet(x) => (x, false)
    };

  let req_b = (f, x) =>
    switch (f(x)) {
    | BoxedValue(x) => (x, true)
    | Uneval(x) => (x, true)
    | Indet(x) => (x, true)
    };

  let req_c = (_, x) => (x, true);

  let apply_rule =
    fun
    | Step(f, Value, _) => BoxedValue(f())
    | Step(f, Expr, _) => Uneval(f())
    | Constructor(f) => BoxedValue(f())
    | Indet(f) => Indet(f());

  let retreat =
    fun
    | Step(_, _, g) => Indet(g())
    | Constructor(f) => Indet(f())
    | Indet(f) => Indet(f());

  let combine = ((x1, b1), (x2, b2)) => ((x1, x2), b1 && b2);
  let bind = ((x, b), rl) =>
    if (b) {
      apply_rule(rl(x));
    } else {
      retreat(rl(x));
    };

  let no_req = apply_rule;
};
