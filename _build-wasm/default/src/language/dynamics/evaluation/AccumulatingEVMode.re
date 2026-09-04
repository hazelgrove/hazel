open Transition;

/* EV_MODE instance for analysis passes that walk an expression's transition
 * requirements accumulating a monoid, without actually evaluating. Used by
 * ReusePass (accumulating reusable cache entries) and StreamCollector
 * (accumulating evaluator state from streamed entries). */

module type ACC = {
  type t;
  let empty: t;
  let combine: (t, t) => t;
};

module Make =
       (M: ACC)

         : (
           EV_MODE with
             type inner_result = (M.t, rule) and type result = (M.t, rule)
       ) => {
  type result = (M.t, rule);
  type inner_result = result;
  type requirement('a) = (M.t, 'a);
  type requirements('a, 'b) = (M.t, 'a, 'b);

  let req_final = (f, _, x) => {
    let (acc, _) = f(x);
    (acc, x);
  };

  let rec req_all_final = (f, i, xs) =>
    switch (xs) {
    | [] => (M.empty, [])
    | [x, ...xs] =>
      let (acc, x) = req_final(f, x => x, x);
      let (accs, xs) = req_all_final(f, i, xs);
      (M.combine(acc, accs), [x, ...xs]);
    };

  let otherwise = (_, c) => (M.empty, (), c);

  let (and.) = ((acc1, x1, c1), (acc2, x2)) => (
    M.combine(acc1, acc2),
    (x1, x2),
    c1(x2),
  );

  let (let.) = ((acc, x, _), s) => (acc, s(x));
};
