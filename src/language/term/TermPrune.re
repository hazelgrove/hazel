/* Budget-pruning of runtime VALUES for shipping and display: a
   program's value can be a giant shared graph (a module's value
   embeds every member AST), and every tree walk on the main thread —
   marshal decode, display segment build, result statics — pays for
   it. Values under the budget pass through UNTOUCHED (physically the
   same term). Over-budget values prune STRUCTURE-AWARE: tuples and
   lists keep complete leading elements while the budget lasts,
   dropped tails are marked by ONE trailing hole, and an over-budget
   non-structural subtree becomes a single hole. Holes are the
   type-safe elision (the display statics run on this term; any other
   marker would light up error decorations). */

/* node count if within [budget], None otherwise (bail early: stops
   descending once the count is exceeded) */
let size_within = (budget: int, e: Exp.t): option(int) => {
  let count = ref(0);
  let f = (cont, x: Exp.t) => {
    incr(count);
    count^ > budget ? x : cont(x);
  };
  switch (Exp.map_term(~f_exp=f, e)) {
  | _ => count^ <= budget ? Some(count^) : None
  | exception _ => None
  };
};

let hole = (): Exp.t => Exp.fresh(EmptyHole);

/* returns (pruned, truncated) */
let prune = (~budget: int, e: Exp.t): (Exp.t, bool) => {
  let budget = ref(budget);
  let truncated = ref(false);
  let rec go = (e: Exp.t): Exp.t =>
    if (budget^ <= 0) {
      truncated := true;
      hole();
    } else {
      switch (size_within(budget^, e)) {
      | Some(n) =>
        /* fits whole: keep the original object (sharing preserved) */
        budget := budget^ - n;
        e;
      | None =>
        truncated := true;
        let seq = (elems: list(Exp.t)): (list(Exp.t), bool) => {
          let kept = ref([]);
          let dropped = ref(false);
          List.iter(
            el =>
              if (budget^ <= 0) {
                dropped := true;
              } else {
                kept := [go(el), ...kept^];
              },
            elems,
          );
          (List.rev(dropped^ ? [hole(), ...kept^] : kept^), dropped^);
        };
        let re = (term: Exp.term): Exp.t => {
          ...e,
          term,
        };
        switch (e.term) {
        | Tuple(fields) =>
          let (fields, _) = seq(fields);
          re(Tuple(fields));
        | ListLit(items) =>
          let (items, _) = seq(items);
          re(ListLit(items));
        | TupLabel(l, x) =>
          budget := budget^ - 2;
          re(TupLabel(l, go(x)));
        | Parens(x) =>
          budget := budget^ - 1;
          re(Parens(go(x)));
        | _ =>
          /* non-structural over-budget subtree: one clean hole */
          hole()
        };
      };
    };
  let pruned = go(e);
  (pruned, truncated^);
};

/* strip closure ENVIRONMENTS: they are display-opaque (never printed;
   the stepper re-evaluates from the elab) but reference most of the
   program's runtime state. The env is replaced BEFORE the recursive
   descent, so the walk never enters the shared, program-sized
   environment structures. */
let prune_closure_envs = (e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) =>
        switch (e.term) {
        | Closure(_, body) =>
          cont({
            ...e,
            term: Closure(Environment.empty, body),
          })
        | _ => cont(e)
        },
    e,
  );
