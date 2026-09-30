/* budget-pruning of runtime values for shipping and display: a value
   can be a giant shared graph (a module value embeds every member AST).
   within-budget values come back physically intact; elisions are holes
   because display statics run on the result, and any other marker
   would show as an error */

/* node count if within [budget], else None; stops descending once over */
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

/* closure environments are never displayed (the stepper re-evaluates
   from the elab) but reference most runtime state; replacing them
   before descending keeps the walk out of them */
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
