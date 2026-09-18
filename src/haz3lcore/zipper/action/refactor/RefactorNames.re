/* Pure syntactic name/mention/binding analysis over Exp/Pat/Typ:
 * free variables, mentions, pattern binders, and the generic
 * collect_exp/collect_pat folds everything else is built on. No
 * dependency on the rest of the refactor kit (Language only), so it
 * sits at the bottom of the stack. */
open Language;

let var_pat_name = (p: Pat.t): option(string) =>
  switch (IdTagged.term_of(p)) {
  | Var(name) => Some(name)
  | _ => None
  };

/* binder name through an annotation */
let rec let_head_name = (p: Pat.t): option(string) =>
  switch (IdTagged.term_of(p)) {
  | Var(y) => Some(y)
  | Asc(inner, _) => let_head_name(inner)
  | _ => None
  };

/* the head binder var node: through annotations, and through the
 * f(x)-sugar Ap to the fn name */
let rec head_var_pat = (p: Pat.t): option(Pat.t) =>
  switch (IdTagged.term_of(p)) {
  | Var(_) => Some(p)
  | Asc(inner, _) => head_var_pat(inner)
  | Ap(fv, _) => head_var_pat(fv)
  | _ => None
  };

/* collect over every subnode: gather what `f` yields at each, in
   pre-order. The building block under the family of id/name walks. */
let collect_exp = (f: Exp.t => list('a), e: Exp.t): list('a) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e': Exp.t) => {
          acc := f(e') @ acc^;
          cont(e');
        },
      e,
    );
  acc^;
};
let collect_pat = (f: Pat.t => list('a), p: Pat.t): list('a) => {
  let acc = ref([]);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p': Pat.t) => {
          acc := f(p') @ acc^;
          cont(p');
        },
      p,
    );
  acc^;
};

let pat_var_names = (p: Pat.t): list(string) =>
  p
  |> collect_pat(p' =>
       switch (IdTagged.term_of(p')) {
       | Var(x) => [x]
       | _ => []
       }
     );

let tpat_names = (tp: TPat.t): list(string) =>
  switch (IdTagged.term_of(tp)) {
  | Var(name) => [name]
  | _ => []
  };

/* type names mentioned in any type position of e (ascriptions, pat
 * annotations, tyalias defs) — the capture check for moves across a
 * type binder */
let mentions_typ_names = (names: list(string), e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_typ=
        (cont, t: Typ.t) => {
          switch (IdTagged.term_of(t)) {
          | Var(y) when List.mem(y, names) => found := true
          | _ => ()
          };
          cont(t);
        },
      e,
    );
  found^;
};

/* Does the pattern bind this name? (occurrences under such binders are
 * shadowed and must not be substituted) */
let binds = (x: string, p: Pat.t): bool => {
  let found = ref(false);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p: Pat.t) => {
          switch (IdTagged.term_of(p)) {
          | Var(y) when y == x => found := true
          | _ => ()
          };
          cont(p);
        },
      p,
    );
  found^;
};

let children_of = (e: Exp.t): list(Exp.t) => {
  let acc = ref([]);
  let entered = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e': Exp.t) =>
          if (entered^) {
            acc := [e', ...acc^];
            e';
          } else {
            entered := true;
            cont(e');
          },
      e,
    );
  acc^;
};

/* does x occur FREE in e (an occurrence under a rebinding of x
 * doesn't count)? Movement gates use this rather than raw mentions so
 * shadowed reuses of a name don't block legal moves. */
let rec free_in = (x: string, e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(y) => y == x
  | Let(p, d, body) => free_in(x, d) || !binds(x, p) && free_in(x, body)
  | Fun(p, body, _, _)
  | FixF(p, body, _) => binds(x, p) ? false : free_in(x, body)
  | Match(scrut, rules) =>
    free_in(x, scrut)
    || rules
    |> List.exists(((p, b)) => !binds(x, p) && free_in(x, b))
  | _ => children_of(e) |> List.exists(free_in(x))
  };

let sugar_fn_name = (p: Pat.t): option(string) => {
  let rec go = (p: Pat.t) =>
    switch (IdTagged.term_of(p)) {
    | Ap(fv, _) => var_pat_name(fv)
    | Asc(inner, _) => go(inner)
    | _ => None
    };
  go(p);
};

let vars_of = (e: Exp.t): list(string) =>
  e
  |> collect_exp(e' =>
       switch (IdTagged.term_of(e')) {
       | Var(z) => [z]
       | _ => []
       }
     )
  |> List.sort_uniq(compare);

let typ_names_mentioned = (e: Exp.t): list(string) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_typ=
        (cont, t: Typ.t) => {
          switch (IdTagged.term_of(t)) {
          | Var(y) => acc := [y, ...acc^]
          | _ => ()
          };
          cont(t);
        },
      e,
    );
  List.sort_uniq(compare, acc^);
};

/* capture by a crossed `use` only — the refusal that remains after
   alias-capture was upgraded to freshening (imports can't be
   enumerated, so they can't be renamed around) */
let typ_names_in = (e: Exp.t): list(string) => {
  let acc = ref(typ_names_mentioned(e));
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | TyAlias(tp, _, _)
          | TypFun(tp, _, _) => acc := tpat_names(tp) @ acc^
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  List.sort_uniq(compare, acc^);
};

/* rename the alias BOUND at this TyAlias node to y: the binder
   token morphs in place (ids kept) and its scope's uses follow */

/* unshadowed-use check is conservative: any occurrence counts */
let mentions = (x: string, e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | Var(y) when y == x => found := true
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  found^;
};

/* === Hoist / Sink ===
 * Move a binding up or down ONE level per invocation. These are the
 * explicit opt-in to evaluation-count/conditionality changes that
 * extract deliberately avoids: hoisting out of a fun evaluates once
 * instead of per call; sinking into an arm evaluates only when the
 * arm matches. Gates are conservative name checks (mentions), so a
 * blocked move is simply not offered. */

let names_mentioned = (names: list(string), e: Exp.t): bool =>
  names |> List.exists(n => free_in(n, e));

let disjoint_names = (a: list(string), b: list(string)): bool =>
  !(a |> List.exists(n => List.mem(n, b)));
let var_use_ids = (names: list(string), e: Exp.t): list(Id.t) =>
  e
  |> collect_exp(e' =>
       switch (IdTagged.term_of(e')) {
       | Var(x) when List.mem(x, names) => [Exp.rep_id(e')]
       | _ => []
       }
     );

/* ids of pattern BINDERS of any of `names` (a shadowing rebind is
   the culprit token) */
let pat_binder_ids = (names: list(string), p: Pat.t): list(Id.t) =>
  p
  |> collect_pat(p' =>
       switch (IdTagged.term_of(p')) {
       | Var(x) when List.mem(x, names) => [Pat.rep_id(p')]
       | _ => []
       }
     );

let binder_ids_in = (names: list(string), e: Exp.t): list(Id.t) =>
  e
  |> collect_exp(e' =>
       switch (IdTagged.term_of(e')) {
       | Let(p, _, _)
       | Fun(p, _, _, _) => pat_binder_ids(names, p)
       | Match(_, rules) =>
         rules |> List.concat_map(((rp, _)) => pat_binder_ids(names, rp))
       | _ => []
       }
     );

/* out-channel for lift refusals: each failing wall reports its
   culprit ids; set by lift_site, read by lift_wall_blockers on dead
   presses — same-call-stack use only */
