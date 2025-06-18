module Pat = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Wild
    | Atom(Atom.cls)
    | ListLit
    | Constructor
    | Cons
    | Var
    | Label
    | TupLabel
    | Tuple
    | Parens
    | Probe
    | Ap
    | Cast;

  include TermBase.Pat;

  let rep_id = ({annotation: {ids, _}, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => TermBase.Pat.term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let hole = (tms: list(TermBase.Any.t)): TermBase.Pat.term =>
    switch (tms) {
    | [] => EmptyHole
    | [_, ..._] => MultiHole(tms)
    };

  let cls_of_term: Grammar.pat_term('a) => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | Wild => Wild
    | Atom(c) => Atom(Atom.cls_of_t(c))
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Cons(_) => Cons
    | Var(_) => Var
    | Label(_) => Label
    | TupLabel(_) => TupLabel
    | Tuple(_) => Tuple
    | Parens(_) => Parens
    | Probe(_) => Probe
    | Ap(_) => Ap
    | Cast(_) => Cast;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid pattern"
    | MultiHole => "Broken pattern"
    | EmptyHole => "Empty pattern hole"
    | Wild => "Wildcard"
    | Atom(Int) => "Number literal"
    | Atom(Float) => "Float literal"
    | Atom(Bool) => "Boolean literal"
    | Atom(String) => "String literal"
    | Atom(Nat) => "Natural number literal"
    | Atom(SInt) => "System integer literal"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Cons => "Cons"
    | Var => "Variable binding"
    | Label => "Label"
    | TupLabel => "Labeled Tuple Item"
    | Tuple => "Tuple"
    | Parens => "Parenthesized pattern"
    | Probe => "Probe"
    | Ap => "Constructor application"
    | Cast => "Annotation";

  let rec is_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat)
    | Probe(pat, _)
    | TupLabel(_, pat)
    | Cast(pat, _, _) => is_var(pat)
    | Var(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Atom(_)
    | ListLit(_)
    | Cons(_, _)
    | Tuple(_)
    | Label(_)
    | Constructor(_)
    | Ap(_) => false
    };
  };

  let rec is_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat)
    | Probe(pat, _)
    | TupLabel(_, pat) => is_fun_var(pat)
    | Cast(pat, typ, _) =>
      is_var(pat) && (Typ.is_arrow(typ) || Typ.is_forall(typ))
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Atom(_)
    | ListLit(_)
    | Cons(_, _)
    | Var(_)
    | Label(_)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => false
    };
  };

  let rec is_tuple_of_arrows = (pat: t) =>
    is_fun_var(pat)
    || (
      switch (pat.term) {
      | Parens(pat)
      | Probe(pat, _)
      | TupLabel(_, pat) => is_tuple_of_arrows(pat)
      | Tuple(pats) => pats |> List.for_all(is_fun_var)
      | Label(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Atom(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Cast(_)
      | Constructor(_)
      | Ap(_) => false
      }
    );

  let rec is_tuple_of_vars = (pat: t) =>
    is_var(pat)
    || (
      switch (pat.term) {
      | Parens(pat)
      | Probe(pat, _)
      | Cast(pat, _, _)
      | TupLabel(_, pat) => is_tuple_of_vars(pat)
      | Tuple(pats) => pats |> List.for_all(is_var)
      | Label(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Atom(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Constructor(_)
      | Ap(_) => false
      }
    );

  let rec get_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat)
    | Probe(pat, _)
    | TupLabel(_, pat) => get_var(pat)
    | Var(x) => Some(x)
    | Cast(x, _, _) => get_var(x)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Atom(_)
    | ListLit(_)
    | Cons(_, _)
    | Label(_)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => None
    };
  };

  let rec get_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat)
    | Probe(pat, _)
    | TupLabel(_, pat) => get_fun_var(pat)
    | Cast(pat, t1, _) =>
      if (Typ.is_arrow(t1) || Typ.is_forall(t1)) {
        get_var(pat) |> Option.map(var => var);
      } else {
        None;
      }
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Atom(_)
    | ListLit(_)
    | Cons(_, _)
    | Var(_)
    | Label(_)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => None
    };
  };

  let rec get_bindings = (pat: t) =>
    switch (get_var(pat)) {
    | Some(x) => Some([x])
    | None =>
      switch (pat.term) {
      | Parens(pat)
      | Probe(pat, _)
      | Cast(pat, _, _)
      | TupLabel(_, pat) => get_bindings(pat)
      | Tuple(pats) =>
        let vars = pats |> List.map(get_var);
        if (List.exists(Option.is_none, vars)) {
          None;
        } else {
          Some(List.map(Option.get, vars));
        };
      | Label(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Atom(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Constructor(_)
      | Ap(_) => None
      }
    };

  let rec get_num_of_vars = (pat: t) =>
    if (is_var(pat)) {
      Some(1);
    } else {
      switch (pat.term) {
      | Parens(pat)
      | Probe(pat, _)
      | Cast(pat, _, _)
      | TupLabel(_, pat) => get_num_of_vars(pat)
      | Tuple(pats) =>
        is_tuple_of_vars(pat) ? Some(List.length(pats)) : None
      | Label(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Atom(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Constructor(_)
      | Ap(_) => None
      };
    };

  let ctr_name = (p: t): option(Constructor.t) =>
    switch (p.term) {
    | Constructor(name, _) => Some(name)
    | _ => None
    };

  let rec match_tup_label: t => option((LabeledTuple.label, t)) =
    p =>
      switch (p.term) {
      | Parens(p) => match_tup_label(p)
      | TupLabel(plab, p') =>
        switch (plab.term) {
        | Label(name) => Some((name, p'))
        | _ => None
        }
      | _ => None
      };

  let get_label: t => option(LabeledTuple.label) =
    p => match_tup_label(p) |> Option.map(fst);

  let rec bindings = (dp: t): Binding.s =>
    switch (dp |> term_of) {
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Invalid(_)
    | Atom(_)
    | Label(_)
    | Constructor(_) => []
    | Cast(y, _, _)
    | Parens(y)
    | TupLabel(_, y)
    | Probe(y, _) => bindings(y)
    | Var(name) => [
        {
          name,
          id: rep_id(dp),
        },
      ]
    | Tuple(dps) => List.flatten(List.map(bindings, dps))
    | Cons(dp1, dp2) => bindings(dp1) @ bindings(dp2)
    | ListLit(dps) => List.flatten(List.map(bindings, dps))
    | Ap(_, dp1) => bindings(dp1)
    };

  let bound_vars = (dp: t): list(Var.t) =>
    dp |> bindings |> List.map((b: Binding.t) => b.name);

  let bound_var_ids = (ctx, pat): list(Binding.t) =>
    bound_vars(pat)
    |> List.map(name =>
         switch (Ctx.lookup_var(ctx, name)) {
         | Some({id, _}) =>
           Binding.{
             id,
             name,
           }
         | None => {
             id: Id.invalid,
             name,
           }
         }
       );
};

module Exp = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | DynamicErrorHole
    | FailedCast
    | Deferral
    | Undefined
    | Atom(Atom.cls)
    | DrvExp
    | ListLit
    | Constructor
    | Fun
    | TypFun
    | Label
    | TupLabel
    | Tuple
    | Dot
    | Var
    | Let
    | FixF
    | TyAlias
    | Use
    | Ap
    | TypAp
    | DeferredAp
    | If
    | Seq
    | Test
    | Filter
    | Closure
    | Parens
    | Probe
    | Cons
    | UnOp(Operators.op_un)
    | BinOp(Operators.op_bin)
    | BuiltinFun
    | Match
    | Cast
    | LivelitName
    | LivelitAp
    | ListConcat;

  include TermBase.Exp;

  let temp: term => t =
    term => {
      term,
      annotation: {
        ids: [Id.invalid],
      },
    };
  let fresh: term => t = IdTagged.fresh;

  let hole = (tms: list(TermBase.Any.t)): term =>
    switch (tms) {
    | [] => EmptyHole
    | [_, ..._] => MultiHole(tms)
    };

  let rep_id: t => Id.t = IdTagged.rep_id;
  let term_of: t => term = IdTagged.term_of;
  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let cls_of_term: type a. Grammar.exp_term(a) => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | DynamicErrorHole(_) => DynamicErrorHole
    | FailedCast(_) => FailedCast
    | Deferral(_) => Deferral
    | Undefined => Undefined
    | Atom(c) => Atom(Atom.cls_of_t(c))
    | DrvExp(_) => DrvExp
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Fun(_) => Fun
    | TypFun(_) => TypFun
    | Tuple(_) => Tuple
    | Label(_) => Label
    | TupLabel(_, _) => TupLabel
    | Dot(_) => Dot
    | Var(_) => Var
    | Let(_) => Let
    | FixF(_) => FixF
    | TyAlias(_) => TyAlias
    | Use(_) => Use
    | Ap(_, e1, _) =>
      switch (e1.term) {
      | LivelitName(_) => LivelitAp
      | _ => Ap
      }
    | TypAp(_) => TypAp
    | DeferredAp(_) => DeferredAp
    | If(_) => If
    | Seq(_) => Seq
    | Test(_) => Test
    | Filter(_) => Filter
    | Closure(_) => Closure
    | Parens(_) => Parens
    | Probe(_) => Probe
    | Cons(_) => Cons
    | ListConcat(_) => ListConcat
    | UnOp(op, _) => UnOp(op)
    | BinOp(op, _, _) => BinOp(op)
    | BuiltinFun(_) => BuiltinFun
    | Match(_) => Match
    | LivelitName(_) => LivelitName
    | Cast(_) => Cast;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid expression"
    | MultiHole => "Broken expression"
    | EmptyHole => "Empty expression hole"
    | DynamicErrorHole => "Dynamic error hole"
    | FailedCast => "Failed cast"
    | Deferral => "Deferral"
    | Undefined => "Undefined expression"
    | Atom(Int) => "Number literal"
    | Atom(Float) => "Float literal"
    | Atom(Bool) => "Boolean literal"
    | Atom(String) => "String literal"
    | Atom(Nat) => "Natural number literal"
    | Atom(SInt) => "System integer literal"
    | DrvExp => "Drivation expression"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Fun => "Function literal"
    | TypFun => "Type Function Literal"
    | Tuple => "Tuple literal"
    | Label => "Label"
    | TupLabel => "Labeled Tuple Item"
    | Dot => "Dot operator"
    | Var => "Variable reference"
    | Let => "Let expression"
    | FixF => "Fixpoint operator"
    | TyAlias => "Type Alias definition"
    | Use => "Specify number format to use"
    | Ap => "Application"
    | TypAp => "Type application"
    | DeferredAp => "Partial Application"
    | If => "If expression"
    | Seq => "Sequence expression"
    | Test => "Test"
    | Filter => "Filter"
    | Closure => "Closure"
    | Parens => "Parenthesized expression"
    | Probe => "Probe"
    | Cons => "Cons"
    | ListConcat => "List Concatenation"
    | BinOp(op) => Operators.show_binop(op)
    | UnOp(op) => Operators.show_unop(op)
    | BuiltinFun => "Built-in Function"
    | Match => "Case expression"
    | LivelitName => "Livelit name"
    | LivelitAp => "Livelit application"
    | Cast => "Cast expression";

  let rec match_tup_label: t => option((LabeledTuple.label, t)) = {
    e => {
      switch (e.term) {
      | Parens(e) => match_tup_label(e)
      | TupLabel(elab, e') =>
        switch (elab.term) {
        | Label(name) => Some((name, e'))
        | _ => None
        }
      | Cast(e, _, _) => match_tup_label(e)
      | _ => None
      };
    };
  };

  let get_label: t => option(LabeledTuple.label) = {
    e => match_tup_label(e) |> Option.map(fst);
  };

  // Typfun should be treated as a function here as this is only used to
  // determine when to allow for recursive definitions in a let binding.
  let rec is_fun = (e: t) => {
    switch (e.term) {
    | Parens(e)
    | Probe(e, _) => is_fun(e)
    | Cast(e, _, _) => is_fun(e)
    | TypFun(_)
    | Fun(_)
    | BuiltinFun(_) => true
    | TupLabel(_, e) => is_fun(e)
    | Dot(e1, e2) =>
      let rec check_tuple = (e1: t, e2: t) =>
        switch (e1.term) {
        | Parens(e) => check_tuple(e, e2)
        | Tuple(ts) =>
          switch (e2.term) {
          | Label(name) => LabeledTuple.find_label(match_tup_label, ts, name)
          | _ => None
          }
        | _ => None
        };
      let element: option(t) = check_tuple(e1, e2);
      switch (element) {
      | Some(exp) => is_fun(exp)
      | None => false
      };
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | DynamicErrorHole(_)
    | FailedCast(_)
    | Deferral(_)
    | Undefined
    | Atom(_)
    | DrvExp(_)
    | Label(_)
    | ListLit(_)
    | Tuple(_)
    | Var(_)
    | Let(_)
    | FixF(_)
    | TyAlias(_)
    | Use(_)
    | Ap(_)
    | TypAp(_)
    | DeferredAp(_)
    | If(_)
    | Seq(_)
    | Test(_)
    | Filter(_)
    | Cons(_)
    | ListConcat(_)
    | Closure(_)
    | UnOp(_)
    | BinOp(_)
    | Match(_)
    | LivelitName(_)
    | Constructor(_) => false
    };
  };

  let rec is_tuple_of_functions = (e: t) =>
    is_fun(e)
    || (
      switch (e.term) {
      | Cast(e, _, _)
      | Parens(e)
      | Probe(e, _)
      | TupLabel(_, e) => is_tuple_of_functions(e)
      | Tuple(es) => es |> List.for_all(is_fun)
      | Dot(e1, e2) =>
        let rec check_tuple = (e1: t, e2: t) =>
          switch (e1.term) {
          | Parens(e) => check_tuple(e, e2)
          | Tuple(ts) =>
            switch (e2.term) {
            | Label(name) =>
              LabeledTuple.find_label(match_tup_label, ts, name)
            | _ => None
            }
          | _ => None
          };
        let element: option(t) = check_tuple(e1, e2);
        switch (element) {
        | Some(exp) => is_tuple_of_functions(exp)
        | None => false
        };
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | DynamicErrorHole(_)
      | FailedCast(_)
      | Deferral(_)
      | Undefined
      | Atom(_)
      | DrvExp(_)
      | Label(_)
      | ListLit(_)
      | Fun(_)
      | TypFun(_)
      | Closure(_)
      | BuiltinFun(_)
      | Var(_)
      | Let(_)
      | FixF(_)
      | TyAlias(_)
      | Use(_)
      | Ap(_)
      | TypAp(_)
      | DeferredAp(_)
      | If(_)
      | Seq(_)
      | Test(_)
      | Filter(_)
      | Cons(_)
      | ListConcat(_)
      | UnOp(_)
      | BinOp(_)
      | Match(_)
      | LivelitName(_)
      | Constructor(_) => false
      }
    );

  let ctr_name = (e: t): option(Constructor.t) =>
    switch (e.term) {
    | Constructor(name, _) => Some(name)
    | _ => None
    };

  let is_deferral = (e: t) => {
    switch (e.term) {
    | Deferral(_) => true
    | _ => false
    };
  };

  let rec get_num_of_functions = (e: t) =>
    if (is_fun(e)) {
      Some(1);
    } else {
      switch (e.term) {
      | Parens(e)
      | Probe(e, _)
      | TupLabel(_, e)
      | Dot(e, _) => get_num_of_functions(e)
      | Tuple(es) => is_tuple_of_functions(e) ? Some(List.length(es)) : None
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | DynamicErrorHole(_)
      | FailedCast(_)
      | FixF(_)
      | Closure(_)
      | BuiltinFun(_)
      | Cast(_)
      | Deferral(_)
      | Undefined
      | Atom(_)
      | DrvExp(_)
      | Label(_)
      | ListLit(_)
      | Fun(_)
      | TypFun(_)
      | Var(_)
      | Let(_)
      | Filter(_)
      | TyAlias(_)
      | Use(_)
      | Ap(_)
      | TypAp(_)
      | DeferredAp(_)
      | If(_)
      | Seq(_)
      | Test(_)
      | Cons(_)
      | ListConcat(_)
      | UnOp(_)
      | BinOp(_)
      | Match(_)
      | LivelitName(_)
      | Constructor(_) => None
      };
    };

  let (replace_all_ids, replace_all_ids_typ) = {
    let f:
      'a.
      (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
     =
      (continue, exp) =>
        {
          ...exp,
          annotation: {
            ids: [Id.mk()],
          },
        }
        |> continue;
    (
      map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
      Typ.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
    );
  };

  let rec substitute_closures =
          (
            env: Environment.t,
            old_bound_vars: list(string),
            new_bound_vars: list(string),
          ) =>
    map_term(
      ~f_exp=
        (cont, e) => {
          let (term, rewrap) = unwrap(e);
          switch (term) {
          // Variables: lookup if bound
          | Var(x) =>
            switch (Environment.lookup(env, x)) {
            | Some(e) =>
              e
              |> replace_all_ids
              |> substitute_closures(env, old_bound_vars, new_bound_vars)
            | None =>
              Var(
                List.mem(x, old_bound_vars)
                  ? x : Var.free_name(x, new_bound_vars),
              )
              |> rewrap
            }
          // Forms with environments: look up in new environment
          | Closure(env, e) =>
            substitute_closures(
              env |> ClosureEnvironment.map_of,
              [],
              new_bound_vars,
              e,
            )
          | Fun(p, e, t, n) =>
            let pat_bound_vars = Pat.bound_vars(p);
            Fun(
              p,
              substitute_closures(
                env |> Environment.without_keys(pat_bound_vars),
                pat_bound_vars @ old_bound_vars,
                pat_bound_vars @ new_bound_vars,
                e,
              ),
              t,
              n,
            )
            |> rewrap;
          | FixF(p, e, Some(env)) =>
            let pat_bound_vars = Pat.bound_vars(p);
            FixF(
              p,
              substitute_closures(
                env
                |> ClosureEnvironment.map_of
                |> Environment.without_keys(pat_bound_vars),
                pat_bound_vars @ old_bound_vars,
                pat_bound_vars @ new_bound_vars,
                e,
              ),
              None,
            )
            |> rewrap;
          // Cases with binders: remove binder from env
          | Let(p, e1, e2) =>
            let pat_bound_vars = Pat.bound_vars(p);
            Let(
              p,
              substitute_closures(env, old_bound_vars, new_bound_vars, e1),
              substitute_closures(
                env |> Environment.without_keys(pat_bound_vars),
                pat_bound_vars @ old_bound_vars,
                pat_bound_vars @ new_bound_vars,
                e2,
              ),
            )
            |> rewrap;
          | Match(e, cases) =>
            Match(
              substitute_closures(env, old_bound_vars, new_bound_vars, e),
              cases
              |> List.map(((p, e)) => {
                   let pat_bound_vars = Pat.bound_vars(p);
                   (
                     p,
                     substitute_closures(
                       env |> Environment.without_keys(pat_bound_vars),
                       pat_bound_vars @ old_bound_vars,
                       pat_bound_vars @ new_bound_vars,
                       e,
                     ),
                   );
                 }),
            )
            |> rewrap
          | FixF(p, e, None) =>
            let pat_bound_vars = Pat.bound_vars(p);
            FixF(
              p,
              substitute_closures(
                env |> Environment.without_keys(pat_bound_vars),
                pat_bound_vars @ old_bound_vars,
                pat_bound_vars @ new_bound_vars,
                e,
              ),
              None,
            )
            |> rewrap;
          // Other cases: recurse
          | Invalid(_)
          | EmptyHole
          | MultiHole(_)
          | DynamicErrorHole(_)
          | FailedCast(_)
          | Deferral(_)
          | Atom(_)
          | DrvExp(_)
          | ListLit(_)
          | Constructor(_)
          | TypFun(_)
          | Tuple(_)
          | TupLabel(_)
          | Label(_)
          | Dot(_)
          | TyAlias(_)
          | Use(_)
          | Ap(_)
          | TypAp(_)
          | DeferredAp(_)
          | If(_)
          | Seq(_)
          | Test(_)
          | Filter(_)
          | Parens(_)
          | Probe(_)
          | Cons(_)
          | ListConcat(_)
          | UnOp(_)
          | BinOp(_)
          | BuiltinFun(_)
          | Cast(_)
          | LivelitName(_)
          | Undefined => cont(e)
          };
        },
      _,
    );
  let substitute_closures = substitute_closures(_, [], []);

  let unfix = (e: t, p: Pat.t) => {
    switch (e.term) {
    | FixF(p1, e1, _) =>
      if (Pat.fast_equal(p, p1)) {
        e1;
      } else {
        e;
      }
    | _ => e
    };
  };

  let rec get_fn_name = (e: t) => {
    switch (e.term) {
    | Fun(_, _, _, n) => n
    | FixF(_, e, _) => get_fn_name(e)
    | Parens(e)
    | Probe(e, _) => get_fn_name(e)
    | TypFun(_, _, n) => n
    | _ => None
    };
  };
};

module Rul = {
  include TermBase.Rul;

  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type cls =
    | Rule;

  // example of awkwardness induced by having forms like rules
  // that may have a different-sorted child with no delimiters
  // (eg scrut with no rules)
  let ids = (~any_ids, {term, annotation: {ids, _}}: t) =>
    switch (ids) {
    | [_, ..._] => ids
    | [] =>
      switch (term) {
      | Hole([tm, ..._]) => any_ids(tm)
      | Rules(scrut, []) => IdTagged.ids(scrut)
      | _ => []
      }
    };

  let rep_id = (~any_ids, tm) =>
    switch (ids(~any_ids, tm)) {
    | [] => raise(Invalid_argument("Exp.rep_id"))
    | [id, ..._] => id
    };
};

module Any = {
  include TermBase.Any;

  let is_exp: t => option(TermBase.Exp.t) =
    fun
    | Exp(e) => Some(e)
    | _ => None;
  let is_pat: t => option(TermBase.Pat.t) =
    fun
    | Pat(p) => Some(p)
    | _ => None;
  let is_typ: t => option(TermBase.Typ.t) =
    fun
    | Typ(t) => Some(t)
    | _ => None;
  let is_drv_exp: t => option(DrvTermBase.Exp.t) =
    fun
    | Drv(Exp(e)) => Some(e)
    | _ => None;

  let drv_hole = (tms: list(TermBase.Any.t)): DrvTermBase.type_hole =>
    tms
    |> List.filter_map(
         fun
         | Grammar.Drv(exp) => Some(exp)
         | _ => None,
       )
    |> (
      fun
      | [] => Grammar.Drv.EmptyHole
      | tms => Grammar.Drv.MultiHole(tms)
    );

  let rec ids: TermBase.any_t => list(Id.t) =
    fun
    | Exp(tm) => IdTagged.ids(tm)
    | Pat(tm) => IdTagged.ids(tm)
    | Typ(tm) => IdTagged.ids(tm)
    | TPat(tm) => IdTagged.ids(tm)
    | Rul(tm) => Rul.ids(~any_ids=ids, tm)
    | Drv(tm) => Drv.Any.ids(tm)
    | Any () => [];

  // Terms may consist of multiple tiles, eg the commas in an n-tuple,
  // the rules of a case expression + the surrounding case-end tile,
  // the list brackets tile coupled with the elem-separating commas.
  // The _representative id_ is the canonical tile id used to identify
  // and look up info about a term.
  //
  // In instances like case expressions and list literals, where a parent
  // tile surrounds the other tiles, the representative id is the parent tile's.
  // In other instances like n-tuples, where the commas are all siblings,
  // the representative id is one of the comma ids, unspecified which one.
  // (This would change for n-tuples if we decided parentheses are necessary.)
  let rep_id =
    fun
    | (Exp(tm): TermBase.any_t) => Exp.rep_id(tm)
    | Pat(tm) => Pat.rep_id(tm)
    | Typ(tm) => Typ.rep_id(tm)
    | TPat(tm) => TPat.rep_id(tm)
    | Rul(tm) => Rul.rep_id(~any_ids=ids, tm)
    | Drv(tm) => Drv.Any.rep_id(tm)
    | Any () => raise(Invalid_argument("Term.rep_id"));
};
