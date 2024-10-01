module Pat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Wild
    | Int
    | Float
    | Bool
    | String
    | ListLit
    | Constructor
    | Cons
    | Var
    | Tuple
    | Parens
    | Ap
    | Cast;

  include TermBase.Pat;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => TermBase.Pat.term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let hole = (tms: list(TermBase.Any.t)) =>
    switch (tms) {
    | [] => EmptyHole
    | [_, ..._] => MultiHole(tms)
    };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | Wild => Wild
    | Int(_) => Int
    | Float(_) => Float
    | Bool(_) => Bool
    | String(_) => String
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Cons(_) => Cons
    | Var(_) => Var
    | Tuple(_) => Tuple
    | Parens(_) => Parens
    | Ap(_) => Ap
    | Cast(_) => Cast;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid pattern"
    | MultiHole => "Broken pattern"
    | EmptyHole => "Empty pattern hole"
    | Wild => "Wildcard"
    | Int => "Integer literal"
    | Float => "Float literal"
    | Bool => "Boolean literal"
    | String => "String literal"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Cons => "Cons"
    | Var => "Variable binding"
    | Tuple => "Tuple"
    | Parens => "Parenthesized pattern"
    | Ap => "Constructor application"
    | Cast => "Annotation";

  let rec is_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat)
    | Cast(pat, _, _) => is_var(pat)
    | Var(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | ListLit(_)
    | Cons(_, _)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => false
    };
  };

  let rec is_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => is_fun_var(pat)
    | Cast(pat, typ, _) =>
      is_var(pat) && (UTyp.is_arrow(typ) || Typ.is_forall(typ))
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | ListLit(_)
    | Cons(_, _)
    | Var(_)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => false
    };
  };

  let rec is_tuple_of_arrows = (pat: t) =>
    is_fun_var(pat)
    || (
      switch (pat.term) {
      | Parens(pat) => is_tuple_of_arrows(pat)
      | Tuple(pats) => pats |> List.for_all(is_fun_var)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
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
      | Cast(pat, _, _) => is_tuple_of_vars(pat)
      | Tuple(pats) => pats |> List.for_all(is_var)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Constructor(_)
      | Ap(_) => false
      }
    );

  let rec get_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_var(pat)
    | Var(x) => Some(x)
    | Cast(x, _, _) => get_var(x)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | ListLit(_)
    | Cons(_, _)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => None
    };
  };

  let rec get_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_fun_var(pat)
    | Cast(pat, t1, _) =>
      if (Typ.is_arrow(t1) || UTyp.is_forall(t1)) {
        get_var(pat) |> Option.map(var => var);
      } else {
        None;
      }
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | ListLit(_)
    | Cons(_, _)
    | Var(_)
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
      | Cast(pat, _, _) => get_bindings(pat)
      | Tuple(pats) =>
        let vars = pats |> List.map(get_var);
        if (List.exists(Option.is_none, vars)) {
          None;
        } else {
          Some(List.map(Option.get, vars));
        };
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
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
      | Cast(pat, _, _) => get_num_of_vars(pat)
      | Tuple(pats) =>
        is_tuple_of_vars(pat) ? Some(List.length(pats)) : None
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
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

  let rec bound_vars = (dp: t): list(Var.t) =>
    switch (dp |> term_of) {
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Invalid(_)
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | Constructor(_) => []
    | Cast(y, _, _)
    | Parens(y) => bound_vars(y)
    | Var(y) => [y]
    | Tuple(dps) => List.flatten(List.map(bound_vars, dps))
    | Cons(dp1, dp2) => bound_vars(dp1) @ bound_vars(dp2)
    | ListLit(dps) => List.flatten(List.map(bound_vars, dps))
    | Ap(_, dp1) => bound_vars(dp1)
    };
};

module Exp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | StaticErrorHole
    | DynamicErrorHole
    | FailedCast
    | Deferral
    | Undefined
    | Bool
    | Int
    | Float
    | String
    | ListLit
    | Constructor
    | Fun
    | TypFun
    | Tuple
    | Var
    | MetaVar
    | Let
    | FixF
    | TyAlias
    | Ap
    | TypAp
    | DeferredAp
    | Pipeline
    | If
    | Seq
    | Test
    | Filter
    | Closure
    | Parens
    | Cons
    | UnOp(Operators.op_un)
    | BinOp(Operators.op_bin)
    | BuiltinFun
    | Match
    | Cast
    | ListConcat;

  include TermBase.Exp;

  let hole = (tms: list(TermBase.Any.t)): term =>
    switch (tms) {
    | [] => EmptyHole
    | [_, ..._] => MultiHole(tms)
    };

  let rep_id: t => Id.t = IdTagged.rep_id;
  let fresh: term => t = IdTagged.fresh;
  let term_of: t => term = IdTagged.term_of;
  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | DynamicErrorHole(_) => DynamicErrorHole
    | FailedCast(_) => FailedCast
    | Deferral(_) => Deferral
    | Undefined => Undefined
    | Bool(_) => Bool
    | Int(_) => Int
    | Float(_) => Float
    | String(_) => String
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Fun(_) => Fun
    | TypFun(_) => TypFun
    | Tuple(_) => Tuple
    | Var(_) => Var
    | Let(_) => Let
    | FixF(_) => FixF
    | TyAlias(_) => TyAlias
    | Ap(_) => Ap
    | TypAp(_) => TypAp
    | DeferredAp(_) => DeferredAp
    | If(_) => If
    | Seq(_) => Seq
    | Test(_) => Test
    | Filter(_) => Filter
    | Closure(_) => Closure
    | Parens(_) => Parens
    | Cons(_) => Cons
    | ListConcat(_) => ListConcat
    | UnOp(op, _) => UnOp(op)
    | BinOp(op, _, _) => BinOp(op)
    | BuiltinFun(_) => BuiltinFun
    | Match(_) => Match
    | Cast(_) => Cast;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid expression"
    | MultiHole => "Broken expression"
    | EmptyHole => "Empty expression hole"
    | StaticErrorHole => "Static error hole"
    | DynamicErrorHole => "Dynamic error hole"
    | FailedCast => "Failed cast"
    | Deferral => "Deferral"
    | Undefined => "Undefined expression"
    | Bool => "Boolean literal"
    | Int => "Integer literal"
    | Float => "Float literal"
    | String => "String literal"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Fun => "Function literal"
    | TypFun => "Type Function Literal"
    | Tuple => "Tuple literal"
    | Var => "Variable reference"
    | MetaVar => "Meta variable reference"
    | Let => "Let expression"
    | FixF => "Fixpoint operator"
    | TyAlias => "Type Alias definition"
    | Ap => "Application"
    | TypAp => "Type application"
    | DeferredAp => "Partial Application"
    | Pipeline => "Pipeline expression"
    | If => "If expression"
    | Seq => "Sequence expression"
    | Test => "Test"
    | Filter => "Filter"
    | Closure => "Closure"
    | Parens => "Parenthesized expression"
    | Cons => "Cons"
    | ListConcat => "List Concatenation"
    | BinOp(op) => Operators.show_binop(op)
    | UnOp(op) => Operators.show_unop(op)
    | BuiltinFun => "Built-in Function"
    | Match => "Case expression"
    | Cast => "Cast expression";

  // Typfun should be treated as a function here as this is only used to
  // determine when to allow for recursive definitions in a let binding.
  let rec is_fun = (e: t) => {
    switch (e.term) {
    | Parens(e) => is_fun(e)
    | Cast(e, _, _) => is_fun(e)
    | TypFun(_)
    | Fun(_)
    | BuiltinFun(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | DynamicErrorHole(_)
    | FailedCast(_)
    | Deferral(_)
    | Undefined
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | ListLit(_)
    | Tuple(_)
    | Var(_)
    | Let(_)
    | FixF(_)
    | TyAlias(_)
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
    | Constructor(_) => false
    };
  };

  let rec is_tuple_of_functions = (e: t) =>
    is_fun(e)
    || (
      switch (e.term) {
      | Cast(e, _, _)
      | Parens(e) => is_tuple_of_functions(e)
      | Tuple(es) => es |> List.for_all(is_fun)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | DynamicErrorHole(_)
      | FailedCast(_)
      | Deferral(_)
      | Undefined
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | ListLit(_)
      | Fun(_)
      | TypFun(_)
      | Closure(_)
      | BuiltinFun(_)
      | Var(_)
      | Let(_)
      | FixF(_)
      | TyAlias(_)
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
      | Parens(e) => get_num_of_functions(e)
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
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | ListLit(_)
      | Fun(_)
      | TypFun(_)
      | Var(_)
      | Let(_)
      | Filter(_)
      | TyAlias(_)
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
      | Constructor(_) => None
      };
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
              e |> substitute_closures(env, old_bound_vars, new_bound_vars)
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
          | Fun(p, e, Some(env), n) =>
            let pat_bound_vars = Pat.bound_vars(p);
            Fun(
              p,
              substitute_closures(
                env
                |> ClosureEnvironment.map_of
                |> Environment.without_keys(pat_bound_vars),
                pat_bound_vars,
                pat_bound_vars @ new_bound_vars,
                e,
              ),
              None,
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
                pat_bound_vars,
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
          | Fun(p, e, None, n) =>
            let pat_bound_vars = Pat.bound_vars(p);
            Fun(
              p,
              substitute_closures(
                env |> Environment.without_keys(pat_bound_vars),
                pat_bound_vars @ old_bound_vars,
                pat_bound_vars @ new_bound_vars,
                e,
              ),
              None,
              n,
            )
            |> rewrap;
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
          | Bool(_)
          | Int(_)
          | Float(_)
          | String(_)
          | ListLit(_)
          | Constructor(_)
          | TypFun(_)
          | Tuple(_)
          | TyAlias(_)
          | Ap(_)
          | TypAp(_)
          | DeferredAp(_)
          | If(_)
          | Seq(_)
          | Test(_)
          | Filter(_)
          | Parens(_)
          | Cons(_)
          | ListConcat(_)
          | UnOp(_)
          | BinOp(_)
          | BuiltinFun(_)
          | Cast(_)
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
    | Parens(e) => get_fn_name(e)
    | TypFun(_, _, n) => n
    | _ => None
    };
  };
};

module Rul = {
  include TermBase.Rul;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Rule;

  // example of awkwardness induced by having forms like rules
  // that may have a different-sorted child with no delimiters
  // (eg scrut with no rules)
  let ids = (~any_ids, {ids, term, _}: t) =>
    switch (ids) {
    | [_, ..._] => ids
    | [] =>
      switch (term) {
      | Hole([tm, ..._]) => any_ids(tm)
      | Rules(scrut, []) => scrut.ids
      | _ => []
      }
    };

  let rep_id = (~any_ids, tm) =>
    switch (ids(~any_ids, tm)) {
    | [] => raise(Invalid_argument("UExp.rep_id"))
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

  let rec ids =
    fun
    | Exp(tm) => tm.ids
    | Pat(tm) => tm.ids
    | Typ(tm) => tm.ids
    | TPat(tm) => tm.ids
    | Rul(tm) => Rul.ids(~any_ids=ids, tm)
    | Nul ()
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
    | Exp(tm) => Exp.rep_id(tm)
    | Pat(tm) => Pat.rep_id(tm)
    | Typ(tm) => Typ.rep_id(tm)
    | TPat(tm) => TPat.rep_id(tm)
    | Rul(tm) => Rul.rep_id(~any_ids=ids, tm)
    | Nul ()
    | Any () => raise(Invalid_argument("Term.rep_id"));
};
