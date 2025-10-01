[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | DynamicErrorHole
  | Deferral
  | Undefined
  | Atom(Atom.cls)
  | ListLit
  | Constructor
  | Fun
  | TypFun
  | Label
  | TupLabel
  | ExplicitNonlabel
  | TupleExtension
  | Tuple
  | Dot
  | Var
  | Let
  | Theorem
  | ProofObject
  | Forall
  | FixF
  | TyAlias
  | Use
  | Ap
  | TypAp
  | DeferredAp
  | If
  | Seq
  | Test
  | HintedTest
  | Filter
  | Closure
  | Parens
  | Probe
  | Cons
  | UnOp(Operators.op_un)
  | BinOp(Operators.op_bin)
  | BuiltinFun
  | Match
  | Asc
  | LivelitName
  | LivelitAp
  | ListConcat;

include TermBase.Exp;

let fast_equal =
  Equality.(
    equality({
      ...syntactic_settings,
      ignore_parens: true,
      ignore_probes: true,
    })
  ).
    exp;
let equal = fast_equal;

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
  | Deferral(_) => Deferral
  | Undefined => Undefined
  | Atom(c) => Atom(Atom.cls_of_t(c))
  | ListLit(_) => ListLit
  | Constructor(_) => Constructor
  | Fun(_) => Fun
  | TypFun(_) => TypFun
  | Tuple(_) => Tuple
  | TupleExtension(_) => TupleExtension
  | Label(_) => Label
  | TupLabel(_, _) => TupLabel
  | ExplicitNonlabel => ExplicitNonlabel
  | Dot(_) => Dot
  | Var(_) => Var
  | Let(_) => Let
  | Theorem(_) => Theorem
  | ProofObject(_) => ProofObject
  | Forall(_) => Forall
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
  | HintedTest(_) => HintedTest
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
  | Asc(_) => Asc;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid expression"
  | MultiHole => "Broken expression"
  | EmptyHole => "Expression hole"
  | DynamicErrorHole => "Dynamic error hole"
  | Deferral => "Deferral"
  | Undefined => "Undefined expression"
  | Atom(Int) => "Number literal"
  | Atom(Float) => "Float literal"
  | Atom(Bool) => "Boolean literal"
  | Atom(String) => "String literal"
  | Atom(Nat) => "Natural number literal"
  | Atom(SInt) => "System integer literal"
  | ListLit => "List literal"
  | Constructor => "Constructor"
  | Fun => "Function literal"
  | TypFun => "Type Function Literal"
  | Tuple => "Tuple literal"
  | Label => "Label"
  | TupLabel => "Labeled Tuple Item"
  | ExplicitNonlabel => "Explicitly unlabeled entry"
  | TupleExtension => "Tuple Extension"
  | Dot => "Dot operator"
  | Var => "Variable reference"
  | Let => "Let expression"
  | Theorem => "Theorem expression"
  | ProofObject => "Proof placeholder"
  | Forall => "Forall expression"
  | FixF => "Fixpoint operator"
  | TyAlias => "Type Alias definition"
  | Use => "Specify number format to use"
  | Ap => "Application"
  | TypAp => "Type application"
  | DeferredAp => "Partial Application"
  | If => "If expression"
  | Seq => "Sequence expression"
  | Test => "Test"
  | HintedTest => "Hinted Test"
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
  | Asc => "Type ascription expression";

let rec match_tup_label: t => option((LabeledTuple.label, t)) = {
  e => {
    switch (e.term) {
    | Parens(e) => match_tup_label(e)
    | TupLabel(elab, e') =>
      switch (elab.term) {
      | Label(name) => Some((name, e'))
      | _ => None
      }
    | Asc(e, _) => match_tup_label(e)
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
  | Asc(e, _) => is_fun(e)
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
  | Deferral(_)
  | Undefined
  | Atom(_)
  | Label(_)
  | ListLit(_)
  | Tuple(_)
  | TupleExtension(_)
  | Var(_)
  | Let(_)
  | Theorem(_)
  | ProofObject(_)
  | Forall(_)
  | FixF(_)
  | TyAlias(_)
  | Use(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | If(_)
  | Seq(_)
  | Test(_)
  | HintedTest(_)
  | Filter(_)
  | Cons(_)
  | ListConcat(_)
  | Closure(_)
  | UnOp(_)
  | BinOp(_)
  | Match(_)
  | LivelitName(_)
  | ExplicitNonlabel => false
  | Constructor(_) => false
  };
};

let rec is_tuple_of_functions = (e: t) =>
  is_fun(e)
  || (
    switch (e.term) {
    | Asc(e, _)
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
          | Label(name) => LabeledTuple.find_label(match_tup_label, ts, name)
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
    | Deferral(_)
    | Undefined
    | Atom(_)
    | Label(_)
    | ListLit(_)
    | TupleExtension(_)
    | Fun(_)
    | TypFun(_)
    | Closure(_)
    | BuiltinFun(_)
    | Var(_)
    | Let(_)
    | Theorem(_)
    | ProofObject(_)
    | Forall(_)
    | FixF(_)
    | TyAlias(_)
    | Use(_)
    | Ap(_)
    | TypAp(_)
    | DeferredAp(_)
    | If(_)
    | Seq(_)
    | Test(_)
    | HintedTest(_)
    | Filter(_)
    | Cons(_)
    | ListConcat(_)
    | UnOp(_)
    | BinOp(_)
    | Match(_)
    | LivelitName(_)
    | ExplicitNonlabel => false
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
    | FixF(_)
    | Closure(_)
    | BuiltinFun(_)
    | Asc(_)
    | Deferral(_)
    | Undefined
    | Atom(_)
    | Label(_)
    | ListLit(_)
    | TupleExtension(_)
    | Fun(_)
    | TypFun(_)
    | Var(_)
    | Let(_)
    | Theorem(_)
    | ProofObject(_)
    | Forall(_)
    | Filter(_)
    | TyAlias(_)
    | Use(_)
    | Ap(_)
    | TypAp(_)
    | DeferredAp(_)
    | If(_)
    | Seq(_)
    | Test(_)
    | HintedTest(_)
    | Cons(_)
    | ListConcat(_)
    | UnOp(_)
    | BinOp(_)
    | Match(_)
    | LivelitName(_)
    | ExplicitNonlabel
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
          | Some({term: Var(y), _}) when x == y => e
          | Some(e) =>
            e
            |> replace_all_ids
            |> substitute_closures(
                 Environment.empty,
                 old_bound_vars,
                 new_bound_vars,
               )
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
        | Theorem(p, e1, e2) =>
          let pat_bound_vars = Pat.bound_vars(p);
          Theorem(
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
        | Forall(p, e) =>
          let pat_bound_vars = Pat.bound_vars(p);
          Forall(
            p,
            substitute_closures(
              env |> Environment.without_keys(pat_bound_vars),
              pat_bound_vars @ old_bound_vars,
              pat_bound_vars @ new_bound_vars,
              e,
            ),
          )
          |> rewrap;
        // Other cases: recurse
        | ProofObject(_)
        | Invalid(_)
        | EmptyHole
        | MultiHole(_)
        | DynamicErrorHole(_)
        | Deferral(_)
        | Atom(_)
        | ListLit(_)
        | Constructor(_)
        | TypFun(_)
        | Tuple(_)
        | TupLabel(_)
        | ExplicitNonlabel
        | TupleExtension(_)
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
        | HintedTest(_)
        | Filter(_)
        | Parens(_)
        | Probe(_)
        | Cons(_)
        | ListConcat(_)
        | UnOp(_)
        | BinOp(_)
        | BuiltinFun(_)
        | Asc(_)
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

let to_tuple = (es: list(t)): t => TempGrammar.Exp.(tuple(es));
