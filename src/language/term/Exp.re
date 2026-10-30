[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | DynamicErrorHole
  | Deferral
  | Undefined
  | Atom(Atom.cls)
  | DrvQuote
  | ListLit
  | Constructor
  | Fun
  | TypFun
  | ExplicitNonlabel
  | Label
  | TupLabel
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
  | Projector
  | Cons
  | UnOp(Operators.op_un)
  | BinOp(Operators.op_bin)
  | BuiltinFun
  | Match
  | Asc
  | LivelitName
  | LivelitAp
  | ListConcat
  | Module
  | ModuleExp;

include TermBase.Exp;

let fast_equal =
  Equality.(
    equality({
      ...syntactic_settings,
      ignore_parens: true,
      ignore_projectors: true,
    })
  ).
    exp;
let equal = fast_equal;

/* In-order trace of annotation lexemes. Paired with fast_equal (which
   guarantees aligned structure), comparing traces detects lexeme-only
   differences — e.g. editing an unknown operator @@ -> @@@ changes
   display and stuck-application semantics but not term structure. */
let lexeme_trace = (e: t): list(option(string)) => {
  let acc = ref([]);
  let f = (continue, t: IdTagged.t(_)) => {
    acc := [t.annotation.lexeme, ...acc^];
    continue(t);
  };
  let _ =
    map_term(
      ~f_exp=f,
      ~f_pat=f,
      ~f_typ=f,
      ~f_tpat=f,
      ~f_rul=f,
      ~f_mod=f,
      ~f_sig=f,
      ~f_mpat=f,
      e,
    );
  acc^;
};

/* fast_equal plus lexeme comparison. fast_equal ignores annotations, but
   lexemes carry display and semantics (hole flavor, stuck unknown-op
   applications), so recompute/caching gates must use this variant or
   lexeme-only edits serve stale results (cf. IncrEval.reuse_check). */
let fast_equal_with_lexemes = (a: t, b: t): bool =>
  fast_equal(a, b) && lexeme_trace(a) == lexeme_trace(b);

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
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

let strip_projectors =
  map_term(
    ~f_exp=
      (continue, exp) =>
        switch (term_of(exp)) {
        | Projector(_, e) => continue(e)
        | _ => continue(exp)
        },
    _,
  );

/* `e` with the type variable `x` renamed to `x'` where it refers to the
   binding being renamed: not in the scope of a type binder of `x`, nor in
   that of a module named `x`, since a module's name roots type paths. A
   `let` names a module when its definition is a module or a name
   [is_module] holds of. */
let rec rename_tvar =
        (~is_module: string => bool, x: string, x': string, e: t): t => {
  let rename_tvar = rename_tvar(~is_module);
  let rename_typ = (_, ty) => Typ.rename_var(x, x', ty);
  let defines_module = (def: t) =>
    switch (term_of(def)) {
    | Module(_) => true
    | Var(y)
    | Constructor(y, _) => is_module(y)
    | _ => false
    };
  map_term(
    ~f_exp=
      (continue, e) => {
        let (term, rewrap) = unwrap(e);
        switch (term) {
        | TyAlias(tp, _, _)
        | TypFun(tp, _, _) when TPat.binds(x, tp) => e
        | ModuleExp(mp, def, body) when MPat.binds(x, mp) =>
          ModuleExp(
            MPat.map_term(~f_typ=rename_typ, mp),
            rename_tvar(x, x', def),
            body,
          )
          |> rewrap
        | Let(p, def, body)
            when
              List.exists(String.equal(x), Pat.bound_vars(p))
              && defines_module(def) =>
          Let(
            Pat.map_term(~f_typ=rename_typ, p),
            rename_tvar(x, x', def),
            body,
          )
          |> rewrap
        | Module(items) =>
          Module(rename_tvar_in_items(~is_module, x, x', items)) |> rewrap
        | _ => continue(e)
        };
      },
    ~f_typ=rename_typ,
    e,
  );
}
and rename_tvar_in_items =
    (~is_module: string => bool, x: string, x': string, items: list(Mod.t))
    : list(Mod.t) => {
  let rename_typ = (_, ty) => Typ.rename_var(x, x', ty);
  switch (items) {
  | [] => []
  | [{term: ModType(tp, _), _}, ..._] when TPat.binds(x, tp) => items
  | [{term: ModuleMod(mp, def), _} as item, ...items]
      when MPat.binds(x, mp) =>
    let (_, rewrap) = IdTagged.unwrap(item);
    [
      (
        ModuleMod(
          MPat.map_term(~f_typ=rename_typ, mp),
          rename_tvar(~is_module, x, x', def),
        ): Mod.term
      )
      |> rewrap,
      ...items,
    ];
  | [item, ...items] => [
      Mod.map_term(
        ~f_exp=(_, e) => rename_tvar(~is_module, x, x', e),
        ~f_typ=rename_typ,
        item,
      ),
      ...rename_tvar_in_items(~is_module, x, x', items),
    ]
  };
};

/* Every type variable name written in an expression, bound or free, and
   every name a pattern binds, since a module's name roots type paths. */
let tvar_names = (e: t): list(string) => {
  let names = ref([]);
  let _: t =
    map_term(
      ~f_typ=
        (_, ty) => {
          names := Typ.tvar_names(ty) @ names^;
          ty;
        },
      ~f_tpat=
        (continue, tp) => {
          switch (TPat.tyvar_of_utpat(tp)) {
          | Some(x) => names := [x, ...names^]
          | None => ()
          };
          continue(tp);
        },
      ~f_pat=
        (_, p) => {
          names := Pat.bound_vars(p) @ names^;
          p;
        },
      ~f_mpat=
        (continue, mp) => {
          switch (IdTagged.term_of(mp)) {
          | Var(x) => names := [x, ...names^]
          | Invalid(_)
          | EmptyHole
          | MultiHole(_)
          | Asc(_) => ()
          };
          continue(mp);
        },
      e,
    );
  names^;
};

let rec cls_of_term: type a. Grammar.exp_term(a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | DynamicErrorHole(_) => DynamicErrorHole
  | Deferral(_) => Deferral
  | Undefined => Undefined
  | Atom(c) => Atom(Atom.cls_of_t(c))
  | DrvQuote(_) => DrvQuote
  | ListLit(_) => ListLit
  | Constructor(_) => Constructor
  | Fun(_) => Fun
  | TypFun(_) => TypFun
  | Tuple(_) => Tuple
  | TupleExtension(_) => TupleExtension
  | Label(_) => Label
  | ExplicitNonlabel => ExplicitNonlabel
  | TupLabel(_, _) => TupLabel
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
  | Parens(e) => cls_of_term(e.term)
  // We're bypassing projectors from cls because they're breaking cursor inspector messages.
  // Future work could be to specialize projectors in the cursor inspector.
  | Projector(_, e) => cls_of_term(e.term)
  | Cons(_) => Cons
  | ListConcat(_) => ListConcat
  | UnOp(op, _) => UnOp(op)
  | BinOp(op, _, _) => BinOp(op)
  | BuiltinFun(_) => BuiltinFun
  | Match(_) => Match
  | LivelitName(_) => LivelitName
  | Asc(_) => Asc
  | Module(_) => Module
  | ModuleExp(_) => ModuleExp;

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
  | DrvQuote => "Derivation-Mode Quotation"
  | ListLit => "List literal"
  | Constructor => "Constructor"
  | Fun => "Function literal"
  | TypFun => "Type Function Literal"
  | Tuple => "Tuple literal"
  | Label => "Label"
  | ExplicitNonlabel => "Explicitly unlabeled entry"
  | TupLabel => "Tuple Item"
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
  | Cons => "Cons"
  | ListConcat => "List Concatenation"
  | BinOp(op) => Operators.show_binop(op)
  | UnOp(op) => Operators.show_unop(op)
  | BuiltinFun => "Built-in Function"
  | Match => "Case expression"
  | LivelitName => "Livelit name"
  | LivelitAp => "Livelit application"
  | Projector => "Projector"
  | Asc => "Type ascription expression"
  | Module => "Module expression"
  | ModuleExp => "Module definition";

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
  | Projector(_, e) => is_fun(e)
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
  | DrvQuote(_)
  | Label(_)
  | ExplicitNonlabel
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
  | Constructor(_)
  | Module(_)
  | ModuleExp(_) => false
  };
};

let rec is_tuple_of_functions = (e: t) =>
  is_fun(e)
  || (
    switch (e.term) {
    | Asc(e, _)
    | Parens(e)
    | Projector(_, e)
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
    | DrvQuote(_)
    | Label(_)
    | ExplicitNonlabel
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
    | Constructor(_)
    | Module(_)
    | ModuleExp(_) => false
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
    | Projector(_, e)
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
    | ExplicitNonlabel
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
    | Constructor(_)
    | Module(_)
    | ModuleExp(_)
    | DrvQuote(_) => None
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
          /* fresh ids; keep the lexeme (hole flavor, unknown-op tokens)
             so re-idded results still display faithfully. Secondary and
             shard provenance stay dropped: display of results is
             AutoFormat and the tile-id references are invalidated. */
          ...IdTagged.IdTag.mk_internal([Id.mk()]),
          lexeme: exp.annotation.lexeme,
        },
      }
      |> continue;
  (
    map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
    Typ.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
  );
};

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

/* Get the definition-site ID of a function expression.
 * Used to enable jump-to-definition from the closure cursor bar
 * even when the app_id comes from built-in internal code. */
let rec get_fn_def_id = (e: t) =>
  switch (e.term) {
  | Fun(_)
  | TypFun(_) => Some(rep_id(e))
  | FixF(_, e, _)
  | Parens(e) => get_fn_def_id(e)
  | _ => None
  };

let to_tuple = (es: list(t)): t => TempGrammar.Exp.(tuple(es));

let find_by_id = (id: Id.t, exp: t): option(t) => {
  module M = {
    exception Found(t);
  };
  switch (
    map_term(
      ~f_exp=
        (cont, exp) =>
          if (rep_id(exp) == id) {
            raise(M.Found(exp));
          } else {
            cont(exp);
          },
      exp,
    )
  ) {
  | exception (M.Found(x)) => Some(x)
  | _ => None
  };
};

/* Inject a function name into a Fun or TypFun expression. */
let add_name = (name: option(string), exp: t): t => {
  let (term, rewrap) = unwrap(exp);
  switch (term) {
  | Fun(p, e, t, _) => Fun(p, e, t, name) |> rewrap
  | TypFun(tpat, e, _) => TypFun(tpat, e, name) |> rewrap
  | _ => exp
  };
};
