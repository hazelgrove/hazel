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
  fast_equal(a, b)
  && List.equal(
       Option.equal(String.equal),
       lexeme_trace(a),
       lexeme_trace(b),
     );

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

/* Parens and Projector are transparent: elaboration keeps both, so code
   that inspects the head or shape of an expression must look through them.
   Asc is NOT stripped here; whether an ascription is transparent depends on
   the call site. */
let rec strip_wrappers = (e: t): t =>
  switch (e.term) {
  | Parens(e)
  | Projector(_, e) => strip_wrappers(e)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Deferral(_)
  | Undefined
  | Atom(_)
  | DrvQuote(_)
  | ListLit(_)
  | Constructor(_)
  | Fun(_)
  | TypFun(_)
  | Tuple(_)
  | Label(_)
  | ExplicitNonlabel
  | TupLabel(_)
  | Dot(_)
  | LivelitName(_)
  | Var(_)
  | Let(_)
  | Theorem(_)
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
  | Closure(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Match(_)
  | TupleExtension(_)
  | Asc(_)
  | Module(_)
  | ModuleExp(_) => e
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
    | Parens(e)
    | Projector(_, e)
    | Asc(e, _) => match_tup_label(e)
    | TupLabel(elab, e') =>
      switch (strip_wrappers(elab).term) {
      | Label(name) => Some((name, e'))
      | _ => None
      }
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | DynamicErrorHole(_)
    | Deferral(_)
    | Undefined
    | Atom(_)
    | DrvQuote(_)
    | ListLit(_)
    | Constructor(_)
    | Fun(_)
    | TypFun(_)
    | Tuple(_)
    | Label(_)
    | ExplicitNonlabel
    | Dot(_)
    | LivelitName(_)
    | Var(_)
    | Let(_)
    | Theorem(_)
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
    | Closure(_)
    | Cons(_)
    | ListConcat(_)
    | UnOp(_)
    | BinOp(_)
    | BuiltinFun(_)
    | Match(_)
    | TupleExtension(_)
    | Module(_)
    | ModuleExp(_) => None
    };
  };
};

/* The entry that `e1.e2` selects when `e1` is a tuple literal. */
let dot_tuple_entry = (e1: t, e2: t): option(t) =>
  switch (strip_wrappers(e1).term, strip_wrappers(e2).term) {
  | (Tuple(ts), Label(name)) =>
    LabeledTuple.find_label(match_tup_label, ts, name)
  | _ => None
  };

let get_label: t => option(LabeledTuple.label) = {
  e => match_tup_label(e) |> Option.map(~f=fst);
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
    switch (dot_tuple_entry(e1, e2)) {
    | Some(exp) => is_fun(exp)
    | None => false
    }
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
    | Tuple(es) => es |> List.for_all(~f=is_fun)
    | Dot(e1, e2) =>
      switch (dot_tuple_entry(e1, e2)) {
      | Some(exp) => is_tuple_of_functions(exp)
      | None => false
      }
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

let rec ctr_name = (e: t): option(Constructor.t) =>
  switch (e.term) {
  | Constructor(name, _) => Some(name)
  | Parens(e)
  | Projector(_, e) => ctr_name(e)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Deferral(_)
  | Undefined
  | Atom(_)
  | DrvQuote(_)
  | ListLit(_)
  | Fun(_)
  | TypFun(_)
  | Tuple(_)
  | Label(_)
  | ExplicitNonlabel
  | TupLabel(_)
  | Dot(_)
  | LivelitName(_)
  | Var(_)
  | Let(_)
  | Theorem(_)
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
  | Closure(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Match(_)
  | TupleExtension(_)
  | Asc(_)
  | Module(_)
  | ModuleExp(_) => None
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
  | TypFun(_, _, n) => n
  | FixF(_, e, _)
  | Parens(e)
  | Projector(_, e)
  | Asc(e, _) => get_fn_name(e)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Deferral(_)
  | Undefined
  | Atom(_)
  | DrvQuote(_)
  | ListLit(_)
  | Constructor(_)
  | Tuple(_)
  | Label(_)
  | ExplicitNonlabel
  | TupLabel(_)
  | Dot(_)
  | LivelitName(_)
  | Var(_)
  | Let(_)
  | Theorem(_)
  | Forall(_)
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
  | Closure(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Match(_)
  | TupleExtension(_)
  | Module(_)
  | ModuleExp(_) => None
  };
};

/* Rewrite the name of the Fun or TypFun that `e` denotes, looking through
   FixF, Parens, Projector and Asc (as get_fn_name does), so that
   `let f = (fun x -> x) in ...` names its function too. */
let rec map_fn_name = (f: option(Var.t) => option(Var.t), e: t): t => {
  let (term, rewrap) = unwrap(e);
  switch (term) {
  | Fun(p, body, t, name) => Fun(p, body, t, f(name)) |> rewrap
  | TypFun(tpat, body, name) => TypFun(tpat, body, f(name)) |> rewrap
  | Parens(e) => Parens(map_fn_name(f, e)) |> rewrap
  | Projector(pr, e) => Projector(pr, map_fn_name(f, e)) |> rewrap
  | Asc(e, ty) => Asc(map_fn_name(f, e), ty) |> rewrap
  | FixF(p, e, env) => FixF(p, map_fn_name(f, e), env) |> rewrap
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Deferral(_)
  | Undefined
  | Atom(_)
  | DrvQuote(_)
  | ListLit(_)
  | Constructor(_)
  | Tuple(_)
  | Label(_)
  | ExplicitNonlabel
  | TupLabel(_)
  | Dot(_)
  | LivelitName(_)
  | Var(_)
  | Let(_)
  | Theorem(_)
  | Forall(_)
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
  | Closure(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Match(_)
  | TupleExtension(_)
  | Module(_)
  | ModuleExp(_) => e
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
          if (Id.equal(rep_id(exp), id)) {
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
  map_fn_name(_ => name, exp);
};

/* Read a pattern as an expression, so that the statics and the checker
 * build a split's case equation `scrut == pat` the same way. It lives here because `Pat` can't depend on `Exp`.
 *
 * `Wild` has no expression counterpart; it becomes `true`, which is what
 * a wildcard case's equation degenerates to. */
let rec of_pat = (pat: Pat.t): t => {
  let term = pat |> Pat.term_of;
  let rewrap: term => t =
    term => {
      term,
      annotation: pat.annotation,
    };
  switch (term) {
  | Invalid(x) => rewrap(Invalid(x))
  | EmptyHole => rewrap(EmptyHole)
  | MultiHole(xs) => rewrap(MultiHole(xs))
  | Wild => rewrap(Atom(Bool(true)))
  | Atom(a) => rewrap(Atom(a))
  | ListLit(xs) => rewrap(ListLit(List.map(~f=of_pat, xs)))
  | Constructor(c, t) => rewrap(Constructor(c, t))
  | Cons(e1, e2) => rewrap(Cons(of_pat(e1), of_pat(e2)))
  | Var(x) => rewrap(Var(x))
  | Tuple(xs) => rewrap(Tuple(List.map(~f=of_pat, xs)))
  | Parens(e) => rewrap(Parens(of_pat(e)))
  | Projector(data, e) => rewrap(Projector(data, of_pat(e)))
  | Ap(e1, e2) => rewrap(Ap(Forward, of_pat(e1), of_pat(e2)))
  | Asc(e, t1) => rewrap(Asc(of_pat(e), t1))
  | Label(l) => rewrap(Label(l))
  | ExplicitNonlabel => rewrap(ExplicitNonlabel)
  | TupLabel(l, e) => rewrap(TupLabel(of_pat(l), of_pat(e)))
  };
};

/* Does any name in `vs` occur free in `e`, given `bound` already in scope?
 *
 * Binding structure follows `Substitution.in_exp`, except that, as in
 * the statics, `Closure(env, body)` looks only at `body`: expanding `env`
 * would inline the whole environment, the blow-up this walk exists to
 * avoid.
 *
 * Non-binding forms fall through to `map_term`'s generic traversal,
 * which also walks expressions nested inside patterns (`Asc`) under the
 * enclosing scope, as `Substitution.in_pat` does. */
let occurs_free_any = (~bound: list(Var.t)=[], vs: list(Var.t), e: t): bool => {
  let found = ref(false);
  let rec f_exp = (bound: list(Var.t), cont, e: t): t => {
    if (! found^) {
      let under = (bound', es) => List.iter(~f=go(bound'), es);
      switch (e |> term_of) {
      | Var(x) =>
        if (List.mem(vs, x, ~equal=String.equal)
            && !List.mem(bound, x, ~equal=String.equal)) {
          found := true;
        }
      | Fun(p, body, t, _) =>
        Option.iter(~f=go_typ(bound), t);
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [body]);
      | FixF(p, body, _) =>
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [body]);
      | Let(p, def, body) =>
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [def, body]);
      | Theorem(p, stmt, _, body) =>
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [stmt, body]);
      | Forall(p, body) =>
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [body]);
      | Match(scrut, cases) =>
        go(bound, scrut);
        List.iter(
          ~f=
            ((p, body)) => {
              go_pat(bound, p);
              under(Pat.bound_vars(p) @ bound, [body]);
            },
          cases,
        );
      | _ => ignore(cont(e))
      };
    };
    e;
  }
  and go = (bound, e) => ignore(map_term(~f_exp=f_exp(bound), e))
  and go_pat = (bound, p) => ignore(Pat.map_term(~f_exp=f_exp(bound), p))
  and go_typ = (bound, t) => ignore(Typ.map_term(~f_exp=f_exp(bound), t));
  go(bound, e);
  found^;
};
