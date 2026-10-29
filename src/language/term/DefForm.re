/* Definition forms: the let-like expressions `<kw> <header> in <body>`,
 * whose header scopes over a continuation body. Syntactically these are the
 * Exp-sorted `mk_pre_c(L, P.let_, ...)` rows of Form.rows_of: let, theorem,
 * type, use, module, and the stepper filters (hide/eval/pause/debug).
 *
 * Code that asks "is this a definition, and which part is its body?" (cursor
 * walks, selection, probing, the agent's binding map, the pending-eval
 * worklist) goes through [parts] rather than listing a subset of these forms
 * by hand. [parts] matches every Exp.term constructor, so a new term form has
 * to decide whether it is a definition form. */

type t = {
  /* The definition that is evaluated to give the binder its value: a let's
     def and a module's body. None when the header holds a type (type, use),
     a proposition that is substituted but never evaluated (theorem), or a
     stepper-filter pattern. */
  value: option(Exp.t),
  /* The continuation the definition scopes over. */
  body: Exp.t,
};

let parts = (e: Exp.t): option(t) =>
  switch (Exp.term_of(e)) {
  | Let(_, def, body)
  | ModuleExp(_, def, body) =>
    Some({
      value: Some(def),
      body,
    })
  | Theorem(_, _, body)
  | TyAlias(_, _, body)
  | Use(_, body)
  | Filter(_, body) =>
    Some({
      value: None,
      body,
    })
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
  | ProofObject(_)
  | Forall(_)
  | FixF(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | If(_)
  | Seq(_)
  | Test(_)
  | HintedTest(_)
  | Closure(_)
  | Parens(_)
  | Projector(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Match(_)
  | TupleExtension(_)
  | Asc(_)
  | Module(_) => None
  };

let is_def_form = (e: Exp.t): bool => Option.is_some(parts(e));

/* The body of a definition form (None for any other term). */
let body = (e: Exp.t): option(Exp.t) =>
  Option.map(({body, _}) => body, parts(e));
