open Util;
open PrettySegment;
open Base;

let abbreviate_str = (min_len: int, s: string): string => {
  let len = String.length(s);
  let ellipsis = "...";
  if (len <= min_len) {
    s;
  } else {
    String.sub(s, 0, min_len - String.length(ellipsis)) ++ ellipsis;
  };
};

let rec abbreviate_exp = (~available=12, exp: Exp.t): Exp.t => {
  let rewrap = (term: Exp.term): Exp.t => {
    {...exp, term};
  };
  let abbreviate_str = abbreviate_str(available);
  let comp_elipses = "...";
  let ellipses_term = () => IdTagged.fresh(Invalid(comp_elipses): Exp.term);
  let indet_term: Exp.term = Invalid("<INDET>");
  let go = (~available) =>
    abbreviate_exp(~available=available - String.length(comp_elipses));
  let term: Exp.term =
    switch (exp |> Exp.term_of) {
    | Fun(_p, _e, _, Some(s)) => Invalid("<" ++ s ++ ">")
    | Fun(_p, _e, _, None) => Invalid("<FUN>")
    | BuiltinFun(_f) => Invalid("<BUILTIN>")
    | Tuple([_]) => failwith("Singleton Tuples are not allowed")
    //TODO(andrew): show exp below?
    | DynamicErrorHole(_exp, err) =>
      Invalid("<" ++ InvalidOperationError.show(err) ++ ">")
    // Atomic string cases
    | Invalid(x) => Invalid(abbreviate_str(x))
    | String(s) => String(abbreviate_str(s))
    | Var(v) => Var(abbreviate_str(v))
    | Constructor(c, t) => Constructor(abbreviate_str(c), t)

    // Atomic Fixed cases
    //TODO: length check these cases:
    | EmptyHole => EmptyHole
    | ListLit([]) => ListLit([])
    | Tuple([]) => Tuple([])
    | Bool(b) => Bool(b)
    | Undefined => Undefined
    | Int(n) => Int(n)
    | Float(f) => Float(f)

    // composite literal cases
    | ListLit([x, ..._xs]) =>
      //TODO: return used length from call, use that to make incorporate next elems
      ListLit([go(~available, x), ellipses_term()])

    | Tuple([x, ..._xs]) =>
      //TODO: return used length from call, use that to make incorporate next elems
      Tuple([go(~available, x), ellipses_term()])
    | Ap(Forward, {term: Constructor(_), _} as konst, _e2) =>
      //TODO: return used length from call, use that to make incorporate next elems
      let available = available - 5; //chars for ap delimiters, ellipses
      Ap(Forward, abbreviate_exp(~available, konst), ellipses_term());
    | Cons(e1, _e2) =>
      //TODO: return used length from call, use that to make incorporate next elems
      let available = available - 2; //chars for cons op
      Cons(abbreviate_exp(~available, e1), ellipses_term());

    | Parens(e, pt) =>
      let available = available - 2; //chars for parens
      Parens(abbreviate_exp(~available, e), pt);

    //TODO(andrew)
    | Filter(_) => failwith("TODO(andrew): Filter")
    | Closure(_) => failwith("TODO(andrew): Closure")
    | MultiHole(_es) => failwith("TODO(andrew)")
    | TypFun(_tp, _e, _) => failwith("TODO(andrew)")
    | FailedCast(_e, _, _t) => failwith("TODO(andrew)")
    | Cast(_e, _, _t) => failwith("TODO(andrew)")

    //non-value
    | Ap(Forward, _e1, _e2) => indet_term
    | Ap(Reverse, _e1, _e2) => indet_term
    | Deferral(_d) => indet_term
    | BinOp(_op, _l, _r) => indet_term
    | Let(_p, _e1, _e2) => indet_term
    | FixF(_p, _e, _) => indet_term
    | TyAlias(_tp, _t, _e) => indet_term
    | TypAp(_e, _t) => indet_term
    | DeferredAp(_e, _es) => indet_term
    | If(_e1, _e2, _e3) => indet_term
    | Seq(_e1, _e2) => indet_term
    | Test(_e) => indet_term
    | ListConcat(_e1, _e2) => indet_term
    | UnOp(Bool(Not), _e) => indet_term
    | UnOp(Int(Minus), _e) => indet_term
    | UnOp(Meta(Unquote), _e) => indet_term
    | Match(_e, _rs) => indet_term
    };
  rewrap(term);
}
and abbreviate_pat = (pat: Pat.t): Pat.t => {
  switch (pat |> Pat.term_of) {
  | Invalid(_t) => failwith("abbreviate_pat")
  | EmptyHole => failwith("abbreviate_pat")
  | Wild => failwith("abbreviate_pat")
  | Var(_v) => failwith("abbreviate_pat")
  | Int(_n) => failwith("abbreviate_pat")
  | Float(_f) => failwith("abbreviate_pat")
  | Bool(_b) => failwith("abbreviate_pat")
  | String(_s) => failwith("abbreviate_pat")
  | Constructor(_c, _) => failwith("abbreviate_pat")
  | ListLit([]) => failwith("abbreviate_pat")
  | ListLit([_x, ..._xs]) => failwith("abbreviate_pat")
  | Cons(_p1, _p2) => failwith("abbreviate_pat")
  | Tuple([]) => failwith("abbreviate_pat")
  | Tuple([_]) => failwith("Singleton Tuples are not allowed")
  | Tuple([_x, ..._xs]) => failwith("abbreviate_pat")
  | Parens(_p) => failwith("abbreviate_pat")
  | MultiHole(_es) => failwith("abbreviate_pat")
  | Ap(_p1, _p2) => failwith("abbreviate_pat")
  | Cast(_p, _t, _) => failwith("abbreviate_pat")
  };
}
and abbreviate_typ = (typ: Typ.t): Typ.t => {
  switch (typ |> Typ.term_of) {
  | Unknown(Hole(Invalid(_s))) => failwith("abbreviate_typ")
  | Unknown(_) => failwith("abbreviate_typ")
  | Var(_) => failwith("abbreviate_typ")
  | Int => failwith("abbreviate_typ")
  | Float => failwith("abbreviate_typ")
  | Bool => failwith("abbreviate_typ")
  | String => failwith("abbreviate_typ")
  | List(_t) => failwith("abbreviate_typ")
  | Prod([]) => failwith("abbreviate_typ")
  | Prod([_]) => failwith("Singleton Prods are not allowed")
  | Prod([_t, ..._ts]) => failwith("abbreviate_typ")
  | Parens(_t) => failwith("abbreviate_typ")
  | Ap(_t1, _t2) => failwith("abbreviate_typ")
  | Rec(_tp, _t) => failwith("abbreviate_typ")
  | Forall(_tp, _t) => failwith("abbreviate_typ")
  | Arrow(_t1, _t2) => failwith("abbreviate_typ")
  | Sum([]) => failwith("abbreviate_typ")
  | Sum([_t]) => failwith("abbreviate_typ")
  | Sum([_t, ..._ts]) => failwith("abbreviate_typ")
  };
}
and abbreviate_tpat = (tpat: TPat.t): TPat.t => {
  switch (tpat |> IdTagged.term_of) {
  | Invalid(_t) => failwith("abbreviate_tpat")
  | EmptyHole => failwith("abbreviate_tpat")
  | MultiHole(_xs) => failwith("abbreviate_tpat")
  | Var(_v) => failwith("abbreviate_tpat")
  };
}
and abbreviate_any = (any: Any.t): Any.t => {
  switch (any) {
  | Exp(e) => Exp(abbreviate_exp(e))
  | Pat(p) => Pat(abbreviate_pat(p))
  | Typ(t) => Typ(abbreviate_typ(t))
  | TPat(tp) => TPat(abbreviate_tpat(tp))
  | Any(_)
  | Nul(_)
  | Rul(_) => failwith("TODO: abbreviate_any: Rul | Any | Nul")
  };
};
