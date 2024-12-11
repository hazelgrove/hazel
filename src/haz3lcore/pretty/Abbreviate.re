let comp_elipses = "⋱";
let flat_ellipses = "…";
let ellipses_term = () => IdTagged.fresh(Invalid(comp_elipses): Exp.term);
let flat_ellipses_term = () =>
  IdTagged.fresh(Invalid(flat_ellipses): Exp.term);
let available = ref(0);

let abbreviate_str = (min_len: int, s: string): string => {
  let len = String.length(s);
  let ellipsis = "…";
  if (len <= min_len || min_len < 1) {
    available := available^ - len;
    s;
  } else if (min_len < 1) {
    let str = String.sub(s, 0, 1) ++ ellipsis;
    available := available^ - String.length(str);
    str;
  } else {
    let str = String.sub(s, 0, min_len - 1) ++ ellipsis;
    available := available^ - String.length(str);
    str;
  };
};

let rec abbreviate_exp = (exp: Exp.t): Exp.t => {
  /*
      Maybe we can also use this to format, ie insert linebreaks?
      Hard when it's just exp but maybe we can track them via
      some inserted form, or as a side effect? eg emit ids
      to insert lb after during ExpToSeg?
   */
  // print_endline("abbreviate_exp");
  let rewrap = (term: Exp.term): Exp.t => {
    {...exp, term};
  };

  let wrap_or = (term, str): Exp.term =>
    if (available^ > String.length(str)) {
      available := available^ - String.length(str);
      term;
    } else {
      Invalid(abbreviate_str(available^, str));
    };

  let indet_term: Exp.term = Invalid("<INDET>");
  let term: Exp.term =
    switch (exp |> Exp.term_of) {
    | Fun(_p, _e, _, Some(s)) => Invalid("<" ++ s ++ ">")
    | Fun(_p, _e, _, None) => Invalid("<FUN>")
    | BuiltinFun(_f) => Invalid("<BUILTIN>")
    | Tuple([_]) => failwith("Singleton Tuples are not allowed")
    | DynamicErrorHole(_exp, err) =>
      Invalid("<" ++ InvalidOperationError.show(err) ++ ">")

    // Atomic string cases
    | Invalid(x) => Invalid(abbreviate_str(available^, x))
    | String(s) => String(abbreviate_str(available^, s))
    | Var(v) => Var(abbreviate_str(available^, v))
    | Constructor(c, t) => Constructor(abbreviate_str(available^, c), t)

    // Other atomic cases
    | EmptyHole => EmptyHole
    | ListLit([]) => ListLit([])
    | Tuple([]) => Tuple([])
    | Undefined => wrap_or(Undefined, "undefined")
    | Bool(b) => wrap_or(Bool(b), string_of_bool(b))
    | Int(n) =>
      //TODO: smarter number summarization?
      wrap_or(Int(n), string_of_int(n))
    | Float(f) =>
      //TODO: smarter number summarization?
      wrap_or(Float(f), string_of_float(f))

    // composite literal cases
    | ListLit(xs) =>
      //TODO(andrew): improve this logic
      if (available^ < 6) {
        ListLit([flat_ellipses_term()]);
      } else {
        available := available^ - 2;
        let rec go = xs =>
          switch (xs) {
          | [] => []
          | [x] => [abbreviate_exp(x)]
          | [x, ...xs] =>
            let hd = abbreviate_exp(x);
            let tl =
              if (available^ > 0) {
                go(xs);
              } else {
                [flat_ellipses_term()];
              };
            [hd, ...tl];
          };
        ListLit(go(xs));
      }

    | Tuple(xs) =>
      available := available^ - 2;
      let rec go = xs =>
        switch (xs) {
        | [] => []
        | [x] =>
          if (available^ > 1) {
            [abbreviate_exp(x)];
          } else {
            [flat_ellipses_term()];
          }
        | [x, ...xs] =>
          let hd = abbreviate_exp(x);
          let tl =
            if (available^ > 0) {
              available := available^ - 2;
              go(xs);
            } else {
              [flat_ellipses_term()];
            };
          [hd, ...tl];
        };
      Tuple(go(xs));
    | Ap(Forward, {term: Constructor(_str, _), _} as konst, arg) =>
      let konst = abbreviate_exp(konst);
      available := available^ - 2;
      let arg =
        if (available^ > 0) {
          abbreviate_exp(arg);
        } else {
          ellipses_term();
        };
      Ap(Forward, konst, arg);
    | Parens(e, pt) =>
      available := available^ - 2;
      Parens(abbreviate_exp(e), pt);

    //unhandled atm
    | Closure(_) => indet_term
    | MultiHole(_es) => indet_term
    | TypFun(_tp, _e, _) => indet_term
    | FailedCast(_e, _, _t) => indet_term
    | Cast(_e, _, _t) => indet_term

    //non-value
    | Cons(_) => indet_term
    | Filter(_) => indet_term
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

let abbreviate_exp = (~available as a=12, exp: Exp.t): (Exp.t, bool) => {
  available := a;
  available^ <= 1
    ? (ellipses_term(), false)
    : {
      let exp = abbreviate_exp(exp);
      (exp, available^ < 0);
    };
};
