/* Abbreviate a term for display, specifically for the live
 * value probe projector. This is currently specialized for
 * expressions which are (at least partially) values. This
 * is a bit rough right now, and should be redone when we
 * projectors (in particular, fold) within value displays */

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
      //TODO: improve this logic
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
    | Wrap(e, pt) =>
      available := available^ - 2;
      Wrap(abbreviate_exp(e), pt);

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
};

let abbreviate_exp = (~available as a=12, exp: Exp.t): (Exp.t, bool) => {
  available := a;
  let exp = abbreviate_exp(exp);
  let length_exp = a - available^;
  a < 0 || a <= 1 && length_exp > 1
    ? (ellipses_term(), false)
    : {
      (exp, available^ < 0);
    };
};
