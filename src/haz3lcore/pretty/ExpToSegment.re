open Util;
open PrettySegment;
open Base;

let should_add_space = (s1, s2) =>
  switch () {
  | _ when String.ends_with(s1, ~suffix="(") => false
  | _ when String.ends_with(s1, ~suffix="[") => false
  | _ when String.starts_with(s2, ~prefix=")") => false
  | _ when String.starts_with(s2, ~prefix="]") => false
  | _ when String.starts_with(s2, ~prefix=",") => false
  | _ when String.starts_with(s2, ~prefix=";") => false
  | _ when String.starts_with(s2, ~prefix=":") => false
  | _ when String.ends_with(s1, ~suffix=" ") => false
  | _ when String.starts_with(s2, ~prefix=" ") => false
  | _ when String.ends_with(s1, ~suffix="\n") => false
  | _ when String.starts_with(s2, ~prefix="\n") => false
  | _ => true
  };

let text_to_pretty = (id, sort, str): pretty => {
  p_just([
    Tile({
      id,
      label: [str],
      mold: Mold.mk_op(sort, []),
      shards: [0],
      children: [],
    }),
  ]);
};

let mk_form = (form_name: string, id, children): Piece.t => {
  let form: Form.t = Form.get(form_name);
  assert(List.length(children) == List.length(form.mold.in_));
  // Add whitespaces
  let children =
    Aba.map_abas(
      ((l, child, r)) => {
        let lspace = should_add_space(l, child |> Segment.first_string);
        let rspace = should_add_space(child |> Segment.last_string, r);
        (lspace ? [Secondary(Secondary.mk_space(Id.mk()))] : [])
        @ (
          rspace ? child @ [Secondary(Secondary.mk_space(Id.mk()))] : child
        );
      },
      Aba.mk(form.label, children),
    )
    |> Aba.get_bs;
  Tile({
    id,
    label: form.label,
    mold: form.mold,
    shards: List.init(List.length(children) + 1, n => n),
    children,
  });
};

let (@) = (seg1: Segment.t, seg2: Segment.t): Segment.t =>
  switch (seg1, seg2) {
  | ([], _) => seg2
  | (_, []) => seg1
  | _ =>
    if (should_add_space(
          Segment.last_string(seg1),
          Segment.first_string(seg2),
        )) {
      seg1 @ [Secondary(Secondary.mk_space(Id.mk()))] @ seg2;
    } else {
      seg1 @ seg2;
    }
  };

/* We assume that parentheses have already been added as necessary, and
      that the expression has no DynamicErrorHoles, Casts, or FailedCasts
   */
let rec exp_to_pretty = (~inline, exp: Exp.t): pretty => {
  let go = (~inline=inline) => exp_to_pretty(~inline);
  switch (exp |> Exp.term_of) {
  // Assume these have been removed by the parenthesizer
  | DynamicErrorHole(_)
  | Cast(_)
  | FailedCast(_)
  | Closure(_)
  | Filter(_) => failwith("printing these not implemented yet")
  // Other cases
  | Invalid(x) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, x)
  | EmptyHole =>
    let id = exp |> Exp.rep_id;
    p_just([Grout({id, shape: Convex})]);
  | Bool(b) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, Bool.to_string(b))
  | Int(n) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, Int.to_string(n))
  // TODO: do floats print right?
  | Float(f) =>
    text_to_pretty(exp |> Exp.rep_id, Sort.Exp, Float.to_string(f))
  | String(s) =>
    text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "\"" ++ s ++ "\"")
  | Constructor(c) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, c)
  | ListLit([]) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "[]")
  | Deferral(_) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "deferral")
  | ListLit([x, ...xs]) =>
    // TODO: Add optional newlines
    let (id, ids) = (exp.ids |> List.hd, exp.ids |> List.tl);
    let* x = go(x)
    and* xs = xs |> List.map(go) |> all;
    let form = (x, xs) =>
      mk_form(
        "list_lit_exp",
        id,
        [
          x
          @ List.flatten(
              List.map2(
                (id, x) => [mk_form("comma_exp", id, [])] @ x,
                ids,
                xs,
              ),
            ),
        ],
      );
    p_just([form(x, xs)])
    |> p_orif(
         !inline,
         p_just(
           {
             let x = [Secondary(Secondary.mk_newline(Id.mk()))] @ x;
             let xs =
               xs
               |> List.map(x =>
                    [Secondary(Secondary.mk_newline(Id.mk()))] @ x
                  )
               |> ListUtil.map_last_only(x =>
                    x @ [Secondary(Secondary.mk_newline(Id.mk()))]
                  );
             [form(x, xs)];
           },
         ),
       );
  | Var(v) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, v)
  | BinOp(op, l, r) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ l = go(l)
    and+ r = go(r);
    l
    @ [
      Tile({
        id,
        label: [Operators.bin_op_to_string(op)],
        mold: Mold.mk_bin(Precedence.of_bin_op(op), Sort.Exp, []),
        shards: [0],
        children: [],
      }),
    ]
    @ r;
  | MultiHole(es) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ es = es |> List.map(any_to_pretty(~inline)) |> all;
    ListUtil.flat_intersperse(Grout({id, shape: Concave}), es);
  | Fun(p, e, _, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~inline, p)
    and+ e = go(e);
    [mk_form("fun_", id, [p])] @ e;
  | TypFun(tp, e, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ tp = tpat_to_pretty(~inline, tp)
    and+ e = go(e);
    [mk_form("typfun", id, [tp])] @ e;
  | Tuple([]) =>
    let id = exp |> Exp.rep_id;
    p_just([mk_form("ap_exp_empty", id, [])]);
  | Tuple([_]) => failwith("Singleton Tuples are not allowed")
  | Tuple([x, ...xs]) =>
    // TODO: Add optional newlines
    let ids = exp.ids;
    let+ x = go(x)
    and+ xs = xs |> List.map(go) |> all;
    x
    @ List.flatten(
        List.map2((id, x) => [mk_form("comma_exp", id, [])] @ x, ids, xs),
      );
  | Let(p, e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~inline, p)
    and+ e1 = go(e1)
    and+ e2 = go(e2);
    let e2 = inline ? e2 : [Secondary(Secondary.mk_newline(Id.mk()))] @ e2;
    [mk_form("let_", id, [p, e1])] @ e2;
  | FixF(p, e, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~inline, p)
    and+ e = go(e);
    [mk_form("fix", id, [p])] @ e;
  | TyAlias(tp, t, e) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ tp = tpat_to_pretty(~inline, tp)
    and+ t = typ_to_pretty(~inline, t)
    and+ e = go(e);
    let e = inline ? e : [Secondary(Secondary.mk_newline(Id.mk()))] @ e;
    [mk_form("tyalias", id, [tp, t])] @ e;
  | Ap(Forward, e1, e2) =>
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    e1 @ [mk_form("ap_exp", id, [e2])];
  | Ap(Reverse, e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2)
    and+ op = text_to_pretty(id, Sort.Exp, "|>");
    e1 @ op @ e2;
  | TypAp(e, t) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e = go(e)
    and+ tp = typ_to_pretty(~inline, t);
    e @ [mk_form("ap_exp_typ", id, [tp])];
  | DeferredAp(e, es) =>
    // TODO: Add optional newlines
    let (id, ids) = (exp.ids |> List.hd, exp.ids |> List.tl);
    let+ e = go(e)
    and+ es = es |> List.map(go) |> all;
    e
    @ [
      mk_form(
        "ap_exp",
        id,
        [
          List.flatten(
            List.map2(
              (id, e) => [mk_form("comma_exp", id, [])] @ e,
              ids,
              es,
            ),
          ),
        ],
      ),
    ];
  | If(e1, e2, e3) =>
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2)
    and+ e3 = go(e3);
    let e2 = inline ? e2 : [Secondary(Secondary.mk_newline(Id.mk()))] @ e2;
    let e3 = inline ? e3 : [Secondary(Secondary.mk_newline(Id.mk()))] @ e3;
    [mk_form("if_", id, [e1, e2])] @ e3;
  | Seq(e1, e2) =>
    // TODO: Make newline optional
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    let e2 = inline ? e2 : [Secondary(Secondary.mk_newline(Id.mk()))] @ e2;
    e1 @ [mk_form("cell-join", id, [])] @ e2;
  | Test(e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form("test", id, [])] @ e;
  | Parens(e) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form("parens_exp", id, [e])];
  | Cons(e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    e1 @ [mk_form("cons_exp", id, [])] @ e2;
  | ListConcat(e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    e1 @ [mk_form("list_concat", id, [])] @ e2;
  | UnOp(Meta(Unquote), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form("unquote", id, [])] @ e;
  | UnOp(Bool(Not), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form("not", id, [])] @ e;
  | UnOp(Int(Minus), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form("unary_minus", id, [])] @ e;
  /* TODO: this isn't actually correct because we could the builtin
     could have been overriden in this scope; worth fixing when we fix
     closures. */
  | BuiltinFun(f) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, f)
  | Match(e, rs) =>
    // TODO: Add newlines
    let (id, ids) = (exp.ids |> List.hd, exp.ids |> List.tl);
    let+ e = go(e)
    and+ rs: list(list((Segment.t, Segment.t))) = {
      rs
      |> List.map(((p, e)) => (pat_to_pretty(~inline, p), go(e)))
      |> List.map(((x, y)) => ListUtil.cross(x, y))
      |> all;
    };
    [
      mk_form(
        "case",
        id,
        [
          e
          @ (
            List.map2(
              (id, (p, e)) => [mk_form("rule", id, [p])] @ e,
              ids,
              rs,
            )
            |> List.flatten
          ),
        ],
      ),
    ];
  };
}
and pat_to_pretty = (~inline, pat: Pat.t): pretty => {
  let go = pat_to_pretty(~inline);
  switch (pat |> Pat.term_of) {
  | Invalid(t) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, t)
  | EmptyHole =>
    let id = pat |> Pat.rep_id;
    p_just([Grout({id, shape: Convex})]);
  | Wild => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "_")
  | Var(v) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, v)
  | Int(n) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, Int.to_string(n))
  | Float(f) =>
    text_to_pretty(pat |> Pat.rep_id, Sort.Pat, Float.to_string(f))
  | Bool(b) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, Bool.to_string(b))
  | String(s) =>
    text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "\"" ++ s ++ "\"")
  | Constructor(c) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, c)
  | ListLit([]) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "[]")
  | ListLit([x, ...xs]) =>
    let (id, ids) = (pat.ids |> List.hd, pat.ids |> List.tl);
    let* x = go(x)
    and* xs = xs |> List.map(go) |> all;
    p_just([
      mk_form(
        "list_lit_pat",
        id,
        [
          x
          @ List.flatten(
              List.map2(
                (id, x) => [mk_form("comma_pat", id, [])] @ x,
                ids,
                xs,
              ),
            ),
        ],
      ),
    ]);
  | Cons(p1, p2) =>
    let id = pat |> Pat.rep_id;
    let+ p1 = go(p1)
    and+ p2 = go(p2);
    p1 @ [mk_form("cons_pat", id, [])] @ p2;
  | Tuple([]) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "()")
  | Tuple([_]) => failwith("Singleton Tuples are not allowed")
  | Tuple([x, ...xs]) =>
    let ids = pat.ids;
    let+ x = go(x)
    and+ xs = xs |> List.map(go) |> all;
    x
    @ List.flatten(
        List.map2((id, x) => [mk_form("comma_pat", id, [])] @ x, ids, xs),
      );
  | Parens(p) =>
    let id = pat |> Pat.rep_id;
    let+ p = go(p);
    [mk_form("parens_pat", id, [p])];
  | MultiHole(es) =>
    let id = pat |> Pat.rep_id;
    let+ es = es |> List.map(any_to_pretty(~inline)) |> all;
    ListUtil.flat_intersperse(Grout({id, shape: Concave}), es);
  | Ap(p1, p2) =>
    let id = pat |> Pat.rep_id;
    let+ p1 = go(p1)
    and+ p2 = go(p2);
    p1 @ [mk_form("ap_pat", id, [p2])];
  | Cast(p, t, _) =>
    let id = pat |> Pat.rep_id;
    let+ p = go(p)
    and+ t = typ_to_pretty(~inline, t);
    p @ [mk_form("typeann", id, [])] @ t;
  };
}
and typ_to_pretty = (~inline, typ: Typ.t): pretty => {
  let go = typ_to_pretty(~inline);
  let go_constructor: ConstructorMap.variant(Typ.t) => pretty =
    fun
    | Variant(c, ids, None) => text_to_pretty(List.hd(ids), Sort.Typ, c)
    | Variant(c, ids, Some(x)) => {
        let+ constructor =
          text_to_pretty(List.hd(List.tl(ids)), Sort.Typ, c);
        constructor @ [mk_form("ap_typ", List.hd(ids), go(x))];
      }
    | BadEntry(x) => go(x);
  switch (typ |> Typ.term_of) {
  | Unknown(Hole(Invalid(s))) =>
    text_to_pretty(typ |> Typ.rep_id, Sort.Typ, s)
  | Unknown(Internal)
  | Unknown(SynSwitch)
  | Unknown(Hole(EmptyHole)) =>
    let id = typ |> Typ.rep_id;
    p_just([Grout({id, shape: Convex})]);
  | Unknown(Hole(MultiHole(es))) =>
    let id = typ |> Typ.rep_id;
    let+ es = es |> List.map(any_to_pretty(~inline)) |> all;
    ListUtil.flat_intersperse(Grout({id, shape: Concave}), es);
  | Var(v) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, v)
  | Int => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Int")
  | Float => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Float")
  | Bool => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Bool")
  | String => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "String")
  | List(t) =>
    let id = typ |> Typ.rep_id;
    let+ t = go(t);
    [mk_form("list_typ", id, [t])];
  | Prod([]) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "()")
  | Prod([_]) => failwith("Singleton Prods are not allowed")
  | Prod([t, ...ts]) =>
    let+ t = go(t)
    and+ ts = ts |> List.map(go) |> all;
    t
    @ List.flatten(
        List.map2(
          (id, t) => [mk_form("comma_typ", id, [])] @ t,
          typ.ids,
          ts,
        ),
      );
  | Parens(t) =>
    let id = typ |> Typ.rep_id;
    let+ t = go(t);
    [mk_form("parens_typ", id, [t])];
  | Ap(t1, t2) =>
    let id = typ |> Typ.rep_id;
    let+ t1 = go(t1)
    and+ t2 = go(t2);
    t1 @ [mk_form("ap_typ", id, [t2])];
  | Rec(tp, t) =>
    let id = typ |> Typ.rep_id;
    let+ tp = tpat_to_pretty(~inline, tp)
    and+ t = go(t);
    [mk_form("rec", id, [tp])] @ t;
  | Forall(tp, t) =>
    let id = typ |> Typ.rep_id;
    let+ tp = tpat_to_pretty(~inline, tp)
    and+ t = go(t);
    [mk_form("forall", id, [tp])] @ t;
  | Arrow(t1, t2) =>
    let id = typ |> Typ.rep_id;
    let+ t1 = go(t1)
    and+ t2 = go(t2);
    t1 @ [mk_form("arrow", id, [])] @ t2;
  | Sum([]) => failwith("Empty Sums are not allowed")
  | Sum([t]) =>
    let id = typ |> Typ.rep_id;
    let+ t = go_constructor(t);
    [mk_form("typ_sum_single", id, [])] @ t;
  | Sum([t, ...ts]) =>
    let+ t = go_constructor(t)
    and+ ts = ts |> List.map(go_constructor) |> all;
    t
    @ List.flatten(
        List.map2(
          (id, t) => [mk_form("typ_plus", id, [])] @ t,
          typ.ids,
          ts,
        ),
      );
  };
}
and tpat_to_pretty = (~inline, tpat: TPat.t): pretty => {
  switch (tpat |> IdTagged.term_of) {
  | Invalid(t) => text_to_pretty(tpat |> TPat.rep_id, Sort.Typ, t)
  | EmptyHole =>
    let id = tpat |> TPat.rep_id;
    p_just([Grout({id, shape: Convex})]);
  | MultiHole(xs) =>
    let id = tpat |> TPat.rep_id;
    let+ xs = xs |> List.map(any_to_pretty(~inline)) |> all;
    ListUtil.flat_intersperse(Grout({id, shape: Concave}), xs);
  | Var(v) => text_to_pretty(tpat |> TPat.rep_id, Sort.Typ, v)
  };
}
and any_to_pretty = (~inline, any: Any.t): pretty => {
  switch (any) {
  | Exp(e) => exp_to_pretty(~inline, e)
  | Pat(p) => pat_to_pretty(~inline, p)
  | Typ(t) => typ_to_pretty(~inline, t)
  | TPat(tp) => tpat_to_pretty(~inline, tp)
  | Any(_)
  | Nul(_)
  | Rul(_) =>
    //TODO: print out invalid rules properly
    let id = any |> Any.rep_id;
    p_just([Grout({id, shape: Convex})]);
  };
};

// Use Precedence.re to work out where your construct goes here.
let rec external_precedence = (exp: Exp.t): Precedence.t => {
  switch (Exp.term_of(exp)) {
  // Forms which we are about to strip, so we just look inside
  | Closure(_, x)
  | DynamicErrorHole(x, _) => external_precedence(x)

  // Binary operations are handled in Precedence.re
  | BinOp(op, _, _) => Precedence.of_bin_op(op)

  // Indivisible forms never need parentheses around them
  | Var(_)
  | Invalid(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | EmptyHole
  | Constructor(_)
  | Deferral(_)
  | BuiltinFun(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | Parens(_)
  | ListLit(_)
  | Test(_)
  | Match(_) => Precedence.max

  // Other forms
  | UnOp(Meta(Unquote), _) => Precedence.unquote
  | Cast(_)
  | FailedCast(_) => Precedence.cast
  | Ap(Forward, _, _)
  | DeferredAp(_)
  | TypAp(_) => Precedence.ap
  | UnOp(Bool(Not), _) => Precedence.not_
  | UnOp(Int(Minus), _) => Precedence.neg
  | Cons(_) => Precedence.cons
  | Ap(Reverse, _, _) => Precedence.eqs
  | ListConcat(_) => Precedence.concat
  | If(_) => Precedence.if_
  | TypFun(_)
  | Fun(_)
  | FixF(_) => Precedence.fun_
  | Tuple(_) => Precedence.prod
  | Seq(_) => Precedence.semi

  // Top-level things
  | Filter(_)
  | TyAlias(_)
  | Let(_) => Precedence.let_

  // I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | MultiHole(_) => Precedence.min
  };
};

let external_precedence_pat = (dp: Pat.t) =>
  switch (DHPat.term_of(dp)) {
  // Indivisible forms never need parentheses around them
  | EmptyHole
  | Wild
  | Invalid(_)
  | Var(_)
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Constructor(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | ListLit(_)
  | Parens(_) => Precedence.max

  // Other forms
  | Cons(_) => Precedence.cons
  | Ap(_) => Precedence.ap
  | Cast(_) => Precedence.ann
  | Tuple(_) => Precedence.prod

  // I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | MultiHole(_) => Precedence.min
  };

let paren_at = (internal_precedence: Precedence.t, exp: Exp.t): Exp.t =>
  external_precedence(exp) >= internal_precedence
    ? Exp.fresh(Parens(exp)) : exp;

let paren_assoc_at = (internal_precedence: Precedence.t, exp: Exp.t): Exp.t =>
  external_precedence(exp) > internal_precedence
    ? Exp.fresh(Parens(exp)) : exp;

let paren_pat_at = (internal_precedence: Precedence.t, pat: Pat.t): Pat.t =>
  external_precedence_pat(pat) >= internal_precedence
    ? Pat.fresh(Parens(pat)) : pat;

let rec parenthesize = (exp: Exp.t): Exp.t => {
  let (term, rewrap) = Exp.unwrap(exp);
  switch (term) {
  // Indivisible forms dont' change
  | Var(_)
  | Invalid(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | EmptyHole
  | Constructor(_)
  | Deferral(_)
  | BuiltinFun(_) => exp

  // Forms that currently need to stripped before oututting
  | Closure(_, x)
  | DynamicErrorHole(x, _)
  | Tuple([x])
  | Filter(_, x)
  | Cast(x, _, _)
  | FailedCast(x, _, _) => x |> parenthesize

  // Other forms
  | Fun(p, e, c, n) =>
    Fun(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.fun_),
      c, // TODO: Parenthesize through closure
      n,
    )
    |> rewrap
  | TypFun(tp, e, n) =>
    TypFun(tp, parenthesize(e) |> paren_assoc_at(Precedence.fun_), n)
    |> rewrap
  | Tuple(es) =>
    Tuple(
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.prod)),
    )
    |> rewrap
  | ListLit(es) =>
    ListLit(
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.concat)),
    )
    |> rewrap
  | Let(p, e1, e2) =>
    Let(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e1) |> paren_at(Precedence.min),
      parenthesize(e2) |> paren_assoc_at(Precedence.let_),
    )
    |> rewrap
  | FixF(p, e, c) =>
    FixF(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.fun_),
      c // TODO: Parenthesize through closure
    )
    |> rewrap
  | TyAlias(tp, t, e) =>
    TyAlias(
      tp,
      t, // TODO: Types
      parenthesize(e) |> paren_assoc_at(Precedence.let_),
    )
    |> rewrap
  | Ap(Forward, e1, e2) =>
    Ap(
      Forward,
      parenthesize(e1) |> paren_at(Precedence.min),
      parenthesize(e2) |> paren_at(Precedence.min),
    )
    |> rewrap
  | Ap(Reverse, e1, e2) =>
    Ap(
      Reverse,
      parenthesize(e1) |> paren_assoc_at(Precedence.eqs),
      parenthesize(e2) |> paren_at(Precedence.eqs),
    )
    |> rewrap
  | TypAp(e, tp) =>
    TypAp(
      parenthesize(e) |> paren_assoc_at(Precedence.ap),
      tp // TODO: Types
    )
    |> rewrap
  | DeferredAp(e, es) =>
    DeferredAp(
      parenthesize(e) |> paren_assoc_at(Precedence.ap),
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.prod)),
    )
    |> rewrap
  | If(e1, e2, e3) =>
    If(
      parenthesize(e1) |> paren_at(Precedence.min),
      parenthesize(e2) |> paren_at(Precedence.min),
      parenthesize(e3) |> paren_assoc_at(Precedence.if_),
    )
    |> rewrap
  | Seq(e1, e2) =>
    Seq(
      parenthesize(e1) |> paren_at(Precedence.semi), // tempting to make this one assoc too
      parenthesize(e2) |> paren_assoc_at(Precedence.semi),
    )
    |> rewrap
  | Test(e) => Test(parenthesize(e) |> paren_at(Precedence.min)) |> rewrap
  // | Filter(f, e) =>
  //   Filter(
  //     f, // TODO: Filters
  //     parenthesize(e) |> paren_at(Precedence.min),
  //   )
  //   |> rewrap
  | Parens(e) =>
    Parens(parenthesize(e) |> paren_at(Precedence.min)) |> rewrap
  | Cons(e1, e2) =>
    Cons(
      parenthesize(e1) |> paren_at(Precedence.cons),
      parenthesize(e2) |> paren_assoc_at(Precedence.cons),
    )
    |> rewrap
  | ListConcat(e1, e2) =>
    ListConcat(
      parenthesize(e1) |> paren_at(Precedence.concat),
      parenthesize(e2) |> paren_assoc_at(Precedence.concat),
    )
    |> rewrap
  | UnOp(Meta(Unquote), e) =>
    UnOp(Meta(Unquote), parenthesize(e) |> paren_at(Precedence.unquote))
    |> rewrap
  | UnOp(Bool(Not), e) =>
    UnOp(Bool(Not), parenthesize(e) |> paren_at(Precedence.not_)) |> rewrap
  | UnOp(Int(Minus), e) =>
    UnOp(Int(Minus), parenthesize(e) |> paren_at(Precedence.neg)) |> rewrap
  | BinOp(op, e1, e2) =>
    BinOp(
      op,
      parenthesize(e1) |> paren_assoc_at(Precedence.of_bin_op(op)),
      parenthesize(e2) |> paren_at(Precedence.of_bin_op(op)),
    )
    |> rewrap
  | Match(e, rs) =>
    Match(
      parenthesize(e) |> paren_at(Precedence.min),
      rs
      |> List.map(((p, e)) =>
           (
             parenthesize_pat(p) |> paren_pat_at(Precedence.min),
             parenthesize(e) |> paren_assoc_at(Precedence.case_),
           )
         ),
    )
    |> rewrap
  | MultiHole(_) => exp // TODO: Parenthesize through multiholes
  };
}
and parenthesize_pat = (pat: Pat.t): Pat.t => {
  // TODO: patterns
  pat /* TODO: Ensure each expr has enough ids*/;
};
