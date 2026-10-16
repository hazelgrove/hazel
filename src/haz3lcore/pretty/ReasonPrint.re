/* Printing values and types in the ReasonML surface.

   The editor's printers (ExpToSegment, TypToSegment) emit Hazel syntax;
   under the Reason surface their entry points route the forms below
   here, so probe samples, results and inspector types read as Reason.
   Tiles are built directly with the Reason families (no reparse). A
   term outside this subset returns None and the caller falls back to
   Hazel printing. */
open Util;
module Exp = Language.Exp;
module Pat = Language.Pat;
module Typ = Language.Typ;
module Any = Language.Any;
module Atom = Language.Atom;
module ConstructorMap = Language.ConstructorMap;

exception Unprintable;

let sp = (): Piece.t => Secondary(Secondary.mk_space(Id.mk()));

let tok = (sort: Sort.t, t: Token.t): Piece.t => {
  let (form, sort) = Form.classify_label(sort, [t]);
  Tile({
    id: Id.mk(),
    form,
    sort,
    shards: [0],
    children: [],
  });
};

let form = (fam: Form.family, sort: Sort.t, children): Piece.t =>
  Tile({
    id: Id.mk(),
    form: Compound(fam),
    sort,
    shards: List.init(List.length(children) + 1, i => i),
    children,
  });

/* `a, b, c` */
let commas = (sort: Sort.t, segs: list(Segment.t)): Segment.t =>
  switch (segs) {
  | [] => []
  | [s, ...ss] =>
    s @ List.concat_map(s => [form(Comma, sort, []), sp(), ...s], ss)
  };

let parens = (sort, seg) => [form(Parens, sort, [seg])];
let braces = (sort, seg) => [form(ModBody, sort, [seg])];

let float_text = (f: float): string =>
  if (Float.is_integer(f) && Float.abs(f) < 1e15) {
    Printf.sprintf("%.0f.", f);
  } else {
    Printf.sprintf("%.12g", f);
  };

let atom_text = (a: Atom.t): string =>
  switch (a) {
  | Float(f) => float_text(f)
  | String(s) => "\"" ++ String.escaped(s) ++ "\""
  | a => Atom.to_literal(a)
  };

let typ_atom_text: Atom.cls => string =
  fun
  | Int
  | SInt
  | Nat => "int"
  | Float => "float"
  | Bool => "bool"
  | String => "string";

/* Types */

let rec typ = (t: Typ.t): Segment.t =>
  switch (t.term) {
  | Atom(a) => [tok(Typ, typ_atom_text(a))]
  | Prod([]) => [tok(Typ, "unit")]
  | Unknown(_) => [tok(Typ, "?")]
  | Var(x) => [tok(Typ, x)]
  | Parens(t) => typ(t)
  | List(t) => [tok(Typ, "list"), form(Ap, Typ, [typ(t)])]
  | Arrow(a, b) =>
    let a =
      switch (a.term) {
      | Arrow(_) => parens(Typ, typ(a))
      | _ => typ(a)
      };
    a @ [sp(), form(ReArrow, Typ, []), sp()] @ typ(b);
  | Prod(ts) when List.for_all(is_labeled, ts) =>
    braces(Typ, commas(Typ, List.map(field, ts)))
  | Prod(ts) => parens(Typ, commas(Typ, List.map(typ, ts)))
  | Sum(variants) =>
    switch (option_arg(variants)) {
    | Some(arg) => [tok(Typ, "option"), form(Ap, Typ, [typ(arg)])]
    | None =>
      let variant = (v: ConstructorMap.variant(Typ.t)): Segment.t =>
        switch (v) {
        | Variant(c, _, None) => [tok(Typ, c)]
        | Variant(c, _, Some(arg)) => [
            tok(Typ, c),
            form(Ap, Typ, [ctor_args(arg)]),
          ]
        | BadEntry(_) => raise(Unprintable)
        };
      switch (List.map(variant, variants)) {
      | [] => raise(Unprintable)
      | [v, ...vs] =>
        [form(ReSumBarPre, Typ, []), sp(), ...v]
        @ List.concat_map(
            v => [sp(), form(ReSumBar, Typ, []), sp(), ...v],
            vs,
          )
      };
    }
  | Rec({term: Var(x), _}, _) => [tok(Typ, x)]
  | _ => raise(Unprintable)
  }
and is_labeled = (t: Typ.t) =>
  switch (t.term) {
  | TupLabel(_) => true
  | _ => false
  }
and field = (t: Typ.t): Segment.t =>
  switch (t.term) {
  | TupLabel({term: Label(l), _}, t) =>
    [tok(Typ, l), form(ReField, Typ, []), sp()] @ typ(t)
  | _ => raise(Unprintable)
  }
/* constructor arguments print their tuple bare: Rect(float, float) */
and ctor_args = (t: Typ.t): Segment.t =>
  switch (t.term) {
  | Prod(ts) when ts != [] && !List.exists(is_labeled, ts) =>
    commas(Typ, List.map(typ, ts))
  | Parens(t) => ctor_args(t)
  | _ => typ(t)
  }
and option_arg = (variants: ConstructorMap.t(Typ.t)): option(Typ.t) =>
  switch (variants) {
  | [Variant("None", _, None), Variant("Some", _, Some(t))]
  | [Variant("Some", _, Some(t)), Variant("None", _, None)] => Some(t)
  | _ => None
  };

/* Patterns (closure parameters) */

let rec pat = (p: Pat.t): Segment.t =>
  switch (p.term) {
  | Var(x) => [tok(Pat, x)]
  | Wild => [tok(Pat, "_")]
  | Atom(a) => [tok(Pat, atom_text(a))]
  | Tuple([]) => [tok(Pat, "()")]
  | Tuple(ps) => parens(Pat, commas(Pat, List.map(pat, ps)))
  | Parens(p) => pat(p)
  | Constructor(c, _) => [tok(Pat, c)]
  | Asc(p, _) => pat(p)
  | _ => raise(Unprintable)
  };

/* Values */

/* ~fold_fns: function values print as their parameters with an elided
   body; when unset (code, not values) functions are unprintable here */
let rec exp = (~fold_fns: bool, e: Exp.t): Segment.t => {
  let exp = exp(~fold_fns);
  let entry = entry(~fold_fns);
  let ctor_arg = ctor_arg(~fold_fns);
  switch (e.term) {
  | Atom(a) => [tok(Exp, atom_text(a))]
  | EmptyHole => [tok(Exp, "?")]
  | Tuple([]) => [tok(Exp, "()")]
  | Tuple(es) when List.for_all(is_entry, es) =>
    braces(Exp, commas(Exp, List.map(entry, es)))
  | Tuple(es) => parens(Exp, commas(Exp, List.map(exp, es)))
  | ListLit([]) => [tok(Exp, "[]")]
  | ListLit(es) => [form(ListLit, Exp, [commas(Exp, List.map(exp, es))])]
  | Constructor(c, _) => [tok(Exp, c)]
  | Ap(_, {term: Constructor(c, _), _}, arg) => [
      tok(Exp, c),
      form(Ap, Exp, [ctor_arg(arg)]),
    ]
  | Parens(e) => exp(e)
  | Closure(_, e) => exp(e)
  | Fun(_) when !fold_fns => raise(Unprintable)
  | Fun(p, _, _, _) =>
    /* the body is not shown: a function value reads as its parameter */
    let params =
      switch (p.term) {
      | Parens(_)
      | Tuple(_) => pat(p)
      | _ => parens(Pat, pat(p))
      };
    params @ [sp(), form(ReArrow, Exp, []), sp(), tok(Exp, "…")];
  | FixF(_, e, _) => exp(e)
  | BuiltinFun(name) => [tok(Exp, name)]
  | Var(x) => [tok(Exp, x)]
  | _ => raise(Unprintable)
  };
}
and is_entry = (e: Exp.t) =>
  switch (e.term) {
  | TupLabel({term: Label(_), _}, _) => true
  | _ => false
  }
and entry = (~fold_fns, e: Exp.t): Segment.t =>
  switch (e.term) {
  | TupLabel({term: Label(l), _}, v) =>
    [tok(Exp, l), form(ReField, Exp, []), sp()] @ exp(~fold_fns, v)
  | _ => raise(Unprintable)
  }
and ctor_arg = (~fold_fns, e: Exp.t): Segment.t =>
  switch (e.term) {
  | Tuple(es) when es != [] && !List.exists(is_entry, es) =>
    commas(Exp, List.map(exp(~fold_fns), es))
  | Parens(e) => ctor_arg(~fold_fns, e)
  | _ => exp(~fold_fns, e)
  };

let attempt = (f, x): option(Segment.t) =>
  switch (f(x)) {
  | seg => Some(seg)
  | exception Unprintable => None
  };

let typ_to_segment = (t: Typ.t): option(Segment.t) => attempt(typ, t);
let exp_to_segment = (~fold_fns, e: Exp.t): option(Segment.t) =>
  attempt(exp(~fold_fns), e);
let any_to_segment = (~fold_fns, a: Any.t): option(Segment.t) =>
  switch (a) {
  | Exp(e) => exp_to_segment(~fold_fns, e)
  | Typ(t) => typ_to_segment(t)
  | _ => None
  };
