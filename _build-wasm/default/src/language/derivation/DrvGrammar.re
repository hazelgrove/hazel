open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin =
  | Plus
  | Minus
  | Times
  | Gt
  | Lt
  | Eq;

module M =
       (
         W: {
           [@deriving (show({with_path: false}), sexp, yojson, eq)]
           type t('a, 'b);
         },
       ) => {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type any_t('a) =
    | Exp(exp_t('a))
    | Pat(pat_t('a))
    | Typ(typ_t('a))
    | TPat(tpat_t('a))
  and exp_term('a) =
    | Hole(type_hole('a))
    | Quote(Var.t)
    | Var(Var.t) // Prop / Exp
    | Parens(exp_t('a)) // Jdmt / Ctxt / Prop / Exp
    | Tuple(list(exp_t('a))) // [invalid]
    // Jdmt
    | Val(exp_t('a))
    | Eval(exp_t('a), exp_t('a))
    | Entail(exp_t('a), exp_t('a))
    | Consistent(typ_t('a), typ_t('a))
    | MatchedArrow(typ_t('a), typ_t('a))
    | MatchedProd(typ_t('a), typ_t('a))
    | MatchedSum(typ_t('a), typ_t('a))
    // Ctx
    | Ctx(list(exp_t('a)))
    | Cons(exp_t('a), exp_t('a))
    | Concat(exp_t('a), exp_t('a))
    // Prop
    | Type(typ_t('a))
    | HasType(exp_t('a), typ_t('a))
    | Syn(exp_t('a), typ_t('a))
    | Ana(exp_t('a), typ_t('a))
    | And(exp_t('a), exp_t('a))
    | Or(exp_t('a), exp_t('a))
    | Impl(exp_t('a), exp_t('a))
    | Truth
    | Falsity
    // Exp
    | NumLit(int)
    | Neg(exp_t('a))
    | BinOp(op_bin, exp_t('a), exp_t('a))
    | True
    | False
    | If(exp_t('a), exp_t('a), exp_t('a))
    | Let(pat_t('a), exp_t('a), exp_t('a))
    | Fix(pat_t('a), exp_t('a))
    | Fun(pat_t('a), exp_t('a))
    | Ap(exp_t('a), exp_t('a))
    | Pair(exp_t('a), exp_t('a))
    | Triv
    | PrjL(exp_t('a))
    | PrjR(exp_t('a))
    | InjL(exp_t('a))
    | InjR(exp_t('a))
    | Case(exp_t('a), pat_t('a), exp_t('a), pat_t('a), exp_t('a))
    | Roll(exp_t('a))
    | Unroll(exp_t('a))
    | ExpHole
  and exp_t('a) = W.t(exp_term('a), 'a)
  and pat_term('a) =
    | Hole(type_hole('a))
    | Quote(Var.t)
    | Var(Var.t)
    | Parens(pat_t('a))
    | Cast(pat_t('a), typ_t('a))
    | InjL(pat_t('a))
    | InjR(pat_t('a))
    | Pair(pat_t('a), pat_t('a))
  and pat_t('a) = W.t(pat_term('a), 'a)
  and typ_term('a) =
    | Hole(type_hole('a))
    | Quote(Var.t)
    | Var(Var.t)
    | Parens(typ_t('a))
    | Num
    | Bool
    | Arrow(typ_t('a), typ_t('a))
    | Prod(typ_t('a), typ_t('a))
    | Unit
    | Sum(typ_t('a), typ_t('a))
    | Rec(tpat_t('a), typ_t('a))
    | TypHole
  and typ_t('a) = W.t(typ_term('a), 'a)
  and tpat_term('a) =
    | Hole(type_hole('a))
    | Quote(Var.t)
    | Var(Var.t)
  and tpat_t('a) = W.t(tpat_term('a), 'a)
  and type_hole('a) =
    | AbbrNotVar
    | AbbrNotFound
    | AbbrNotDrvTerm
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(any_t('a)));
};

module M_Annotated = M(Annotated);
include M_Annotated;


let rec map_exp_annotation: type a b. (a => b, exp_t(a)) => exp_t(b) =
  (f, e) => {
    let (term, annotation) = (e.term, e.annotation);
    let new_annotation: b = f(annotation);
    let term: exp_term(b) =
      switch (term) {
      | Hole(h) => Hole(map_type_hole_annotation(f, h))
      | Quote(v) => Quote(v)
      | Var(v) => Var(v)
      | Parens(e) => Parens(map_exp_annotation(f, e))
      | Tuple(l) => Tuple(List.map(x => map_exp_annotation(f, x), l))
      | Val(e) => Val(map_exp_annotation(f, e))
      | Eval(e1, e2) =>
        Eval(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | Entail(e1, e2) =>
        Entail(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | Consistent(t1, t2) =>
        Consistent(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
      | MatchedArrow(t1, t2) =>
        MatchedArrow(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
      | MatchedProd(t1, t2) =>
        MatchedProd(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
      | MatchedSum(t1, t2) =>
        MatchedSum(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
      | Ctx(l) => Ctx(List.map(x => map_exp_annotation(f, x), l))
      | Cons(e1, e2) =>
        Cons(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | Concat(e1, e2) =>
        Concat(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | Type(t) => Type(map_typ_annotation(f, t))
      | HasType(e, t) =>
        HasType(map_exp_annotation(f, e), map_typ_annotation(f, t))
      | Syn(e, t) =>
        Syn(map_exp_annotation(f, e), map_typ_annotation(f, t))
      | Ana(e, t) =>
        Ana(map_exp_annotation(f, e), map_typ_annotation(f, t))
      | Truth => Truth
      | Falsity => Falsity
      | And(e1, e2) =>
        And(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | Or(e1, e2) =>
        Or(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | Impl(e1, e2) =>
        Impl(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | NumLit(i) => NumLit(i)
      | Neg(e) => Neg(map_exp_annotation(f, e))
      | BinOp(op, e1, e2) =>
        BinOp(op, map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | True => True
      | False => False
      | If(e1, e2, e3) =>
        If(
          map_exp_annotation(f, e1),
          map_exp_annotation(f, e2),
          map_exp_annotation(f, e3),
        )
      | Let(p, e1, e2) =>
        Let(
          map_pat_annotation(f, p),
          map_exp_annotation(f, e1),
          map_exp_annotation(f, e2),
        )
      | Fix(p, e) =>
        Fix(map_pat_annotation(f, p), map_exp_annotation(f, e))
      | Fun(p, e) =>
        Fun(map_pat_annotation(f, p), map_exp_annotation(f, e))
      | Ap(e1, e2) =>
        Ap(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | Pair(e1, e2) =>
        Pair(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
      | Triv => Triv
      | PrjL(e) => PrjL(map_exp_annotation(f, e))
      | PrjR(e) => PrjR(map_exp_annotation(f, e))
      | InjL(e) => InjL(map_exp_annotation(f, e))
      | InjR(e) => InjR(map_exp_annotation(f, e))
      | Case(e, p1, e1, p2, e2) =>
        Case(
          map_exp_annotation(f, e),
          map_pat_annotation(f, p1),
          map_exp_annotation(f, e1),
          map_pat_annotation(f, p2),
          map_exp_annotation(f, e2),
        )
      | Roll(e) => Roll(map_exp_annotation(f, e))
      | Unroll(e) => Unroll(map_exp_annotation(f, e))
      | ExpHole => ExpHole
      };
    {
      term,
      annotation: new_annotation,
    };
  }

and map_pat_annotation: type a b. (a => b, pat_t(a)) => pat_t(b) =
  (f, e) => {
    let (term, annotation) = (e.term, e.annotation);
    let new_annotation = f(annotation);
    let term: pat_term(b) =
      switch (term) {
      | Hole(h) => Hole(map_type_hole_annotation(f, h))
      | Quote(v) => Quote(v)
      | Var(v) => Var(v)
      | Parens(p) => Parens(map_pat_annotation(f, p))
      | Cast(p, t) =>
        Cast(map_pat_annotation(f, p), map_typ_annotation(f, t))
      | InjL(p) => InjL(map_pat_annotation(f, p))
      | InjR(p) => InjR(map_pat_annotation(f, p))
      | Pair(p1, p2) =>
        Pair(map_pat_annotation(f, p1), map_pat_annotation(f, p2))
      };
    {
      term,
      annotation: new_annotation,
    };
  }

and map_typ_annotation: type a b. (a => b, typ_t(a)) => typ_t(b) =
  (f, e) => {
    let (term, annotation) = (e.term, e.annotation);
    let new_annotation = f(annotation);
    let term: typ_term(b) =
      switch (term) {
      | Hole(h) => Hole(map_type_hole_annotation(f, h))
      | Quote(v) => Quote(v)
      | Var(v) => Var(v)
      | Parens(t) => Parens(map_typ_annotation(f, t))
      | Num => Num
      | Bool => Bool
      | Arrow(t1, t2) =>
        Arrow(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
      | Prod(t1, t2) =>
        Prod(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
      | Unit => Unit
      | Sum(t1, t2) =>
        Sum(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
      | Rec(tp, t) =>
        Rec(map_tpat_annotation(f, tp), map_typ_annotation(f, t))
      | TypHole => TypHole
      };
    {
      term,
      annotation: new_annotation,
    };
  }

and map_tpat_annotation: type a b. (a => b, tpat_t(a)) => tpat_t(b) =
  (f, e) => {
    let (term, annotation) = (e.term, e.annotation);
    let new_annotation = f(annotation);
    let term: tpat_term(b) =
      switch (term) {
      | Hole(h) => Hole(map_type_hole_annotation(f, h))
      | Quote(v) => Quote(v)
      | Var(v) => Var(v)
      };
    {
      term,
      annotation: new_annotation,
    };
  }

and map_type_hole_annotation:
  type a b. (a => b, type_hole(a)) => type_hole(b) =
  (f, e) => {
    switch (e) {
    | AbbrNotVar => AbbrNotVar
    | AbbrNotFound => AbbrNotFound
    | AbbrNotDrvTerm => AbbrNotDrvTerm
    | Invalid(s) => Invalid(s)
    | EmptyHole => EmptyHole
    | MultiHole(l) => MultiHole(List.map(x => map_any_annotation(f, x), l))
    };
  }

and map_any_annotation: type a b. (a => b, any_t(a)) => any_t(b) =
  (f, e) => {
    switch (e) {
    | Exp(e) => Exp(map_exp_annotation(f, e))
    | Pat(p) => Pat(map_pat_annotation(f, p))
    | Typ(t) => Typ(map_typ_annotation(f, t))
    | TPat(tp) => TPat(map_tpat_annotation(f, tp))
    };
  };
