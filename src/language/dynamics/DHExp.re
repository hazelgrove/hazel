/* DHExp.re

   This module is specifically for dynamic expressions. They are stored
   using the same data structure as user expressions, have been modified
   slightly as described in Elaborator.re.
   */

include Exp;

let term_of: t => term = IdTagged.term_of;
let fast_copy: (Id.t, t) => t = IdTagged.fast_copy;

let mk = (ids, term): t => {
  {
    term,
    annotation: {
      ids: ids,
    },
  };
};

// Also strips static error holes - kinda like unelaboration
let rec strip_ascriptions =
  map_term(
    ~f_pat=
      (continue, t) =>
        switch (t.term) {
        | Asc(p, _) => strip_ascriptions_pat(p)
        | _ => continue(t)
        },
    ~f_exp=
      (continue, exp) => {
        switch (term_of(exp)) {
        /* Remove Asciptions */
        | Asc(d, _) => strip_ascriptions(d)
        | _ => continue(exp)
        }
      },
    _,
  )
and strip_ascriptions_pat = (p: Pat.t): Pat.t => {
  Pat.map_term(
    ~f_pat=
      (continue, t) =>
        switch (t.term) {
        | Asc(p, _) => strip_ascriptions_pat(p)
        | _ => continue(t)
        },
    ~f_exp=
      (continue, t) =>
        switch (t.term) {
        | Asc(e, _) => strip_ascriptions(e)
        | _ => continue(t)
        },
    p,
  );
};

let assign_name_if_none = (t, name) => {
  let (term, rewrap) = unwrap(t);
  switch (term) {
  | Fun(arg, body, typ, None) => Fun(arg, body, typ, name) |> rewrap
  | TypFun(utpat, body, None) => TypFun(utpat, body, name) |> rewrap
  | _ => t
  };
};

let ty_subst = (s: Typ.t, tpat: TPat.t, exp: t): t => {
  switch (TPat.tyvar_of_utpat(tpat)) {
  | None => exp
  | Some(x) =>
    Exp.map_term(
      ~f_typ=(_, typ) => Typ.subst(s, tpat, typ),
      ~f_exp=
        (continue, exp) =>
          switch (term_of(exp)) {
          | TypFun(utpat, _, _) =>
            switch (TPat.tyvar_of_utpat(utpat)) {
            | Some(x') when x == x' => exp
            | Some(_)
            | None => continue(exp)
            /* Note that we do not have to worry about capture avoidance, since s will always be closed. */
            }
          | Asc(_)
          | FixF(_)
          | Fun(_)
          | TypAp(_)
          | ListLit(_)
          | Test(_)
          | HintedTest(_)
          | Closure(_)
          | Seq(_)
          | Let(_)
          | Theorem(_)
          | ProofObject(_)
          | Forall(_)
          | Ap(_)
          | BuiltinFun(_)
          | BinOp(_)
          | Cons(_)
          | ListConcat(_)
          | Tuple(_)
          | TupLabel(_)
          | TupleExtension(_)
          | Label(_)
          | Dot(_)
          | Match(_)
          | LivelitName(_)
          | DynamicErrorHole(_)
          | Filter(_)
          | If(_)
          | EmptyHole
          | Invalid(_)
          | Undefined
          | Constructor(_)
          | Var(_)
          | Atom(_)
          | DrvExp(_)
          | MultiHole(_)
          | Deferral(_)
          | TyAlias(_)
          | Use(_)
          | DeferredAp(_)
          | Parens(_)
          | Probe(_)
          | UnOp(_) => continue(exp)
          },
      exp,
    )
  };
};

let rec ty_comparable = (d1, d2) => {
  switch (term_of(d1), term_of(d2)) {
  | (Invalid(_), _)
  | (EmptyHole, _)
  | (MultiHole(_), _)
  | (DynamicErrorHole(_), _)
  | (Deferral(_), _)
  | (DeferredAp(_), _)
  | (Undefined, _)
  | (Var(_), _)
  | (Let(_), _)
  | (Theorem(_), _)
  | (ProofObject(_), _)
  | (Forall(_), _)
  | (FixF(_), _)
  | (TyAlias(_), _)
  | (TypAp(_), _)
  | (If(_), _)
  | (Seq(_), _)
  | (Test(_), _)
  | (HintedTest(_), _)
  | (Filter(_), _)
  | (Closure(_), _)
  | (Cons(_), _)
  | (ListConcat(_), _)
  | (UnOp(_), _)
  | (BinOp(_), _)
  | (Match(_), _)
  | (Use(_), _)
  | (Dot(_), _)
  | (LivelitName(_), _)
  | (Fun(_), _)
  | (BuiltinFun(_), _)
  | (TypFun(_), _)
  | (TupleExtension(_), _) => false
  | (Probe(d1, _), _) => ty_comparable(d1, d2)
  | (_, Probe(d2, _)) => ty_comparable(d1, d2)
  | (Parens(d1), _) => ty_comparable(d1, d2)
  | (_, Parens(d2)) => ty_comparable(d1, d2)
  | (Asc(d1, _), _) => ty_comparable(d1, d2)
  | (_, Asc(d2, _)) => ty_comparable(d1, d2)
  | (Atom(t1), Atom(t2)) =>
    switch (t1, t2) {
    | (Int(_), Int(_)) => true
    | (Int(_), _) => false
    | (SInt(_), SInt(_)) => true
    | (SInt(_), _) => false
    | (Nat(_), Nat(_)) => true
    | (Nat(_), _) => false
    | (Bool(_), Bool(_)) => true
    | (Bool(_), _) => false
    | (String(_), String(_)) => true
    | (String(_), _) => false
    | (Float(_), Float(_)) => true
    | (Float(_), _) => false
    }
  | (Atom(_), _) => false
  | (DrvExp(_, t1), DrvExp(_, t2)) => t1 == t2
  | (DrvExp(_, _), _) => false
  | (Label(l1), Label(l2)) => l1 == l2
  | (Label(_), _) => false
  | (TupLabel(l1, d1), TupLabel(l2, d2)) =>
    ty_comparable(l1, l2) && ty_comparable(d1, d2)
  | (TupLabel(_), _) => false
  // Note(zhiyao): Listlit checks the consistency of all elements,
  // which is different from Tuple (check pairwise consistency).
  | (ListLit(ds1), ListLit(ds2)) =>
    switch (ds1 @ ds2) {
    | [] => true
    | [hd, ...tl] => List.for_all(ty_comparable(hd), tl)
    }
  | (ListLit(_), _) => false
  | (Tuple(ds1), Tuple(ds2)) =>
    List.length(ds1) == List.length(ds2)
    && List.for_all2(ty_comparable, ds1, ds2)
  | (Tuple(_), _) => false
  | (
      Constructor(_, Some(Some(t1))) |
      Ap(
        _,
        {term: Constructor(_, Some(Some({term: Arrow(_, t1), _}))), _},
        _,
      ),
      Constructor(_, Some(Some(t2))) |
      Ap(
        _,
        {term: Constructor(_, Some(Some({term: Arrow(_, t2), _}))), _},
        _,
      ),
    ) =>
    Typ.is_consistent(Ctx.empty_post_elaboration, t1, t2) && !Typ.has_fun(t1)
  | (Constructor(_), _) => false
  | (Ap(_), _) => false
  };
};

let rec poly_equal = (d1, d2): bool => {
  // With assumption that the types are consistent and have no arrow type
  switch (term_of(d1), term_of(d2)) {
  | (Invalid(_), _)
  | (EmptyHole, _)
  | (MultiHole(_), _)
  | (DynamicErrorHole(_), _)
  | (Deferral(_), _)
  | (DeferredAp(_), _)
  | (Undefined, _)
  | (Var(_), _)
  | (Let(_), _)
  | (Theorem(_), _)
  | (ProofObject(_), _)
  | (Forall(_), _)
  | (FixF(_), _)
  | (TyAlias(_), _)
  | (TypAp(_), _)
  | (If(_), _)
  | (Seq(_), _)
  | (Test(_), _)
  | (HintedTest(_), _)
  | (Filter(_), _)
  | (Closure(_), _)
  | (Cons(_), _)
  | (ListConcat(_), _)
  | (UnOp(_), _)
  | (BinOp(_), _)
  | (TupleExtension(_), _)
  | (Match(_), _)
  | (Dot(_), _)
  | (LivelitName(_), _)
  | (Fun(_), _)
  | (TypFun(_), _)
  | (Use(_), _)
  | (BuiltinFun(_), _) => false
  | (Probe(d1, _), _) => poly_equal(d1, d2)
  | (_, Probe(d2, _)) => poly_equal(d1, d2)
  | (Parens(d1), _) => poly_equal(d1, d2)
  | (_, Parens(d2)) => poly_equal(d1, d2)
  | (Asc(d1, _), _) => poly_equal(d1, d2)
  | (_, Asc(d2, _)) => poly_equal(d1, d2)
  | (Atom(t1), Atom(t2)) =>
    switch (t1, t2) {
    | (Int(n1), Int(n2)) => n1 == n2
    | (Int(_), _) => false
    | (SInt(n1), SInt(n2)) => n1 == n2
    | (SInt(_), _) => false
    | (Nat(n1), Nat(n2)) => n1 == n2
    | (Nat(_), _) => false
    | (Bool(b1), Bool(b2)) => b1 == b2
    | (Bool(_), _) => false
    | (String(s1), String(s2)) => s1 == s2
    | (String(_), _) => false
    | (Float(f1), Float(f2)) => f1 == f2
    | (Float(_), _) => false
    }
  | (Atom(_), _) => false
  | (DrvExp(d1, _), DrvExp(d2, _)) => Drv.Any.eq(d1, d2, ~skip_hole=false)
  | (DrvExp(_, _), _) => false
  | (Label(l1), Label(l2)) => l1 == l2
  | (Label(_), _) => false
  | (TupLabel(l1, d1), TupLabel(l2, d2)) =>
    poly_equal(l1, l2) && poly_equal(d1, d2)
  | (TupLabel(_), _) => false
  | (ListLit(ds1), ListLit(ds2)) =>
    List.length(ds1) == List.length(ds2)
    && List.for_all2(poly_equal, ds1, ds2)
  | (ListLit(_), _) => false
  | (Tuple(ds1), Tuple(ds2)) =>
    List.length(ds1) == List.length(ds2)
    && List.for_all2(poly_equal, ds1, ds2)
  | (Tuple(_), _) => false
  | (Constructor(c1, _), Constructor(c2, _)) => c1 == c2
  | (Constructor(_), _) => false
  // Note: Only Constructor Ap is comparable
  | (
      Ap(_, {term: Constructor(c1, _), _}, d1),
      Ap(_, {term: Constructor(c2, _), _}, d2),
    ) =>
    c1 == c2 && poly_equal(d1, d2)
  | (Ap(_), _) => false
  };
};
