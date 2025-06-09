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
let rec strip_casts =
  map_term(
    ~f_pat=
      (continue, t) =>
        switch (t.term) {
        | Cast(p, _, _) => strip_casts_pat(p)
        | _ => continue(t)
        },
    ~f_exp=
      (continue, exp) => {
        switch (term_of(exp)) {
        /* Remove casts*/
        | Cast(d, _, _) => strip_casts(d)
        /* Keep failed casts*/
        | FailedCast(_, _, _)
        | _ => continue(exp)
        }
      },
    _,
  )
and strip_casts_pat = (p: Pat.t): Pat.t => {
  Pat.map_term(
    ~f_pat=
      (continue, t) =>
        switch (t.term) {
        | Cast(p, _, _) => strip_casts_pat(p)
        | _ => continue(t)
        },
    ~f_exp=
      (continue, t) =>
        switch (t.term) {
        | Cast(e, _, _) => strip_casts(e)
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
          | Cast(_)
          | FixF(_)
          | Fun(_)
          | TypAp(_)
          | ListLit(_)
          | Test(_)
          | Closure(_)
          | Seq(_)
          | Let(_)
          | Ap(_)
          | BuiltinFun(_)
          | BinOp(_)
          | Cons(_)
          | ListConcat(_)
          | Tuple(_)
          | TupLabel(_)
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
          | FailedCast(_, _, _)
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

let rec ty_consistent = (d1, d2) => {
  // Note(zhiyao): This is a necessary condition for consistency, but not
  // sufficient. If for any reason an Arrow type escapes the type checker,
  // we will not be able to check the inconsistency here, because the type
  // is hidden and not elaborated to DHExp, though it will still be caught as
  // CompareArrow in later stage.
  switch (term_of(d1), term_of(d2)) {
  | (Invalid(_), _)
  | (EmptyHole, _)
  | (MultiHole(_), _)
  | (DynamicErrorHole(_), _)
  | (FailedCast(_), _)
  | (Deferral(_), _)
  | (DeferredAp(_), _)
  | (Undefined, _)
  | (Var(_), _)
  | (Let(_), _)
  | (FixF(_), _)
  | (TyAlias(_), _)
  | (TypAp(_), _)
  | (If(_), _)
  | (Seq(_), _)
  | (Test(_), _)
  | (Filter(_), _)
  | (Closure(_), _)
  | (Cons(_), _)
  | (ListConcat(_), _)
  | (UnOp(_), _)
  | (BinOp(_), _)
  | (Match(_), _)
  | (Probe(_), _)
  | (Use(_), _)
  | (Dot(_), _)
  | (LivelitName(_), _) => false
  | (Parens(d1), _) => ty_consistent(d1, d2)
  | (_, Parens(d2)) => ty_consistent(d1, d2)
  | (Cast(d1, _, _), _) => ty_consistent(d1, d2)
  | (_, Cast(d2, _, _)) => ty_consistent(d1, d2)
  // TODO(zhiyao): are we allowed to compare int/sint/nat?
  | (Atom(t1), Atom(t2)) =>
    switch (t1, t2) {
    | (Int(_) | SInt(_) | Nat(_), Int(_) | SInt(_) | Nat(_)) => true
    | (Int(_) | SInt(_) | Nat(_), _) => false
    | (Bool(_), Bool(_)) => true
    | (Bool(_), _) => false
    | (String(_), String(_)) => true
    | (String(_), _) => false
    | (Float(_), Float(_)) => true
    | (Float(_), _) => false
    }
  | (Atom(_), _) => false
  | (Label(_), Label(_)) => true
  | (Label(_), _) => false
  | (TupLabel(l1, d1), TupLabel(l2, d2)) =>
    ty_consistent(l1, l2) && ty_consistent(d1, d2)
  | (TupLabel(_), _) => false
  // Note(zhiyao): We want to leave err:CompareArrow to be caught in
  // the later stage because we don't have the type information here.
  | (Fun(_) | BuiltinFun(_), Fun(_) | BuiltinFun(_)) => true
  | (Fun(_) | BuiltinFun(_), _) => false
  | (TypFun(_), TypFun(_)) => true
  | (TypFun(_), _) => false
  // Note(zhiyao): Listlit checks the consistency of all elements,
  // which is different from Tuple (check pairwise consistency).
  | (ListLit(ds1), ListLit(ds2)) =>
    switch (ds1 @ ds2) {
    | [] => true
    | [hd, ...tl] => List.for_all(ty_consistent(hd), tl)
    }
  | (ListLit(_), _) => false
  | (Tuple(ds1), Tuple(ds2)) =>
    List.length(ds1) == List.length(ds2)
    && List.for_all2(ty_consistent, ds1, ds2)
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
    Typ.is_consistent(Ctx.empty_post_elaboration, t1, t2)
  | (Constructor(_), _) => false
  | (Ap(_), _) => false
  };
};

let rec ty_has_arrow = (d: t): bool =>
  switch (term_of(d)) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_)
  | FailedCast(_)
  | Deferral(_)
  | DeferredAp(_)
  | Undefined
  | Var(_)
  | Let(_)
  | FixF(_)
  | TyAlias(_)
  | TypAp(_)
  | If(_)
  | Seq(_)
  | Test(_)
  | Filter(_)
  | Closure(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | Match(_)
  | Dot(_)
  | LivelitName(_)
  | Atom(_)
  | Probe(_)
  | Use(_)
  | Label(_) => false
  | Parens(d)
  | Cast(d, _, _)
  | TupLabel(_, d) => ty_has_arrow(d)
  | Fun(_)
  | BuiltinFun(_)
  | TypFun(_) => true
  | ListLit(ds)
  | Tuple(ds) => List.exists(ty_has_arrow, ds)
  | Constructor(_, Some(Some(t))) =>
    Typ.has_arrow(Ctx.empty_post_elaboration, t)
  | Constructor(_) => false
  | Ap(
      _,
      {term: Constructor(_, Some(Some({term: Arrow(_, t), _}))), _},
      d,
    ) =>
    // Note(zhiyao): It's necessary to check the type of the argument because
    // elaborated types may contain Hole.
    Typ.has_arrow(Ctx.empty_post_elaboration, t) || ty_has_arrow(d)
  | Ap(_, _, _) => false
  };

let rec poly_equal = (d1, d2): bool => {
  // With assumption that the types are consistent and have no arrow type
  switch (term_of(d1), term_of(d2)) {
  | (Invalid(_), _)
  | (EmptyHole, _)
  | (MultiHole(_), _)
  | (DynamicErrorHole(_), _)
  | (FailedCast(_), _)
  | (Deferral(_), _)
  | (DeferredAp(_), _)
  | (Undefined, _)
  | (Var(_), _)
  | (Let(_), _)
  | (FixF(_), _)
  | (TyAlias(_), _)
  | (TypAp(_), _)
  | (If(_), _)
  | (Seq(_), _)
  | (Test(_), _)
  | (Filter(_), _)
  | (Closure(_), _)
  | (Cons(_), _)
  | (ListConcat(_), _)
  | (UnOp(_), _)
  | (BinOp(_), _)
  | (Match(_), _)
  | (Dot(_), _)
  | (LivelitName(_), _)
  | (Fun(_), _)
  | (TypFun(_), _)
  | (Probe(_), _)
  | (Use(_), _)
  | (BuiltinFun(_), _) => false
  | (Parens(d1), _) => poly_equal(d1, d2)
  | (_, Parens(d2)) => poly_equal(d1, d2)
  | (Cast(d1, _, _), _) => poly_equal(d1, d2)
  | (_, Cast(d2, _, _)) => poly_equal(d1, d2)
  // TODO(zhiyao): do we want to define equality for atom separately?
  | (Atom(t1), Atom(t2)) => t1 == t2
  | (Atom(_), _) => false
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
