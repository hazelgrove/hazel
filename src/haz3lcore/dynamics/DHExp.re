/* DHExp.re

   This module is specifically for dynamic expressions. They are stored
   using the same data structure as user expressions, have been modified
   slightly as described in Elaborator.re.
   */

include Exp;

let term_of: t => term = IdTagged.term_of;
let fast_copy: (Id.t, t) => t = IdTagged.fast_copy;

let mk = (ids, term): t => {
  {ids, copied: true, term};
};

// TODO: make this function emit a map of changes
let replace_all_ids =
  map_term(
    ~f_exp=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
    ~f_pat=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
    ~f_typ=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
    ~f_tpat=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
    ~f_rul=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
  );

// TODO: make this function emit a map of changes
let repair_ids =
  map_term(
    ~f_exp=
      (continue, exp) =>
        if (exp.copied) {
          replace_all_ids(exp);
        } else {
          continue(exp);
        },
    _,
  );

// Also strips static error holes - kinda like unelaboration
let rec strip_casts =
  map_term(
    ~f_exp=
      (continue, exp) => {
        switch (term_of(exp)) {
        /* Leave non-casts unchanged */
        | Tuple(_)
        | Cons(_)
        | ListConcat(_)
        | ListLit(_)
        | MultiHole(_)
        | Seq(_)
        | Filter(_)
        | Let(_)
        | FixF(_)
        | TyAlias(_)
        | Fun(_)
        | Ap(_)
        | Deferral(_)
        | DeferredAp(_)
        | Test(_)
        | BuiltinFun(_)
        | UnOp(_)
        | BinOp(_)
        | Match(_)
        | Parens(_)
        | EmptyHole
        | Invalid(_)
        | Var(_)
        | Bool(_)
        | Int(_)
        | Float(_)
        | String(_)
        | Constructor(_)
        | DynamicErrorHole(_)
        | Closure(_)
        | TypFun(_)
        | TypAp(_)
        | Undefined
        | If(_) => continue(exp)
        /* Remove casts*/
        | FailedCast(d, _, _)
        | Cast(d, _, _) => strip_casts(d)
        }
      },
    _,
  );

let assign_name_if_none = (t, name) => {
  let (term, rewrap) = unwrap(t);
  switch (term) {
  | Fun(arg, ty, body, None) => Fun(arg, ty, body, name) |> rewrap
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
          | Match(_)
          | DynamicErrorHole(_)
          | Filter(_)
          | If(_)
          | EmptyHole
          | Invalid(_)
          | Undefined
          | Constructor(_)
          | Var(_)
          | Bool(_)
          | Int(_)
          | Float(_)
          | String(_)
          | FailedCast(_, _, _)
          | MultiHole(_)
          | Deferral(_)
          | TyAlias(_)
          | DeferredAp(_)
          | Parens(_)
          | UnOp(_) => continue(exp)
          },
      exp,
    )
  };
};

let rec ty_comparable = (exp: t): bool =>
  switch (term_of(exp)) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_)
  | FailedCast(_)
  | Deferral(_)
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
  | Parens(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | Match(_)
  | Cast(_) => false
  // Note: Fun, TypFun, BuiltinFun are not comparable but will be handled elsewhere
  | DeferredAp(_)
  | Fun(_)
  | TypFun(_)
  | BuiltinFun(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_) => true
  | ListLit(tys)
  | Tuple(tys) => tys |> List.for_all(ty_comparable)
  // Note: Only Constructor Ap is comparable
  | Ap(_, {term: Constructor(_), _}, ty) => ty_comparable(ty)
  | Ap(_) => false
  };

type poly_equal_result =
  | Ok(bool)
  | Inconsistent
  | CompareArrow;

let rec poly_equal = (d1, d2) => {
  let d1 = term_of(d1);
  let d2 = term_of(d2);
  switch (d1, d2) {
  // Note(zhiyao): if for any reason an Arrow type escapes the type checker,
  // we will not be able to check consistency of two Arrow types, because
  // Arrow type is not elaborated to DHExp. So we will just treat them as
  // CompareArrow. Ideally, Inconsistent should be found prior to CompareArrow.
  | (Fun(_) | BuiltinFun(_), Fun(_) | BuiltinFun(_))
  | (TypFun(_), TypFun(_)) => CompareArrow
  | (Bool(_), Bool(_))
  | (Int(_), Int(_))
  | (Float(_), Float(_))
  | (String(_), String(_)) => Ok(d1 == d2)
  | (ListLit(ds1), ListLit(ds2))
  | (Tuple(ds1), Tuple(ds2)) =>
    // Note(zhiyao): Notice there is a possibility to create a list of
    // different types without static/dynamic error (is it intended?), e.g.
    // ```hazel
    // let a : ? = 1. in
    // [1, a]
    // ```
    // I wrote is function with the assumption that the list should have the
    // same type, but please be aware of this.
    let rec combine_min = (l1, l2) =>
      switch (l1, l2) {
      | ([], _)
      | (_, []) => []
      | ([x, ...xs], [y, ...ys]) => [(x, y), ...combine_min(xs, ys)]
      };
    let merge_res = (r1, r2): poly_equal_result =>
      switch (r1, r2) {
      | (Ok(b1), Ok(b2)) => Ok(b1 && b2)
      | (Inconsistent, _)
      | (_, Inconsistent) => Inconsistent
      | (CompareArrow, _)
      | (_, CompareArrow) => CompareArrow
      };
    List.fold_left(
      (acc, (d1, d2)) => merge_res(acc, poly_equal(d1, d2)),
      Ok(List.length(ds1) == List.length(ds2)),
      combine_min(ds1, ds2),
    );
  | (Constructor(c1, t1), Constructor(c2, t2)) =>
    switch (Typ.join(~fix=false, [], t1, t2)) {
    | None => Inconsistent
    | Some(ty) when Typ.has_arrow([], ty) => CompareArrow
    | Some(_) => Ok(String.equal(c1, c2))
    }
  | (
      Ap(_, {term: Constructor(_, {term: Arrow(_, t1), _}), _}, _),
      Constructor(_, t2),
    )
  | (
      Constructor(_, t1),
      Ap(_, {term: Constructor(_, {term: Arrow(_, t2), _}), _}, _),
    ) =>
    switch (Typ.join(~fix=false, [], t1, t2)) {
    | None => Inconsistent
    | Some(ty) when Typ.has_arrow([], ty) => CompareArrow
    | Some(_) => Ok(false)
    }
  | (
      Ap(_, {term: Constructor(c1, {term: Arrow(_, t1), _}), _}, d1),
      Ap(_, {term: Constructor(c2, {term: Arrow(_, t2), _}), _}, d2),
    ) =>
    switch (Typ.join(~fix=false, [], t1, t2)) {
    | None => Inconsistent
    | Some({term: Arrow(_, ty), _}) when Typ.has_arrow([], ty) =>
      CompareArrow
    | Some(_) => String.equal(c1, c2) ? poly_equal(d1, d2) : Ok(false)
    }
  | _ => Inconsistent
  };
};
