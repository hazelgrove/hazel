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
        /* Remove casts*/
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
