open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  outer: list(Filter.t),
  active: option(Filter.t),
  inner: list(Filter.t),
};

let empty = (active: option(Filter.t)): t => {outer: [], active, inner: []};

let rec matches_exp = (d: TermBase.UExp.t, f: TermBase.UExp.t): bool => {
  switch (d.term, f.term) {
  | (_, EmptyHole) => true
  | (EmptyHole, _) => false

  | (Triv, Triv) => true

  | (Bool(dv), Bool(fv)) => dv == fv
  | (Int(dv), Int(fv)) => dv == fv
  | (Float(dv), Float(fv)) => dv == fv
  | (String(dv), String(fv)) => dv == fv

  | (Tag(dt), Tag(ft)) => dt == ft

  | (Fun(dp1, d1), Fun(fp1, f1)) =>
    matches_pat(dp1, fp1) && matches_exp(d1, f1)

  | (Var(dx), Var(fx)) => dx == fx

  | (Let(dp, d1, d2), Let(fp, f1, f2)) =>
    matches_pat(dp, fp) && matches_exp(d1, f1) && matches_exp(d2, f2)

  | (Ap(d1, d2), Ap(f1, f2)) => matches_exp(d1, f1) && matches_exp(d2, f2)

  | (Seq(d1, d2), Seq(f1, f2)) =>
    matches_exp(d1, f1) && matches_exp(d2, f2)

  | (Test(d1), Test(f1)) => matches_exp(d1, f1)

  | (Filter(_, _, d1), _) => matches_exp(d1, f)

  | (Parens(d1), Parens(f1)) => matches_exp(d1, f1)

  | (Cons(d1, d2), Cons(f1, f2)) =>
    matches_exp(d1, f1) && matches_exp(d2, f2)

  | (UnOp(d_op_un, d1), UnOp(f_op_un, f1)) =>
    d_op_un == f_op_un && matches_exp(d1, f1)

  | (BinOp(d_op_bin, d1, d2), BinOp(f_op_bin, f1, f2)) =>
    d_op_bin == f_op_bin && matches_exp(d1, f1) && matches_exp(d2, f2)

  | (Match(dscrut, drule), Match(fscrut, frule)) =>
    matches_exp(dscrut, fscrut)
    && (
      switch (
        List.fold_left2(
          (res, (dp, d), (fp, f)) =>
            res && matches_pat(dp, fp) && matches_exp(d, f),
          true,
          drule,
          frule,
        )
      ) {
      | exception (Invalid_argument(_)) => false
      | res => res
      }
    )

  | (_, _) => false
  };
}
and matches_pat = (d: TermBase.UPat.t, f: TermBase.UPat.t): bool => {
  switch (d.term, f.term) {
  | (_, EmptyHole) => true
  | (Wild, Wild) => true
  | (Int(dv), Int(fv)) => dv == fv
  | (Float(dv), Float(fv)) => dv == fv
  | (Bool(dv), Bool(fv)) => dv == fv
  | (String(dv), String(fv)) => dv == fv
  | (Triv, Triv) => true
  | (ListLit(dl), ListLit(fl)) =>
    switch (
      List.fold_left2((res, d, f) => res && matches_pat(d, f), true, dl, fl)
    ) {
    | exception (Invalid_argument(_)) => false
    | res => res
    }
  | (Tag(dt), Tag(ft)) => dt == ft
  | (Var(dx), Var(fx)) => dx == fx
  | (Tuple(dl), Tuple(fl)) =>
    switch (
      List.fold_left2((res, d, f) => res && matches_pat(d, f), true, dl, fl)
    ) {
    | exception (Invalid_argument(_)) => false
    | res => res
    }
  | (Parens(d1), Parens(f1)) => matches_pat(d1, f1)
  | (Ap(d1, d2), Ap(f1, f2)) => matches_pat(d1, f1) && matches_pat(d2, f2)
  | (TypeAnn(d1, dty1), TypeAnn(f1, fty1)) =>
    dty1 == fty1 && matches_pat(d1, f1)
  | (_, _) => false
  };
}
and matches_typ = (d: Typ.t, f: Typ.t) => {
  switch (d, f) {
  | (_, _) => false
  };
}
and matches_rul = (_d: DHExp.rule, _f: DHExp.rule) => {
  false;
};

let map = (f: option(Filter.t) => option(Filter.t), env: t): t => {
  ...env,
  active: f(env.active),
};

let matches = (d: TermBase.UExp.t, env: t): t => {
  let rec matches' = (res, outer, inner: list(Filter.t)): t => {
    switch (inner) {
    | [] => res
    | [hd, ...tl] =>
      if (matches_exp(d, hd.pat)) {
        let res = {outer, active: Some(hd), inner: tl};
        matches'(res, [hd, ...outer], tl);
      } else {
        matches'(res, [hd, ...outer], tl);
      }
    };
  };
  let init = {outer: [], active: None, inner: []};
  let m_inner = matches'({...init, inner: env.inner}, [], env.inner);
  switch (m_inner.active) {
  | Some(ep) => {
      outer: env.outer @ m_inner.outer @ [ep],
      active: Some(ep),
      inner: m_inner.inner,
    }
  | None =>
    switch (env.active) {
    | Some({pat: {term: EmptyHole, _}, _})
    | None =>
      let m_outer = matches'({...init, inner: env.outer}, [], env.outer);
      switch (m_outer.active) {
      | Some(ep) => {
          outer: m_outer.outer @ [ep],
          active: Some(ep),
          inner: m_outer.inner @ env.inner,
        }
      | None => {...env, active: None}
      };
    | Some(ep) =>
      if (matches_exp(d, ep.pat)) {
        env;
      } else {
        let m_outer = matches'({...init, inner: env.outer}, [], env.outer);
        switch (m_outer.active) {
        | Some(ep) => {
            outer: m_outer.outer @ [ep],
            active: Some(ep),
            inner: m_outer.inner @ env.inner,
          }
        | None => {...env, active: None}
        };
      }
    }
  };
};

// let extends = (env': t, env: t): t =>
//   switch (env'.active, env.active) {
//   | (None, Some(_)) => {
//       outer: env.outer,
//       active: env.active,
//       inner: env.inner @ env'.outer @ env'.inner,
//     }
//   | (Some(_), _)
//   | (None, None) => {
//       outer: env.outer @ env.inner @ env'.outer,
//       active: env'.active,
//       inner: env'.inner,
//     }
//   };

let extends = (f: Filter.t, env: t): t => {...env, inner: env.inner @ [f]};
