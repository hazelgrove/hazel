open Sexplib.Std;

let continue = x => x;
let stop = (_, x) => x;

/* TODO[Matt]: Explain map_term */

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(Exp.t)
    | Pat(Pat.t)
    | Typ(TypTerm.t)
    | TPat(TPat.t)
    | Rul(Rul.t)
    | Nul(unit)
    | Any(unit);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (TypTerm.t => TypTerm.t, TypTerm.t) => TypTerm.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(Exp.t)
    | Pat(Pat.t)
    | Typ(TypTerm.t)
    | TPat(TPat.t)
    | Rul(Rul.t)
    | Nul(unit)
    | Any(unit);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let rec_call = y =>
      switch (y) {
      | Exp(x) =>
        Exp(Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Pat(x) =>
        Pat(Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Typ(x) =>
        Typ(
          TypTerm.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_any,
            x,
          ),
        )
      | TPat(x) =>
        TPat(
          TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x),
        )
      | Rul(x) =>
        Rul(Rul.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Nul () => Nul()
      | Any () => Any()
      };
    x |> f_any(rec_call);
  };
}
and Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | StaticErrorHole(Id.t, t)
    | DynamicErrorHole(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t)
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Fun(
        Pat.t,
        t,
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | Tuple(list(t))
    | Var(Var.t)
    | Let(Pat.t, t, t)
    | FixF(Pat.t, t, [@show.opaque] option(ClosureEnvironment.t))
    | TyAlias(TPat.t, TypTerm.t, t)
    | Ap(Operators.ap_direction, t, t)
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Filter(StepperFilterKind.t, t)
    | Closure([@show.opaque] ClosureEnvironment.t, t)
    | Parens(t) // (
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(Operators.op_un, t)
    | BinOp(Operators.op_bin, t, t)
    | BuiltinFun(string)
    | Match(t, list((Pat.t, t)))
    | Cast(t, Typ.t, Typ.t)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (TypTerm.t => TypTerm.t, TypTerm.t) => TypTerm.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole // Combine the problems into one construct
    | MultiHole(list(Any.t))
    | StaticErrorHole(Id.t, t)
    | DynamicErrorHole(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t) // TODO: get rid of failedcast
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Fun(
        Pat.t,
        t,
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | Tuple(list(t))
    | Var(Var.t)
    | Let(Pat.t, t, t)
    | FixF(Pat.t, t, [@show.opaque] option(ClosureEnvironment.t))
    | TyAlias(TPat.t, TypTerm.t, t)
    | Ap(Operators.ap_direction, t, t) // note: function is always first then argument; even in pipe mode
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Filter(StepperFilterKind.t, t)
    | Closure([@show.opaque] ClosureEnvironment.t, t)
    | Parens(t)
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(Operators.op_un, t)
    | BinOp(Operators.op_bin, t, t)
    | BuiltinFun(string) /// Doesn't currently have a distinguishable syntax
    | Match(t, list((Pat.t, t)))
    | Cast(t, Typ.t, Typ.t)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let typ_map_term =
      TypTerm.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let flt_map_term =
      StepperFilterKind.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_any,
      );
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Bool(_)
        | Int(_)
        | Float(_)
        | Constructor(_)
        | String(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | StaticErrorHole(id, e) => StaticErrorHole(id, exp_map_term(e))
        | DynamicErrorHole(e, err) => DynamicErrorHole(exp_map_term(e), err)
        | FailedCast(e, t1, t2) => FailedCast(exp_map_term(e), t1, t2)
        | ListLit(ts) => ListLit(List.map(exp_map_term, ts))
        | Fun(p, e, env, f) =>
          Fun(pat_map_term(p), exp_map_term(e), env, f)
        | Tuple(xs) => Tuple(List.map(exp_map_term, xs))
        | Let(p, e1, e2) =>
          Let(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | FixF(p, e, env) => FixF(pat_map_term(p), exp_map_term(e), env)
        | TyAlias(tp, t, e) =>
          TyAlias(tpat_map_term(tp), typ_map_term(t), exp_map_term(e))
        | Ap(op, e1, e2) => Ap(op, exp_map_term(e1), exp_map_term(e2))
        | If(e1, e2, e3) =>
          If(exp_map_term(e1), exp_map_term(e2), exp_map_term(e3))
        | Seq(e1, e2) => Seq(exp_map_term(e1), exp_map_term(e2))
        | Test(e) => Test(exp_map_term(e))
        | Filter(f, e) => Filter(flt_map_term(f), exp_map_term(e))
        | Closure(env, e) => Closure(env, exp_map_term(e))
        | Parens(e) => Parens(exp_map_term(e))
        | Cons(e1, e2) => Cons(exp_map_term(e1), exp_map_term(e2))
        | ListConcat(e1, e2) =>
          ListConcat(exp_map_term(e1), exp_map_term(e2))
        | UnOp(op, e) => UnOp(op, exp_map_term(e))
        | BinOp(op, e1, e2) =>
          BinOp(op, exp_map_term(e1), exp_map_term(e2))
        | BuiltinFun(str) => BuiltinFun(str)
        | Match(e, rls) =>
          Match(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        | Cast(e, t1, t2) => Cast(exp_map_term(e), t1, t2)
        },
    };
    x |> f_exp(rec_call);
  };
}
and Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Cons(t, t)
    | Var(Var.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | TypeAnn(t, TypTerm.t)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (TypTerm.t => TypTerm.t, TypTerm.t) => TypTerm.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Cons(t, t)
    | Var(Var.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | TypeAnn(t, TypTerm.t)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let typ_map_term =
      TypTerm.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Wild
        | Bool(_)
        | Int(_)
        | Float(_)
        | Constructor(_)
        | String(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | ListLit(ts) => ListLit(List.map(pat_map_term, ts))
        | Ap(e1, e2) => Ap(pat_map_term(e1), pat_map_term(e2))
        | Cons(e1, e2) => Cons(pat_map_term(e1), pat_map_term(e2))
        | Tuple(xs) => Tuple(List.map(pat_map_term, xs))
        | Parens(e) => Parens(pat_map_term(e))
        | TypeAnn(e, t) => TypeAnn(pat_map_term(e), typ_map_term(t))
        },
    };
    x |> f_pat(rec_call);
  };
}
and TypTerm: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string) // TODO[Matt]: Switch to unknown
    | EmptyHole // TODO[Matt]: Switch to unknown
    | MultiHole(list(Any.t)) // TODO[Matt]: Switch to unknown
    | Int
    | Float
    | Bool
    | String
    | Var(string)
    | List(t)
    | Arrow(t, t)
    | Sum(list(variant))
    | Prod(list(t))
    | Parens(t)
    | Ap(t, t)
    | Rec(TPat.t, t)
  and variant =
    | Variant(Constructor.t, list(Id.t), option(t)) // What are the ids for?
    | BadEntry(t)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (TypTerm.t => TypTerm.t, TypTerm.t) => TypTerm.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | Var(string)
    | List(t)
    | Arrow(t, t)
    | Sum(list(variant))
    | Prod(list(t))
    | Parens(t)
    | Ap(t, t)
    | Rec(TPat.t, t)
  and variant =
    | Variant(Constructor.t, list(Id.t), option(t))
    | BadEntry(t)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let typ_map_term =
      TypTerm.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Bool
        | Int
        | Float
        | String
        | Var(_) => term
        | List(t) => List(typ_map_term(t))
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | Ap(e1, e2) => Ap(typ_map_term(e1), typ_map_term(e2))
        | Prod(xs) => Prod(List.map(typ_map_term, xs))
        | Parens(e) => Parens(typ_map_term(e))
        | Arrow(t1, t2) => Arrow(typ_map_term(t1), typ_map_term(t2))
        | Sum(variants) =>
          Sum(
            List.map(
              fun
              | Variant(c, ids, t) =>
                Variant(c, ids, Option.map(typ_map_term, t))
              | BadEntry(t) => BadEntry(typ_map_term(t)),
              variants,
            ),
          )
        | Rec(tp, t) => Rec(tpat_map_term(tp), typ_map_term(t))
        },
    };
    x |> f_typ(rec_call);
  };
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(string)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (TypTerm.t => TypTerm.t, TypTerm.t) => TypTerm.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(string)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        },
    };
    x |> f_tpat(rec_call);
  };
}
and Rul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | Hole(list(Any.t))
    | Rules(Exp.t, list((Pat.t, Exp.t)))
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (TypTerm.t => TypTerm.t, TypTerm.t) => TypTerm.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | Hole(list(Any.t))
    | Rules(Exp.t, list((Pat.t, Exp.t)))
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid(_) => term
        | Hole(things) => Hole(List.map(any_map_term, things))
        | Rules(e, rls) =>
          Rules(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        },
    };
    x |> f_rul(rec_call);
  };
}

and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(Exp.t);
} = {
  include VarBstMap.Ordered;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(Exp.t);
}

and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let wrap: (EnvironmentId.t, Environment.t) => t;

  let id_of: t => EnvironmentId.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, Exp.t));

  let of_environment: Environment.t => t;

  let id_equal: (t, t) => bool;

  let empty: t;
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(Exp.t);
  let contains: (t, Var.t) => bool;
  let update: (Environment.t => Environment.t, t) => t;
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend: (t, (Var.t, Exp.t)) => t;
  let extend_keep_id: (t, (Var.t, Exp.t)) => t;
  let union: (t, t) => t;
  let union_keep_id: (t, t) => t;
  let map: (((Var.t, Exp.t)) => Exp.t, t) => t;
  let map_keep_id: (((Var.t, Exp.t)) => Exp.t, t) => t;
  let filter: (((Var.t, Exp.t)) => bool, t) => t;
  let filter_keep_id: (((Var.t, Exp.t)) => bool, t) => t;
  let fold: (((Var.t, Exp.t), 'b) => 'b, 'b, t) => 'b;

  let without_keys: (list(Var.t), t) => t;

  let placeholder: t;
} = {
  module Inner: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let wrap: (EnvironmentId.t, Environment.t) => t;

    let id_of: t => EnvironmentId.t;
    let map_of: t => Environment.t;
  } = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (EnvironmentId.t, Environment.t);

    let wrap = (ei, map): t => (ei, map);

    let id_of = ((ei, _)) => ei;
    let map_of = ((_, map)) => map;
    let (sexp_of_t, t_of_sexp) =
      StructureShareSexp.structure_share_here(id_of, sexp_of_t, t_of_sexp);
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = map => {
    let ei = Id.mk();
    wrap(ei, map);
  };

  /* Equals only needs to check environment id's (faster than structural equality
   * checking.) */
  let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

  let empty = Environment.empty |> of_environment;

  let is_empty = env => env |> map_of |> Environment.is_empty;

  let length = env => Environment.length(map_of(env));

  let lookup = (env, x) =>
    env |> map_of |> (map => Environment.lookup(map, x));

  let contains = (env, x) =>
    env |> map_of |> (map => Environment.contains(map, x));

  let update = (f, env) => env |> map_of |> f |> of_environment;

  let update_keep_id = (f, env) => env |> map_of |> f |> wrap(env |> id_of);

  let extend = (env, xr) =>
    env |> update(map => Environment.extend(map, xr));

  let extend_keep_id = (env, xr) =>
    env |> update_keep_id(map => Environment.extend(map, xr));

  let union = (env1, env2) =>
    env2 |> update(map2 => Environment.union(env1 |> map_of, map2));

  let union_keep_id = (env1, env2) =>
    env2 |> update_keep_id(map2 => Environment.union(env1 |> map_of, map2));

  let map = (f, env) => env |> update(Environment.mapo(f));

  let map_keep_id = (f, env) => env |> update_keep_id(Environment.mapo(f));

  let filter = (f, env) => env |> update(Environment.filtero(f));

  let filter_keep_id = (f, env) =>
    env |> update_keep_id(Environment.filtero(f));

  let fold = (f, init, env) => env |> map_of |> Environment.foldo(f, init);

  let placeholder = wrap(EnvironmentId.invalid, Environment.empty);

  let without_keys = keys => update(Environment.without_keys(keys));
}
and StepperFilterKind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filter = {
    pat: Exp.t,
    act: FilterAction.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filter(filter)
    | Residue(int, FilterAction.t);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (TypTerm.t => TypTerm.t, TypTerm.t) => TypTerm.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let map: (Exp.t => Exp.t, t) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filter = {
    pat: Exp.t,
    act: FilterAction.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filter(filter)
    | Residue(int, FilterAction.t);

  let map = (mapper, filter) => {
    switch (filter) {
    | Filter({act, pat}) => Filter({act, pat: mapper(pat)})
    | Residue(idx, act) => Residue(idx, act)
    };
  };

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    fun
    | Filter({pat: e, act}) => Filter({pat: exp_map_term(e), act})
    | Residue(i, a) => Residue(i, a);
  };
};
