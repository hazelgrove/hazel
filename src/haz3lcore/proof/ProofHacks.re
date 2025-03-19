open Util;

// Find exp with id using ugly exception route

exception Found(Exp.t);

let find_exp_id = (id: Id.t, exp: Exp.t) =>
  switch (
    Exp.map_term(
      ~f_exp=
        (cont, exp) =>
          if (Exp.rep_id(exp) == id) {
            raise(Found(exp));
          } else {
            cont(exp);
          },
      exp,
    )
  ) {
  | exception (Found(x)) => Some(x)
  | _ => None
  };

let replace_exp_id = (id: Id.t, exp: Exp.t, new_exp: Exp.t) =>
  Exp.map_term(
    ~f_exp=
      (cont, exp) =>
        if (Exp.rep_id(exp) == id) {
          new_exp;
        } else {
          cont(exp);
        },
    exp,
  );

let rec exp_to_pat = (exp: Exp.t): Pat.t => {
  let term = exp |> Exp.term_of;
  let rewrap: Pat.term => Pat.t =
    term => IdTagged.{term, copied: exp.copied, ids: exp.ids};
  switch (term) {
  | Invalid(x) => rewrap(Invalid(x))
  | EmptyHole => rewrap(EmptyHole)
  | MultiHole(xs) => rewrap(MultiHole(xs))
  | Bool(b) => rewrap(Bool(b))
  | Int(i) => rewrap(Int(i))
  | Float(f) => rewrap(Float(f))
  | String(s) => rewrap(String(s))
  | ListLit(xs) => rewrap(ListLit(List.map(exp_to_pat, xs)))
  | Constructor(c, t) => rewrap(Constructor(c, t))
  | Cons(e1, e2) => rewrap(Cons(exp_to_pat(e1), exp_to_pat(e2)))
  | Var(x) => rewrap(Var(x))
  | Tuple(xs) => rewrap(Tuple(List.map(exp_to_pat, xs)))
  | Parens(e) => rewrap(Parens(exp_to_pat(e)))
  | Ap(_, e1, e2) => rewrap(Ap(exp_to_pat(e1), exp_to_pat(e2)))
  | Cast(e, t1, t2) => rewrap(Cast(exp_to_pat(e), t1, t2))
  | _ => MultiHole([Exp(exp)]) |> Pat.fresh
  };
};

let rec pat_to_exp = (pat: Pat.t): Exp.t => {
  let term = pat |> Pat.term_of;
  let rewrap: Exp.term => Exp.t =
    term => IdTagged.{term, copied: pat.copied, ids: pat.ids};
  switch (term) {
  | Invalid(x) => rewrap(Invalid(x))
  | EmptyHole => rewrap(EmptyHole)
  | MultiHole(xs) => rewrap(MultiHole(xs))
  | Wild => rewrap(Bool(true))
  | Bool(b) => rewrap(Bool(b))
  | Int(i) => rewrap(Int(i))
  | Float(f) => rewrap(Float(f))
  | String(s) => rewrap(String(s))
  | ListLit(xs) => rewrap(ListLit(List.map(pat_to_exp, xs)))
  | Constructor(c, t) => rewrap(Constructor(c, t))
  | Cons(e1, e2) => rewrap(Cons(pat_to_exp(e1), pat_to_exp(e2)))
  | Var(x) => rewrap(Var(x))
  | Tuple(xs) => rewrap(Tuple(List.map(pat_to_exp, xs)))
  | Parens(e) => rewrap(Parens(pat_to_exp(e)))
  | Ap(e1, e2) => rewrap(Ap(Forward, pat_to_exp(e1), pat_to_exp(e2)))
  | Cast(e, t1, t2) => rewrap(Cast(pat_to_exp(e), t1, t2))
  };
};

let add_wrapping_function = (pat: Pat.t): Exp.t => {
  Fun(pat, EmptyHole |> Exp.fresh, None, None) |> Exp.fresh;
};

let remove_wrapping_function = (exp: Exp.t): Pat.t => {
  switch (exp |> Exp.term_of) {
  | Fun(p, _, _, _) => p
  | _ => MultiHole([Exp(exp)]) |> Pat.fresh
  };
};

let rec get_inductive_hypotheses = (m: Statics.Map.t, t: Typ.t, p: Pat.t) => {
  switch (p |> Pat.term_of) {
  | Invalid(_) => []
  | EmptyHole => []
  | MultiHole(_) => []
  | Wild => []
  | Bool(_) => []
  | Int(_) => []
  | Float(_) => []
  | String(_) => []
  | ListLit(xs) =>
    List.concat(List.map(get_inductive_hypotheses(m, t, _), xs))
  | Constructor(c, _) => []
  | Cons(e1, e2) =>
    get_inductive_hypotheses(m, t, e1) @ get_inductive_hypotheses(m, t, e2)
  | Var(x) =>
    OptUtil.Syntax.(
      {
        let* info = Id.Map.find_opt(Pat.rep_id(p), m);
        let* info =
          switch (info) {
          | Info.InfoPat(pinfo) => Some(pinfo)
          | _ => None
          };
        let t' = info.ty;
        if (Typ.eq(t, t')) {
          Some([x]);
        } else {
          None;
        };
      }
      |> Option.value(~default=[])
    )
  | Tuple(xs) =>
    List.concat(List.map(get_inductive_hypotheses(m, t, _), xs))
  | Parens(e) => get_inductive_hypotheses(m, t, e)
  | Ap(e1, e2) =>
    get_inductive_hypotheses(m, t, e1) @ get_inductive_hypotheses(m, t, e2)
  | Cast(e, _, _) => get_inductive_hypotheses(m, t, e)
  };
};
