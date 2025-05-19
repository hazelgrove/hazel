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
    term => {
      term,
      annotation: exp.annotation,
    };
  switch (term) {
  | Invalid(x) => rewrap(Invalid(x))
  | EmptyHole => rewrap(EmptyHole)
  | MultiHole(xs) => rewrap(MultiHole(xs))
  | Atom(Bool(b)) => rewrap(Atom(Bool(b)))
  | Atom(Int(i)) => rewrap(Atom(Int(i)))
  | Atom(Float(f)) => rewrap(Atom(Float(f)))
  | Atom(String(s)) => rewrap(Atom(String(s)))
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
    term => {
      term,
      annotation: pat.annotation,
    };
  switch (term) {
  | Invalid(x) => rewrap(Invalid(x))
  | EmptyHole => rewrap(EmptyHole)
  | MultiHole(xs) => rewrap(MultiHole(xs))
  | Wild => rewrap(Atom(Bool(true)))
  | Atom(a) => rewrap(Atom(a))
  | ListLit(xs) => rewrap(ListLit(List.map(pat_to_exp, xs)))
  | Constructor(c, t) => rewrap(Constructor(c, t))
  | Cons(e1, e2) => rewrap(Cons(pat_to_exp(e1), pat_to_exp(e2)))
  | Var(x) => rewrap(Var(x))
  | Tuple(xs) => rewrap(Tuple(List.map(pat_to_exp, xs)))
  | Parens(e) => rewrap(Parens(pat_to_exp(e)))
  | Ap(e1, e2) => rewrap(Ap(Forward, pat_to_exp(e1), pat_to_exp(e2)))
  | Cast(e, t1, t2) => rewrap(Cast(pat_to_exp(e), t1, t2))
  | Label(l) => rewrap(Label(l))
  | TupLabel(l, e) => rewrap(TupLabel(pat_to_exp(l), pat_to_exp(e)))
  | Probe(e, probe) => rewrap(Probe(pat_to_exp(e), probe))
  };
};

let add_wrapping_function = (~typ=?, pat: Pat.t): Exp.t => {
  Fun(pat, EmptyHole |> Exp.fresh, typ, None) |> Exp.fresh;
};

let rec remove_wrapping_function = (exp: Exp.t): Pat.t => {
  switch (exp |> Exp.term_of) {
  | Fun(p, _, _, _) => p
  | Cast(e, _, _) => remove_wrapping_function(e) // see https://github.com/hazelgrove/hazel/issues/1586
  | _ => MultiHole([Exp(exp)]) |> Pat.fresh
  };
};
