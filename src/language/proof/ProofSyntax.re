open Util;

/* Small readers and conversions of the syntax proofs are written in,
 * shared by the checker, its definedness analyses and the UI stepper. */

// Reading through wrappers

/* Looks through the wrappers that change neither what an expression
 * denotes nor what it names: the editor's parens and projectors,
 * ascriptions and closures. */
let rec unwrap = (e: Exp.t): Exp.t =>
  switch (e |> Exp.term_of) {
  | Parens(inner)
  | Projector(_, inner)
  | Asc(inner, _)
  | Closure(_, inner) => unwrap(inner)
  | _ => e
  };

/* Proof syntax stores an `at <n>` index as an expression. */
let exp_to_int = (e: Exp.t): option(int) =>
  switch (e |> Exp.term_of) {
  | Atom(Int(n)) => Bigint.to_int(n)
  | _ => None
  };

/* The name a bare variable spells: a cited rule or fact, a `with <var>`
 * binder, a variable to generalize. */
let exp_to_name = (e: Exp.t): option(Var.t) =>
  switch (unwrap(e) |> Exp.term_of) {
  | Var(name) => Some(name)
  | _ => None
  };

/* The variable a simple binder pattern binds. A destructuring pattern
 * binds no single name. */
let rec pat_to_name = (p: Pat.t): option(Var.t) =>
  switch (p |> Pat.term_of) {
  | Var(x) => Some(x)
  | Parens(p1)
  | Asc(p1, _) => pat_to_name(p1)
  | _ => None
  };

// Expressions as patterns

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
  | Asc(e, t1) => rewrap(Asc(exp_to_pat(e), t1))
  | _ => MultiHole([Exp(exp)]) |> Pat.fresh
  };
};

let add_wrapping_function = (~typ=?, pat: Pat.t): Exp.t => {
  Fun(pat, EmptyHole |> Exp.fresh, typ, None) |> Exp.fresh;
};

let rec remove_wrapping_function = (exp: Exp.t): Pat.t => {
  switch (exp |> Exp.term_of) {
  | Fun(p, _, _, _) => p
  | Asc(e, _) => remove_wrapping_function(e) // see https://github.com/hazelgrove/hazel/issues/1586
  | _ => MultiHole([Exp(exp)]) |> Pat.fresh
  };
};

// Erasing theorems

let strip_theorems = (exp: Exp.t): Exp.t => {
  Exp.map_term(
    ~f_exp=
      (cont, exp) => {
        switch (exp |> Exp.term_of) {
        | Theorem(_, _, _, e2) => cont(e2)
        | _ => cont(exp)
        }
      },
    exp,
  );
};
