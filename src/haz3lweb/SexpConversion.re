open Haz3lcore;
open Sexplib;

let rec go: Term.UExp.t => Sexp.t =
  term => {
    switch (term.term) {
    // | Invalid(string)
    // | EmptyHole
    // | MultiHole(list(Any.t))
    | Triv => List([])
    | Bool(bool) => Atom(string_of_bool(bool))
    | Int(int) => Atom(string_of_int(int))
    | Float(float) => Atom(string_of_float(float))
    | String(string) => Atom(string)
    | ListLit(list) => List([Sexp.Atom("list")] @ List.map(go, list))
    // | Constructor(string)
    // | Fun(UPat.t, t)
    // | Tuple(list(t))
    // | Var(Var.t)
    // | Let(UPat.t, t, t)
    // | TyAlias(UTPat.t, UTyp.t, t)
    // | Ap(t, t)
    // | If(t, t, t)
    // | Seq(t, t)
    // | Test(t)
    // | Parens(t) // (
    // | Cons(t, t)
    // | ListConcat(t, t)
    // | UnOp(op_un, t)
    // | BinOp(op_bin, t, t)
    // | Match(t, list((UPat.t, t)))
    | _ =>
      print_endline(Term.UExp.show(term));
      Atom("Not implemented");
    };
  };

// likely need UPat, UType, UTPat to Sexp

// ? for empty hole