open Haz3lcore;
open Sexplib;

let rec go: Term.UExp.t => Sexp.t =
  term => {
    switch (term.term) {
    | Invalid(string) => Atom(string)
    | EmptyHole => Atom("?")
    | MultiHole(_) => Atom("Not implemented")
    | Triv => List([])
    | Bool(bool) => Atom(string_of_bool(bool))
    | Int(int) => Atom(string_of_int(int))
    | Float(float) => Atom(string_of_float(float))
    | String(string) => Atom("\"" ++ string ++ "\"")
    | ListLit(list) => List([Sexp.Atom("list")] @ List.map(go, list))
    | Parens(exp) => go(exp)
    | Constructor(string) => Atom(string)
    | Fun(pat, exp) => List([Atom("fun"), goUPat(pat), go(exp)])
    | Tuple(list) => List([Sexp.Atom("tuple")] @ List.map(go, list))
    | Var(t) => Atom(t)
    | Let(pat, exp1, exp2) =>
      List([Atom("let"), List([go(exp1), go(exp2)]), goUPat(pat)])
    | TyAlias(pat, typ, exp) =>
      List([Atom("type"), goUTPat(pat), goTyp(typ), go(exp)])
    | Ap(func, arg) => List([go(func), go(arg)])
    | If(cond, thenBranch, elseBranch) =>
      List([Atom("if"), go(cond), go(thenBranch), go(elseBranch)])
    | Seq(exp1, exp2) => List([Atom("seq"), go(exp1), go(exp2)])
    | Test(t) => List([Atom("test"), go(t)])
    | Cons(head, tail) => List([Atom("cons"), go(head), go(tail)])
    | ListConcat(list1, list2) => List([Atom("@"), go(list1), go(list2)])
    | UnOp(op_un, a) =>
      List([Atom(TermBase.UExp.un_op_to_string(op_un)), go(a)])
    | BinOp(op_bin, a, b) =>
      List([Atom(TermBase.UExp.bin_op_to_string(op_bin)), go(a), go(b)])
    | Match(exp, cases) =>
      List([Atom("case"), go(exp), List(List.map(goRule, cases))])
    };
  }

and goRule: ((Term.UPat.t, Term.UExp.t)) => Sexp.t = {
  ((pat, exp)) => List([goUPat(pat), go(exp)]);
}

and goUPat: Term.UPat.t => Sexp.t =
  pat => {
    switch (pat.term) {
    | EmptyHole => Atom("?")
    | Triv => List([])
    | MultiHole(_) => Atom("Not implemented")
    | Int(x) => Atom(string_of_int(x))
    | Float(x) => Atom(string_of_float(x))
    | Bool(x) => Atom(string_of_bool(x))
    | String(x) => Atom("\"" ++ x ++ "\"")
    | ListLit(list) => List([Sexp.Atom("list")] @ List.map(goUPat, list))
    | Cons(head, tail) =>
      List([Sexp.Atom("cons"), goUPat(head), goUPat(tail)])
    | Parens(pat) => goUPat(pat)
    | Ap(pat1, pat2) => List([goUPat(pat1), goUPat(pat2)])
    | Invalid(string) => Atom(string)
    | Var(t) => Atom(t)
    | Constructor(string) => Atom(string)
    | Tuple(list) => List([Sexp.Atom("tuple")] @ List.map(goUPat, list))
    | Wild => Atom("_")
    | TypeAnn(pat, typ) => List([goUPat(pat), goTyp(typ)])
    // | _ => Atom("Not implemented")
    };
  }

// Only for type declarations!
and goUTPat: Term.UTPat.t => Sexp.t =
  pat => {
    switch (pat.term) {
    | EmptyHole => Atom("EmptyHole")
    | MultiHole(_) => Atom("MultiHole")
    | Invalid(string) => Atom("Invalid: " ++ string)
    | Var(string) => Atom(string)
    };
  }

and goTyp: Term.UTyp.t => Sexp.t =
  typ => {
    switch (typ.term) {
    | EmptyHole => Atom("EmptyHole")
    // | Wild => Atom("Wild")
    // | Triv => Atom("Triv")
    | MultiHole(_) => Atom("MultiHole")
    | Int => Atom("Int")
    | Float => Atom("Float")
    | Bool => Atom("Bool")
    | String => Atom("String")
    | List(_) => Atom("List")
    | Tuple(list) => List([Sexp.Atom("tuple")] @ List.map(goTyp, list))
    // | Fun(_, _) => Atom("Fun")
    | Var(string) => Atom(string)
    | Constructor(string) => Atom(string)
    // | TypeApp(_, _) => Atom("TypeApp")
    // | TypeAnn(_, _) => Atom("TypeAnn")
    | Invalid(string) => Atom("Invalid: " ++ string)
    | Sum(_list) => Atom("list") //List([Sexp.Atom("sum")] @ List.map(goTyp, list))
    | Arrow(_, _) => Atom("Arrow")
    | Parens(_) => Atom("Parens")
    | Ap(_, _) => Atom("Ap")
    };
  };

let rec sexp_of_uexp: Sexplib.Sexp.t => string =
  sexp => {
    switch (sexp) {
    | Atom(string) => string
    | List([Atom("fun"), pat, exp]) =>
      "fun " ++ sexp_of_uexp(pat) ++ " -> " ++ sexp_of_uexp(exp)
    | List([Atom("case"), exp, List(cases)]) =>
      "case "
      ++ sexp_of_uexp(exp)
      ++ "\n"
      ++ String.concat("\n", List.map(rule_to_string, cases))
      ++ "\nend"
    | List(list) =>
      "(" ++ String.concat(" ", List.map(sexp_of_uexp, list)) ++ ")"
    };
  }

and rule_to_string: Sexplib.Sexp.t => string =
  sexp => {
    switch (sexp) {
    | List([pat, exp]) =>
      " | " ++ sexp_of_uexp(pat) ++ " => " ++ sexp_of_uexp(exp)
    | _ => failwith("expected rule")
    };
  };
