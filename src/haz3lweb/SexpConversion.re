open Haz3lcore;
open Sexplib;

let rec go: Term.UExp.t => Sexp.t =
  term => {
    switch (term.term) {
    | Invalid(string) => Atom("Invalid: " ++ string)
    // | EmptyHole
    // | MultiHole(list(Any.t))
    | Triv => List([])
    | Bool(bool) => Atom(string_of_bool(bool))
    | Int(int) => Atom(string_of_int(int))
    | Float(float) => Atom(string_of_float(float))
    | String(string) => Atom(string)
    | ListLit(list) => List([Sexp.Atom("list")] @ List.map(go, list))
    | Constructor(string) => Atom(string)
    | Fun(pat, exp) => List([Atom("fun"), goUPat(pat), go(exp)])
    | Tuple(list) => List([Sexp.Atom("tuple")] @ List.map(go, list))
    | Var(t) => Atom(t)
    | Let(pat, exp1, exp2) =>
      List([Atom("let"), List([go(exp1), go(exp2)]), goUPat(pat)])
    | TyAlias(pat, typ, exp) =>
      List([Atom("tyAlias"), goUTPat(pat), goTyp(typ), go(exp)])
    | Ap(func, arg) => List([go(func), go(arg)])
    | If(cond, thenBranch, elseBranch) =>
      List([Atom("if"), go(cond), go(thenBranch), go(elseBranch)])
    | Seq(exp1, exp2) => List([Atom("seq"), go(exp1), go(exp2)])
    | Test(t) => List([Atom("test"), go(t)])
    | Cons(head, tail) => List([Atom("cons"), go(head), go(tail)])
    | ListConcat(list1, list2) =>
      List([Atom("listConcat"), go(list1), go(list2)])
    // TODO: how to access TermBase.re un_op_to_string?
    | UnOp(_op_un, a) => List([Atom("op_un"), go(a)])
    // | BinOp(op_bin, a, b) => List([Atom("op_bin"), go(a), go(b)])
    // | Match(exp, [pat, cases]) =>
    //   List([Atom("match"), go(exp), List([goUPat(pat), go(cases)])])
    | _ =>
      print_endline(Term.UExp.show(term));
      Atom("Not implemented"); /* ? for empty hol*/
    };
  }

// likely need UPat, UType, UTPat to Sexp
// (EmptyHole|Wild|Triv|MultiHole _|Int _|Float _|Bool _|String _|ListLit _| Cons (_, _)|Parens _|Ap (_, _)|TypeAnn (_, _))ocamllsp
and goUPat: Term.UPat.t => Sexp.t =
  pat => {
    switch (pat.term) {
    | EmptyHole => Atom("EmptyHole")
    | Wild => Atom("Wild")
    | Triv => Atom("Triv")
    | MultiHole(_) => Atom("MultiHole")
    | Int(_) => Atom("Int")
    | Float(_) => Atom("Float")
    | Bool(_) => Atom("Bool")
    | String(_) => Atom("String")
    | ListLit(_) => Atom("ListPat")
    | Cons(_, _) => Atom("Cons")
    | Parens(_) => Atom("Parens")
    | Ap(_, _) => Atom("Ap")
    | TypeAnn(_, _) => Atom("TypeAnn")
    | Invalid(string) => Atom("Invalid: " ++ string)
    | Var(string) => Atom(string)
    | Constructor(string) => Atom(string)
    | Tuple(list) => List([Sexp.Atom("tuple")] @ List.map(goUPat, list))
    };
  }

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
