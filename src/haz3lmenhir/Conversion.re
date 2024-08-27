include Sexplib.Std;

module FilterAction = {
  open Haz3lcore.FilterAction;
  let of_menhir_ast = (a: AST.filter_action): t => {
    switch (a) {
    | Eval => (Eval, All)
    | Pause => (Step, One)
    | Debug => (Step, All)
    | Hide => (Eval, One)
    };
  };
};

module Operators = {
  open Haz3lcore.Operators;

  let op_un_meta_of_menhir_ast = (op: AST.op_un_meta) => {
    switch (op) {
    | Unquote => Unquote
    };
  };

  let op_un_int_of_menhir_ast = (op: AST.op_un_int): op_un_int => {
    switch (op) {
    | Minus => Minus
    };
  };

  let op_un_bool_of_menhir_ast = (op: AST.op_un_bool): op_un_bool => {
    switch (op) {
    | Not => Not
    };
  };

  let op_un_of_menhir_ast = (op: AST.op_un): op_un => {
    switch (op) {
    | Meta(meta) => Meta(op_un_meta_of_menhir_ast(meta))
    | Int(i) => Int(op_un_int_of_menhir_ast(i))
    | Bool(b) => Bool(op_un_bool_of_menhir_ast(b))
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let float_op_of_menhir_ast = (op: AST.op_bin_float): op_bin_float => {
    switch (op) {
    | Plus => Plus
    | Minus => Minus
    | Times => Times
    | Power => Power
    | Divide => Divide
    | LessThan => LessThan
    | LessThanOrEqual => LessThanOrEqual
    | GreaterThan => GreaterThan
    | GreaterThanOrEqual => GreaterThanOrEqual
    | Equals => Equals
    | NotEquals => NotEquals
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let bool_op_of_menhir_ast = (op: AST.op_bin_bool): op_bin_bool => {
    switch (op) {
    | And => And
    | Or => Or
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let int_op_of_menhir_ast = (op: AST.op_bin_int): op_bin_int => {
    switch (op) {
    | Plus => Plus
    | Minus => Minus
    | Times => Times
    | Power => Power
    | Divide => Divide
    | LessThan => LessThan
    | LessThanOrEqual => LessThanOrEqual
    | GreaterThan => GreaterThan
    | GreaterThanOrEqual => GreaterThanOrEqual
    | Equals => Equals
    | NotEquals => NotEquals
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let of_menhir_ast = (op: AST.binOp): op_bin => {
    switch (op) {
    | IntOp(op_int) => Int(int_op_of_menhir_ast(op_int))
    | FloatOp(op_float) => Float(float_op_of_menhir_ast(op_float))
    | BoolOp(op_bool) => Bool(bool_op_of_menhir_ast(op_bool))
    };
  };
};

module rec Exp: {
  let term_of_menhir_ast: AST.exp => Haz3lcore.Exp.term;
  let of_menhir_ast: AST.exp => Haz3lcore.Exp.t;
} = {
  let rec term_of_menhir_ast = (exp: AST.exp): Haz3lcore.Exp.term => {
    switch (exp) {
    | InvalidExp(s) => Invalid(s)
    | Int(i) => Int(i)
    | Float(f) => Float(f)
    | String(s) => String(s)
    | Bool(b) => Bool(b)
    | Var(x) => Var(x)
    | Constructor(x, ty) => Constructor(x, Typ.of_menhir_ast(ty))
    | Deferral(pos) =>
      switch (pos) {
      | InAp => Deferral(InAp)
      | OutsideAp => Deferral(OutsideAp)
      }
    | ListExp(l) => ListLit(List.map(of_menhir_ast, l))
    | TupleExp(t) =>
      if (List.length(t) == 1) {
        Parens(of_menhir_ast(List.hd(t)));
      } else {
        Tuple(List.map(of_menhir_ast, t));
      }
    | Let(p, e1, e2) =>
      Let(Pat.of_menhir_ast(p), of_menhir_ast(e1), of_menhir_ast(e2))
    | FixF(p, e) => FixF(Pat.of_menhir_ast(p), of_menhir_ast(e), None)
    | TypFun(t, e) => TypFun(TPat.of_menhir_ast(t), of_menhir_ast(e), None)
    | Undefined => Undefined
    | TyAlias(tp, ty, e) =>
      TyAlias(
        TPat.of_menhir_ast(tp),
        Typ.of_menhir_ast(ty),
        of_menhir_ast(e),
      )
    | BuiltinFun(s) => BuiltinFun(s)
    | DeferredAp(f, a) => DeferredAp(of_menhir_ast(f), [of_menhir_ast(a)])
    | Fun(p, e, name_opt) =>
      switch (name_opt) {
      | Some(name_str) =>
        Fun(
          Pat.of_menhir_ast(p),
          of_menhir_ast(e),
          None,
          Some(name_str ++ "+"),
        )
      | None => Fun(Pat.of_menhir_ast(p), of_menhir_ast(e), None, None)
      }
    | ApExp(e1, e2) =>
      Ap(Haz3lcore.Operators.Forward, of_menhir_ast(e1), of_menhir_ast(e2))
    | BinExp(e1, op, e2) =>
      BinOp(
        Operators.of_menhir_ast(op),
        of_menhir_ast(e1),
        of_menhir_ast(e2),
      )

    | If(e1, e2, e3) =>
      If(of_menhir_ast(e1), of_menhir_ast(e2), of_menhir_ast(e3))
    | CaseExp(e, l) =>
      let d_scrut = of_menhir_ast(e);
      let d_rules =
        List.map(
          ((pat, exp)) => (Pat.of_menhir_ast(pat), of_menhir_ast(exp)),
          l,
        );
      Match(d_scrut, d_rules);
    | Cast(e, t1, t2) =>
      Cast(of_menhir_ast(e), Typ.of_menhir_ast(t1), Typ.of_menhir_ast(t2))
    | FailedCast(e, t1, t2) =>
      FailedCast(
        of_menhir_ast(e),
        Typ.of_menhir_ast(t1),
        Typ.of_menhir_ast(t2),
      )
    | EmptyHole => EmptyHole
    | Seq(e1, e2) => Seq(of_menhir_ast(e1), of_menhir_ast(e2))
    | Test(e) => Test(of_menhir_ast(e))
    | Cons(e1, e2) => Cons(of_menhir_ast(e1), of_menhir_ast(e2))
    | ListConcat(e1, e2) =>
      ListConcat(of_menhir_ast(e1), of_menhir_ast(e2))
    | Filter(a, cond, body) =>
      let dcond = of_menhir_ast(cond);
      let dbody = of_menhir_ast(body);
      let act = FilterAction.of_menhir_ast(a);
      Filter(
        Haz3lcore.TermBase.StepperFilterKind.Filter({pat: dcond, act}),
        dbody,
      );
    | TypAp(e, ty) => TypAp(of_menhir_ast(e), Typ.of_menhir_ast(ty))
    | UnOp(op, e) =>
      UnOp(Operators.op_un_of_menhir_ast(op), of_menhir_ast(e))
    | DynamicErrorHole(e, s) =>
      DynamicErrorHole(
        of_menhir_ast(e),
        Haz3lcore.InvalidOperationError.t_of_sexp(sexp_of_string(s)),
      )
    };
  }
  and of_menhir_ast = (exp: AST.exp): Haz3lcore.Exp.t => {
    Haz3lcore.IdTagged.fresh(term_of_menhir_ast(exp));
  };
}
and Typ: {
  let of_menhir_ast: AST.typ => Haz3lcore.Typ.t;
} = {
  let rec of_menhir_ast = (typ: AST.typ): Haz3lcore.Typ.t => {
    Haz3lcore.IdTagged.fresh(term_of_menhir_ast(typ));
  }
  and term_of_menhir_ast = (typ: AST.typ): Haz3lcore.Typ.term => {
    switch (typ) {
    | InvalidTyp(s) => Unknown(Hole(Invalid(s)))
    | IntType => Int
    | FloatType => Float
    | BoolType => Bool
    | StringType => String
    | UnitType => Prod([])
    | UnknownType(p) =>
      switch (p) {
      | Internal => Unknown(Internal)
      }
    | TupleType(ts) => Prod(List.map(of_menhir_ast, ts))
    | ArrayType(t) => List(of_menhir_ast(t))
    | ArrowType(t1, t2) => Arrow(of_menhir_ast(t1), of_menhir_ast(t2))
    };
  };
}
and TPat: {
  let of_menhir_ast: AST.tpat => Haz3lcore.TPat.t;
  let term_of_menhir_ast: AST.tpat => Haz3lcore.TPat.term;
} = {
  let rec term_of_menhir_ast = (tpat: AST.tpat): Haz3lcore.TPat.term => {
    switch (tpat) {
    | InvalidTPat(s) => Invalid(s)
    | EmptyHoleTPat => EmptyHole
    | VarTPat(s) => Var(s)
    };
  }
  and of_menhir_ast = (tpat: AST.tpat) => {
    Haz3lcore.IdTagged.fresh(term_of_menhir_ast(tpat));
  };
}
and Pat: {
  let term_of_menhir_ast: AST.pat => Haz3lcore.Pat.term;
  let of_menhir_ast: AST.pat => Haz3lcore.Pat.t;
} = {
  let rec term_of_menhir_ast = (pat: AST.pat): Haz3lcore.Pat.term => {
    switch (pat) {
    | InvalidPat(s) => Invalid(s)
    | IntPat(i) => Int(i)
    | FloatPat(f) => Float(f)
    | VarPat(x) => Var(x)
    | ConstructorPat(x, ty) => Constructor(x, Typ.of_menhir_ast(ty))
    | StringPat(s) => String(s)
    | TuplePat(pats) => Tuple(List.map(of_menhir_ast, pats))
    | ApPat(pat1, pat2) => Ap(of_menhir_ast(pat1), of_menhir_ast(pat2))
    | ConsPat(p1, p2) => Cons(of_menhir_ast(p1), of_menhir_ast(p2))
    | BoolPat(b) => Bool(b)
    | EmptyHolePat => EmptyHole
    | WildPat => Wild
    | ListPat(l) => ListLit(List.map(of_menhir_ast, l))
    };
  }
  and of_menhir_ast = (pat: AST.pat): Haz3lcore.Pat.t => {
    Haz3lcore.IdTagged.fresh(term_of_menhir_ast(pat));
  };
};
