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

  let of_core = (a: t): AST.filter_action => {
    switch (a) {
    | (Eval, All) => Eval
    | (Step, One) => Pause
    | (Step, All) => Debug
    | (Eval, One) => Hide
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

  let of_core_op_bin = (op: op_bin): AST.binOp => {
    switch (op) {
    | Int(op_int) =>
      IntOp(
        switch (op_int) {
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
        },
      )
    | Float(op_float) =>
      FloatOp(
        switch (op_float) {
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
        },
      )
    | Bool(op_bool) =>
      BoolOp(
        switch (op_bool) {
        | And => And
        | Or => Or
        },
      )
    | String(op_string) =>
      StringOp(
        switch (op_string) {
        | Concat => Concat
        | Equals => Equals
        },
      )
    };
  };

  let of_core_op_un = (op: op_un): AST.op_un => {
    switch (op) {
    | Meta(meta) =>
      Meta(
        switch (meta) {
        | Unquote => Unquote
        },
      )
    | Int(i) =>
      Int(
        switch (i) {
        | Minus => Minus
        },
      )
    | Bool(b) =>
      Bool(
        switch (b) {
        | Not => Not
        },
      )
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
  let string_op_of_menhir_ast = (op: AST.op_bin_string): op_bin_string => {
    switch (op) {
    | Concat => Concat
    | Equals => Equals
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
    | StringOp(op_string) => String(string_op_of_menhir_ast(op_string))
    };
  };
};

module rec Exp: {
  let term_of_menhir_ast: AST.exp => Haz3lcore.Exp.term;
  let of_menhir_ast: AST.exp => Haz3lcore.Exp.t;
  let of_core: Haz3lcore.Exp.t => AST.exp;
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
    | Deferral => Deferral(InAp)
    // switch (pos) {
    // | InAp => Deferral(InAp)
    // | OutsideAp => Deferral(OutsideAp)
    // }
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
    | Fun(p, e, name_opt) =>
      switch (name_opt) {
      | Some(name_str) =>
        Fun(Pat.of_menhir_ast(p), of_menhir_ast(e), None, Some(name_str))
      | None => Fun(Pat.of_menhir_ast(p), of_menhir_ast(e), None, None)
      }
    | ApExp(e1, args) =>
      switch (args) {
      | TupleExp(l) =>
        List.mem(AST.Deferral, l)
          ? DeferredAp(of_menhir_ast(e1), List.map(of_menhir_ast, l))
          : Ap(
              Haz3lcore.Operators.Forward,
              of_menhir_ast(e1),
              of_menhir_ast(args),
            )
      | Deferral => DeferredAp(of_menhir_ast(e1), [args |> of_menhir_ast])

      | _ =>
        Ap(
          Haz3lcore.Operators.Forward,
          of_menhir_ast(e1),
          of_menhir_ast(args),
        )
      }
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
      Filter(Filter({pat: dcond, act}), dbody);
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

  let rec of_core = (exp: Haz3lcore.Exp.t): AST.exp => {
    switch (exp.term) {
    | Invalid(_) => InvalidExp("Invalid")
    | Int(i) => Int(i)
    | Float(f) => Float(f)
    | String(s) => String(s)
    | Bool(b) => Bool(b)
    | Var(x) => Var(x)
    | Deferral(InAp) => Deferral
    | ListLit(l) => ListExp(List.map(of_core, l))
    | Tuple(l) => TupleExp(List.map(of_core, l))
    | Let(p, e1, e2) => Let(Pat.of_core(p), of_core(e1), of_core(e2))
    | FixF(p, e, _) => FixF(Pat.of_core(p), of_core(e))
    | TypFun(tp, e, _) => TypFun(TPat.of_core(tp), of_core(e))
    | Undefined => Undefined
    | TyAlias(tp, ty, e) =>
      TyAlias(TPat.of_core(tp), Typ.of_core(ty), of_core(e))
    | BuiltinFun(s) => BuiltinFun(s)
    | Ap(Forward, e1, e2) => ApExp(of_core(e1), TupleExp([of_core(e2)]))
    | BinOp(op, e1, e2) =>
      BinExp(of_core(e1), Operators.of_core_op_bin(op), of_core(e2))
    | If(e1, e2, e3) => If(of_core(e1), of_core(e2), of_core(e3))
    | Match(e, l) =>
      CaseExp(
        of_core(e),
        List.map(((p, e)) => (Pat.of_core(p), of_core(e)), l),
      )
    | Cast(e, t1, t2) =>
      Cast(of_core(e), Typ.of_core(t1), Typ.of_core(t2))
    | FailedCast(e, t1, t2) =>
      FailedCast(of_core(e), Typ.of_core(t1), Typ.of_core(t2))
    | EmptyHole => EmptyHole
    | Seq(e1, e2) => Seq(of_core(e1), of_core(e2))
    | Test(e) => Test(of_core(e))
    | Cons(e1, e2) => Cons(of_core(e1), of_core(e2))
    | ListConcat(e1, e2) => ListConcat(of_core(e1), of_core(e2))
    | Filter(Filter({pat, act}), body) =>
      Filter(FilterAction.of_core(act), of_core(pat), of_core(body))
    | TypAp(e, ty) => TypAp(of_core(e), Typ.of_core(ty))
    | UnOp(op, e) => UnOp(Operators.of_core_op_un(op), of_core(e))
    | DynamicErrorHole(e, s) =>
      DynamicErrorHole(
        of_core(e),
        Sexplib.Sexp.to_string(Haz3lcore.InvalidOperationError.sexp_of_t(s)),
      )
    | Deferral(_) => Deferral
    | Filter(_) => raise(Failure("Filter not supported")) // TODO
    | MultiHole(_) => raise(Failure("MultiHole not supported")) // TODO
    | Closure(_) => raise(Failure("Closure not supported")) // TODO
    | Parens(e) => of_core(e)
    | Constructor(s, typ) => Constructor(s, Typ.of_core(typ))
    | DeferredAp(e, es) =>
      ApExp(of_core(e), TupleExp(List.map(of_core, es)))
    | Fun(p, e, _, name_opt) => Fun(Pat.of_core(p), of_core(e), name_opt)
    | Ap(Reverse, _, _) => raise(Failure("Reverse not supported")) // TODO
    };
  };
}
and Typ: {
  let of_menhir_ast: AST.typ => Haz3lcore.Typ.t;

  let of_core: Haz3lcore.Typ.t => AST.typ;
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
      | EmptyHole => Unknown(Hole(EmptyHole))
      }
    | TypVar(s) => Var(s)
    | TupleType([t]) => Parens(of_menhir_ast(t))
    | TupleType(ts) => Prod(List.map(of_menhir_ast, ts))
    | ArrayType(t) => List(of_menhir_ast(t))
    | ArrowType(t1, t2) => Arrow(of_menhir_ast(t1), of_menhir_ast(t2))
    | SumTyp(sumterms) =>
      open Haz3lcore;
      let converted_terms: list(ConstructorMap.variant(TermBase.typ_t)) =
        List.map(
          (sumterm: AST.sumterm): ConstructorMap.variant(TermBase.typ_t) =>
            switch (sumterm) {
            | Variant(name, typ) =>
              Variant(name, [], Option.map(of_menhir_ast, typ))
            | BadEntry(typ) => BadEntry(of_menhir_ast(typ))
            },
          sumterms,
        );
      Sum(converted_terms);
    | ForallType(tp, t) => Forall(TPat.of_menhir_ast(tp), of_menhir_ast(t))
    | RecType(tp, t) => Rec(TPat.of_menhir_ast(tp), of_menhir_ast(t))
    };
  };
  let of_core_type_provenance =
      (p: Haz3lcore.TermBase.type_provenance): AST.typ_provenance => {
    switch (p) {
    | Internal => Internal
    | Hole(EmptyHole) => EmptyHole
    | _ => raise(Failure("Unknown type_provenance"))
    };
  };
  let rec of_core = (typ: Haz3lcore.Typ.t): AST.typ => {
    switch (typ.term) {
    | Int => IntType
    | Float => FloatType
    | String => StringType
    | Bool => BoolType
    | Var(x) => TypVar(x)
    | Prod([]) => UnitType
    | Prod(ts) => TupleType(List.map(of_core, ts))
    | List(t) => ArrayType(of_core(t))
    | Arrow(t1, t2) => ArrowType(of_core(t1), of_core(t2))
    | Unknown(p) => UnknownType(of_core_type_provenance(p))
    | Forall(tp, t) => ForallType(TPat.of_core(tp), of_core(t))
    | Rec(tp, t) => RecType(TPat.of_core(tp), of_core(t))
    | Parens(t) => of_core(t)
    | Ap(_) => raise(Failure("Ap not supported")) // TODO
    | Sum(_) => raise(Failure("Sum not supported")) // TODO
    };
  };
}
and TPat: {
  let of_menhir_ast: AST.tpat => Haz3lcore.TPat.t;
  let term_of_menhir_ast: AST.tpat => Haz3lcore.TPat.term;
  let of_core: Haz3lcore.TPat.t => AST.tpat;
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

  let of_core = (tpat: Haz3lcore.TPat.t): AST.tpat => {
    switch (tpat.term) {
    | EmptyHole => EmptyHoleTPat
    | Var(x) => VarTPat(x)
    | Invalid(i) => InvalidTPat(i)
    | MultiHole(_) => raise(Failure("MultiHole not supported")) // TODO
    };
  };
}
and Pat: {
  let term_of_menhir_ast: AST.pat => Haz3lcore.Pat.term;
  let of_menhir_ast: AST.pat => Haz3lcore.Pat.t;
  let of_core: Haz3lcore.Pat.t => AST.pat;
} = {
  let rec term_of_menhir_ast = (pat: AST.pat): Haz3lcore.Pat.term => {
    switch (pat) {
    | InvalidPat(s) => Invalid(s)
    | IntPat(i) => Int(i)
    | FloatPat(f) => Float(f)
    | CastPat(p, t1, t2) =>
      Cast(of_menhir_ast(p), Typ.of_menhir_ast(t1), Typ.of_menhir_ast(t2))
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
  let rec of_core = (pat: Haz3lcore.Pat.t): AST.pat => {
    switch (pat.term) {
    | Invalid(_) => InvalidPat("Invalid")
    | Int(i) => IntPat(i)
    | Float(f) => FloatPat(f)
    | Var(x) => VarPat(x)
    | Constructor(x, ty) => ConstructorPat(x, Typ.of_core(ty))
    | String(s) => StringPat(s)
    | Tuple(l) => TuplePat(List.map(of_core, l))
    | Bool(b) => BoolPat(b)
    | Cons(p1, p2) => ConsPat(of_core(p1), of_core(p2))
    | ListLit(l) => ListPat(List.map(of_core, l))
    | Ap(p1, p2) => ApPat(of_core(p1), of_core(p2))
    | EmptyHole => EmptyHolePat
    | Wild => WildPat
    | MultiHole(_) => raise(Failure("MultiHole not supported")) // TODO
    | Cast(p, t1, t2) =>
      CastPat(of_core(p), Typ.of_core(t1), Typ.of_core(t2))
    | Parens(p) => of_core(p)
    };
  };
};
