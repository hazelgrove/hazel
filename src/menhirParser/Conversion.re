include Sexplib.Std;

module IndicatedG =
  Language.Grammar.Factory({
    type t = bool;
    let default_value = () => false;
  });
module FilterAction = {
  open Language.FilterAction;
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
  open Language.Operators;

  let op_un_meta_of_menhir_ast = (op: AST.op_un_meta) => {
    switch (op) {
    | Unquote => Unquote
    };
  };

  let op_un_int_of_menhir_ast = (op: AST.op_un_int): op_un_num => {
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

  let of_core_op_bin = (op: op_bin): AST.bin_op => {
    switch (op) {
    | Nat(op_int)
    | SInt(op_int)
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
    | Poly(op_poly) =>
      PolyOp(
        switch (op_poly) {
        | Equals => Equals
        | NotEquals => NotEquals
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
    | Nat(i)
    | Float(i)
    | SInt(i)
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
  let poly_op_of_menhir_ast = (op: AST.op_bin_poly): op_bin_poly => {
    switch (op) {
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
  let int_op_of_menhir_ast = (op: AST.op_bin_int): op_bin_num => {
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
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let of_menhir_ast = (op: AST.bin_op): op_bin => {
    switch (op) {
    | IntOp(op_int) => Int(int_op_of_menhir_ast(op_int))
    | FloatOp(op_float) => Float(float_op_of_menhir_ast(op_float))
    | BoolOp(op_bool) => Bool(bool_op_of_menhir_ast(op_bool))
    | StringOp(op_string) => String(string_op_of_menhir_ast(op_string))
    | PolyOp(op_poly) => Poly(poly_op_of_menhir_ast(op_poly))
    };
  };
};

module rec Exp: {
  let of_menhir_ast: AST.exp => IndicatedG.exp;
  let of_core: IndicatedG.exp => AST.exp;
  let get_indicated_ids:
    IndicatedG.exp => (Language.Exp.t, list(Language.Id.t));
} = {
  open IndicatedG.Exp;
  let rec of_menhir_ast = (exp: AST.exp): IndicatedG.exp => {
    switch (exp) {
    | InvalidExp(s) => invalid(s)
    | Atom(c) => basic(c)
    | Var(x) => var(x)
    | Constructor(x, ty) =>
      constructor(x, Option.map(Option.map(Typ.of_menhir_ast), ty))
    | Deferral => deferral(InAp)
    | ListExp(l) => list_lit(List.map(of_menhir_ast, l))
    | TupleExp([TupLabel(_) as tl]) => parens(tuple([of_menhir_ast(tl)]))
    | TupleExp([e]) => parens(of_menhir_ast(e))
    | TupleExp(e) => parens(tuple(List.map(of_menhir_ast, e)))
    | Label(s) => label(s)
    | TupLabel(e1, e2) => tup_label(of_menhir_ast(e1), of_menhir_ast(e2))
    | Dot(e1, e2) => dot(of_menhir_ast(e1), of_menhir_ast(e2))
    | Let(p, e1, e2) =>
      let_(Pat.of_menhir_ast(p), of_menhir_ast(e1), of_menhir_ast(e2))
    | FixF(p, e) => fix_f(Pat.of_menhir_ast(p), of_menhir_ast(e), None)
    | TypFun(t, e) =>
      typ_fun(TPat.of_menhir_ast(t), of_menhir_ast(e), None)
    | Undefined => undefined()
    | TyAlias(tp, ty, e) =>
      let ty = Typ.of_menhir_ast(ty);
      let ty =
        switch (ty) {
        | {term: Parens(ty), _} => ty
        | _ => ty
        };
      ty_alias(TPat.of_menhir_ast(tp), ty, of_menhir_ast(e));
    | Use(t, e) => use(Typ.of_menhir_ast(t), of_menhir_ast(e))
    | BuiltinFun(s) => builtin_fun(s)
    | Fun(p, e, name_opt) =>
      switch (name_opt) {
      | Some(name_str) =>
        fn(Pat.of_menhir_ast(p), of_menhir_ast(e), None, Some(name_str))
      | None => fn(Pat.of_menhir_ast(p), of_menhir_ast(e), None, None)
      }
    | ApExp(e1, args) =>
      switch (args) {
      | TupleExp(l) =>
        List.mem(AST.Deferral, l)
          ? deferred_ap(of_menhir_ast(e1), List.map(of_menhir_ast, l))
          : ap(
              Language.Operators.Forward,
              of_menhir_ast(e1),
              of_menhir_ast(args),
            )
      | Deferral => deferred_ap(of_menhir_ast(e1), [args |> of_menhir_ast])

      | _ =>
        ap(
          Language.Operators.Forward,
          of_menhir_ast(e1),
          of_menhir_ast(args),
        )
      }
    | BinExp(e1, op, e2) =>
      bin_op(
        Operators.of_menhir_ast(op),
        of_menhir_ast(e1),
        of_menhir_ast(e2),
      )

    | If(e1, e2, e3) =>
      if_(of_menhir_ast(e1), of_menhir_ast(e2), of_menhir_ast(e3))
    | CaseExp(e, l) =>
      let d_scrut = of_menhir_ast(e);
      let d_rules =
        List.map(
          ((pat, exp)) => (Pat.of_menhir_ast(pat), of_menhir_ast(exp)),
          l,
        );
      match(d_scrut, d_rules);
    | Asc(e, t) => asc(of_menhir_ast(e), Typ.of_menhir_ast(t))
    | EmptyHole => empty_hole()
    | Seq(e1, e2) => seq(of_menhir_ast(e1), of_menhir_ast(e2))
    | Test(e) => test(of_menhir_ast(e))
    | HintedTest(e, hint) =>
      hinted_test(of_menhir_ast(e), of_menhir_ast(hint))
    | Cons(e1, e2) => cons(of_menhir_ast(e1), of_menhir_ast(e2))
    | ListConcat(e1, e2) =>
      list_concat(of_menhir_ast(e1), of_menhir_ast(e2))
    | Filter(a, cond, body) =>
      let dcond = of_menhir_ast(cond);
      let dbody = of_menhir_ast(body);
      let act = FilterAction.of_menhir_ast(a);
      filter(
        Filter({
          pat: dcond,
          act,
        }),
        dbody,
      );
    | TypAp(e, ty) => typ_ap(of_menhir_ast(e), Typ.of_menhir_ast(ty))
    | UnOp(op, e) =>
      un_op(Operators.op_un_of_menhir_ast(op), of_menhir_ast(e))
    | DynamicErrorHole(e, s) =>
      dynamic_error_hole(
        of_menhir_ast(e),
        Language.InvalidOperationError.t_of_sexp(sexp_of_string(s)),
      )
    | IndicationExp(e) => {
        annotation: true,
        term: of_menhir_ast(e).term,
      }
    };
  };

  let rec of_core = (exp: IndicatedG.exp): AST.exp => {
    switch (exp.term) {
    | Invalid(_) => InvalidExp("Invalid")
    | Atom(c) => Atom(c)
    | Var(x) => Var(x)
    | LivelitName(_) => InvalidExp("Not supported")
    | Deferral(InAp) => Deferral
    | ListLit(l) => ListExp(List.map(of_core, l))
    | Tuple(l) => TupleExp(List.map(of_core, l))
    | Let(p, e1, e2) => Let(Pat.of_core(p), of_core(e1), of_core(e2))
    | FixF(p, e, _) => FixF(Pat.of_core(p), of_core(e))
    | TypFun(tp, e, _) => TypFun(TPat.of_core(tp), of_core(e))
    | Undefined => Undefined
    | TyAlias(tp, ty, e) =>
      TyAlias(TPat.of_core(tp), Typ.of_core(ty), of_core(e))
    | Use(ty, e) => Use(Typ.of_core(ty), of_core(e))
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
    | Asc(e, t) => Asc(of_core(e), Typ.of_core(t))
    | EmptyHole => EmptyHole
    | Seq(e1, e2) => Seq(of_core(e1), of_core(e2))
    | Test(e) => Test(of_core(e))
    | HintedTest(e, hint) => HintedTest(of_core(e), of_core(hint))
    | Cons(e1, e2) => Cons(of_core(e1), of_core(e2))
    | ListConcat(e1, e2) => ListConcat(of_core(e1), of_core(e2))
    | Filter(Filter({pat, act}), body) =>
      Filter(FilterAction.of_core(act), of_core(pat), of_core(body))
    | TypAp(e, ty) => TypAp(of_core(e), Typ.of_core(ty))
    | UnOp(op, e) => UnOp(Operators.of_core_op_un(op), of_core(e))
    | DynamicErrorHole(e, s) =>
      DynamicErrorHole(
        of_core(e),
        Sexplib.Sexp.to_string(Language.InvalidOperationError.sexp_of_t(s)),
      )
    | Deferral(_) => Deferral
    | Filter(Residue(_), _) => raise(Failure("Residue not supported"))
    | MultiHole(_) => raise(Failure("MultiHole not supported"))
    | Closure(_) => raise(Failure("Closure not supported"))
    | Parens(e) => of_core(e)
    | Probe(e, _) => of_core(e)
    | Constructor(s, typ) =>
      Constructor(s, Option.map(Option.map(Typ.of_core), typ))
    | DeferredAp(e, es) =>
      ApExp(of_core(e), TupleExp(List.map(of_core, es)))
    | Fun(p, e, _, name_opt) => Fun(Pat.of_core(p), of_core(e), name_opt)
    | Label(s) => Label(s)
    | TupLabel(e1, e2) => TupLabel(of_core(e1), of_core(e2))
    | Dot(e1, e2) => Dot(of_core(e1), of_core(e2))
    | Ap(Reverse, _, _) => raise(Failure("Reverse not supported"))
    };
  };

  let get_indicated_ids =
      (indicated_exp: IndicatedG.exp): (Language.Exp.t, list(Language.Id.t)) => {
    open Language;
    // mutable array to gather indicated IDs when generating expressions
    let indicated_ids = Dynarray.create();
    let e =
      Grammar.map_exp_annotation(
        (indicated): IdTagged.IdTag.t => {
          let id = Id.mk();
          if (indicated) {
            Dynarray.add_last(indicated_ids, id);
          };
          {ids: [id]};
        },
        indicated_exp,
      );
    let ids = Dynarray.to_list(indicated_ids);
    (e, ids);
  };
}
and Typ: {
  let of_menhir_ast: AST.typ => IndicatedG.typ;
  let of_core: IndicatedG.typ => AST.typ;
} = {
  open IndicatedG.Typ;
  let rec of_menhir_ast = (typ: AST.typ): IndicatedG.typ => {
    switch (typ) {
    | InvalidTyp(s) => unknown(Hole(Invalid(s)))
    | IntType => int()
    | SIntType => sint()
    | FloatType => float()
    | BoolType => bool()
    | StringType => string()
    | NatType => nat()
    | UnknownType(p) =>
      switch (p) {
      | Internal => unknown(Internal)
      | EmptyHole => unknown(Hole(EmptyHole))
      }
    | TypVar(s) => var(s)
    | TupleType([t]) => parens(of_menhir_ast(t))
    | TupleType(ts) => parens(prod(List.map(of_menhir_ast, ts)))
    | LabelType(s) => label(s)
    | TupLabelType(t1, t2) =>
      tup_label(of_menhir_ast(t1), of_menhir_ast(t2))
    | ArrayType(t) => list(of_menhir_ast(t))
    | ArrowType(t1, t2) => arrow(of_menhir_ast(t1), of_menhir_ast(t2))
    | SumTyp(sumterms) =>
      open Language;
      let converted_terms: list(ConstructorMap.variant(IndicatedG.typ)) =
        List.map(
          (sumterm: AST.sumterm): ConstructorMap.variant(IndicatedG.typ) =>
            switch (sumterm) {
            | Variant(name, typ) =>
              Variant(name, [Id.mk()], Option.map(of_menhir_ast, typ))
            | BadEntry(typ) => BadEntry(of_menhir_ast(typ))
            },
          sumterms,
        );
      parens(sum(converted_terms));
    | ForallType(tp, t) =>
      parens(forall(TPat.of_menhir_ast(tp), of_menhir_ast(t)))
    | RecType(tp, t) =>
      parens(rec_(TPat.of_menhir_ast(tp), of_menhir_ast(t)))
    | IndicationTyp(t) => {
        annotation: true,
        term: of_menhir_ast(t).term,
      }
    | ApTyp(t1, t2) => ap(of_menhir_ast(t1), of_menhir_ast(t2))
    };
  };

  let of_core_type_provenance =
      (p: IndicatedG.typ_provenance): AST.typ_provenance => {
    switch (p) {
    | Internal => Internal
    | Hole(EmptyHole) => EmptyHole
    | _ => raise(Failure("Unknown type_provenance"))
    };
  };
  let rec of_core = (typ: IndicatedG.typ): AST.typ => {
    switch (typ.term) {
    | Atom(Int) => IntType
    | Atom(SInt) => SIntType
    | Atom(Float) => FloatType
    | Atom(String) => StringType
    | Atom(Bool) => BoolType
    | Atom(Nat) => NatType
    | Var(x) => TypVar(x)
    | Prod(ts) => TupleType(List.map(of_core, ts))
    | List(t) => ArrayType(of_core(t))
    | Arrow(t1, t2) => ArrowType(of_core(t1), of_core(t2))
    | Unknown(p) => UnknownType(of_core_type_provenance(p))
    | Forall(tp, t) => ForallType(TPat.of_core(tp), of_core(t))
    | Rec(tp, t) => RecType(TPat.of_core(tp), of_core(t))
    | Parens(t) => of_core(t)
    | Label(s) => LabelType(s)
    | TupLabel(t1, t2) => TupLabelType(of_core(t1), of_core(t2))
    | Ap(t1, t2) => ApTyp(of_core(t1), of_core(t2))
    | Sum(constructors) =>
      let sumterms =
        List.map(
          (variant: Language.ConstructorMap.variant(IndicatedG.typ)): AST.sumterm => {
            switch (variant) {
            | Variant(name, _, None) => Variant(name, None)
            | Variant(name, _, Some(t)) => Variant(name, Some(of_core(t)))
            | BadEntry(t) => BadEntry(of_core(t))
            }
          },
          constructors,
        );
      SumTyp(sumterms);
    };
  };
}
and TPat: {
  let of_menhir_ast: AST.tpat => IndicatedG.tpat;
  let of_core: IndicatedG.tpat => AST.tpat;
} = {
  open IndicatedG.TPat;
  let of_menhir_ast = (tpat: AST.tpat): IndicatedG.tpat => {
    switch (tpat) {
    | InvalidTPat(s) => invalid(s)
    | EmptyHoleTPat => empty_hole()
    | VarTPat(s) => var(s)
    };
  };

  let of_core = (tpat: IndicatedG.tpat): AST.tpat => {
    switch (tpat.term) {
    | EmptyHole => EmptyHoleTPat
    | Var(x) => VarTPat(x)
    | Invalid(i) => InvalidTPat(i)
    | MultiHole(_) => raise(Failure("MultiHole not supported"))
    };
  };
}
and Pat: {
  let of_menhir_ast: AST.pat => IndicatedG.pat;
  let of_core: IndicatedG.pat => AST.pat;
} = {
  open IndicatedG.Pat;
  let rec of_menhir_ast = (pat: AST.pat): IndicatedG.pat => {
    switch (pat) {
    | InvalidPat(s) => invalid(s)
    | AtomPat(c) => basic(c)
    | AscPat(p, t) => parens(asc(of_menhir_ast(p), Typ.of_menhir_ast(t)))
    | VarPat(x) => var(x)
    | ConstructorPat(x, ty) =>
      constructor(x, Option.map(Option.map(Typ.of_menhir_ast), ty))
    | TuplePat(pats) => parens(tuple(List.map(of_menhir_ast, pats)))
    | ApPat(pat1, pat2) => ap(of_menhir_ast(pat1), of_menhir_ast(pat2))
    | ConsPat(p1, p2) =>
      parens(cons(of_menhir_ast(p1), of_menhir_ast(p2)))
    | EmptyHolePat => empty_hole()
    | WildPat => wild()
    | LabelPat(s) => label(s)
    | TupLabelPat(p1, p2) =>
      tup_label(of_menhir_ast(p1), of_menhir_ast(p2))
    | ListPat(l) => list_lit(List.map(of_menhir_ast, l))
    | IndicationPat(p) => {
        annotation: true,
        term: of_menhir_ast(p).term,
      }
    };
  };
  let rec of_core = (pat: IndicatedG.pat): AST.pat => {
    switch (pat.term) {
    | Invalid(_) => InvalidPat("Invalid")
    | Atom(c) => AtomPat(c)
    | Var(x) => VarPat(x)
    | Constructor(x, ty) =>
      ConstructorPat(x, Option.map(Option.map(Typ.of_core), ty))
    | Tuple(l) => TuplePat(List.map(of_core, l))
    | Cons(p1, p2) => ConsPat(of_core(p1), of_core(p2))
    | ListLit(l) => ListPat(List.map(of_core, l))
    | Ap(p1, p2) => ApPat(of_core(p1), of_core(p2))
    | EmptyHole => EmptyHolePat
    | Wild => WildPat
    | MultiHole(_) => raise(Failure("MultiHole not supported"))
    | Asc(p, t) => AscPat(of_core(p), Typ.of_core(t))
    | Parens(p) => of_core(p)
    | Probe(p, _) => of_core(p)
    | Label(s) => LabelPat(s)
    | TupLabel(p1, p2) => TupLabelPat(of_core(p1), of_core(p2))
    };
  };
};
