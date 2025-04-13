open Z3;
open Haz3lmenhir;

let rec generate = (ctx: Z3.context, expr: AST.exp(unit)): Z3.Expr.expr => {
  switch (expr) {
  | AST.Atom(Int(value)) =>
    Z3.Arithmetic.Integer.mk_numeral_i(
      ctx,
      Option.get(Bigint.to_int(value)) // TODO: Handle Bigint
    )
  | AST.Atom(SInt(value)) => Z3.Arithmetic.Integer.mk_numeral_i(ctx, value)
  | AST.Atom(Float(value)) =>
    Z3.FloatingPoint.mk_numeral_f(
      ctx,
      value,
      Z3.Sort.mk_uninterpreted_s(ctx, "exp"),
    )
  | AST.Atom(Bool(value)) => Z3.Boolean.mk_val(ctx, value)
  | AST.Atom(String(value)) => Z3.Seq.mk_string(ctx, value)
  | AST.UnOp(op, expr) =>
    switch (op) {
    | Bool(Not) => Z3.Boolean.mk_not(ctx, generate(ctx, expr.term))
    | Int(Minus) =>
      Z3.Arithmetic.mk_unary_minus(ctx, generate(ctx, expr.term))
    | Meta(_) =>
      raise(Failure("Unsupported generate constraint expression: Meta"))
    }
  | BinExp(left, op, right) =>
    let left' = generate(ctx, left.term);
    let right' = generate(ctx, right.term);
    switch (op) {
    | BoolOp(And) => Z3.Boolean.mk_and(ctx, [left', right'])
    | BoolOp(Or) => Z3.Boolean.mk_or(ctx, [left', right'])
    | IntOp(Plus) => Z3.Arithmetic.mk_add(ctx, [left', right'])
    | IntOp(Minus) => Z3.Arithmetic.mk_sub(ctx, [left', right'])
    | IntOp(Times) => Z3.Arithmetic.mk_mul(ctx, [left', right'])
    | IntOp(Divide) => Z3.Arithmetic.mk_div(ctx, left', right')
    | IntOp(Power) => Z3.Arithmetic.mk_power(ctx, left', right')
    | IntOp(LessThan) => Z3.Arithmetic.mk_lt(ctx, left', right')
    | IntOp(LessThanOrEqual) => Z3.Arithmetic.mk_le(ctx, left', right')
    | IntOp(GreaterThan) => Z3.Arithmetic.mk_gt(ctx, left', right')
    | IntOp(GreaterThanOrEqual) => Z3.Arithmetic.mk_ge(ctx, left', right')
    | IntOp(Equals) => Z3.Boolean.mk_eq(ctx, left', right')
    | IntOp(NotEquals) =>
      Z3.Boolean.mk_not(ctx, Z3.Boolean.mk_eq(ctx, left', right'))
    | _ =>
      raise(Failure("Unsupported binary operator: " ++ AST.show_bin_op(op)))
    };
  | AST.Atom(Nat(_))
  | _ =>
    raise(
      Failure(
        "Unsupported generate constraint expression: "
        ++ [%derive.show: AST.exp(unit)](expr),
      ),
    )
  };
};
