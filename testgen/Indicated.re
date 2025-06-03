open Haz3lmenhir;

let rec extract_indicated_exps =
        (expr: AST.Annotated.t(AST.exp('a), 'a)): list('a) => {
  let {term, _}: AST.Annotated.t(AST.exp('a), 'a) = expr;
  switch (term) {
  | IndicationExp(x) => [expr.annotation] @ extract_indicated_exps(x)
  | If(cond, then_branch, else_branch) =>
    List.concat([
      extract_indicated_exps(cond),
      extract_indicated_exps(then_branch),
      extract_indicated_exps(else_branch),
    ])
  | BinExp(left, _, right) =>
    List.concat([
      extract_indicated_exps(left),
      extract_indicated_exps(right),
    ])
  | UnOp(_, x) => extract_indicated_exps(x)
  | Let(_, x, body) =>
    List.concat([extract_indicated_exps(x), extract_indicated_exps(body)])
  | Var(_) => []
  | Atom(_) => []
  | _ => raise(Failure("Unsupported expression"))
  };
};
