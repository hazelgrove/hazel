open Haz3lmenhir;
[@deriving (show({with_path: false}), eq)]
type reachpoint_result =
  | Satisfiable(list((string, option(string))))
  | Unsatisfiable
  | Unknown;

let get_indicated_constraints =
    (
      ctx: Z3.context,
      expr:
        AST.Annotated.t(
          Haz3lmenhir.AST.exp(Symex.assumptions),
          Symex.assumptions,
        ),
    ) => {
  let assumptions: list(Symex.assumptions) =
    Indicated.extract_indicated_exps(expr);
  List.concat_map(
    (assumption: Symex.assumptions) => {
      List.map(ConstraintGeneration.generate(ctx, _), assumption)
    },
    assumptions,
  );
};

let solve_indicated_reachability = (ctx, expr) => {
  let constraints = get_indicated_constraints(ctx, expr);
  let solver = Z3.Solver.mk_solver(ctx, None);
  Z3.Solver.add(solver, constraints);
  switch (Z3.Solver.check(solver, [])) {
  | Z3.Solver.SATISFIABLE =>
    let model = Z3.Solver.get_model(solver) |> Option.get;
    let assignments =
      List.map(
        decl => {
          let name = Z3.FuncDecl.get_name(decl) |> Z3.Symbol.to_string;
          let value = Z3.Model.get_const_interp(model, decl);
          (name, Option.map(Z3.Expr.to_string, value));
        },
        Z3.Model.get_decls(model),
      );
    Satisfiable(assignments);
  | Z3.Solver.UNSATISFIABLE => Unsatisfiable
  | Z3.Solver.UNKNOWN => Unknown
  };
};
