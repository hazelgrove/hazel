include VarMap;

type nonrec t = t(DHExp.t);

let sexp_of_t = (vars: t): Sexplib.Sexp.t =>
  vars
  |> VarMap.to_list
  |> Sexplib.Std.sexp_of_list(((x, d)) =>
       List([Var.sexp_of_t(x), DHExp.sexp_of_t(d)])
     );

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  sexp
  |> Sexplib.Std.list_of_sexp(
       fun
       | List([x_sexp, d_sexp]) => (
           Var.t_of_sexp(x_sexp),
           DHExp.t_of_sexp(d_sexp),
         )
       | s =>
         raise(
           Sexplib.Conv_error.tuple_of_size_n_expected(
             "expected a (Var, DHExp) pair",
             2,
             s,
           ),
         ),
     )
  |> VarMap.of_list;

let id_env = (ctx: Context.t): t =>
  Context.vars(ctx)
  |> List.map(((_, x, _)) => (x, DHExp.BoundVar(x)))
  |> VarMap.of_list;
