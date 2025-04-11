open Junit_alcotest;
module Z3Int = Z3.Arithmetic.Integer

print_endline("Starting tigen");
let ctx = Z3.mk_context([( "model", "true" )]);
let foo = Z3Int.mk_numeral_i(ctx, 1);

let solver = Z3.Solver.mk_solver(ctx, None);

let foo = Haz3lmenhir.AST.Atom(Int(Bigint.of_int(1)));
let x = Z3Int.mk_const(ctx, Z3.Symbol.mk_string(ctx, "x"));
let y = Z3Int.mk_const(ctx, Z3.Symbol.mk_string(ctx, "y"));

let x_plus_y = Z3.Arithmetic.mk_add(ctx, [x, y]);
let constraint1 = Z3.Boolean.mk_eq(ctx, x_plus_y, Z3Int.mk_numeral_i(ctx, 10));
let constraint2 = Z3.Arithmetic.mk_gt(ctx, x, Z3Int.mk_numeral_i(ctx, 0));
let constraint3 = Z3.Arithmetic.mk_gt(ctx, y, Z3Int.mk_numeral_i(ctx, 0));

Z3.Solver.add(solver, [constraint1, constraint2, constraint3]);

switch (Z3.Solver.check(solver, [])) {
| Z3.Solver.SATISFIABLE =>
  let model = Z3.Solver.get_model(solver) |> Option.get;
  print_endline("SAT");
  print_endline(Z3.Model.to_string(model));
| Z3.Solver.UNSATISFIABLE => print_endline("UNSAT");
| Z3.Solver.UNKNOWN => print_endline("UNKNOWN");
};

print_endline([%derive.show: Haz3lmenhir.AST.exp](foo));
// let _ = Z3.Solver.push(solver);
// let (suite, _) =
//   run_and_report(~and_exit=false, "HazelTests", [Test_ListUtil.tests]);
// Junit.to_file(Junit.make([suite]), "junit_tests.xml");
// Bisect.Runtime.write_coverage_data();
