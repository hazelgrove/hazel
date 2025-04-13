open Junit_alcotest;
open Symex;
module Z3Int = Z3.Arithmetic.Integer;
open Haz3lmenhir;
print_endline("Starting tigen");
let ctx = Z3.mk_context([("model", "true")]);
let foo = Z3Int.mk_numeral_i(ctx, 1);

let solver = Z3.Solver.mk_solver(ctx, None);

let foo = Haz3lmenhir.AST.Atom(Int(Bigint.of_int(1)));
let x = Z3Int.mk_const(ctx, Z3.Symbol.mk_string(ctx, "x"));
let y = Z3Int.mk_const(ctx, Z3.Symbol.mk_string(ctx, "y"));

let x_plus_y = Z3.Arithmetic.mk_add(ctx, [x, y]);
let constraint1 =
  Z3.Boolean.mk_eq(ctx, x_plus_y, Z3Int.mk_numeral_i(ctx, 10));
let constraint2 = Z3.Arithmetic.mk_gt(ctx, x, Z3Int.mk_numeral_i(ctx, 0));
let constraint3 = Z3.Arithmetic.mk_gt(ctx, y, Z3Int.mk_numeral_i(ctx, 0));

Z3.Solver.add(solver, [constraint1, constraint2, constraint3]);

switch (Z3.Solver.check(solver, [])) {
| Z3.Solver.SATISFIABLE =>
  let model = Z3.Solver.get_model(solver) |> Option.get;
  print_endline("SAT");
  print_endline(Z3.Model.to_string(model));
  let decls = Z3.Model.get_decls(model);
  List.iter(
    decl => {
      let name = Z3.FuncDecl.get_name(decl) |> Z3.Symbol.to_string;
      let value = Z3.Model.get_const_interp(model, decl);
      print_endline(
        name
        ++ " = "
        ++ Option.value(
             ~default="None",
             Option.map(Z3.Expr.to_string, value),
           ),
      );
    },
    decls,
  );

| Z3.Solver.UNSATISFIABLE => print_endline("UNSAT")
| Z3.Solver.UNKNOWN => print_endline("UNKNOWN")
};

print_endline([%derive.show: Haz3lmenhir.AST.exp(unit)](foo));

let example_program =
  Haz3lmenhir.Interface.parse_program(
    {|if a > b
        then a
        else (if c > b then {{{ 1 }}} else 2)|},
  );

print_endline([%derive.show: Haz3lmenhir.AST.exp(unit)](example_program));



let symex_result =
  symbolic_execution(~state=initial_symex_state, example_program);

print_endline([%derive.show: symex_exp](symex_result));

let _ = Z3.Solver.push(solver);

run_and_report(
  ~and_exit=false,
  "TestGen",
  Alcotest.[
    (
      "Symbolic Execution",
      {
        let symex_exp =
          testable(Fmt.using(show_symex_exp, Fmt.string), equal_symex_exp);
        [
          test_case("constant", `Quick, () => {
            check(
              symex_exp,
              "Symbolic execution of constant",
              {
                term: Atom(Int(Bigint.of_int(1))),
                annotation: initial_symex_state,
              },
              symbolic_execution(
                ~state=initial_symex_state,
                Haz3lmenhir.AST.Atom(Int(Bigint.of_int(1))),
              ),
            )
          }),
          test_case("Branched if", `Quick, () => {
            check(
              symex_exp,
              "Symbolic execution of if",
              {
                term:
                  If(
                    {
                      term: Atom(Bool(true)),
                      annotation: initial_symex_state,
                    },
                    {
                      term: Atom(Int(Bigint.of_int(2))),
                      annotation: {
                        symbolic_variable_state: VariableState.empty,
                        assumptions: [Atom(Bool(true))],
                      },
                    },
                    {
                      term: Atom(Int(Bigint.of_int(3))),
                      annotation: {
                        symbolic_variable_state: VariableState.empty,
                        assumptions: [
                          UnOp(
                            Bool(Not),
                            AST.lift(Atom(Bool(true)): AST.exp(unit)),
                          ),
                        ],
                      },
                    },
                  ),
                annotation: initial_symex_state,
              },
              symbolic_execution(
                ~state=initial_symex_state,
                AST.If(
                  AST.lift(AST.Atom(Bool(true))),
                  AST.lift(AST.Atom(Int(Bigint.of_int(2)))),
                  AST.lift(AST.Atom(Int(Bigint.of_int(3)))),
                ),
              ),
            )
          }),
        ];
      },
    ),
  ],
);
