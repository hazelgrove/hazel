open Junit_alcotest;
open Sexplib.Std;

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
[@deriving (show({with_path: false}), eq)]
module VariableState = {
  include Map.Make({
    type t = string;
    let compare = compare;
  });

  let pp =
      (
        fmt_a: (Format.formatter, 'a) => unit,
        formatter: Format.formatter,
        m: t('a),
      ) => {
    Format.fprintf(formatter, "{");
    iter(
      (key, value) => {
        Format.fprintf(formatter, "%s: ", key);
        fmt_a(formatter, value);
      },
      m,
    );
    Format.fprintf(formatter, "}");
  };
};

[@deriving (show({with_path: false}), eq)]
type symbolic_variable_state = VariableState.t(AST.exp(unit));

[@deriving (show({with_path: false}), eq)]
type assumptions = list(AST.exp(unit));
[@deriving (show({with_path: false}), eq)]
type symex_state = {
  symbolic_variable_state,
  assumptions,
};
let initial_state: symbolic_variable_state = VariableState.empty;
let initial_assumptions: assumptions = [];

let initial_symex_state: symex_state = {
  symbolic_variable_state: initial_state,
  assumptions: initial_assumptions,
};

[@deriving (show({with_path: false}), eq)]
type symex_exp = AST.Annotated.t(AST.exp(symex_state), symex_state);
let rec symbolic_execution =
        (~state: symex_state, exp: AST.exp(unit)): symex_exp => {
  switch (exp) {
  | If(cond, then_branch, else_branch) =>
    // TODO When do we substitute variables and such?
    let cond': symex_exp = symbolic_execution(~state, cond.term);
    let then' =
      symbolic_execution(
        ~state={
          symbolic_variable_state: state.symbolic_variable_state,
          assumptions: [cond.term, ...state.assumptions],
        },
        then_branch.term,
      );
    let else' =
      symbolic_execution(
        ~state={
          symbolic_variable_state: state.symbolic_variable_state,
          assumptions: [UnOp(Bool(Not), cond), ...state.assumptions],
        },
        else_branch.term,
      );

    let foo: AST.exp(symex_state) = If(cond', then', else');
    {
      term: foo,
      annotation: state,
    };
  | BinExp(left, op, right) =>
    let left' = symbolic_execution(~state, left.term);
    let right' = symbolic_execution(~state, right.term);
    {
      term: BinExp(left', op, right'),
      annotation: state,
    };
  | Var(name) => {
      term: Var(name),
      annotation: state,
    }
  | IndicationExp(e) =>
    let indicated = symbolic_execution(~state, e.term);
    {
      term: IndicationExp(indicated),
      annotation: state,
    };
  | Atom(atom) => {
      term: Atom(atom),
      annotation: state,
    }
  | _ =>
    raise(
      Failure(
        "Unsupported expression" ++ [%derive.show: AST.exp(unit)](exp),
      ),
    )
  };
};

print_endline([%derive.show: Haz3lmenhir.AST.exp(unit)](example_program));

let symex_result =
  symbolic_execution(~state=initial_symex_state, example_program);

print_endline([%derive.show: symex_exp](symex_result));

let _ = Z3.Solver.push(solver);
let (suite, _) = run_and_report(~and_exit=false, "TestGen", []);
