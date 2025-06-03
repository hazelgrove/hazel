// Usage: cat sample_input.hz | dune exec ./Main.exe --

open Testgen;
open Symex;
module Z3Int = Z3.Arithmetic.Integer;
open Haz3lmenhir;
print_endline("Starting tigen");
let input_program = Stdio.In_channel.input_all(Stdio.stdin);
let example_program = Haz3lmenhir.Interface.parse_program(input_program);

let ctx = Z3.mk_context([("model", "true")]);

let symex_result =
  symbolic_execution(~state=initial_symex_state, example_program);

let assumed: AST.Annotated.t(AST.exp(assumptions), assumptions) =
  AST.map_exp_annotation(x => x.assumptions, symex_result);

let solved = ReachPoint.solve_indicated_reachability(ctx, assumed);
switch (solved) {
| Satisfiable(assignments) =>
  print_endline("Satisfiable with assignments:");
  List.iter(
    assignment => {
      let (name, value) = assignment;
      print_endline(name ++ " = " ++ Option.value(~default="None", value));
    },
    assignments,
  );
| Unsatisfiable => print_endline("Unsatisfiable assignment")
| Unknown => print_endline("Unknown assignment")
};
