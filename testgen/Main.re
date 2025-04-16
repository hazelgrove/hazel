open Testgen;
open Symex;
module Z3Int = Z3.Arithmetic.Integer;
open Haz3lmenhir;
open Indicated;
print_endline("Starting tigen");
let example_program =
  Haz3lmenhir.Interface.parse_program(
    {|  
    let x =
      if y > 32 
      then 3 
    else {{{4}}}  in
    let a = if x > z + y then 
    {{{7}}}  else 8 in

    if b > a then x else {{{y}}} 
    |},
  );



let ctx = Z3.mk_context([("model", "true")]);

let symex_result =
  symbolic_execution(~state=initial_symex_state, example_program);

let assumed: AST.Annotated.t(AST.exp(assumptions), assumptions) =
  AST.map_exp_annotation(x => x.assumptions, symex_result);


let solved = ReachPoint.solve_indicated_reachability(ctx, assumed);

print_endline("Variable assignments:");
print_endline(
  [%derive.show: option(list((string, option(string))))](solved),
);

