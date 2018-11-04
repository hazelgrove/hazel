(* associator parser definition (we use menhir) *)
(* another approach would be to use, e.g., the shunting yard algorithm,
 *   https://en.wikipedia.org/wiki/Shunting-yard_algorithm
 * but this is easy and fast enough so far *)

%{
  open SemanticsCore
%}

%token <int> PLACEHOLDER
%token PLUS
%token TIMES
%token SPACEOP
%token EOF

%left PLUS
%left TIMES
%left SPACEOP

%start <SemanticsCore.UHExp.op SemanticsCore.Skel.t> skel_expr

(* %% ends the declarations section of the grammar definition *)

%%

skel_expr: 
  | e = expr; EOF { e }
  ;

expr: 
  | n = PLACEHOLDER { Skel.Placeholder n }
  | e1 = expr; PLUS; e2 = expr { 
    Skel.BinOp(
      NotInHole, 
      UHExp.Plus, 
      e1, e2) }
  | e1 = expr; TIMES; e2 = expr { 
    Skel.BinOp(
      NotInHole,
      UHExp.Times, 
      e1, e2) }
  | e1 = expr; SPACEOP; e2 = expr { 
    Skel.BinOp(
      NotInHole,
      UHExp.Space, 
      e1, e2) }
  ;


