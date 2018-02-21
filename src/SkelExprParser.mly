(* associator parser definition (we use menhir) *)

%{
  open Semantics.Core
%}

%token <int> PLACEHOLDER
%token PLUS
%token TIMES
%token SPACEOP
%token EOF

%left PLUS
%left TIMES
%left SPACEOP

%start <Semantics.Core.UHExp.op Semantics.Core.Skel.t> skel_expr

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


