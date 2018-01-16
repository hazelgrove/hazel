(* associator parser definition (we use menhir) *)

%{
  open Skel
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

%start <Skel.t> skel

(* %% ends the declarations section of the grammar definition *)

%%

skel: 
  | e = expr; EOF { e }
  ;

expr: 
  | n = PLACEHOLDER { Placeholder n }
  | e1 = expr; PLUS; e2 = expr { BinOp(AHExp.Plus, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { BinOp(AHExp.Times, e1, e2) }
  | e1 = expr; SPACEOP; e2 = expr { BinOp(AHExp.Space, e1, e2) }
  ;

