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

%start <Semantics.Core.UHExp.Skel.t> skel

(* %% ends the declarations section of the grammar definition *)

%%

skel: 
  | e = expr; EOF { e }
  ;

expr: 
  | n = PLACEHOLDER { UHExp.Skel.Placeholder n }
  | e1 = expr; PLUS; e2 = expr { 
    UHExp.Skel.BinOp(
      UHExp.NotInHole, 
      UHExp.Plus, 
      e1, e2) }
  | e1 = expr; TIMES; e2 = expr { 
    UHExp.Skel.BinOp(
      UHExp.NotInHole,
      UHExp.Times, 
      e1, e2) }
  | e1 = expr; SPACEOP; e2 = expr { 
    UHExp.Skel.BinOp(
      UHExp.NotInHole,
      UHExp.Space, 
      e1, e2) }
  ;

