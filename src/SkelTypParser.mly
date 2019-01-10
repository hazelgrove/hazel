(* associator parser definition (we use menhir) *)

%{
  open SemanticsCore
%}

%token <int> PLACEHOLDER
%token COMMA
%token VBAR
%token ARROW
%token EOF

%left COMMA
%right ARROW
%right VBAR

%start <SemanticsCore.UHTyp.op SemanticsCore.Skel.t> skel_typ

(* %% ends the declarations section of the grammar definition *)

%%

skel_typ: 
  | ty = ty; EOF { ty }
  ;

ty: 
  | n = PLACEHOLDER { Skel.Placeholder n }
  | ty1 = ty; COMMA; ty2 = ty {
    Skel.BinOp(
      NotInHole,
      UHTyp.Prod,
      ty1, ty2) }
  | ty1 = ty; ARROW; ty2 = ty { 
    Skel.BinOp(
      NotInHole,
      UHTyp.Arrow, 
      ty1, ty2) }
  | ty1 = ty; VBAR; ty2 = ty { 
    Skel.BinOp(
      NotInHole, 
      UHTyp.Sum, 
      ty1, ty2) }
  ;


