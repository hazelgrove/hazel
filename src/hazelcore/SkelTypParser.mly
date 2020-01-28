(* associator parser definition (we use menhir) *)

%{
%}

%token <int> PLACEHOLDER
%token COMMA
%token VBAR
%token AMP
%token ARROW
%token EOF

%right COMMA
%right ARROW
%right VBAR
%right AMP

%start <UHTyp.operator Skel.t> skel_typ
%type <UHTyp.operator Skel.t> ty

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
  | ty1 = ty; AMP; ty2 = ty {
    Skel.BinOp(
      NotInHole,
      UHTyp.Prod,
      ty1, ty2) }
  ;


