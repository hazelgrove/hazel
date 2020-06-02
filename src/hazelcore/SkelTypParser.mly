(* associator parser definition (we use menhir) *)

%{
%}

%token <int> PLACEHOLDER
%token COMMA
%token VBAR
%token AMP
%token ARROW
%token SPACE
%token EOF

%right COMMA
%right ARROW
%right VBAR
%right AMP
%left SPACE

%start <Operators.Typ.t Skel.t> skel_typ
%type <Operators.Typ.t Skel.t> ty

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
      Operators.Typ.Prod,
      ty1, ty2) }
  | ty1 = ty; ARROW; ty2 = ty {
    Skel.BinOp(
      NotInHole,
      Operators.Typ.Arrow,
      ty1, ty2) }
  | ty1 = ty; VBAR; ty2 = ty {
    Skel.BinOp(
      NotInHole,
      Operators.Typ.Sum,
      ty1, ty2) }
  | ty1 = ty; AMP; ty2 = ty {
    Skel.BinOp(
      NotInHole,
      Operators.Typ.Prod,
      ty1, ty2) }
  | ty1 = ty; SPACE; ty2 = ty {
    Skel.BinOp(
      NotInHole,
      Operators.Typ.Space,
      ty1, ty2) }
  ;


