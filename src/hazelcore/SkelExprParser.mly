(* associator parser definition (we use menhir) *)
(* another approach would be to use, e.g., the shunting yard algorithm,
 *   https://en.wikipedia.org/wiki/Shunting-yard_algorithm
 * but this is easy and fast enough so far *)

%{
%}

%token <int> PLACEHOLDER
%token COMMA
%token OR
%token AND
%token LT
%token GT
%token EQ
%token FLT
%token FGT
%token FEQ
%token CONS
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token FPLUS
%token FMINUS
%token FTIMES
%token FDIVIDE
%token CARET
%token SPACEOP
%token EOF

%right COMMA
%right OR
%right AND
%left LT
%left GT
%left EQ
%left FLT
%left FGT
%left FEQ
%right CONS
%left PLUS
%left MINUS
%left TIMES
%left DIVIDE
%left FPLUS
%left FMINUS
%left FTIMES
%left FDIVIDE
%right CARET
%left SPACEOP

%start <Operators_Exp.t Skel.t> skel_expr
%type <Operators_Exp.t Skel.t> expr

(* %% ends the declarations section of the grammar definition *)

%%

skel_expr:
  | e = expr; EOF { e }
  ;

expr:
  | n = PLACEHOLDER { Skel.Placeholder n }
  | e1 = expr; COMMA; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Comma,
      e1, e2) }
  | e1 = expr; LT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.LessThan,
      e1, e2) }
  | e1 = expr; GT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.GreaterThan,
      e1, e2) }
  | e1 = expr; EQ; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Equals,
      e1, e2) }
  | e1 = expr; FLT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.FLessThan,
      e1, e2) }
  | e1 = expr; FGT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.FGreaterThan,
      e1, e2) }
  | e1 = expr; FEQ; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.FEquals,
      e1, e2) }
  | e1 = expr; CONS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Cons,
      e1, e2) }
  | e1 = expr; PLUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Plus,
      e1, e2) }
  | e1 = expr; MINUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Minus,
      e1, e2) }
  | e1 = expr; FPLUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.FPlus,
      e1, e2) }
  | e1 = expr; FMINUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.FMinus,
      e1, e2) }
  | e1 = expr; CARET; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Caret,
      e1, e2) }
  | e1 = expr; AND; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.And,
      e1, e2) }
  | e1 = expr; OR; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Or,
      e1, e2) }
  | e1 = expr; TIMES; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Times,
      e1, e2) }
  | e1 = expr; DIVIDE; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Divide,
      e1, e2) }
  | e1 = expr; FTIMES; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.FTimes,
      e1, e2) }
  | e1 = expr; FDIVIDE; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.FDivide,
      e1, e2) }
  | e1 = expr; SPACEOP; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Space,
      e1, e2) }
  ;


