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
%token SPACEOP
%token EOF

%right COMMA
%left OR
%left AND
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
%left SPACEOP

%start <Operators.Exp.t Skel.t> skel_expr
%type <Operators.Exp.t Skel.t> expr

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
      Operators.Exp.Comma,
      e1, e2) }
  | e1 = expr; LT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.LessThan,
      e1, e2) }
  | e1 = expr; GT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.GreaterThan,
      e1, e2) }
  | e1 = expr; EQ; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Equals,
      e1, e2) }
  | e1 = expr; FLT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.FLessThan,
      e1, e2) }
  | e1 = expr; FGT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.FGreaterThan,
      e1, e2) }
  | e1 = expr; FEQ; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.FEquals,
      e1, e2) }
  | e1 = expr; CONS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Cons,
      e1, e2) }
  | e1 = expr; PLUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Plus,
      e1, e2) }
  | e1 = expr; MINUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Minus,
      e1, e2) }
  | e1 = expr; FPLUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.FPlus,
      e1, e2) }
  | e1 = expr; FMINUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.FMinus,
      e1, e2) }
  | e1 = expr; AND; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.And,
      e1, e2) }
  | e1 = expr; OR; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Or,
      e1, e2) }
  | e1 = expr; TIMES; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Times,
      e1, e2) }
  | e1 = expr; DIVIDE; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Divide,
      e1, e2) }
  | e1 = expr; FTIMES; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.FTimes,
      e1, e2) }
  | e1 = expr; FDIVIDE; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.FDivide,
      e1, e2) }
  | e1 = expr; SPACEOP; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Space,
      e1, e2) }
  ;


