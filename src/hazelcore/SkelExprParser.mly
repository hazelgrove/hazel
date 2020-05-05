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
%token CONS
%token PLUS
%token MINUS
%token TIMES
%token SPACEOP
%token EOF

%nonassoc COMMA
%left OR
%left AND
%left LT
%left GT
%left EQ
%right CONS
%left PLUS
%left MINUS
%left TIMES
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
  | e1 = expr; SPACEOP; e2 = expr {
    Skel.BinOp(
      NotInHole,
      Operators.Exp.Space,
      e1, e2) }
  ;


