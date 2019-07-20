(* associator parser definition (we use menhir) *)
(* another approach would be to use, e.g., the shunting yard algorithm,
 *   https://en.wikipedia.org/wiki/Shunting-yard_algorithm
 * but this is easy and fast enough so far *)

%{
%}

%token <int> PLACEHOLDER
%token COMMA
%token LT
%token CONS
%token PLUS
%token MINUS
%token TIMES
%token AND
%token OR
%token SPACEOP
%token EOF

%right COMMA
%left LT
%right CONS
%left PLUS
%left MINUS
%left TIMES
%left SPACEOP
%left AND
%left OR

%start <UHExp.op Skel.t> skel_expr

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
      UHExp.Comma,
      e1, e2) }
  | e1 = expr; LT; e2 = expr {
    Skel.BinOp(
      NotInHole,
      UHExp.LessThan,
      e1, e2) }
  | e1 = expr; CONS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      UHExp.Cons,
      e1, e2) }
  | e1 = expr; PLUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      UHExp.Plus,
      e1, e2) }
  | e1 = expr; MINUS; e2 = expr {
    Skel.BinOp(
      NotInHole,
      UHExp.Minus,
      e1, e2) }
  | e1 = expr; AND; e2 = expr {
    Skel.BinOp(
      NotInHole,
      UHExp.And,
      e1, e2) }
  | e1 = expr; OR; e2 = expr {
    Skel.BinOp(
      NotInHole,
      UHExp.Or,
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


