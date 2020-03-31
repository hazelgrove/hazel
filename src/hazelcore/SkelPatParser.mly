(* associator parser definition (we use menhir) *)
(* another approach would be to use, e.g., the shunting yard algorithm,
 *   https://en.wikipedia.org/wiki/Shunting-yard_algorithm
 * but this is easy and fast enough so far *)

%{
%}

%token <int> PLACEHOLDER
%token COMMA
%token CONS
%token SPACEOP
%token EOF

%right COMMA
%right CONS
%left SPACEOP

%start <Operator.Pat.operator Skel.t> skel_pat
%type <Operator.Pat.operator Skel.t> pat

(* %% ends the declarations section of the grammar definition *)

%%

skel_pat:
  | p = pat; EOF { p }
  ;

pat:
  | n = PLACEHOLDER { Skel.Placeholder n }
  | p1 = pat; COMMA; p2 = pat {
    Skel.BinOp(
      NotInHole,
      Operator.Pat.Comma,
      p1, p2) }
  | p1 = pat; CONS; p2 = pat {
    Skel.BinOp(
      NotInHole,
      Operator.Pat.Cons,
      p1, p2) }
  | p1 = pat; SPACEOP; p2 = pat {
    Skel.BinOp(
      NotInHole,
      Operator.Pat.Space,
      p1, p2) }
  ;


