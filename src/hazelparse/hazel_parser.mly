%{
%}

%token <string> INT
%token PLUS
%token EOF

%start main
%type <UHExp.t> main
%%

main:
  e = expr EOF { [e] }
;

expr:
  | e = binary { UHExp.ExpLine e }
;

binary:
  | arith { OpSeq(Placeholder(0), $1) }
  | arith PLUS arith  {
    OpSeq(Skel.BinOp(NotInHole, Operators_Exp.Plus,
      (Placeholder 0), (Placeholder 1)),
      $1
      )
    }
;

arith:
  | constant { Seq.S( $1, Seq.E) }
;

constant:
  INT { UHExp.IntLit(ErrStatus.NotInHole, $1) }
;
