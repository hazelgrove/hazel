%{

  let mk_binop l op r : (UHExp.operand, Operators_Exp.t) OpSeq.t =
    let OpSeq.OpSeq(_, l) = l in
    let OpSeq.OpSeq(_, r) = r in
    let seq = Seq.seq_op_seq l op r in
    UHExp.mk_OpSeq seq

%}

%token <string> INT
%token PLUS MINUS
%token EOF

%left PLUS MINUS

%start main
%type <UHExp.t> main
%%

main:
  e = expr EOF { [e] }
;

expr:
  | e = expr_ { UHExp.ExpLine e }
;

expr_:
  | constant { UHExp.mk_OpSeq $1 }
  | expr_ PLUS expr_ { mk_binop $1 Operators_Exp.Plus $3 }
  | expr_ MINUS expr_ { mk_binop $1 Operators_Exp.Minus $3 }
;

constant:
  | constant_ { Seq.S($1, Seq.E) }
;

constant_:
  INT { UHExp.IntLit(ErrStatus.NotInHole, $1) }
;
