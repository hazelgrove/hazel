%{

  let mk_binop l op r : (UHExp.operand, Operators_Exp.t) OpSeq.t =
    let OpSeq.OpSeq(_, l) = l in
    let OpSeq.OpSeq(_, r) = r in
    let seq = Seq.seq_op_seq l op r in
    UHExp.mk_OpSeq seq

  let mk_parenthesized e =
    let seq = Seq.S(UHExp.Parenthesized([e]), Seq.E) in
    UHExp.mk_OpSeq seq

  let mk_exp_var id =
    UHExp.Var(ErrStatus.NotInHole, VarErrStatus.NotInVarHole, id)

  let mk_seq operand =
    Seq.S(operand, Seq.E)

%}

%token LET
%token IN
%token <string> INT
%token PLUS MINUS
%token MULT DIV
%token COLON
%token EQUAL
%token EOF
%token <string> IDENT
%token LPAREN RPAREN
%token LAMBDA

%left PLUS MINUS
%left MULT DIV

%start main
%type <UHExp.t> main
%%

main:
  | e = expr EOF { [e] }
  | l = let_binding EOF { [l] }
  | l = let_binding expr+ EOF { List.append [l] $2 }
;

let_binding:
  | LET pat EQUAL expr IN { UHExp.LetLine($2, None, [$4]) }

pat:
  | pat_variable { UHPat.mk_OpSeq $1 }

pat_variable:
  | IDENT {
    mk_seq (UHPat.Var(ErrStatus.NotInHole, VarErrStatus.NotInVarHole, $1))
  }

expr:
  | e = expr_ { UHExp.ExpLine(e) }
;

expr_:
  | constant { UHExp.mk_OpSeq $1 }
  | expr_variable { UHExp.mk_OpSeq $1 }
  | LPAREN expr RPAREN { mk_parenthesized $2 }
  | expr_ op expr_ { mk_binop $1 $2 $3 }
;

%inline op:
  | PLUS { Operators_Exp.Plus }
  | MINUS { Operators_Exp.Minus }
  | MULT { Operators_Exp.Times }
  | DIV { Operators_Exp.Divide }

expr_variable:
  | expr_variable_ { mk_seq $1 }

expr_variable_:
  | IDENT { mk_exp_var $1 }

constant:
  | constant_ { mk_seq $1 }
;

constant_:
  INT { UHExp.IntLit(ErrStatus.NotInHole, $1) }
;
