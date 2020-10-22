%{

  let mk_expline e =
    UHExp.ExpLine e

  let mk_letline pat exp =
    UHExp.LetLine(pat, None, [mk_expline exp])

  let mk_binop l op r : (UHExp.operand, Operators_Exp.t) OpSeq.t =
    let OpSeq.OpSeq(_, l) = l in
    let OpSeq.OpSeq(_, r) = r in
    let seq = Seq.seq_op_seq l op r in
    UHExp.mk_OpSeq seq

  let mk_exp_parenthesized e =
    let e = mk_expline e in
    let seq = Seq.S(UHExp.Parenthesized([e]), Seq.E) in
    UHExp.mk_OpSeq seq

  let mk_pat_parenthesized e =
    let seq = Seq.S(UHPat.Parenthesized(e), Seq.E) in
    UHPat.mk_OpSeq seq

  let mk_exp_var id =
    UHExp.Var(ErrStatus.NotInHole, VarErrStatus.NotInVarHole, id)

  let mk_pat_var id =
    UHPat.Var(ErrStatus.NotInHole, VarErrStatus.NotInVarHole, id)

  let mk_lambda pat expr =
    UHExp.Lam(ErrStatus.NotInHole, pat, None, [mk_expline expr])

  let mk_intlit v =
    UHExp.IntLit(ErrStatus.NotInHole, v)

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
%token PERIOD
%token EOF
%token <string> IDENT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LAMBDA

%left PLUS MINUS
%left MULT DIV

%start main
%type <UHExp.t> main
%%

main:
  | e = expr EOF { [mk_expline e] }
  | l = let_binding EOF { [l] }
  | l = let_binding main EOF { List.append [l] $2 }
;

let_binding:
  | LET pat EQUAL expr IN { mk_letline $2 $4 }

pat:
  | LPAREN pat RPAREN { mk_pat_parenthesized $2 }
  | pat_variable { UHPat.mk_OpSeq $1 }
;

pat_variable:
  | IDENT { mk_seq (mk_pat_var $1) }
;

expr:
  | constant { UHExp.mk_OpSeq $1 }
  | expr_variable { UHExp.mk_OpSeq $1 }
  | LPAREN expr RPAREN { mk_exp_parenthesized $2 }
  | expr op expr { mk_binop $1 $2 $3 }
  | fn { UHExp.mk_OpSeq $1 }
;

fn:
  | LAMBDA p = pat PERIOD LBRACE e = expr RBRACE {
    mk_seq (mk_lambda p e)
  }
;

%inline op:
  | PLUS { Operators_Exp.Plus }
  | MINUS { Operators_Exp.Minus }
  | MULT { Operators_Exp.Times }
  | DIV { Operators_Exp.Divide }
;

expr_variable:
  | IDENT { mk_seq (mk_exp_var $1) }
;

constant:
  | INT { mk_seq (mk_intlit $1) }
;
