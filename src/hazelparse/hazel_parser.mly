%{

  let mk_expline e =
    UHExp.ExpLine e

  let mk_letline pat exp =
    let block = [mk_expline exp] in
    UHExp.letline pat block

  let mk_binop l op r : (UHExp.operand, Operators_Exp.t) OpSeq.t =
    let OpSeq.OpSeq(_, l) = l in
    let OpSeq.OpSeq(_, r) = r in
    let seq = Seq.seq_op_seq l op r in
    UHExp.mk_OpSeq seq

  let mk_exp_parenthesized e =
    let e = mk_expline e in
    let operand = UHExp.Parenthesized([e]) in
    let seq = Seq.mk operand [] in
    UHExp.mk_OpSeq seq

  let mk_pat_parenthesized e =
    let operand = UHPat.Parenthesized(e) in
    let seq = Seq.mk operand [] in
    UHPat.mk_OpSeq seq

  let mk_application e arg =
    mk_binop e Operators_Exp.Space arg

  let mk_exp_var id =
    UHExp.var id

  let mk_pat_var id =
    UHPat.var id

  let mk_lambda pat expr =
    let block = [mk_expline expr] in
    UHExp.lam pat block

  let mk_intlit v =
    UHExp.intlit v

  let mk_seq operand =
    Seq.mk operand []

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
  | expr op expr { mk_binop $1 $2 $3 }
  | expr_ { $1 }
;

expr_:
  | fn { UHExp.mk_OpSeq $1 }
  | expr_ simple_expr { mk_application $1 $2 }
  | simple_expr { $1 }
;

simple_expr:
  | constant { UHExp.mk_OpSeq $1 }
  | expr_variable { UHExp.mk_OpSeq $1 }
  | LPAREN expr RPAREN { mk_exp_parenthesized $2 }
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
