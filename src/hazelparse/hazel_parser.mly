%{

  let mk_expline e =
    UHExp.ExpLine e

  let mk_letline pat block =
    UHExp.letline pat block

  let mk_binop l op r : (UHExp.operand, Operators_Exp.t) OpSeq.t =
    let OpSeq.OpSeq(_, l) = l in
    let OpSeq.OpSeq(_, r) = r in
    let seq = Seq.seq_op_seq l op r in
    UHExp.mk_OpSeq seq

  let mk_exp_parenthesized e =
    let operand = UHExp.Parenthesized(e) in
    let seq = Seq.mk operand [] in
    UHExp.mk_OpSeq seq

  let mk_pat_parenthesized e =
    let operand = UHPat.Parenthesized(e) in
    let seq = Seq.mk operand [] in
    UHPat.mk_OpSeq seq

  let mk_application e args =
    let rec mk_app e args =
      match args with
      | [] -> e
      | x::xs -> (
        let opseq = mk_app x xs in
        mk_binop e Operators_Exp.Space opseq
      )
    in
    mk_app e args

  let mk_exp_var id =
    UHExp.var id

  let mk_pat_var id =
    UHPat.var id

  let mk_lambda pat expr =
    let block = [mk_expline expr] in
    UHExp.lam pat block

  let mk_case block rules =
    UHExp.case block rules

  let mk_rule pat block =
    UHExp.Rule(pat, block)

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
%token CASE
%token BAR
%token ARROW
%token END
%token <string> COMMENT
%token EMPTY

%left PLUS MINUS
%left MULT DIV
%nonassoc LET LPAREN LAMBDA INT IDENT IN

%start main
%type <UHExp.t> main
%%

main:
  block EOF { $1 }
;

block:
  triv_line+ { $1 }
  | triv_line* exp_line triv_line* {
    List.concat [$1; [$2]; $3]
  }
  | triv_line* let_line line* exp_line triv_line* {
    List.concat [$1; [$2]; $3; [$4]; $5]
  }
;

line:
  let_line { $1 }
  | triv_line { $1 }
;

triv_line:
  COMMENT { UHExp.CommentLine $1 }
  | EMPTY { UHExp.EmptyLine }
;

let_line:
  (* FIXME: Currently this accepts a block of just comments/empty lines *)
  LET pat EQUAL block IN { mk_letline $2 $4 }
;

exp_line:
  expr { mk_expline $1 }
;

pat:
  LPAREN pat RPAREN { mk_pat_parenthesized $2 }
  | pat_variable { UHPat.mk_OpSeq $1 }
;

pat_variable:
  IDENT { mk_seq (mk_pat_var $1) }
;

expr:
  simple_expr {  $1 }
  | case { UHExp.mk_OpSeq (mk_seq $1) }
  | expr_ { $1 }
;

expr_:
  | simple_expr args { mk_application $1 $2 }
  | expr op expr { mk_binop $1 $2 $3 }
;

args:
  | simple_expr+ { $1 }
;

simple_expr:
  | LPAREN block RPAREN { mk_exp_parenthesized $2 }
  | simple_expr_ { $1 }
;

simple_expr_:
  | fn { UHExp.mk_OpSeq $1 }
  | constant { UHExp.mk_OpSeq $1 }
  | expr_variable { UHExp.mk_OpSeq $1 }
;

fn:
  | LAMBDA p = pat PERIOD LBRACE e = expr RBRACE {
    mk_seq (mk_lambda p e)
  }
;

case:
  CASE block rule+ END { mk_case $2 $3 }
;

rule:
  BAR pat ARROW block { mk_rule $2 $4 }
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
