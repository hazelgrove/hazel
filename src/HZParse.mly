%{
  open Semantics.Core
%}

%token <int> NATURAL
%token <string> ID
%token NUM_TYPE
%token LET
%token IN
%token INJECT
%token LAMBDA
%token CASE
%token LEFT
%token RIGHT
%token COLON
%token EQUAL
%token DOT
%token PLUS
%token TIMES
%token BAR
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCBRACE
%token RCBRACE
%token CASE_ARROW
%token TYPE_ARROW
%token EOF

%start <Semantics.Core.UHExp.t> parse_uhexp

%%

parse_uhexp:
  | e = uhexp; EOF { e }
  ;

uhtyp:
  | t = bidelim_typ
    { t }
  | os = ty_opseq
    { UHTyp.OpSeq(Associator.associate_ty os, os) }
  ;

bidelim_typ:
  | LPAREN; t = uhtyp; RPAREN
    { UHTyp.Parenthesized(t) }
  | NUM_TYPE
    { UHTyp.Num }
  | LCBRACE; RCBRACE
    { UHTyp.Hole }
  ;

ty_opseq:
  | t1 = bidelim_typ; o = tyop; t2 = bidelim_typ
    { OperatorSeq.ExpOpExp(t1, o, t2) }
  | os = ty_opseq; o = tyop; t = bidelim_typ
    { OperatorSeq.SeqOpExp(os, o, t) }
  ;

tyop:
  | TYPE_ARROW
    { UHTyp.Arrow }
  | BAR
    { UHTyp.Sum }
  ;

uhexp:
  | e = left_delim_exp
  | e = asc_exp
  | e = opseq_exp
  | e = bidelim_exp
    { e }
  ;

nonparen_exp(T):
  | e = T
    { UHExp.Tm(NotInHole, e) }
  ;

left_delim_exp:
  | e = nonparen_exp(left_delim_term)
    { e }
  ;

asc_exp:
  | e = nonparen_exp(asc_term)
    { e }
  ;

opseq_exp:
  | e = nonparen_exp(opseq_term)
    { e }
  ;

bidelim_exp:
  | e = nonparen_exp(bidelim_term)
    { e }
  | LPAREN; e = uhexp; RPAREN
    { UHExp.Parenthesized(e) }
  ;

left_delim_term:
  | LET; v = ID; EQUAL; e1 = uhexp; IN; e2 = uhexp
    { UHExp.Let(v, e1, e2) }
  | LAMBDA; v = ID; DOT; e = uhexp
    { UHExp.Lam(v, e) }
  | CASE; e = uhexp;
    LEFT; LPAREN; vL = ID; RPAREN; CASE_ARROW; eL = uhexp;
    RIGHT; LPAREN; vR = ID; RPAREN; CASE_ARROW; eR = uhexp
    { UHExp.Case(e, (vL, eL), (vR, eR)) }
  ;

asc_term:
  | e = bidelim_or_parenthesize_exp(asc_exp); COLON; t = uhtyp
    { UHExp.Asc(e, t) }
  ;

opseq_term:
  | os = exp_opseq
    { UHExp.OpSeq(Associator.associate_exp os, os) }
  ;

bidelim_term:
  | v = ID
    { UHExp.Var v }
  | n = NATURAL
    { UHExp.NumLit n }
  | INJECT; LBRACKET; s = left_or_right; RBRACKET; LPAREN; e = uhexp; RPAREN
    { UHExp.Inj(s, e) }
  | LCBRACE; n = NATURAL; RCBRACE
    { UHExp.EmptyHole n }
  ;

exp_opseq:
  | os = exp_opseq_rec
  | os = exp_opseq_ending_in(parenthesize_exp(left_delim_exp))
    { os }
  ;

exp_opseq_rec:
  | os = exp_opseq_ending_in(bidelim_or_parenthesize_exp(asc_exp))
    { os }
  ;

exp_opseq_ending_in(E):
  | e1 = bidelim_or_parenthesize_exp(asc_exp); o = expop; e2 = E
    { OperatorSeq.ExpOpExp(e1, o, e2) }
  | os = exp_opseq_rec; o = expop; e = E
    { OperatorSeq.SeqOpExp(os, o, e) }
  ;

bidelim_or_parenthesize_exp(E):
  | e = bidelim_exp
  | e = parenthesize_exp(E)
    { e }
  ;

parenthesize_exp(E):
  | e = E
    { UHExp.Parenthesized(e) }
  ;

expop:
  | PLUS
    { UHExp.Plus }
  | TIMES
    { UHExp.Times }
  |
    { UHExp.Space }
  ;

left_or_right:
  | LEFT { UHExp.L }
  | RIGHT { UHExp.R }
  ;
