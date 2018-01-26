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

%start <Semantics.Core.HExp.t> parse_hexp

%%

parse_uhexp:
  | e = uhexp; EOF { e }
  ;

uhtyp:
  | t = bidelim_typ
    { t }
  | os = opseqtyp
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

opseqtyp:
  | t1 = bidelim_typ; o = tyop; t2 = bidelim_typ
    { OperatorSeq.ExpOpExp(t1, o, t2) }
  (* TODO: inefficient - should use left recursion then reverse the result *)
  | t = bidelim_typ; o = tyop; os = opseqtyp
    { OperatorSeq.exp_op_seq t o os }
  ;

tyop:
  | TYPE_ARROW
    { UHExp.Arrow }
  | BAR
    { UHExp.Sum }
  ;

uhexp:
  | e = bidelim_exp
  | e = norm_exp
    { e }
  ;

non_paren_exp(T):
  | e = T
    { UHExp.Tm(NotInHole, e) }
  | LCBRACE; n = NATURAL; BAR; e = T; RCBRACE
    { UHExp.Tm(InHole(n), e) }
  ;

norm_exp:
  | e = non_paren_exp(norm_term)
    { e }
  ;

bidelim_exp:
  | e = non_paren_exp(bidelim_term)
    { e }
  | LPAREN; e = uhexp; RPAREN
    { UHExp.Parenthesized(e) }
  ;

norm_term:
  | e = bidelim_exp; COLON; t = uhtyp
    { UHExp.Asc(e, t) }
  | LET; v = ID; EQUAL; e1 = uhexp; IN; e2 = uhexp
    { UHExp.Let(v, e1, e2) }
  | LAMBDA; v = ID; DOT; e = uhexp
    { UHExp.Lam(v, e) }
  | CASE; e = uhexp;
    LEFT; LPAREN; vL = ID; RPAREN; CASE_ARROW; eL = uhexp;
    RIGHT; LPAREN; vR = ID; RPAREN; CASE_ARROW; eR = uhexp
    { UHExp.Case(e, (vL, eL), (vR, eR)) }
  | os = opseqterm
    { UHExp.OpSeq(Associator.associate_exp os, os) }
  ;

opseqterm:
  | e1 = bidelim_exp; o = expop; e2 = bidelim_exp
    { OperatorSeq.ExpOpExp(e1, o, e2) }
  (* TODO: inefficient - should use left recursion then reverse the result *)
  | e = bidelim_exp; o = expop; os = opseqterm
    { OperatorSeq.exp_op_seq e o os }
  ;

expop:
  | PLUS
    { UHExp.Plus }
  | TIMES
    { UHExp.Times }
  |
    { UHExp.Space }
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

left_or_right:
  | LEFT { UHExp.L }
  | RIGHT { UHExp.R }
  ;
