%{
%}

%token <int> NATURAL
%token <string> ID
%token <string> PALETTE_NAME
%token <string> PALETTE_MODEL
%token NUM_TYPE
%token BOOL_TYPE
%token UNIT_TYPE
%token TRUE
%token FALSE
%token WILDCARD
%token LET
%token IN
%token INJECT
%token LAMBDA
%token CASE
%token LEFT
%token RIGHT
%token COLON
%token DOUBLE_COLON
%token SEMICOLON
%token EQUAL
%token DOT
%token COMMA
%token PLUS
%token TIMES
%token LESS_THAN
%token BAR
%token DOUBLE_BAR
%token AMP
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCBRACE
%token RCBRACE
%token CASE_ARROW
%token TYPE_ARROW
%token EOF

%start <UHExp.t> parse_uhexp

%%

parse_uhexp:
  | e = uhexp; EOF { e }
  ;

uhtyp:
  | t = bidelim_typ
    { t }
  | os = ty_opseq
    { UHTyp.OpSeq(Associator.associate_ty(os), os) }
  ;

bidelim_typ:
  | LPAREN; t = uhtyp; RPAREN
    { UHTyp.Parenthesized(t) }
  | NUM_TYPE
    { UHTyp.Num }
  | BOOL_TYPE
    { UHTyp.Bool }
  | UNIT_TYPE 
    { UHTyp.Unit }
  | LBRACKET; t = uhtyp; RBRACKET
    { UHTyp.List(t) }
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
  | AMP
    { UHTyp.Prod }
  ;

uhpat:
  | p = bidelim_pat
  | p = opseq_pat
    { p }

bidelim_pat:
  | LPAREN; p = uhpat; RPAREN
    { UHPat.Parenthesized(p) }
  | p = bidelim_pat_term
    { UHPat.Pat(NotInHole, p) }

bidelim_pat_term:
  | LCBRACE; RCBRACE
    { UHPat.EmptyHole(0) }
  | WILDCARD
    { UHPat.Wild }
  | v = ID
    { UHPat.Var(v) }
  | n = NATURAL
    { UHPat.NumLit(n) }
  | TRUE
    { UHPat.BoolLit(true) }
  | FALSE
    { UHPat.BoolLit(false) }
  | INJECT; LBRACKET; s = left_or_right; RBRACKET; LPAREN; p = uhpat; RPAREN
    { UHPat.Inj(s, p) }
  | LBRACKET; RBRACKET
    { UHPat.ListNil }

opseq_pat:
  | os = pat_opseq
    { UHPat.Pat(NotInHole, UHPat.OpSeq(Associator.associate_pat(os), os)) }

pat_opseq:
  | p1 = bidelim_pat; o = patop; p2 = bidelim_pat
    { OperatorSeq.ExpOpExp(p1, o, p2) }
  | os = pat_opseq; o = patop; p = bidelim_pat
    { OperatorSeq.SeqOpExp(os, o, p) }
  ;

patop:
  | COMMA
    { UHPat.Comma }
  |
    { UHPat.Space }
  | DOUBLE_COLON
    { UHPat.Cons }
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
  | LET; p = uhpat; ann = half_ann?; EQUAL; e1 = uhexp; IN; e2 = uhexp
    { UHExp.Let(p, ann, e1, e2) }
  | LAMBDA; p = uhpat; ann = half_ann?; DOT; e = uhexp
    { UHExp.Lam(p, ann, e) }
  ;

half_ann:
  | COLON; t = uhtyp
    { t }
  ;

rule:
  | DOUBLE_BAR; p = uhpat; CASE_ARROW; e = uhexp
    { UHExp.Rule(p, e) }
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
    { UHExp.Var(NotInVHole, v) }
  | palette_name = PALETTE_NAME; palette_model = PALETTE_MODEL
    {
      let model = String.sub palette_model 1 ((String.length palette_model) - 2) in
      let model = Scanf.unescaped model in
      UHExp.ApPalette(palette_name, model, (0, Util.NatMap.empty))
    }
  | n = NATURAL
    { UHExp.NumLit n }
  | TRUE
    { UHExp.BoolLit(true) }
  | FALSE
    { UHExp.BoolLit(false) }
  | INJECT; LBRACKET; s = left_or_right; RBRACKET; LPAREN; e = uhexp; RPAREN
    { UHExp.Inj(s, e) }
  | LBRACKET; RBRACKET
    { UHExp.ListNil }
  | LCBRACE; RCBRACE
    { UHExp.EmptyHole(0) }
  | CASE; e = uhexp; rules = rule+; SEMICOLON
    { UHExp.Case(e, rules) }
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
  | LESS_THAN
    { UHExp.LessThan }
  |
    { UHExp.Space }
  | COMMA
    { UHExp.Comma }
  | DOUBLE_COLON
    { UHExp.Cons }
  ;

left_or_right:
  | LEFT { L }
  | RIGHT { R }
  ;
