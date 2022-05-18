%{
  let mk_binop = Seq.seq_op_seq

  let mk_typ_ann pat typ =
    let typ = UHTyp.mk_OpSeq typ in
    Seq.wrap (UHPat.TypeAnn(ErrStatus.NotInHole, pat, typ))

  let mk_pat_parenthesized e =
    let e = UHPat.mk_OpSeq e in
    UHPat.Parenthesized(e)

  let mk_application e args =
    let e = Seq.wrap e in
    let rec mk_app e args =
      match args with
      | [] -> e
      | x::xs -> (
        let x = Seq.wrap x in
        let opseq = mk_app x xs in
        mk_binop e Operators_Exp.Space opseq
      )
    in
    mk_app e args

  let mk_let_line pat expr =
    let pat = UHPat.mk_OpSeq pat in
    UHExp.letline pat expr

  let mk_typ_paren typ =
    let opseq = UHTyp.mk_OpSeq typ in
    UHTyp.Parenthesized opseq

  let mk_typ_list typ =
    let opseq = UHTyp.mk_OpSeq typ in
    UHTyp.List opseq

  let mk_inj_l expr =
    UHExp.Inj(ErrStatus.NotInHole, InjSide.L, expr)

  let mk_inj_r expr =
    UHExp.Inj(ErrStatus.NotInHole, InjSide.R, expr)

  let mk_fn pat expr =
    let pat = UHPat.mk_OpSeq pat in
    UHExp.lam pat expr

  let mk_rule pat expr =
    let pat = UHPat.mk_OpSeq pat in
    UHExp.Rule(pat, expr)

  let mk_case expr rules = UHExp.case expr rules

  let mk_empty_list = UHExp.listnil ()
%}

%token AND
%token ARROW
%token BAR
%token CASE
%token COLON
%token COLONCOLON
%token COMMA
%token <string> COMMENT
%token DIV
%token EMPTY
%token EMPTY_HOLE
%token END
%token EOF
%token EQUAL
%token EQUALEQUAL
%token FALSE
%token FDIV
%token FEQUALEQUAL
%token FGREATER
%token FLESSER
%token <string> FLOAT
%token FMINUS
%token FMULT
%token FPLUS
%token GREATER
%token <string> IDENT
%token IN
%token INJL
%token INJR
%token <string> INT
%token FUN
%token LBRACE
%token LBRACK
%token LESSER
%token LET
%token LPAREN
%token MINUS
%token MULT
%token OR
%token PLUS
%token RBRACE
%token RBRACK
%token RPAREN
%token SEMICOLON
%token TARROW
%token TRUE
%token WILD

%right OR
%right AND
%left LESSER GREATER FLESSER FGREATER EQUALEQUAL FEQUALEQUAL
%left PLUS MINUS FPLUS FMINUS
%left MULT DIV FMULT FDIV
%right COLONCOLON
%left BAR
%right TARROW
%left COMMA
%left COLON

%start main
%type <UHExp.t> main
%%

main:
  block EOF { $1 }
;

block:
  exp_line { [$1] }
  | line block { $1::$2 }
  | exp_line SEMICOLON block { $1::$3 }
;

%inline exp_line:
  expr { UHExp.ExpLine (UHExp.mk_OpSeq $1) }

line:
  COMMENT { UHExp.CommentLine $1 }
  | EMPTY { UHExp.EmptyLine }
  | LET pat EQUAL block IN { mk_let_line $2 $4 }
;

typ:
  typ typ_op typ { mk_binop $1 $2 $3 }
  | typ_ { Seq.wrap $1 }
;

typ_:
  | LPAREN typ RPAREN { mk_typ_paren $2 }
  | LBRACK typ RBRACK { mk_typ_list $2 }
  | IDENT {
    match $1 with
    | "Int" -> UHTyp.Int
    | "Bool" -> UHTyp.Bool
    | "Float" -> UHTyp.Float
    | _ -> failwith ("Unknown Type: "^$1)
  }
  | EMPTY_HOLE { UHTyp.Hole }
;

%inline typ_op:
  COMMA { Operators_Typ.Prod }
  | TARROW { Operators_Typ.Arrow }
  | BAR { Operators_Typ.Sum }
;

pat:
  pat COLONCOLON pat { mk_binop $1 Operators_Pat.Cons $3 }
  | pat COMMA pat { mk_binop $1 Operators_Pat.Comma $3 }
  | pat_ COLON typ { mk_typ_ann $1 $3 }
  | pat_ { Seq.wrap $1 }
;

pat_:
  LPAREN pat RPAREN { mk_pat_parenthesized $2 }
  | IDENT {
    if Var.is_valid $1 then
      UHPat.var $1
    else
      let (it, _) = UHPat.new_InvalidText 0 $1 in
      it
  }
  | LBRACK RBRACK { UHPat.listnil () }
  | EMPTY_HOLE { UHPat.EmptyHole 0 }
  | INT { UHPat.intlit $1 }
  | FLOAT { UHPat.floatlit $1 }
  | TRUE { UHPat.boollit true }
  | FALSE { UHPat.boollit false }
  | WILD { UHPat.wild () }
;

expr:
  expr_ expr_+ { mk_application $1 $2 }
  | expr op expr { mk_binop $1 $2 $3 }
  | expr COLONCOLON expr { mk_binop $1 Operators_Exp.Cons $3 }
  | expr_ { Seq.wrap $1 }
;

expr_:
  LBRACK RBRACK { mk_empty_list }
  | CASE block rule+ END { mk_case $2 $3 }
  | LPAREN block RPAREN { UHExp.Parenthesized($2) }
  | IDENT {
    if Var.is_valid $1 then
      UHExp.var $1
    else
      let (it, _) = UHExp.new_InvalidText 0 $1 in
      it
  }
  | WILD {
      let (it, _) = UHExp.new_InvalidText 0 "_" in
      it
  }
  | FUN pat LBRACE block RBRACE { mk_fn $2 $4 }
  | INJL LPAREN block RPAREN { mk_inj_l $3 }
  | INJR LPAREN block RPAREN { mk_inj_r $3 }
  | EMPTY_HOLE { UHExp.EmptyHole 0 }
  | INT { UHExp.intlit $1 }
  | FLOAT { UHExp.floatlit $1 }
  | TRUE { UHExp.boollit true }
  | FALSE { UHExp.boollit false }
;

%inline rule:
  BAR pat ARROW block { mk_rule $2 $4 }
;

%inline op:
  PLUS { Operators_Exp.Plus }
  | MINUS { Operators_Exp.Minus }
  | MULT { Operators_Exp.Times }
  | DIV { Operators_Exp.Divide }
  | FPLUS { Operators_Exp.FPlus }
  | FMINUS { Operators_Exp.FMinus }
  | FMULT { Operators_Exp.FTimes }
  | FDIV { Operators_Exp.FDivide }
  | GREATER { Operators_Exp.GreaterThan }
  | LESSER { Operators_Exp.LessThan }
  | FGREATER { Operators_Exp.FGreaterThan }
  | FLESSER { Operators_Exp.FLessThan }
  | EQUALEQUAL { Operators_Exp.Equals }
  | FEQUALEQUAL { Operators_Exp.FEquals }
  | COMMA { Operators_Exp.Comma }
  | AND { Operators_Exp.And }
  | OR { Operators_Exp.Or }
;
