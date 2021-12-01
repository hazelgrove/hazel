%{
  let mk_seq operand =
    Seq.mk operand []

  let mk_binop l op r =
    let seq = Seq.seq_op_seq l op r in
    seq

  let mk_typ_ann pat typ =
    let typ = UHTyp.mk_OpSeq typ in
    let pat = Seq.nth_operand 0 pat in
    mk_seq (UHPat.TypeAnn(ErrStatus.NotInHole, pat, typ))

  let mk_pat_parenthesized e =
    let e = UHPat.mk_OpSeq e in
    UHPat.Parenthesized(e)

  let mk_application e args =
    let e = mk_seq e in
    let rec mk_app e args =
      match args with
      | [] -> e
      | x::xs -> (
        let x = mk_seq x in
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

  let mk_case expr rules =
    let e = UHExp.case expr rules in
    mk_seq e

  let mk_empty_list =
    mk_seq (UHExp.listnil ())
%}

%token LET
%token IN
%token <string> INT
%token <string> FLOAT
%token TRUE FALSE
%token PLUS MINUS
%token MULT DIV
%token FPLUS FMINUS
%token FMULT FDIV
%token COLON
%token COLONCOLON
%token SEMICOLON
%token EQUAL
%token EQUALEQUAL
%token FEQUALEQUAL
%token GREATER LESSER
%token FGREATER FLESSER
%token PERIOD
%token COMMA
%token INJL INJR
%token EOF
%token <string> IDENT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token EMPTY_HOLE
%token LAMBDA
%token CASE
%token BAR
%token ARROW
%token TARROW
%token END
%token <string> COMMENT
%token EMPTY

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
  line EOF { $1 }
  | line SEMICOLON main EOF { List.concat [$1; $3] }
;

let_binding:
  LET pat EQUAL line IN { mk_let_line $2 $4 }
;

typ:
  typ typ_op typ { mk_binop $1 $2 $3 }
  | typ_ { mk_seq $1 }
;

typ_:
  atomic_type { $1 }
  | LPAREN typ RPAREN { mk_typ_paren $2 }
  | LBRACK typ RBRACK { mk_typ_list $2 }
;

typ_annotation:
  pat COLON typ { mk_typ_ann $1 $3 }
;

atomic_type:
  IDENT {
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
  | typ_annotation { $1 }
  | pat_ { mk_seq $1 }
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
  | EMPTY_HOLE { UHPat.EmptyHole 0 }
  | pat_constant { $1 }
  | LBRACK RBRACK { UHPat.listnil () }
;

pat_constant:
  INT { UHPat.intlit $1 }
  | FLOAT { UHPat.floatlit $1 }
  | TRUE { UHPat.boollit true }
  | FALSE { UHPat.boollit false }
;

line:
  expr { [UHExp.ExpLine (UHExp.mk_OpSeq $1)] }
  | COMMENT line { List.concat [[UHExp.CommentLine $1]; $2] }
  | EMPTY line { List.concat [[UHExp.EmptyLine]; $2] }
  | let_binding line { List.concat [[$1]; $2] }
;

expr:
  simple_expr { mk_seq $1 }
  | CASE line rule+ END { mk_case $2 $3 }
  | simple_expr simple_expr+ { mk_application $1 $2 }
  | expr op expr { mk_binop $1 $2 $3 }
  | expr COLONCOLON expr { mk_binop $1 Operators_Exp.Cons $3 }
  | LBRACK RBRACK { mk_empty_list }
;

simple_expr:
  LPAREN line RPAREN { UHExp.Parenthesized($2) }
  | constant { $1 }
  | IDENT {
    if Var.is_valid $1 then
      UHExp.var $1
    else
      let (it, _) = UHExp.new_InvalidText 0 $1 in
      it
  }
  | EMPTY_HOLE { UHExp.EmptyHole 0 }
  | fn { $1 }
  | INJL LPAREN line RPAREN { mk_inj_l $3 }
  | INJR LPAREN line RPAREN { mk_inj_r $3 }
;

fn:
  LAMBDA pat PERIOD LBRACE line RBRACE { mk_fn $2 $5 }
;

rule:
  BAR pat ARROW line { mk_rule $2 $4 }
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
;

constant:
  INT { UHExp.intlit $1 }
  | FLOAT { UHExp.floatlit $1 }
  | TRUE { UHExp.boollit true }
  | FALSE { UHExp.boollit false }
;
