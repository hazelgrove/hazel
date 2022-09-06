%{
  open Haz3lcore.TermBase

  let mk_uexp term =
    {UExp.ids = [0]; term}

  let mk_upat term =
    {UPat.ids = [0]; term}

  let mk_utyp term =
    {UTyp.ids = [0]; term}
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
%token ELSE
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
%token IF
%token IN
%token <int> INT
%token FUN
%token LBRACE
%token LBRACK
%token LESSER
%token LET
%token LPAREN
%token MINUS
%token MULT
%token NIL
%token OF
%token OR
%token PLUS
%token RBRACE
%token RBRACK
%token RPAREN
%token SEMICOLON
%token TARROW
%token TEST
%token THEN
%token TRUE
%token TRIV
%token TYPE
%token WILD

(* Precedence levels and associativity - latter definitions are higher precedence *)
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
%nonassoc LBRACK CASE LPAREN IDENT FUN EMPTY_HOLE INT FLOAT TRUE FALSE TRIV LET TYPE IF SEMICOLON
%nonassoc app

%start main
%type <UExp.t> main

%%

let main :=
  ~ = expr; EOF; <>

let typ :=
  | LPAREN; ~ = typ; RPAREN; { mk_utyp (UTyp.Parens(typ)) }
  | LBRACK; ~ = typ; RBRACK; { mk_utyp (UTyp.List(typ)) }
  | EMPTY_HOLE; { mk_utyp UTyp.EmptyHole }
  | id = IDENT; {
    match id with
    | "Int" -> mk_utyp UTyp.Int
    | "Bool" -> mk_utyp UTyp.Bool
    | "Float" -> mk_utyp UTyp.Float
    | _ -> mk_utyp UTyp.EmptyHole
  }

let pat :=
  | p1 = pat; COMMA; p2 = pat; { mk_upat (UPat.Tuple(p1::[p2])) }
  | p = pat; COLON; t = typ; {mk_upat (UPat.TypeAnn(p, t))}
  | ~ = pat_; <>

let pat_ :=
  | EMPTY_HOLE; { mk_upat (UPat.EmptyHole) }
(* MultiHole *)
  | WILD; {mk_upat (UPat.Wild)}
  | i = INT; { mk_upat (UPat.Int i) }
  | FLOAT; { mk_upat (UPat.Float(0.)) }
  | TRUE; { mk_upat (UPat.Bool(true)) }
  | FALSE; { mk_upat (UPat.Bool(false)) }
  | TRIV; { mk_upat (UPat.Triv) }
  | LBRACK; RBRACK; { mk_upat (UPat.ListLit([])) }
  | id = IDENT; { mk_upat (UPat.Var(id)) }
  | LPAREN; p = pat; RPAREN; { mk_upat (UPat.Parens p)}

let tpat :=
  | EMPTY_HOLE; { mk_utyp UTyp.EmptyHole }

let expr :=
  | e1 = expr; e2 = expr; %prec app { mk_uexp (UExp.Ap(e1, e2)) }
  | e1 = expr; COLONCOLON; e2 = expr; { mk_uexp (UExp.Cons(e1, e2)) }
  | e1 = expr; SEMICOLON; e2 = expr; { mk_uexp (UExp.Seq(e1, e2)) }
  | ~ = expr_; <>

let expr_ :=
(*
type term =
  | Invalid(parse_flag, Piece.t)
  | MultiHole(list(Id.t), list(t))
  | ListLit(list(Id.t), list(t))
  | Tuple(list(Id.t), list(t))
  | Test(t)
  | UnOp(op_un, t)
  | BinOp(op_bin, t, t)
  | Match(list(Id.t), t, list((UPat.t, t)))
*)
  | EMPTY_HOLE; { mk_uexp UExp.EmptyHole }
  | TRIV; { mk_uexp UExp.Triv }
  | FUN; p = pat; TARROW; e = expr; { mk_uexp (UExp.Fun(p, e)) }
  | TRUE; { mk_uexp (UExp.Bool true) }
  | FALSE; { mk_uexp (UExp.Bool false) }
  | i = INT; { mk_uexp (UExp.Int i) }
  | FLOAT; { mk_uexp (UExp.Float 0.) }
  | id = IDENT; { mk_uexp (UExp.Var id) }
  | LET; p = pat; EQUAL; e1 = expr; IN; e2 = expr;
    { mk_uexp (UExp.Let(p, e1, e2)) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr;
    { mk_uexp (UExp.If(e1, e2, e3)) }
  | LPAREN; e = expr; RPAREN; { mk_uexp (UExp.Parens e) }


  (* BINOPS *)
  (*
  | expr op(binop) expr {mk_uexp (UExp.BinOp($2, $1, $3))}
  | expr bin_op expr {mk_uexp (UExp.BinOp($2, $1, $3))}
  | expr PLUS expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr MINUS expr {mk_uexp (UExp.BinOp(Int(Minus), $1, $3))}
  | expr MULT expr {mk_uexp (UExp.BinOp(Int(Minus), $1, $3))}
  | expr DIV expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr LESSER expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr GREATER expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr EQUALEQUAL expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}

  | expr FPLUS expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr FMINUS expr {mk_uexp (UExp.BinOp(Int(Minus), $1, $3))}
  | expr FMULT expr {mk_uexp (UExp.BinOp(Int(Minus), $1, $3))}
  | expr FDIV expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr FLESSER expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr FGREATER expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr FEQUALEQUAL expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}

  | expr AND expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  | expr OR expr {mk_uexp (UExp.BinOp(Int(Plus), $1, $3))}
  *)


  (*
%inline binop:
  | PLUS { UExp.Plus }
  ;



  | MINUS expr {mk_uexp (UExp.UnOp(Int(Minus), $2))}
%inline bin_op:
  PLUS { UExpBinopInt. }
  | MINUS { UExp.Int(Minus) }
  | MULT { UExp.Int(Times) }
  | DIV { UExp.Int(Divide) }
  | LESSER { UExp.Int(LessThan) }
  | GREATER { UExp.Int(GreaterThan) }
  | EQUALEQUAL { UExp.Int(Equals) }

  | FPLUS { UExp.Float(Plus) }
  | FMINUS { UExp.Float(Minus) }
  | FMULT { UExp.Float(Times) }
  | FDIV { UExp.Float(Divide) }
  | FLESSER { UExp.Float(LessThan) }
  | FGREATER { UExp.Float(GreaterThan) }
  | FEQUALEQUAL { UExp.Float(Equals) }
;

%inline bool_op:
  AND { Operators_Exp.And }
  | OR { Operators_Exp.Or }
;
  *)
