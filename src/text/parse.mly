%{
  open Haz3lcore.TermBase

  let id_gen: int ref = ref 0

  let mk_id (): int =
    (let uid = !id_gen in id_gen := ((!id_gen) + 1); uid : int)

  let mk_uexp term =
    {UExp.ids = [mk_id ()]; term}

  let mk_upat term =
    {UPat.ids = [0]; term}

  let mk_utyp term =
    {UTyp.ids = [0]; term}

  type optok =
    (* Int *)
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals
    (* Float *)
    | FPlus
    | FMinus
    | FTimes
    | FDivide
    | FLessThan
    | FGreaterThan
    | FEquals
    (* Binary *)
    | And
    | Or

  let op_of_optok tok : UExp.op_bin =
    match tok with
    | Plus -> UExp.Int(UExp.Plus)
    | Minus -> UExp.Int(UExp.Minus)
    | Times -> UExp.Int(UExp.Times)
    | Divide -> UExp.Int(UExp.Divide)
    | LessThan -> UExp.Int(UExp.LessThan)
    | GreaterThan -> UExp.Int(UExp.GreaterThan)
    | Equals -> UExp.Int(UExp.Equals)
    (* Float *)
    | FPlus -> UExp.Float(UExp.Plus)
    | FMinus -> UExp.Float(UExp.Minus)
    | FTimes -> UExp.Float(UExp.Times)
    | FDivide -> UExp.Float(UExp.Divide)
    | FLessThan -> UExp.Float(UExp.LessThan)
    | FGreaterThan -> UExp.Float(UExp.GreaterThan)
    | FEquals -> UExp.Float(UExp.Equals)
    (* Binary *)
    | And -> UExp.Bool(UExp.And)
    | Or -> UExp.Bool(UExp.Or)
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
%nonassoc IN
%nonassoc LET
%nonassoc THEN
%nonassoc ELSE
%right OR
%right AND
%left LESSER GREATER FLESSER FGREATER EQUALEQUAL FEQUALEQUAL EQUAL
%left PLUS MINUS FPLUS FMINUS
%left MULT DIV FMULT FDIV
%nonassoc unop
%right COLONCOLON
%left BAR
%right TARROW
%left COMMA
%left COLON
%nonassoc LBRACK CASE LPAREN IDENT FUN EMPTY_HOLE INT FLOAT TRUE FALSE TRIV TYPE IF SEMICOLON TEST
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


(* Patterns *)

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


(* Expressions *)

let expr :=
  | e1 = expr; COLONCOLON; e2 = expr; { mk_uexp (UExp.Cons(e1, e2)) }
  | e1 = expr; SEMICOLON; e2 = expr; { mk_uexp (UExp.Seq(e1, e2)) }
  | e1 = expr; COMMA; e2 = expr; { mk_uexp (UExp.Tuple(e1::[e2])) }
  | ~ = simple_expr; <>
  | ~ = expr_; <>

let expr_ :=
  | e1 = expr; e2 = expr; { mk_uexp (UExp.Ap(e1, e2)) } %prec app
  | e1 = expr; op = infix_op; e2 = expr; { mk_uexp (UExp.BinOp(op, e1, e2))}
  | MINUS; e = expr; {mk_uexp (UExp.UnOp(UExp.Int(Minus), e))} %prec unop 

let simple_expr :=
(*
type term =
  | Invalid(parse_flag, Piece.t)
  | MultiHole(list(Id.t), list(t))
  | ListLit(list(Id.t), list(t))
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
  | TEST; e = expr; END; { mk_uexp (UExp.Test e)}
  | LPAREN; e = expr; RPAREN; { mk_uexp (UExp.Parens e) }

let infix_op ==
    | PLUS; {op_of_optok Plus}
    | MINUS; {op_of_optok Minus}
    | MULT; {op_of_optok Times}
    | DIV; {op_of_optok Divide}
    | LESSER; {op_of_optok LessThan}
    | GREATER; {op_of_optok GreaterThan}
    | EQUALEQUAL; {op_of_optok Equals}
    (* Float *)
    | FPLUS; {op_of_optok FPlus}
    | FMINUS; {op_of_optok FMinus}
    | FMULT; {op_of_optok FTimes}
    | FDIV; {op_of_optok FDivide}
    | FLESSER; {op_of_optok FLessThan}
    | FGREATER; {op_of_optok FGreaterThan}
    | FEQUALEQUAL; {op_of_optok FEquals}
    (* Binary *)
    | AND; {op_of_optok And}
    | OR; {op_of_optok Or}
