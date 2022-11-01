%{
  let id_gen: int ref = ref 0

  let mk_id (): int =
    (let uid = !id_gen in id_gen := ((!id_gen) + 1); uid : int)

  let get_form label =
    try Haz3lcore.Form.get label with _ ->
      let mold = List.hd (Haz3lcore.Form.atomic_molds(label)) in
      Haz3lcore.Form.mk Haz3lcore.Form.ss [label] mold

  let mk_tile label children: Haz3lcore.Base.tile =
    let s = List.mapi (fun i _ -> i + 1) children in
    let s = 0::s in
    let form = get_form label in
    {
      id = mk_id ();
      label = form.label;
      mold = form.mold;
      shards = s;
      children = children;
    }

  let mk_segment t children =
    let s = Haz3lcore.Segment.of_tile (mk_tile t children) in
    match Parsing.get_whitespace () with
    | 0 -> s
    | _ -> (Haz3lcore.Base.Whitespace(Haz3lcore.Whitespace.mk_space (mk_id ())))::s

  let mk_segment_suffix t children suff =
    let t = mk_segment t children in
    Haz3lcore.Segment.concat [t; suff]

  let mk_exp ?suffix label children =
    let seg =
      match suffix with
      | Some(s) -> mk_segment_suffix label children s
      | None -> mk_segment label children
    in
    Haz3lcore.Segment.remold seg Haz3lcore.Sort.Exp

  let mk_pat label children =
    let seg = mk_segment label children in
    Haz3lcore.Segment.remold seg Haz3lcore.Sort.Pat

  let mk_typ label children =
    let seg = mk_segment label children in
    Haz3lcore.Segment.remold seg Haz3lcore.Sort.Typ
%}

%token AND
%token ARROW
%token BAR
%token CASE
%token COLON
%token COLONCOLON
%token COMMA
%token DIV
%token ELSE
%token EMPTY_HOLE
%token END
%token EOF
%token EQUAL
%token EQUALEQUAL
%token FALSE
%token FDIV
%token FEQUALEQUAL
%token FGREATER
%token FGREATEREQUAL
%token FLESSER
%token FLESSEREQUAL
%token <string> FLOAT
%token FMINUS
%token FMULT
%token FPLUS
%token GREATER
%token GREATEREQUAL
%token <string> IDENT
%token IF
%token IN
%token <int> INT
%token FUN
%token LBRACK
%token LESSER
%token LESSEREQUAL
%token LET
%token LPAREN
%token MINUS
%token MULT
%token NIL
%token OF
%token OR
%token PLUS
%token RBRACK
%token RPAREN
%token SEMICOLON
%token SEQUAL
%token <string> STRING
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
%nonassoc ELSE
%right COMMA
%right COLON
%right TARROW
%right OR
%right AND
%left LESSER LESSEREQUAL GREATER GREATEREQUAL FLESSER FLESSEREQUAL FGREATER FGREATEREQUAL EQUALEQUAL FEQUALEQUAL
%left PLUS MINUS FPLUS FMINUS
%left MULT DIV FMULT FDIV
%nonassoc uminus
%right COLONCOLON
%nonassoc LBRACK CASE LPAREN IDENT FUN EMPTY_HOLE INT FLOAT TRUE FALSE TRIV IF SEMICOLON TEST NIL
%nonassoc app

%start main
%type <Haz3lcore.Base.segment> main

%%

let main :=
  ~ = expr; EOF; <>

let typ :=
  | t1 = typ; TARROW; t2 = typ_; { mk_typ "plus" [t1; t2] }
  (* Tuple *)
  | ~ = typ_; <>

let typ_ :=
  (* Invalid, Multihole *)
  | EMPTY_HOLE; { mk_typ "plus" [] }
  | LPAREN; ~ = typ; RPAREN; { mk_typ "plus" [typ] }
  | LBRACK; ~ = typ; RBRACK; { mk_typ "plus" [typ] }
  | id = IDENT; {
    match id with
    | "Int" -> mk_typ "plus" []
    | "Bool" -> mk_typ "plus" []
    | "Float" -> mk_typ "plus" []
    | "String" -> mk_typ "plus" []
    | _ -> failwith ("Unknown Type: "^id)
  }


(* Patterns *)

let pat :=
  | p1 = pat; COMMA; p2 = pat; { mk_pat "plus" [p1; p2] }
  | p = pat; COLON; t = typ; { mk_pat "plus" [p; t] }
  | ~ = pat_; <>

let pat_ :=
(* Invalid *)
  | EMPTY_HOLE; { mk_pat "plus" [] }
(* MultiHole *)
  (*
  | WILD; {mk_upat (UPat.Wild)}
  *)
  | i = INT; { mk_pat (Int.to_string i) [] }
  | _ = FLOAT; { mk_pat "plus" [] }
  | TRUE; { mk_pat "plus" [] }
  | FALSE; { mk_pat "plus" [] }
  | TRIV; { mk_pat "plus" [] }
  | LBRACK; RBRACK; { mk_pat "plus" [] }
  (* Cons *)
  | id = IDENT; { mk_pat id [] }
  | LPAREN; p = pat; RPAREN; { mk_pat "plus" [p] }


(* Expressions *)

let expr :=
  | e1 = expr; COLONCOLON; e2 = expr; { mk_exp "plus" [e1; e2] }
  | e1 = expr; SEMICOLON; e2 = expr; { mk_exp "plus" [e1; e2] }
  | e1 = expr; LPAREN; e2 = expr; RPAREN; { mk_exp "plus" [e1; e2] }
  (* FIXME: How to do op? *)
  | e1 = expr; _ = infix_op; e2 = expr; { mk_exp "plus" [e1; e2] }
  (* FIXME: This might allow a match with no rules? *)
  | e1 = expr; COMMA; e2 = expr; { mk_exp "plus" [e1; e2] }
  | ~ = expr_; <>

let rule ==
  BAR; p = pat; ARROW; e = expr; { (p, e) }

let expr_ :=
  (* Invalid *)
  | EMPTY_HOLE; { mk_exp "plus" [] }
  (* Multihole *)
  | TRIV; { mk_exp "plus" [] }
  | TRUE; { mk_exp "plus" [] }
  | FALSE; { mk_exp "plus" [] }
  | i = INT; { mk_exp (Int.to_string i) [] }
  | _ = FLOAT; { mk_exp "plus" [] }
  | NIL; { mk_exp "plus" [] }
  | FUN; p = pat; TARROW; e = expr; { mk_exp "plus" [p; e] }
  (* Need to differentiate between tuple and single list element*)
  | LBRACK; e = expr; RBRACK; { mk_exp "plus" [e] }
  | id = IDENT; { mk_exp id [] }
  | LET; p = pat; EQUAL; e1 = expr; IN; e2 = expr;
    { mk_exp ~suffix:e2 "let_" [p; e1] }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr;
    { mk_exp "if_" [e1; e2; e3] }
  | TEST; e = expr; END; { mk_exp "test" [e] }
  | LPAREN; e = expr; RPAREN; { mk_exp "plus" [e] }
  | CASE; e = expr; _ = list(rule); END; { mk_exp "plus" [e]}
  | MINUS; e = expr; %prec uminus { mk_exp "plus" [e] }
  | _ = STRING; { mk_exp "plus" [] }

let infix_op ==
  | PLUS; {}
  | MINUS; {}
  | MULT; {}
  | DIV; {}
  | LESSER; {}
  | LESSEREQUAL; {}
  | GREATER; {}
  | GREATEREQUAL; {}
  | EQUALEQUAL; {}
  (* Float *)
  | FPLUS; {}
  | FMINUS; {}
  | FMULT; {}
  | FDIV; {}
  | FLESSER; {}
  | FLESSEREQUAL; {}
  | FGREATER; {}
  | FGREATEREQUAL; {}
  | FEQUALEQUAL; {}
  (* Binary *)
  | AND; {}
  | OR; {}
