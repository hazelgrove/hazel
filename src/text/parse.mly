%{
  (*
  let id_gen: int ref = ref 0

  let mk_id (): int =
    (let uid = !id_gen in id_gen := ((!id_gen) + 1); uid : int)
  let s (children: Haz3lcore.Segment.t list) =
    match children with
    | seg::_ -> (
      let rec go child acc =
        match child with
          | [] -> acc
          | Haz3lcore.Base.Tile(c)::xs -> (c.mold.out)::(go xs acc)
          | _::xs -> go xs acc
      in go seg []
    )
    | _ -> []
    *)

  let sorts_of_children children =
    (* for each child, need to see the mold *)
    (* in should be set of outs *)
    let rec go child acc =
      match child with
        | [] -> acc
        | Haz3lcore.Base.Tile(c)::xs -> (c.mold.out)::(go xs acc)
        | _::xs -> go xs acc
    in go children []

  let sorts_of_segments segments =
    List.concat_map sorts_of_children segments

  let mk_tile (sort, label) children: Haz3lcore.Base.tile =
    let sorts = sorts_of_segments children in
    let mold = Haz3lcore.Mold.mk_op sort sorts in
    {
      id = 0;
      label = label;
      mold = mold;
      shards = [];
      children = children;
    }

  let mk_segment t children =
    Haz3lcore.Segment.of_tile (mk_tile t children)

  let mk_exp label children =
    mk_segment (Haz3lcore.Sort.Exp, label) children

  let mk_pat label children =
    mk_segment (Haz3lcore.Sort.Pat, label) children

  let mk_typ label children =
    mk_segment (Haz3lcore.Sort.Typ, label) children

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
  | t1 = typ; TARROW; t2 = typ_; { mk_typ ["->"] [t1; t2] }
  (* Tuple *)
  | ~ = typ_; <>

let typ_ :=
  (* Invalid, Multihole *)
  | EMPTY_HOLE; { mk_typ ["?"] [] }
  | LPAREN; ~ = typ; RPAREN; { mk_typ ["("; ")"] [typ] }
  | LBRACK; ~ = typ; RBRACK; { mk_typ ["["; "]"] [typ] }
  | id = IDENT; {
    match id with
    | "Int" -> mk_typ ["Int"] []
    | "Bool" -> mk_typ ["Bool"] []
    | "Float" -> mk_typ ["Float"] []
    | "String" -> mk_typ ["String"] []
    | _ -> failwith ("Unknown Type: "^id)
  }


(* Patterns *)

let pat :=
  | p1 = pat; COMMA; p2 = pat; { mk_pat [","] [p1; p2] }
  | p = pat; COLON; t = typ; { mk_pat [":"] [p; t] }
  | ~ = pat_; <>

let pat_ :=
(* Invalid *)
  | EMPTY_HOLE; { mk_pat ["?"] [] }
(* MultiHole *)
  (*
  | WILD; {mk_upat (UPat.Wild)}
  *)
  | i = INT; { mk_pat [Int.to_string i] [] }
  | f = FLOAT; { mk_pat [f] [] }
  | TRUE; { mk_pat ["true"] [] }
  | FALSE; { mk_pat ["false"] [] }
  | TRIV; { mk_pat ["triv"] [] }
  | LBRACK; RBRACK; { mk_pat ["{"; "}"] [] }
  (* Cons *)
  | id = IDENT; { mk_pat [id] [] }
  | LPAREN; p = pat; RPAREN; { mk_pat ["("; ")"] [p] }


(* Expressions *)

let expr :=
  | e1 = expr; COLONCOLON; e2 = expr; { mk_exp ["::"] [e1; e2] }
  | e1 = expr; SEMICOLON; e2 = expr; { mk_exp [";"] [e1; e2] }
  | e1 = expr; LPAREN; e2 = expr; RPAREN; { mk_exp ["("; ")"] [e1; e2] }
  (* FIXME: How to do op? *)
  | e1 = expr; _ = infix_op; e2 = expr; { mk_exp [] [e1; e2] }
  (* FIXME: This might allow a match with no rules? *)
  | e1 = expr; COMMA; e2 = expr; { mk_exp [","] [e1; e2] }
  | ~ = expr_; <>

let rule ==
  BAR; p = pat; ARROW; e = expr; { (p, e) }

let expr_ :=
  (* Invalid *)
  | EMPTY_HOLE; { mk_exp ["?"] [] }
  (* Multihole *)
  | TRIV; { mk_exp ["triv"] [] }
  | TRUE; { mk_exp ["true"] [] }
  | FALSE; { mk_exp ["false"] [] }
  | i = INT; { mk_exp [Int.to_string i] [] }
  | f = FLOAT; { mk_exp [f] [] }
  | NIL; { mk_exp ["nil"] [] }
  | FUN; p = pat; TARROW; e = expr; { mk_exp ["fun"; "->"] [p; e] }
  (* Need to differentiate between tuple and single list element*)
  | LBRACK; e = expr; RBRACK; { mk_exp ["["; "]"] [e] }
  | id = IDENT; { mk_exp [id] [] }
  | LET; p = pat; EQUAL; e1 = expr; IN; e2 = expr;
    { mk_exp ["let"; "="; "in"] [p; e1; e2] }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr;
    { mk_exp ["if"; "then"; "else"] [e1; e2; e3] }
  | TEST; e = expr; END; { mk_exp ["test"; "end"] [e] }
  | LPAREN; e = expr; RPAREN; { mk_exp ["("; ")"] [e] }
  | CASE; e = expr; _ = list(rule); END; { mk_exp ["case"; "end"] [e]}
  | MINUS; e = expr; %prec uminus { mk_exp ["-"] [e] }
  | _ = STRING; { mk_exp ["^s^"] [] }

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
