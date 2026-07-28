%{
open AST

let rec taylor_pat_names = function
  | VarPat name -> [name]
  | AscPat (pat, _) | IndicationPat pat -> taylor_pat_names pat
  | TuplePat pats | ListPat pats -> List.flatten (List.map taylor_pat_names pats)
  | ConsPat (left, right) | ApPat (left, right) | TupLabelPat (left, right) ->
      taylor_pat_names left @ taylor_pat_names right
  | _ -> []

let rec taylor_exp_names = function
  | Var name -> [name]
  | BinExp (left, _, right) | TupLabel (left, right) | Dot (left, right)
  | ApExp (left, right) | Cons (left, right) | ListConcat (left, right)
  | Seq (left, right) | TupleExtension (left, right) ->
      taylor_exp_names left @ taylor_exp_names right
  | UnOp (_, inner) | ProofObject inner | Asc (inner, _) | Test inner
  | TypAp (inner, _) | IndicationExp inner -> taylor_exp_names inner
  | ListExp entries | TupleExp entries ->
      List.flatten (List.map taylor_exp_names entries)
  | Let (pat, definition, body) | Theorem (pat, definition, body) ->
      taylor_pat_names pat @ taylor_exp_names definition @ taylor_exp_names body
  | Fun (pat, body, _) | ForallExp (pat, body) | FixF (pat, body) ->
      taylor_pat_names pat @ taylor_exp_names body
  | TypFun (_, body) -> taylor_exp_names body
  | CaseExp (scrutinee, rules) ->
      taylor_exp_names scrutinee
      @ List.flatten
          (List.map
             (fun (pat, body) -> taylor_pat_names pat @ taylor_exp_names body)
             rules)
  | If (condition, then_, else_) ->
      taylor_exp_names condition @ taylor_exp_names then_ @ taylor_exp_names else_
  | Filter (_, filter, body) -> taylor_exp_names filter @ taylor_exp_names body
  | HintedTest (test, hint) -> taylor_exp_names test @ taylor_exp_names hint
  | DynamicErrorHole (inner, _) -> taylor_exp_names inner
  | TyAlias (_, _, body) | Use (_, body) -> taylor_exp_names body
  | Module items ->
      List.flatten
        (List.map
           (function
             | ModItemLet (pat, exp) -> taylor_pat_names pat @ taylor_exp_names exp
             | ModItemType _ -> []
             | ModItemExp exp -> taylor_exp_names exp
             | ModItemModule (pat, exp) -> taylor_pat_names pat @ taylor_exp_names exp)
           items)
  | ModuleExp (pat, definition, body) ->
      taylor_pat_names pat @ taylor_exp_names definition @ taylor_exp_names body
  | Atom _ | Constructor _ | Label _ | ExplicitNonlabel | EmptyHole
  | BuiltinFun _ | Undefined | Deferral | InvalidExp _ -> []

let rec unused_taylor_name candidate used suffix =
  let name =
    if suffix = 0 then candidate else candidate ^ "_" ^ string_of_int suffix
  in
  if List.mem name used then unused_taylor_name candidate used (suffix + 1)
  else name

let lower_scoped_taylor_derivatives function_exp order continuation =
  let base = match function_exp with Var name -> name | _ -> "f" in
  let rec build index current used =
    if index > order then continuation
    else
      let candidate = base ^ "_deriv_" ^ string_of_int index in
      let name = unused_taylor_name candidate used 0 in
      Let
        ( VarPat name,
          ApExp (Var "diff", current),
          build (index + 1) (Var name) (name :: used) )
  in
  build 1 function_exp (taylor_exp_names function_exp)
%}



%token T_TYP
%token P_PAT
%token TP_TPAT
%token E_EXP
%token TILDE
%token NAMED_FUN
%token POLY
%token REC
%token UNDEF
%token <string> PROJECTOR_INVOKE
%token TYP
%token TYP_FUN
%token FIX
%token WILD
%token QUESTION
%token AT_SYMBOL
%token TYP_AP_SYMBOL
%token CONS
%token TEST
%token PAUSE
%token DEBUG
%token HIDE
%token EVAL
%token <string> IDENT
%token <string> CONSTRUCTOR_IDENT
%token <string> STRING
%token <string> QUOTED_LABEL
%token TRUE 
%token FALSE
%token <int> INT
%token <float> FLOAT
%token LET
%token TAYLOR_DERIVATIVES
%token MODULE
%token FUN
%token CASE
%token OPEN_SQUARE_BRACKET
%token CLOSE_SQUARE_BRACKET
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_TRIPLE_CURLY
%token CLOSE_TRIPLE_CURLY
%token OPEN_CURLY
%token CLOSE_CURLY
%token DASH_ARROW
%token EQUAL_ARROW
%token SINGLE_EQUAL
%token TURNSTILE
%token TUPLE_EXTENSION
%token DOT

(* Poly ops *)
%token DOUBLE_EQUAL
%token NOT_EQUAL

(* String ops *)
%token STRING_CONCAT

(* Int ops *)
%token PLUS
%token MINUS
%token POWER
%token TIMES
%token DIVIDE
%token LESS_THAN
%token LESS_THAN_EQUAL
%token GREATER_THAN
%token GREATER_THAN_EQUAL
(* Float ops *)
%token DOUBLE_EQUAL_FLOAT
%token NOT_EQUAL_FLOAT
%token PLUS_FLOAT
%token MINUS_FLOAT
%token DIVIDE_FLOAT
%token POWER_FLOAT
%token TIMES_FLOAT
%token LESS_THAN_FLOAT
%token LESS_THAN_EQUAL_FLOAT
%token GREATER_THAN_FLOAT
%token GREATER_THAN_EQUAL_FLOAT
(*logical ops*)
%token L_AND
%token L_OR
%token L_NOT
(*bitwise ops*)
%token COMMA
%token COLON
%token EOF
%token IN
%token UNIT
%token END

(* type tokens *)
%token INT_TYPE
%token FLOAT_TYPE
%token BOOL_TYPE
%token STRING_TYPE
%token VOID_TYPE
%token UNKNOWN
%token INTERNAL

%token IF
%token THEN
%token ELSE

%token SEMI_COLON



(* Precedences *)



/* Structural mixfix forms - loosest binding (bodies include flat sequences) */
%nonassoc TAYLOR_CALL
%nonassoc IN
%nonassoc LET_EXP
%right SUM_TYP
%right DASH_ARROW
%nonassoc IF_EXP

/* Flat sequences - tighter than structural forms */
%right SEMI_COLON

/* Module item expression reduction: higher than SEMI_COLON so that inside
   module bodies, the parser reduces exp to modItemExp rather than shifting
   ';' for Seq. This only affects the modItemExp production. */
%nonassoc MOD_ITEM_EXP

%right L_OR
%right L_AND


%left GREATER_THAN LESS_THAN DOUBLE_EQUAL NOT_EQUAL LESS_THAN_EQUAL GREATER_THAN_EQUAL NOT_EQUAL_FLOAT LESS_THAN_FLOAT LESS_THAN_EQUAL_FLOAT GREATER_THAN_FLOAT GREATER_THAN_EQUAL_FLOAT DOUBLE_EQUAL_FLOAT
%right STRING_CONCAT AT_SYMBOL
%right  CONS

%left PLUS MINUS PLUS_FLOAT MINUS_FLOAT TUPLE_EXTENSION
%left DIVIDE TIMES TIMES_FLOAT DIVIDE_FLOAT L_NOT

%right POWER POWER_FLOAT
%nonassoc UMINUS   /* Unary minus (prefix) */
%left COLON



%nonassoc TYP_AP_SYMBOL

%left OPEN_PAREN CLOSE_PAREN
%left DOT

%left TILDE
%token SLASH_TILDE




%type <AST.exp> exp
%type <AST.sumtype> sumTyp

%start <AST.exp> program

%%

program:
    | e = exp; EOF {e}

%inline polyOp:
    | DOUBLE_EQUAL { PolyOp(Equals) }
    | NOT_EQUAL { PolyOp(NotEquals) }

%inline intOp:
    | MINUS { IntOp(Minus) }
    | PLUS { IntOp(Plus) }
    | TIMES { IntOp(Times) }
    | POWER { IntOp(Power) }
    | DIVIDE { IntOp(Divide) }
    | LESS_THAN { IntOp(LessThan) }
    | LESS_THAN_EQUAL { IntOp(LessThanOrEqual) }
    | GREATER_THAN { IntOp(GreaterThan) }
    | GREATER_THAN_EQUAL { IntOp(GreaterThanOrEqual) }


%inline floatOp:
    | PLUS_FLOAT { FloatOp(Plus) }
    | MINUS_FLOAT { FloatOp(Minus) }
    | TIMES_FLOAT { FloatOp(Times) }
    | POWER_FLOAT { FloatOp(Power) }
    | DIVIDE_FLOAT { FloatOp(Divide) }
    | LESS_THAN_FLOAT { FloatOp(LessThan) }
    | LESS_THAN_EQUAL_FLOAT { FloatOp(LessThanOrEqual) }
    | GREATER_THAN_FLOAT { FloatOp(GreaterThan) }
    | GREATER_THAN_EQUAL_FLOAT { FloatOp(GreaterThanOrEqual) }
    | DOUBLE_EQUAL_FLOAT { FloatOp(Equals) }
    | NOT_EQUAL_FLOAT { FloatOp(NotEquals) }

%inline boolOp:
    | L_AND { BoolOp(And) }
    | L_OR { BoolOp(Or) }

%inline stringOp:
    | STRING_CONCAT { StringOp(Concat) }

%inline binOp:
    | p = polyOp { p }
    | i = intOp { i }
    | f = floatOp { f }
    | b = boolOp { b }
    | s = stringOp { s }

binExp:
    | e1 = exp; b = binOp; e2 = exp { BinExp (e1, b, e2) }

label:
    | l = IDENT { l }
    | l = QUOTED_LABEL { l }

tupTypeEntry:
    | t = typ {t}
    | l = label; SINGLE_EQUAL; t = typ { TupLabelType(LabelType(l), t) }

%inline tupleType:
    | OPEN_PAREN; hd = tupTypeEntry; COMMA; types = separated_list(COMMA, tupTypeEntry); CLOSE_PAREN { TupleType(hd :: types) }


%inline sumTerm:
    | i = CONSTRUCTOR_IDENT; OPEN_PAREN; hd = tupTypeEntry; COMMA; types = separated_list(COMMA, tupTypeEntry); CLOSE_PAREN  { Variant(i, Some(TupleType(hd :: types))) }
    | i = CONSTRUCTOR_IDENT; OPEN_PAREN; t = typ; CLOSE_PAREN;  { Variant(i, Some(t)) }
    | i = CONSTRUCTOR_IDENT { Variant(i, None) }
    | QUESTION { BadEntry(UnknownType(EmptyHole)) }


// We don't support sum types without the leading plus in the parser syntax
sumTyp:
    | PLUS; s = sumTerm; { [s] } %prec SUM_TYP
    | PLUS; s = sumTerm; t = sumTyp { [s] @ t } 
    
typ:
    | c = CONSTRUCTOR_IDENT { TypVar(c) }
    | c = IDENT { TypVar(c) }
    | T_TYP; s = STRING { InvalidTyp(s) }
    | PROJECTOR_INVOKE; OPEN_PAREN; t = typ; CLOSE_PAREN; { t }
    | INT_TYPE { IntType }
    | FLOAT_TYPE { FloatType }
    | BOOL_TYPE { BoolType }
    | STRING_TYPE { StringType }
    | VOID_TYPE { VoidType }
    | UNKNOWN; INTERNAL { UnknownType(Internal) }
    | QUESTION { UnknownType(EmptyHole) }
    | UNIT { TupleType([]) }
    | POLY; a = tpat; DASH_ARROW; t = typ { PolyType(a, t) }
    | t = tupleType { t }
    | OPEN_SQUARE_BRACKET; t = typ; CLOSE_SQUARE_BRACKET { ArrayType(t) }
    | t1 = typ; DASH_ARROW; t2 = typ { ArrowType(t1, t2) }
    | s = sumTyp; { SumTyp(s) }
    | REC; c=tpat; DASH_ARROW; t = typ { RecType(c, t) }
    | OPEN_TRIPLE_CURLY; t = typ; CLOSE_TRIPLE_CURLY { IndicationTyp(t) }
    | OPEN_PAREN; t = typ; CLOSE_PAREN { t }
    | OPEN_PAREN; l = label; SINGLE_EQUAL; t = typ; CLOSE_PAREN { TupleType([TupLabelType(LabelType(l), t)]) }
    | t1 = typ; TUPLE_EXTENSION; t2 = typ { ProdExtension(t1, t2) } %prec TYP_AP_SYMBOL
    | t1 = typ; DOT; t2 = typ { ProdProjection(t1, t2) }
    | OPEN_CURLY; items = separated_list(SEMI_COLON, sigItem); CLOSE_CURLY { Sig(items) }

tupPatEntry:
    | p = pat {p}
    | l = label; SINGLE_EQUAL; p = pat { TupLabelPat(LabelPat(l), p) }

nonAscriptingPat:
    | OPEN_TRIPLE_CURLY; p = pat; CLOSE_TRIPLE_CURLY { IndicationPat(p) }
    | OPEN_PAREN; p = pat; CLOSE_PAREN { p }
    | OPEN_PAREN; l = label; SINGLE_EQUAL; p = pat; CLOSE_PAREN { TuplePat([TupLabelPat(LabelPat(l), p)]) }
    | OPEN_PAREN; p = tupPatEntry; COMMA; pats = separated_list(COMMA, tupPatEntry); CLOSE_PAREN { TuplePat(p :: pats) }
    |  P_PAT; s = STRING { InvalidPat(s) }
    | WILD { WildPat }
    | QUESTION { EmptyHolePat }
    | OPEN_SQUARE_BRACKET; l = separated_list(COMMA, pat); CLOSE_SQUARE_BRACKET; { ListPat(l) }
    | c = CONSTRUCTOR_IDENT { ConstructorPat(c, None)}
    | c = CONSTRUCTOR_IDENT; TILDE; t = typ;  { AscPat(ConstructorPat(c, None), t) }
    | p = IDENT { VarPat(p) }
    | i = INT { AtomPat (Int (Bigint.of_int i)) }
    | f = FLOAT { AtomPat (Float f) }
    | s = STRING { AtomPat (String s)}
    | TRUE {AtomPat (Bool true)}
    | FALSE {AtomPat (Bool false)}
    | f = pat; OPEN_PAREN; a = pat; CLOSE_PAREN { ApPat(f, a) }

funPat:
    | OPEN_PAREN; p1 = pat; COLON; t1 = typ; CLOSE_PAREN;  { AscPat(p1, t1) }
    | p = nonAscriptingPat; { p }

pat:
    | p1 = pat; COLON; t1 = typ;  { AscPat(p1, t1) }
    (* | p1 = pat; AS; p2 = pat; { AsPat(p1, p2) } *)
    | p1 = pat; CONS; p2 = pat { ConsPat(p1, p2) } 
    | UNIT { TuplePat([]) }
    | p = nonAscriptingPat; { p }
    | PROJECTOR_INVOKE; OPEN_PAREN; p = pat; CLOSE_PAREN; { p }


rul:
    | TURNSTILE; p = pat; EQUAL_ARROW; e = exp; { (p, e) }

case:
    | CASE; e = exp; l = list(rul); END; { CaseExp(e, l) }

funExp: 
    | FUN; p = funPat; DASH_ARROW; e1 = exp; { Fun (p, e1, None) }
    | NAMED_FUN; name = IDENT; p = funPat; DASH_ARROW; e1 = exp { Fun (p, e1, Some(name)) }


%inline ifExp:
    | IF; e1 = exp; THEN; e2 = exp; ELSE; e3 = exp { If (e1, e2, e3) } %prec IF_EXP

filterAction:
    | PAUSE { Pause }
    | DEBUG { Debug }
    | HIDE { Hide }
    | EVAL { Eval }

tpat:
    | TP_TPAT; s = STRING {InvalidTPat(s)}
    | p = PROJECTOR_INVOKE {InvalidTPat(p)}
    | QUESTION {EmptyHoleTPat}
    | v = IDENT {VarTPat v}
    | v = CONSTRUCTOR_IDENT {VarTPat v}

unExp:
    | MINUS; e = exp {UnOp(Int(Minus), e)} %prec UMINUS
    | L_NOT; e = exp {UnOp(Bool(Not), e)}

tupExpEntry:
    | e = exp {e}
    | l = label; SINGLE_EQUAL; e = exp {TupLabel(Label(l), e)}
    | WILD; SINGLE_EQUAL; e = exp {TupLabel(ExplicitNonlabel, e)}

exp:
    | b = binExp { b }
    | i = INT { Atom (Int (Bigint.of_int i)) }
    | f = FLOAT { Atom (Float f) }
    | v = IDENT { Var v }
    | c = CONSTRUCTOR_IDENT { Constructor(c, None)}
    | l = QUOTED_LABEL { Label(l) }
    | c = CONSTRUCTOR_IDENT; SLASH_TILDE; { Constructor(c, Some(None)) }
    | c = CONSTRUCTOR_IDENT; TILDE; t = typ;  { Constructor(c, Some(Some(t))) }
    | e = exp; COLON; t = typ { Asc(e, t) }
    | PROJECTOR_INVOKE; OPEN_PAREN; e = exp; CLOSE_PAREN; { e }
    | s = STRING { Atom (String s)}
    | OPEN_TRIPLE_CURLY; e = exp; CLOSE_TRIPLE_CURLY { IndicationExp(e) }
    | OPEN_PAREN; e = exp; CLOSE_PAREN { e }
    | OPEN_PAREN; e = tupExpEntry; COMMA; l = separated_list(COMMA, tupExpEntry); CLOSE_PAREN { TupleExp(e :: l) }
    | OPEN_PAREN; l = label; SINGLE_EQUAL; e = exp; CLOSE_PAREN { TupleExp([TupLabel(Label(l), e)]) }
    | UNIT { TupleExp([]) }
    | c = case { c }
    | OPEN_SQUARE_BRACKET; e = separated_list(COMMA, exp); CLOSE_SQUARE_BRACKET { ListExp(e) }
    | f = exp; OPEN_PAREN; a = exp; CLOSE_PAREN { ApExp(f, a) }
    | f = exp; OPEN_PAREN; a = exp; COMMA; tl = separated_nonempty_list(COMMA, exp); CLOSE_PAREN { ApExp(f, TupleExp(a :: tl)) }
    | TAYLOR_DERIVATIVES; OPEN_PAREN; function_exp = exp; COMMA; order = exp; CLOSE_PAREN {
        ApExp(
          Var "taylor_derivatives",
          TupleExp([function_exp; order])
        )
      } %prec TAYLOR_CALL
    | TAYLOR_DERIVATIVES; OPEN_PAREN; function_exp = exp; COMMA; order = exp; CLOSE_PAREN; IN; body = exp {
        match order with
        | Atom (Int value) ->
            (match Bigint.to_int value with
             | Some count when count >= 0 ->
                 lower_scoped_taylor_derivatives function_exp count body
             | _ -> InvalidExp "Taylor derivative order must be a nonnegative integer")
        | _ -> InvalidExp "Taylor derivative order must be a nonnegative integer"
      } %prec LET_EXP
    | LET; i = pat; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { Let (i, e1, e2) } %prec LET_EXP
    | MODULE; i = IDENT; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { ModuleExp(VarPat(i), e1, e2) } %prec LET_EXP
    | MODULE; c = CONSTRUCTOR_IDENT; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { ModuleExp(VarPat(c), e1, e2) } %prec LET_EXP
    | MODULE; i = IDENT; COLON; t = typ; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { ModuleExp(AscPat(VarPat(i), t), e1, e2) } %prec LET_EXP
    | MODULE; c = CONSTRUCTOR_IDENT; COLON; t = typ; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { ModuleExp(AscPat(VarPat(c), t), e1, e2) } %prec LET_EXP
    | i = ifExp { i }
    | TRUE { Atom (Bool true) }
    | f = funExp {f}
    | FALSE { Atom (Bool false) }
    | FIX;  p = funPat; DASH_ARROW; e = exp { FixF(p, e) }
    | TYP_FUN; t = tpat; DASH_ARROW; e = exp {TypFun(t, e)}
    | QUESTION { EmptyHole }
    | a = filterAction; cond = exp; IN; body = exp { Filter(a, cond, body)} %prec LET_EXP
    | TEST; e = exp; END { Test(e) }
    | e1 = exp; AT_SYMBOL; e2 = exp { ListConcat(e1, e2) }
    | e1 = exp; CONS; e2 = exp { Cons(e1, e2) }
    | e1 = exp; SEMI_COLON; e2 = exp { Seq(e1, e2) }
    |  E_EXP; s = STRING; { InvalidExp(s) }
    |  WILD {Deferral}
    | e = exp; TYP_AP_SYMBOL; ty = typ; GREATER_THAN; {TypAp(e, ty)}
    | TYP; tp = tpat; SINGLE_EQUAL; ty = typ; IN; e = exp {TyAlias(tp, ty, e)} %prec LET_EXP
    | LESS_THAN; LESS_THAN; e = exp; QUESTION; s = QUOTED_LABEL; GREATER_THAN; GREATER_THAN {DynamicErrorHole(e, s)}
    | UNDEF; {Undefined}
    | u = unExp { u }
    | e1 = exp; TUPLE_EXTENSION; e2 = exp { TupleExtension(e1, e2) } %prec PLUS
    | e1 = exp; DOT; e2 = exp { Dot(e1, e2) }
    | OPEN_CURLY; items = separated_list(SEMI_COLON, modItem); CLOSE_CURLY { Module(items) }

/* Inside module bodies, semicolons are item separators, not Seq operators.
   MOD_ITEM_EXP precedence is higher than SEMI_COLON, so when the parser
   has a complete exp and sees ';', it reduces (treating ';' as a separator)
   rather than shifting (which would try to parse Seq). */
modItemExp:
    | e = exp { e } %prec MOD_ITEM_EXP

modItem:
    | LET; i = pat; SINGLE_EQUAL; e = modItemExp { ModItemLet(i, e) }
    | MODULE; i = IDENT; SINGLE_EQUAL; e = modItemExp { ModItemModule(VarPat(i), e) }
    | MODULE; c = CONSTRUCTOR_IDENT; SINGLE_EQUAL; e = modItemExp { ModItemModule(VarPat(c), e) }
    | MODULE; i = IDENT; COLON; t = typ; SINGLE_EQUAL; e = modItemExp { ModItemModule(AscPat(VarPat(i), t), e) }
    | MODULE; c = CONSTRUCTOR_IDENT; COLON; t = typ; SINGLE_EQUAL; e = modItemExp { ModItemModule(AscPat(VarPat(c), t), e) }
    | TYP; tp = tpat; SINGLE_EQUAL; ty = typ { ModItemType(tp, ty) }
    | e = modItemExp { ModItemExp(e) }

sigItem:
    | LET; p = pat { SigItemLet(p) }
    | TYP; tp = tpat; SINGLE_EQUAL; ty = typ { SigItemType(tp, ty) }
