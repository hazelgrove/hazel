%{
open AST
%}



%token T_TYP
%token P_PAT
%token TP_TPAT
%token E_EXP
%token TILDE
%token NAMED_FUN
%token FORALL
%token REC
%token UNDEF
%token <string> SEXP_STRING
%token <string> PROJECTOR_INVOKE
%token DOLLAR_SIGN
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
%token FUN
%token CASE
%token OPEN_SQUARE_BRACKET
%token CLOSE_SQUARE_BRACKET
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_TRIPLE_CURLY
%token CLOSE_TRIPLE_CURLY
%token DASH_ARROW
%token EQUAL_ARROW
%token SINGLE_EQUAL
%token TURNSTILE
%token TUPLE_EXTENSION

(* Poly ops *)
%token DOUBLE_EQUAL
%token NOT_EQUAL

(* String ops *)
%token STRING_CONCAT
%token STRING_EQUAL

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
%token UNKNOWN
%token INTERNAL

%token IF
%token THEN
%token ELSE

%token SEMI_COLON



(* Precedences *)



%nonassoc LET_EXP
%right SEMI_COLON

%right SUM_TYP


%right DASH_ARROW
%nonassoc IF_EXP

%right L_OR
%right L_AND


%left GREATER_THAN LESS_THAN DOUBLE_EQUAL NOT_EQUAL LESS_THAN_EQUAL GREATER_THAN_EQUAL NOT_EQUAL_FLOAT LESS_THAN_FLOAT LESS_THAN_EQUAL_FLOAT GREATER_THAN_FLOAT GREATER_THAN_EQUAL_FLOAT DOUBLE_EQUAL_FLOAT STRING_EQUAL
%right STRING_CONCAT AT_SYMBOL
%right  CONS

%left PLUS MINUS PLUS_FLOAT MINUS_FLOAT TUPLE_EXTENSION
%left DIVIDE TIMES TIMES_FLOAT DIVIDE_FLOAT L_NOT

%right POWER POWER_FLOAT
%nonassoc UMINUS   /* Unary minus (prefix) */
%left COLON



%nonassoc TYP_AP_SYMBOL

%left OPEN_PAREN CLOSE_PAREN
%left DOLLAR_SIGN

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
    | STRING_EQUAL { StringOp(Equals) }

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
    | UNKNOWN; INTERNAL { UnknownType(Internal) }
    | QUESTION { UnknownType(EmptyHole) }
    | UNIT { TupleType([]) }
    | FORALL; a = tpat; DASH_ARROW; t = typ { ForallType(a, t) }
    | t = tupleType { t }
    | OPEN_SQUARE_BRACKET; t = typ; CLOSE_SQUARE_BRACKET { ArrayType(t) }
    | t1 = typ; DASH_ARROW; t2 = typ { ArrowType(t1, t2) }
    | s = sumTyp; { SumTyp(s) }
    | REC; c=tpat; DASH_ARROW; t = typ { RecType(c, t) }
    | OPEN_TRIPLE_CURLY; t = typ; CLOSE_TRIPLE_CURLY { IndicationTyp(t) }
    | OPEN_PAREN; t = typ; CLOSE_PAREN { t }
    | t1 = typ; OPEN_PAREN; t2 = typ; CLOSE_PAREN { ApTyp(t1, t2) }

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
    | DOLLAR_SIGN; e = exp {UnOp(Meta(Unquote), e)}
    | MINUS; e = exp {UnOp(Int(Minus), e)} %prec UMINUS
    | L_NOT; e = exp {UnOp(Bool(Not), e)}

tupExpEntry:
    | e = exp {e}
    | l = label; SINGLE_EQUAL; e = exp {TupLabel(Label(l), e)}

exp:
    | b = binExp { b }
    | i = INT { Atom (Int (Bigint.of_int i)) }
    | f = FLOAT { Atom (Float f) }
    | v = IDENT { Var v }
    | c = CONSTRUCTOR_IDENT { Constructor(c, None)}
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
    | LET; i = pat; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { Let (i, e1, e2) } %prec LET_EXP
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
    | e1 = exp; TUPLE_EXTENSION; e2 = exp { TupleExtension(e1, e2) }
