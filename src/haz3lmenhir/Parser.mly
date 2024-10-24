%{
open AST
%}




%token UNDEF
%token <string> SEXP_STRING
%token DOLLAR_SIGN
%token TYP
%token TYP_FUN
%token FIX
%token WILD
%token QUESTION
%token AT_SYMBOL
%token CONS
%token TEST
%token PAUSE
%token DEBUG
%token HIDE
%token EVAL
%token <string> IDENT
%token <string> CONSTRUCTOR_IDENT
%token <string> STRING
%token <string> BUILTIN
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
%token DASH_ARROW
%token EQUAL_ARROW
%token SINGLE_EQUAL
%token TURNSTILE

(* Int ops *)
%token DOUBLE_EQUAL
%token NOT_EQUAL
%token PLUS
%token MINUS
%token DIVIDE
%token POWER
%token TIMES
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

%nonassoc IF_EXP
%nonassoc LET_EXP

%left DASH_ARROW

%left PLUS MINUS TIMES POWER DIVIDE DOUBLE_EQUAL NOT_EQUAL LESS_THAN_EQUAL GREATER_THAN_EQUAL 

%left GREATER_THAN LESS_THAN

%left PLUS_FLOAT MINUS_FLOAT TIMES_FLOAT POWER_FLOAT DIVIDE_FLOAT DOUBLE_EQUAL_FLOAT NOT_EQUAL_FLOAT LESS_THAN_FLOAT LESS_THAN_EQUAL_FLOAT GREATER_THAN_FLOAT GREATER_THAN_EQUAL_FLOAT

(* Other *)
%left CONS
%left OPEN_PAREN
%left QUESTION
%left COLON
%left COMMA
%left AT_SYMBOL
%left SEMI_COLON
%left IN
%left DOLLAR_SIGN
%left L_NOT L_AND L_OR

(* Might not be correct - milan *)
%left IDENT

%type <AST.exp> exp

%start <AST.exp> program

%%

program:
    | e = exp; EOF {e}

%inline intOp:
    | PLUS { IntOp(Plus) }
    | MINUS { IntOp(Minus) }
    | TIMES { IntOp(Times) }
    | POWER { IntOp(Power) }
    | DIVIDE { IntOp(Divide) }
    | DOUBLE_EQUAL { IntOp(Equals) }
    | NOT_EQUAL { IntOp(NotEquals) }
    (* | LESS_THAN { IntOp(LessThan) } *)
    | LESS_THAN_EQUAL { IntOp(LessThanOrEqual) }
    | GREATER_THAN { IntOp(GreaterThan) }
    | GREATER_THAN_EQUAL { IntOp(GreaterThanOrEqual) }


%inline floatOp:
    | PLUS_FLOAT { FloatOp(Plus) }
    | MINUS_FLOAT { FloatOp(Minus) }
    | TIMES_FLOAT { FloatOp(Times) }
    | POWER_FLOAT { FloatOp(Power) }
    | DIVIDE_FLOAT { FloatOp(Divide) }
    | DOUBLE_EQUAL_FLOAT { FloatOp(Equals) }
    | NOT_EQUAL_FLOAT { FloatOp(NotEquals) }
    | LESS_THAN_FLOAT { FloatOp(LessThan) }
    | LESS_THAN_EQUAL_FLOAT { FloatOp(LessThanOrEqual) }
    | GREATER_THAN_FLOAT { FloatOp(GreaterThan) }
    | GREATER_THAN_EQUAL_FLOAT { FloatOp(GreaterThanOrEqual) }

%inline boolOp:
    | L_AND { BoolOp(And) }
    | L_OR { BoolOp(Or) }

%inline binOp:
    | i = intOp { i }
    | f = floatOp { f }
    | b = boolOp { b }

binExp:
    | e1 = exp; b = binOp; e2 = exp { BinExp (e1, b, e2) }

typ:
    | QUESTION; s = STRING { InvalidTyp(s) }
    | INT_TYPE { IntType }
    | FLOAT_TYPE { FloatType }
    | BOOL_TYPE { BoolType }
    | STRING_TYPE { StringType }
    | UNKNOWN; INTERNAL { UnknownType(Internal) }
    | UNIT { UnitType }
    | OPEN_PAREN; types = separated_list(COMMA, typ); CLOSE_PAREN { TupleType(types) }
    | OPEN_SQUARE_BRACKET; t = typ; CLOSE_SQUARE_BRACKET { ArrayType(t) }
    | t1 = typ; DASH_ARROW; t2 = typ { ArrowType(t1, t2) }

pat:
    | QUESTION; s = STRING { InvalidPat(s) }
    | WILD { WildPat }
    | QUESTION { EmptyHolePat }
    | OPEN_SQUARE_BRACKET; l = separated_list(COMMA, pat); CLOSE_SQUARE_BRACKET; { ListPat(l) }
    | c = CONSTRUCTOR_IDENT; COLON; t = typ;  { ConstructorPat(c, t) }
    | p = IDENT { VarPat(p) }
    | t = patTuple { t }
    | i = INT { IntPat i }
    | f = FLOAT { FloatPat f }
    | s = STRING { StringPat s}
    | TRUE { BoolPat true}
    | FALSE {BoolPat false}
    (* | p1 = pat; AS; p2 = pat; { AsPat(p1, p2) } *)
    | p1 = pat; CONS; p2 = pat { ConsPat(p1, p2) } 
    | f = pat; OPEN_PAREN; a = pat; CLOSE_PAREN { ApPat(f, a) }


patTuple: 
    | OPEN_PAREN; pats = separated_list(COMMA, pat); CLOSE_PAREN { TuplePat(pats) }

rul:
    | TURNSTILE; p = pat; EQUAL_ARROW; e = exp; { (p, e) }

case:
    | CASE; e = exp; l = list(rul); END; { CaseExp(e, l) }

funExp: 
    | FUN; p = pat; DASH_ARROW; e1 = exp; { Fun (p, e1, None) }
    | FUN; p = pat; DASH_ARROW; e1 = exp; name = IDENT { Fun (p, e1, Some(name)) }


%inline ifExp:
    | IF; e1 = exp; THEN; e2 = exp; ELSE; e3 = exp { If (e1, e2, e3) } %prec IF_EXP

filterAction:
    | PAUSE { Pause }
    | DEBUG { Debug }
    | HIDE { Hide }
    | EVAL { Eval }

tpat:
    | QUESTION; s = STRING {InvalidTPat(s)}
    | QUESTION {EmptyHoleTPat}
    | v = IDENT {VarTPat v}

unExp:
    | DOLLAR_SIGN; e = exp {UnOp(Meta(Unquote), e)}
    | MINUS; e = exp {UnOp(Int(Minus), e)}
    | L_NOT; e = exp {UnOp(Bool(Not), e)}


exp:
    | b = binExp { b }
    | i = INT { Int i }
    | f = FLOAT { Float f }
    | v = IDENT { Var v }
    | c = CONSTRUCTOR_IDENT; COLON; t = typ { Constructor(c, t) }
    | s = STRING { String s}
    | OPEN_PAREN; e = exp; CLOSE_PAREN { e } 
    | OPEN_PAREN; e = exp; COMMA; l = separated_list(COMMA, exp); CLOSE_PAREN { TupleExp(e :: l) }
    | c = case { c }
    | OPEN_SQUARE_BRACKET; e = separated_list(COMMA, exp); CLOSE_SQUARE_BRACKET { ListExp(e) }
    | f = exp; OPEN_PAREN; a = exp; CLOSE_PAREN { ApExp(f, a) } 
    | LET; i = pat; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { Let (i, e1, e2) } %prec LET_EXP
    | i = ifExp { i }
    | e1 = exp; QUESTION; LESS_THAN; t1 = typ; EQUAL_ARROW; t2 = typ; GREATER_THAN {FailedCast(e1, t1, t2)}
    | e1 = exp; LESS_THAN; t1 = typ; EQUAL_ARROW; t2 = typ; GREATER_THAN { Cast(e1, t1, t2) }
    | TRUE { Bool true }
    | f = funExp {f}
    | FALSE { Bool false }    
    | FIX;  p = pat; DASH_ARROW; e = exp { FixF(p, e) }
    | TYP_FUN; t = tpat; DASH_ARROW; e = exp {TypFun(t, e)}
    | QUESTION { EmptyHole }
    | a = filterAction; cond = exp; COMMA; body = exp { Filter(a, cond, body)}
    | TEST; e = exp; END { Test(e) }
    | e1 = exp; AT_SYMBOL; e2 = exp { ListConcat(e1, e2) }
    | e1 = exp; CONS; e2 = exp { Cons(e1, e2) }
    | e1 = exp; SEMI_COLON; e2 = exp { Seq(e1, e2) }
    | QUESTION; s = STRING; { InvalidExp(s) }
    |  WILD {Deferral}
    | e = exp; AT_SYMBOL; LESS_THAN; ty = typ; GREATER_THAN; {TypAp(e, ty)}
    | TYP; tp = tpat; SINGLE_EQUAL; ty = typ; IN; e = exp {TyAlias(tp, ty, e)}
    | LESS_THAN; LESS_THAN; e = exp; QUESTION; s = SEXP_STRING; GREATER_THAN; GREATER_THAN {DynamicErrorHole(e, s)}
    | b = BUILTIN; {BuiltinFun(b)}
    | UNDEF; {Undefined}
    | u = unExp { u }
