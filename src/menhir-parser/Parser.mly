%{
open AST
%}

%token <string> IDENT
%token <string> STRING
%token TRUE 
%token FALSE
%token <int> INT
%token <float> FLOAT
%token LET
%token FUN
%token CASE
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token OPEN_SQUARE_BRACKET
%token CLOSE_SQUARE_BRACKET
%token OPEN_PAREN
%token CLOSE_PAREN
%token DASH_ARROW
%token EQUAL_ARROW
%token SINGLE_EQUAL
%token TURNSTILE

(* bin ops *)
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
(*logical ops*)
%token L_AND
%token L_OR
%token L_NOT
(*bitwise ops*)
%token B_AND

%token COMMA
%token COLON
%token EOF
%token IN
%token UNIT

(* type tokens *)
%token INT_TYPE
%token FLOAT_TYPE
%token BOOL_TYPE
%token STRING_TYPE

%token IF
%token THEN
%token ELSE

%type <AST.exp> exp

%start <AST.exp> program

%%

program:
    | e = exp; EOF {e}

binOp:
    | PLUS { Plus }
    | MINUS { Minus }
    | TIMES { Times }
    | POWER { Power }
    | DIVIDE { Divide }
    | DOUBLE_EQUAL { Equals }
    | NOT_EQUAL { NotEqual }
    | LESS_THAN { LessThan }
    | LESS_THAN_EQUAL { LessThanEqual }
    | GREATER_THAN { GreaterThan }
    | GREATER_THAN_EQUAL { GreaterThanEqual }
    | L_AND { Logical_And }
    | L_OR { Logical_Or }
    | L_NOT { Logical_Not }

binExp:
    | e1 = exp; b = binOp; e2 = exp { BinExp (e1, b, e2) }

typ:
    | INT_TYPE { IntType }
    | FLOAT_TYPE { FloatType }
    | BOOL_TYPE { BoolType }
    | STRING_TYPE { StringType }
    | UNIT { UnitType }
    | OPEN_PAREN; types = separated_list(COMMA, typ); CLOSE_PAREN { TupleType(types) }
    | OPEN_SQUARE_BRACKET; t = typ; CLOSE_SQUARE_BRACKET { ArrayType(t) }

pat: 
    | i = IDENT { Var(i) }
    | t = patTuple { t }
    | t = typeAnn { t }

patTuple: 
    | OPEN_PAREN; pats = separated_list(COMMA, pat); CLOSE_PAREN { TuplePat(pats) }

typeAnn: 
    | p = pat; COLON; t = typ { TypeAnn(p, t) }

rul:
    | TURNSTILE; p = pat; EQUAL_ARROW; e = exp; { (p, e) }

case:
    | CASE; e = exp; l = list(rul); { Rules(e, l) }

exp:
    | i = INT { Int i }
    | f = FLOAT { Float f }
    | v = IDENT { Var v }
    | s = STRING { String s}
    | b = binExp { b }
    | UNIT { Unit }
    | OPEN_SQUARE_BRACKET; e = separated_list(COMMA, exp); CLOSE_SQUARE_BRACKET { ArrayExp(e) }
    | LET; i = pat; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { Let (i, e1, e2) }
    | FUN; t = patTuple; DASH_ARROW; e1 = exp; { Fun (t, e1) }
    | IF; e1 = exp; THEN; e2 = exp; ELSE; e3 = exp { If (e1, e2, e3) }


