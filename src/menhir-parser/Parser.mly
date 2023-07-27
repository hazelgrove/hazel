%{
open AST
%}

%token <string> IDENT
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
%token EQUALS
%token PLUS
%token MINUS
%token COMMA
%token COLON
%token EOF

%type <AST.exp> exp

%start <AST.exp> program

%%

program:
    | e = exp; EOF {e}

binOp:
    | PLUS {"+"}
    | MINUS {"-"}
    | EQUALS {"="}

binExp:
    | e1 = exp; b = binOp; e2 = exp { BinOp (e1, b, e2) }

exp:
    | i = INT { Int i }
    | f = FLOAT { Float f }
    (* | s = STRING { String s } *)
    | i = IDENT { Ident i }
    | b = binExp { b }
