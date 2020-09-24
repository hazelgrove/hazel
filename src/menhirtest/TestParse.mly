%{
        open Types
%}

%token <int> INT
%token PLUS MINUS
%token GT LT EQ
%token EOF

%left PLUS MINUS

%start main
%type <Types.expr> main
%%

main:
        e = expr EOF { e }
;

expr:
        | e = binary { e }
;

binary:
        | arith { $1 }
        | arith LT arith  { BinaryOp (LT, $1, $3) }
        | arith GT arith  { BinaryOp (GT, $1, $3) }
        | arith EQ arith  { BinaryOp (EQ, $1, $3) }
;

arith:
        | constant { $1 }
        | arith PLUS arith  { BinaryOp (Plus, $1, $3) }
        | arith MINUS arith  { BinaryOp (Minus, $1, $3) }
;

constant:
        INT     { Constant (Int $1) }
;
