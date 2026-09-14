%{
open AST
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
%token <string> LIVELIT_IDENT
%token MOD_SEMI
%token TYP
%token TYP_FUN
%token FIX
%token WILD
%token QUESTION
%token AT_SYMBOL
%token TYP_AP_SYMBOL
%token CONS
%token TEST
%token HINT
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
%token <Bigint.t> INT
%token <float> FLOAT
%token LET
%token USE
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
%token PIPELINE
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
%token SINT_TYPE
%token NAT_TYPE
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


%left GREATER_THAN LESS_THAN DOUBLE_EQUAL NOT_EQUAL LESS_THAN_EQUAL GREATER_THAN_EQUAL NOT_EQUAL_FLOAT LESS_THAN_FLOAT LESS_THAN_EQUAL_FLOAT GREATER_THAN_FLOAT GREATER_THAN_EQUAL_FLOAT DOUBLE_EQUAL_FLOAT PIPELINE
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
    (* Ill-sorted labels appear in error-demo slides: (1="hello") *)
    | i = INT { Bigint.to_string(i) }

tupTypeEntry:
    | t = typ {t}
    | l = label; SINGLE_EQUAL; t = typ { TupLabelType(LabelType(l), t) }
    | WILD; SINGLE_EQUAL; t = typ { TupLabelType(ExplicitNonlabel, t) }

%inline tupleType:
    | OPEN_PAREN; hd = tupTypeEntry; COMMA; types = separated_list(COMMA, tupTypeEntry); CLOSE_PAREN { ParenTyp(TupleType(hd :: types)) }


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
    | SINT_TYPE { SIntType }
    | NAT_TYPE { NatType }
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
    (* Sums WITHOUT the leading plus: `Nil + Cons(Int, T)`. The bare-
       constructor head is spelled out so LR can distinguish it from
       TypVar by the PLUS lookahead. *)
    | c = CONSTRUCTOR_IDENT; PLUS; rest = separated_nonempty_list(PLUS, sumTerm) { SumTyp([Variant(c, None)] @ rest) }
    (* General no-lead head (covers Ctor(args) + … ; the bare-ctor head
       above stays explicit so LR distinguishes it from TypVar). *)
    | s1 = sumTerm; PLUS; rest = separated_nonempty_list(PLUS, sumTerm) { SumTyp([s1] @ rest) }
    | REC; c=tpat; DASH_ARROW; t = typ { RecType(c, t) }
    | OPEN_TRIPLE_CURLY; t = typ; CLOSE_TRIPLE_CURLY { IndicationTyp(t) }
    | OPEN_PAREN; t = typ; CLOSE_PAREN { ParenTyp(t) }
    | OPEN_PAREN; l = label; SINGLE_EQUAL; t = typ; CLOSE_PAREN { ParenTyp(TupleType([TupLabelType(LabelType(l), t)])) }
    | OPEN_PAREN; WILD; SINGLE_EQUAL; t = typ; CLOSE_PAREN { ParenTyp(TupleType([TupLabelType(ExplicitNonlabel, t)])) }
    | t1 = typ; TUPLE_EXTENSION; t2 = typ { ProdExtension(t1, t2) } %prec TYP_AP_SYMBOL
    | t1 = typ; DOT; t2 = typ { ProdProjection(t1, t2) }
    | OPEN_CURLY; items = separated_list(MOD_SEMI, sigItem); CLOSE_CURLY { Sig(items) }

tupPatEntry:
    | p = pat {p}
    | l = label; SINGLE_EQUAL; p = pat { TupLabelPat(LabelPat(l), p) }

nonAscriptingPat:
    | OPEN_TRIPLE_CURLY; p = pat; CLOSE_TRIPLE_CURLY { IndicationPat(p) }
    (* Trigger unwrap lives HERE (single home): every pat route,
       including fun params and ap heads/args, passes through. *)
    | PROJECTOR_INVOKE; OPEN_PAREN; p = pat; CLOSE_PAREN; { p }
    | OPEN_PAREN; p = pat; CLOSE_PAREN { ParenPat(p) }
    | OPEN_PAREN; l = label; SINGLE_EQUAL; p = pat; CLOSE_PAREN { ParenPat(TuplePat([TupLabelPat(LabelPat(l), p)])) }
    | OPEN_PAREN; WILD; SINGLE_EQUAL; p = pat; CLOSE_PAREN { ParenPat(TuplePat([TupLabelPat(ExplicitNonlabel, p)])) }
    | OPEN_PAREN; p = tupPatEntry; COMMA; pats = separated_list(COMMA, tupPatEntry); CLOSE_PAREN { ParenPat(TuplePat(p :: pats)) }
    |  P_PAT; s = STRING { InvalidPat(s) }
    | WILD { WildPat }
    | UNIT { TuplePat([]) }
    | QUESTION { EmptyHolePat }
    | OPEN_SQUARE_BRACKET; l = separated_list(COMMA, pat); CLOSE_SQUARE_BRACKET; { ListPat(l) }
    | c = CONSTRUCTOR_IDENT { ConstructorPat(c, None)}
    | c = CONSTRUCTOR_IDENT; TILDE; t = typ;  { AscPat(ConstructorPat(c, None), t) }
    | p = IDENT { VarPat(p) }
    | i = INT { AtomPat (Int i) }
    | f = FLOAT { AtomPat (Float f) }
    | s = STRING { AtomPat (String s)}
    | TRUE {AtomPat (Bool true)}
    | FALSE {AtomPat (Bool false)}
    | f = pat; OPEN_PAREN; a = pat; CLOSE_PAREN { ApPat(f, a) }
    | f = pat; UNIT { ApPat(f, TuplePat([])) }
    (* Multi-argument constructor patterns, labels allowed:
       Down(x, y), Some(value=v, count=c) — mirror of the exp side. *)
    | f = pat; OPEN_PAREN; a = tupPatEntry; COMMA; tl = separated_nonempty_list(COMMA, tupPatEntry); CLOSE_PAREN { ApPat(f, TuplePat(a :: tl)) }

(* One fun parameter, optionally ascribed. The ascription binds to the
   ELEMENT (MakeTerm parity: fun a, b : T -> e ascribes only b). Bare
   cons chains are legal params in Hazel (fun x :: y -> ...). *)
funAscElem:
    | p = funConsPat; { p }
    | p = funConsPat; COLON; t = ascTyp; { AscPat(p, t) }
    (* Labeled parameter: fun label=l, value=v -> ... *)
    | l = label; SINGLE_EQUAL; p = funAscElem; { TupLabelPat(LabelPat(l), p) }

(* KNOWN CONFLICT FAMILIES (menhir default resolutions, all pinned by
   the MenhirParser/MenhirFuzz/MenhirCorpus differential suites):
   1. fun-parameter CONS/COLON (below) — shift keeps them parameter-level.
   2. ~53 s/r states after `<form> ... exp` with COMMA/UNIT lookahead,
      introduced by the bare-tuple-at-let and nullary-ap productions —
      shift continues the inner exp, which is MakeTerm parity.
   3. One r/r between the two no-leading-plus sum productions (bare-ctor
      head vs general head) — identical semantic actions, either wins.
   4. Two r/r states from funConsPat/funConsTail sharing nonAscriptingPat
      and UNIT completions across head/tail contexts — identical
      semantic actions, either wins.
   Adding grammar rules? Rerun the three suites; do not trust silence. *)
(* KNOWN CONFLICT (one s/r + one r/r state, resolved by default): after
   `FUN nonAscriptingPat`, CONS/COLON could continue at the parameter
   level (funConsPat/funAscElem) or inside a generic `pat`. The default
   shift keeps them at the parameter level, which is MakeTerm parity —
   pinned by the MenhirParser equivalence tests. *)
funConsPat:
    | p = nonAscriptingPat; { p }
    | p = nonAscriptingPat; CONS; rest = funConsPat; { ConsPat(p, rest) }

funPat:
    | OPEN_PAREN; p1 = pat; COLON; t1 = typ; CLOSE_PAREN;  { ParenPat(AscPat(p1, t1)) }
    | p = funAscElem; { p }
    (* Multi-parameter sugar: fun a, b -> e binds a tuple pattern *)
    | p = funAscElem; COMMA; ps = separated_nonempty_list(COMMA, funAscElem);
      { TuplePat(p :: ps) }

pat:
    | p1 = pat; COLON; t1 = typ;  { AscPat(p1, t1) }
    (* | p1 = pat; AS; p2 = pat; { AsPat(p1, p2) } *)
    | p1 = pat; CONS; p2 = pat { ConsPat(p1, p2) } 
    | p = nonAscriptingPat; { p }


rul:
    | TURNSTILE; p = pat; EQUAL_ARROW; e = exp; { (p, e) }
    (* Bare tuple rule pattern: | Var(x), Var(y) => ... *)
    | TURNSTILE; p1 = pat; COMMA; ps = separated_nonempty_list(COMMA, pat); EQUAL_ARROW; e = exp; { (TuplePat(p1 :: ps), e) }

case:
    | CASE; e = exp; l = list(rul); END; { CaseExp(e, l) }

(* Types legal in a fun-parameter ascription without parentheses: no
   top-level arrow, so the -> after the ascription always closes the fun.
   Arrow-typed ascriptions need parens: fun f : (A -> B) -> ...
   NB the fun-ascription rules create two benign reduce/reduce conflicts
   (fun-level AscPat vs pat-level AscPat — identical semantics); menhir's
   arbitrary resolution picks the fun-level reduction, pinned by the
   grammar-gap tests in Test_Menhir. *)
ascTyp:
    | PROJECTOR_INVOKE; OPEN_PAREN; t = ascTyp; CLOSE_PAREN; { t }
    (* Binder types in ascription position: fun f : poly c -> (c -> c) -> …
       — the poly body is an ascTyp so the NEXT arrow stays the fun's. *)
    | POLY; a = tpat; DASH_ARROW; t = ascTyp { PolyType(a, t) }
    | REC; c = tpat; DASH_ARROW; t = ascTyp { RecType(c, t) }
    | c = CONSTRUCTOR_IDENT { TypVar(c) }
    | c = IDENT { TypVar(c) }
    | INT_TYPE { IntType }
    | SINT_TYPE { SIntType }
    | NAT_TYPE { NatType }
    | FLOAT_TYPE { FloatType }
    | BOOL_TYPE { BoolType }
    | STRING_TYPE { StringType }
    | UNIT { TupleType([]) }
    | QUESTION { UnknownType(EmptyHole) }
    | t = tupleType { t }
    | OPEN_SQUARE_BRACKET; t = typ; CLOSE_SQUARE_BRACKET { ArrayType(t) }
    | OPEN_PAREN; t = typ; CLOSE_PAREN { ParenTyp(t) }

(* NB fun bodies DO swallow `;` (Hazel: fun x -> 1; 2 is
   Fun(x, Seq(1, 2))). Module member boundaries are safe regardless:
   the lexer emits MOD_SEMI for member separators, which no exp
   production consumes. *)
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
    | i = INT { Atom (Int i) }
    | f = FLOAT { Atom (Float f) }
    | v = IDENT { Var v }
    | l = LIVELIT_IDENT { LivelitName l }
    (* Base-type keywords are ordinary constructors in exp position
       (HTML's Int/Float/Bool/String nodes) — MakeTerm parity. *)
    | INT_TYPE { Constructor("Int", None) }
    | SINT_TYPE { Constructor("SInt", None) }
    | NAT_TYPE { Constructor("Nat", None) }
    | FLOAT_TYPE { Constructor("Float", None) }
    | BOOL_TYPE { Constructor("Bool", None) }
    | STRING_TYPE { Constructor("String", None) }
    | c = CONSTRUCTOR_IDENT { Constructor(c, None)}
    | l = QUOTED_LABEL { Label(l) }
    | c = CONSTRUCTOR_IDENT; SLASH_TILDE; { Constructor(c, Some(None)) }
    | c = CONSTRUCTOR_IDENT; TILDE; t = typ;  { Constructor(c, Some(Some(t))) }
    | e = exp; COLON; t = typ { Asc(e, t) }
    | PROJECTOR_INVOKE; OPEN_PAREN; e = exp; CLOSE_PAREN; { e }
    | s = STRING { Atom (String s)}
    | OPEN_TRIPLE_CURLY; e = exp; CLOSE_TRIPLE_CURLY { IndicationExp(e) }
    | OPEN_PAREN; e = exp; CLOSE_PAREN { ParenExp(e) }
    | OPEN_PAREN; e = tupExpEntry; COMMA; l = separated_list(COMMA, tupExpEntry); CLOSE_PAREN { ParenExp(TupleExp(e :: l)) }
    | OPEN_PAREN; l = label; SINGLE_EQUAL; e = exp; CLOSE_PAREN { ParenExp(TupleExp([TupLabel(Label(l), e)])) }
    | OPEN_PAREN; WILD; SINGLE_EQUAL; e = exp; CLOSE_PAREN { ParenExp(TupleExp([TupLabel(ExplicitNonlabel, e)])) }
    | UNIT { TupleExp([]) }
    | c = case { c }
    | OPEN_SQUARE_BRACKET; e = separated_list(COMMA, exp); CLOSE_SQUARE_BRACKET { ListExp(e) }
    | f = exp; OPEN_PAREN; a = exp; CLOSE_PAREN { ApExp(f, a) }
    | f = exp; OPEN_PAREN; a = tupExpEntry; COMMA; tl = separated_nonempty_list(COMMA, tupExpEntry); CLOSE_PAREN { ApExp(f, TupleExp(a :: tl)) }
    | e1 = exp; PIPELINE; e2 = exp { PipelineExp(e1, e2) }
    | f = exp; UNIT { ApExp(f, TupleExp([])) }
    | f = exp; OPEN_PAREN; l = label; SINGLE_EQUAL; e = exp; CLOSE_PAREN { ApExp(f, TupleExp([TupLabel(Label(l), e)])) }
    | LET; i = pat; SINGLE_EQUAL; e1 = exp; IN; e2 = exp { Let (i, e1, e2) } %prec LET_EXP
    | USE; t = typ; IN; e = exp { Use(t, e) } %prec LET_EXP
    (* Bare tuples at a let: `let a, b = e in` / `let x = e1, e2 in` *)
    | LET; p1 = pat; COMMA; ps = separated_nonempty_list(COMMA, pat); SINGLE_EQUAL; e1 = exp; IN; e2 = exp { Let (TuplePat(p1 :: ps), e1, e2) } %prec LET_EXP
    | LET; i = pat; SINGLE_EQUAL; e1 = exp; COMMA; es = separated_nonempty_list(COMMA, exp); IN; e2 = exp { Let (i, TupleExp(e1 :: es), e2) } %prec LET_EXP
    | LET; p1 = pat; COMMA; ps = separated_nonempty_list(COMMA, pat); SINGLE_EQUAL; e1 = exp; COMMA; es = separated_nonempty_list(COMMA, exp); IN; e2 = exp { Let (TuplePat(p1 :: ps), TupleExp(e1 :: es), e2) } %prec LET_EXP
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
    | HINT; h = STRING; TEST; e = exp; END { HintedTest(e, Atom(Language.Atom.String(h))) }
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
    | OPEN_CURLY; items = separated_list(MOD_SEMI, modItem); CLOSE_CURLY { Module(items) }

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
    | TYP; tp = tpat { SigItemTypeAbstract(tp) }
    | MODULE; i = IDENT { SigItemModule(VarPat(i)) }
    | MODULE; c = CONSTRUCTOR_IDENT { SigItemModule(VarPat(c)) }
    | MODULE; i = IDENT; COLON; t = typ { SigItemModule(AscPat(VarPat(i), t)) }
    | MODULE; c = CONSTRUCTOR_IDENT; COLON; t = typ { SigItemModule(AscPat(VarPat(c), t)) }

