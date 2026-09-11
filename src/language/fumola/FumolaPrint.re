/* Fumola editor terms to Fumola concrete syntax.

   This is the contract of the tile route: everything printed here must be
   accepted by crates/fumola_parser, and must mean there what it means here.
   Nothing else in Hazel has to be right about Fumola's grammar -- the tile
   engine parses, and this prints -- so this module is where the grammar's
   details are written down, and scripts/check-fumola-roundtrip.sh is where
   the claim is checked against the real parser.

   Two facts about Fumola's grammar drive the whole design, and both are
   easy to get wrong from intuition:

   1. `|`, `&` and `^` bind TIGHTER than `+` and `*`. The parser's chain runs
      or < and < rel < add < mul < bitor < bitand < xor < shift < pow, so
      `1 | 2 + 3` is `(1 | 2) + 3`. C says the opposite.

   2. The adapton forms -- `force e`, `@ e`, `thunk { … }`, `e := e`, and the
      `do` family -- live at ExpNonDec, which is LOOSER than every binary
      operator. They are not merely low-precedence: `force x + 1` and
      `1 + thunk { 2 }` are syntax errors, because a binary operator's
      operands are drawn from a tighter nonterminal that cannot reach them.
      So an adapton form inside any operator needs parentheses to parse at
      all, not to read better.

   3. `{ … }` means two different things depending on where it stands. In an
      ExpNest position -- after `if`, `else`, `thunk`, `do`, `func` -- it is a
      block. Anywhere else it is an *object literal*, and `{ x }` evaluates to
      the record `{x = 5}` rather than to the value of `x`. Both spellings are
      accepted by the parser, so this is invisible to a check that only asks
      whether the source parses. The printer therefore never emits bare braces
      outside a nest position: a Block elsewhere is printed `do { … }`, which
      is a block wherever it stands. */

open FumolaGrammar;

/* The precedence ladder, loosest first, mirroring the ExpBin0..ExpBin9 chain
   of parser.lalrpop. The numbers have no meaning beyond their order, but the
   order is the grammar's and must not be tidied. */
let p_stmt = 0; /* ExpNonDec: :=, if, switch, thunk, force, @, do, … */
let p_or = 1; /* ExpBin0 */
let p_and = 2; /* ExpBin1 */
let p_rel = 3; /* ExpBin2 */
let p_add = 4; /* ExpBin3: + +% - -% # */
let p_mul = 5; /* ExpBin4: * *% / % */
let p_bitor = 6; /* ExpBin5: | */
let p_bitand = 7; /* ExpBin6: & */
let p_xor = 8; /* ExpBin7: ^ */
let p_shift = 9; /* ExpBin8: << >> <<> <>> -- non-associative */
let p_pow = 10; /* ExpBin9: ** **% -- left-associative */
let p_un = 11; /* ExpUn */
let p_post = 12; /* ExpPost */
let p_atom = 13; /* ExpNullary */

let prec_of_bin =
  fun
  | Add
  | Sub
  | Cat => p_add
  | Mul
  | Div
  | Mod => p_mul
  | BitOr => p_bitor
  | BitAnd => p_bitand
  | Xor => p_xor
  | ShL
  | ShR
  | RotL
  | RotR => p_shift
  | Pow => p_pow;

let bin_token =
  fun
  | Add => "+"
  | Sub => "-"
  | Mul => "*"
  | Div => "/"
  | Mod => "%"
  | Pow => "**"
  | Cat => "#"
  | BitOr => "|"
  | BitAnd => "&"
  | Xor => "^"
  | ShL => "<<"
  | ShR => ">>"
  | RotL => "<<>"
  | RotR => "<>>";

let rel_token =
  fun
  | Eq => "=="
  | Neq => "!="
  | Lt => "<"
  | Gt => ">"
  | Le => "<="
  | Ge => ">=";

let un_token =
  fun
  | Pos => "+"
  | Neg => "-"
  | BitNot => "^";

let nav_token =
  fun
  | Goto => "goto"
  | Within => "within";

let lit_token =
  fun
  | Nat(s) => s
  | Float(s) => s
  | Char(s) => s
  | Text(s) => s
  | Bool(true) => "true"
  | Bool(false) => "false"
  | Null => "null"
  | Unit => "()";

/* `<<` and `>>` are spelled " << " and " >> " in the grammar: tokens with
   literal spaces in them, a lexer hack keeping them apart from the type
   brackets. Every binary operator here is printed with a space on each side,
   so those two come out right by the same rule as the rest -- but a printer
   that ever tightened its spacing would break them, and only them. */

let parens = s => "(" ++ s ++ ")";

/* `~explicit` parenthesizes every compound term rather than only the ones
   the grammar requires. It exists for the differential check: printing one
   term both ways and evaluating both is what gives the round-trip script the
   power to catch a wrong precedence level. Without it the script can only ask
   whether the source parses, and a misgrouped program parses fine. */
let rec exp = (~explicit=false, ~prec as ctx: int, e: exp('a)): string => {
  let (level, text) = exp_at(~explicit, e);
  level < ctx || explicit && level < p_atom ? parens(text) : text;
}

/* The level a term sits at, and its text with no outer parentheses. */
and exp_at = (~explicit, e: exp('a)): (int, string) =>
  switch (Annotated.term_of(e)) {
  | Hole(h) => (p_atom, hole(h))

  /* --- atoms --- */
  | Var(x) => (p_atom, x)
  | Lit(l) => (p_atom, lit_token(l))
  | Paren(e) => (p_atom, parens(exp(~explicit, ~prec=p_stmt, e)))
  | Tuple(es) => (
      p_atom,
      parens(
        es
        |> List.map(x => exp(~explicit, ~prec=p_stmt, x))
        |> String.concat(", "),
      ),
    )
  /* A brace block is only a block in a nest position; anywhere else bare
     braces read as an object literal, so it goes out as `do { … }`. That is
     an ExpNonDec, hence p_stmt and hence parenthesized inside any operator. */
  | Block(ds) => (p_stmt, "do " ++ block(~explicit, ds))
  | Array(is_var, es) => (
      p_atom,
      "["
      ++ (is_var ? "var " : "")
      ++ (
        es
        |> List.map(x => exp(~explicit, ~prec=p_stmt, x))
        |> String.concat(", ")
      )
      ++ "]",
    )
  | QuotedId(x) => (p_atom, "`" ++ x)
  | Prim(name) => (p_atom, "prim \"" ++ name ++ "\"")

  /* --- ExpPost. The argument of an application is an ExpNullary, so
     `f (g x)` keeps its parentheses and `f g x` means `(f g) x`. --- */
  | Ap(f, a) => (
      p_post,
      exp(~explicit, ~prec=p_post, f)
      ++ " "
      ++ exp(~explicit, ~prec=p_atom, a),
    )
  | Proj(e, field) => (
      p_post,
      exp(~explicit, ~prec=p_post, e) ++ "." ++ field,
    )
  | Index(e, i) => (
      p_post,
      exp(~explicit, ~prec=p_post, e)
      ++ "["
      ++ exp(~explicit, ~prec=p_stmt, i)
      ++ "]",
    )
  | Bang(e) => (p_post, exp(~explicit, ~prec=p_post, e) ++ "!")

  /* --- ExpUn. A variant's payload is an ExpNullary, unlike the other
     unary forms, whose operand is another ExpUn. --- */
  | Variant(tag, None) => (p_un, "#" ++ tag)
  | Variant(tag, Some(e)) => (
      p_un,
      "#" ++ tag ++ " " ++ exp(~explicit, ~prec=p_atom, e),
    )
  | Opt(e) => (p_un, "?" ++ exp(~explicit, ~prec=p_un, e))
  | Un(u, e) => (p_un, un_token(u) ++ exp(~explicit, ~prec=p_un, e))
  | Not(e) => (p_un, "not " ++ exp(~explicit, ~prec=p_un, e))
  | Unquote(e) => (p_un, "~" ++ exp(~explicit, ~prec=p_un, e))

  /* --- the binary chain. Every level is left-recursive in the grammar, so
     the left operand sits at the operator's own level and the right operand
     one level tighter -- except the shifts, which take a tighter
     nonterminal on both sides and so are non-associative. --- */
  | Bin(l, op, r) =>
    let p = prec_of_bin(op);
    /* Non-associative: ExpBin8 draws both operands from ExpBin9. */
    let (lp, rp) = p == p_shift ? (p + 1, p + 1) : (p, p + 1);
    (
      p,
      exp(~explicit, ~prec=lp, l)
      ++ " "
      ++ bin_token(op)
      ++ " "
      ++ exp(~explicit, ~prec=rp, r),
    );
  | Rel(l, op, r) => (
      p_rel,
      exp(~explicit, ~prec=p_rel, l)
      ++ " "
      ++ rel_token(op)
      ++ " "
      ++ exp(~explicit, ~prec=p_rel + 1, r),
    )
  | And(l, r) => (
      p_and,
      exp(~explicit, ~prec=p_and, l)
      ++ " and "
      ++ exp(~explicit, ~prec=p_and + 1, r),
    )
  | Or(l, r) => (
      p_or,
      exp(~explicit, ~prec=p_or, l)
      ++ " or "
      ++ exp(~explicit, ~prec=p_or + 1, r),
    )

  /* --- ExpNonDec. Everything below is looser than every operator above. --- */
  | If(c, t, None) => (
      p_stmt,
      "if "
      ++ exp(~explicit, ~prec=p_atom, c)
      ++ " "
      ++ exp_nest(~explicit, t),
    )
  | If(c, t, Some(f)) => (
      p_stmt,
      "if "
      ++ exp(~explicit, ~prec=p_atom, c)
      ++ " "
      ++ exp_nest(~explicit, t)
      ++ " else "
      ++ exp_nest(~explicit, f),
    )
  | Switch(e, cases) => (
      p_stmt,
      "switch "
      ++ exp(~explicit, ~prec=p_atom, e)
      ++ " { "
      ++ (cases |> List.map(x => case(~explicit, x)) |> String.concat("; "))
      ++ " }",
    )
  | Assert(e) => (p_stmt, "assert " ++ exp_nest(~explicit, e))
  | Ignore(e) => (p_stmt, "ignore " ++ exp_nest(~explicit, e))
  | Return(None) => (p_stmt, "return")
  | Return(Some(e)) => (
      p_stmt,
      "return " ++ exp(~explicit, ~prec=p_stmt, e),
    )

  /* --- the adapton core --- */
  | Thunk(ds) => (p_stmt, "thunk " ++ block(~explicit, ds))
  | Force(e) => (p_stmt, "force " ++ exp(~explicit, ~prec=p_atom, e))
  | Get(e) => (p_stmt, "@ " ++ exp(~explicit, ~prec=p_atom, e))
  /* `:=` takes an ExpBin0 on the left and a whole Exp on the right, which
     makes it right-associative and lets `a := b := c` stand. */
  | Put(l, r) => (
      p_stmt,
      exp(~explicit, ~prec=p_or, l)
      ++ " := "
      ++ exp(~explicit, ~prec=p_stmt, r),
    )
  | DoPutForce(e1, e2) => (
      p_stmt,
      "do @ "
      ++ exp(~explicit, ~prec=p_atom, e1)
      ++ " "
      ++ exp_nest(~explicit, e2),
    )
  | DoNav(nav, dim, e, ds) => (
      p_stmt,
      "do "
      ++ nav_token(nav)
      ++ " "
      ++ exp(~explicit, ~prec=p_atom, dim)
      ++ " "
      ++ exp(~explicit, ~prec=p_atom, e)
      ++ " "
      ++ block(~explicit, ds),
    )
  }

/* An ExpNest position: the one place bare braces are a block. Never
   parenthesized, because `({ x })` is a parenthesized object, not a block. */
and exp_nest = (~explicit, e: exp('a)): string =>
  switch (Annotated.term_of(e)) {
  | Block(ds) => block(~explicit, ds)
  | _ => exp(~explicit, ~prec=p_stmt, e)
  }

and block = (~explicit, ds: list(dec('a))): string =>
  switch (ds) {
  | [] => "{ }"
  | ds =>
    "{ "
    ++ (ds |> List.map(x => dec(~explicit, x)) |> String.concat("; "))
    ++ " }"
  }

and dec = (~explicit, d: dec('a)): string =>
  switch (Annotated.term_of(d)) {
  | DHole(h) => hole(h)
  | DExp(e) => exp(~explicit, ~prec=p_stmt, e)
  | DLet(p, e) =>
    "let " ++ pat(p) ++ " = " ++ exp(~explicit, ~prec=p_stmt, e)
  | DVar(p, e) =>
    "var " ++ pat(p) ++ " = " ++ exp(~explicit, ~prec=p_stmt, e)
  | DFunc(name, p, ds) =>
    "func " ++ name ++ pat_plain(p) ++ " " ++ block(~explicit, ds)
  }

and case = (~explicit, c: case('a)): string =>
  "case "
  ++ pat_nullary(c.pat)
  ++ " "
  ++ exp(~explicit, ~prec=p_stmt, c.body)

and pat = (p: pat('a)): string =>
  switch (Annotated.term_of(p)) {
  | PHole(h) => hole(h)
  | PVar(x) => x
  | PWild => "_"
  | PLit(l) => lit_token(l)
  | PParen(p) => parens(pat(p))
  | PTuple(ps) => parens(ps |> List.map(pat) |> String.concat(", "))
  | PVariant(tag, None) => "#" ++ tag
  | PVariant(tag, Some(p)) => "#" ++ tag ++ " " ++ pat_nullary(p)
  | POpt(p) => "?" ++ pat_nullary(p)
  }

/* A function's parameter is a PatPlain -- a parenthesized or tuple pattern --
   so a bare name has to be wrapped to stand there. */
and pat_plain = (p: pat('a)): string =>
  switch (Annotated.term_of(p)) {
  | PParen(_)
  | PTuple(_) => pat(p)
  | _ => parens(pat(p))
  }

/* A case's pattern and a variant's payload are PatNullary: an atom. */
and pat_nullary = (p: pat('a)): string =>
  switch (Annotated.term_of(p)) {
  | PVariant(_, Some(_))
  | POpt(_) => parens(pat(p))
  | _ => pat(p)
  }

/* A hole has no Fumola spelling. Printing one produces source the Fumola
   parser will reject, which is the honest outcome: an incomplete program
   should not be handed to the runtime as if it were complete. Callers check
   for holes before printing; this exists so that showing a term in a
   message or a test failure never raises. */
and hole =
  fun
  | EmptyHole => "?\u{25a1}"
  | Invalid(s) => s
  | MultiHole(_) => "?\u{25a1}";

/* The whole program, as the runtime is handed it. */
let program = (~explicit=false, ds: list(dec('a))): string =>
  ds |> List.map(x => dec(~explicit, x)) |> String.concat("; ");

let of_exp = (~explicit=false, e: exp('a)): string =>
  exp(~explicit, ~prec=p_stmt, e);

/* Does this term contain a hole? A term that does cannot be printed as
   Fumola source, and the caller should say so rather than send it. */
let rec has_hole = (e: exp('a)): bool =>
  switch (Annotated.term_of(e)) {
  | Hole(_) => true
  | Var(_)
  | Lit(_)
  | QuotedId(_)
  | Prim(_) => false
  | Paren(e)
  | Opt(e)
  | Un(_, e)
  | Not(e)
  | Unquote(e)
  | Bang(e)
  | Proj(e, _)
  | Assert(e)
  | Ignore(e)
  | Force(e)
  | Get(e)
  | Variant(_, Some(e)) => has_hole(e)
  | Variant(_, None)
  | Return(None) => false
  | Return(Some(e)) => has_hole(e)
  | Tuple(es)
  | Array(_, es) => List.exists(has_hole, es)
  | Block(ds)
  | Thunk(ds) => List.exists(has_hole_dec, ds)
  | Ap(a, b)
  | Index(a, b)
  | Bin(a, _, b)
  | Rel(a, _, b)
  | And(a, b)
  | Or(a, b)
  | Put(a, b)
  | DoPutForce(a, b) => has_hole(a) || has_hole(b)
  | If(c, t, f) =>
    has_hole(c)
    || has_hole(t)
    || Option.fold(~none=false, ~some=has_hole, f)
  | Switch(e, cases) =>
    has_hole(e) || List.exists(c => has_hole(c.body), cases)
  | DoNav(_, dim, e, ds) =>
    has_hole(dim) || has_hole(e) || List.exists(has_hole_dec, ds)
  }

and has_hole_dec = (d: dec('a)): bool =>
  switch (Annotated.term_of(d)) {
  | DHole(_) => true
  | DExp(e)
  | DLet(_, e)
  | DVar(_, e) => has_hole(e)
  | DFunc(_, _, ds) => List.exists(has_hole_dec, ds)
  };
