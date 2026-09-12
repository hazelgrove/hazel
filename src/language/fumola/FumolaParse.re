/* Fumola concrete syntax to editor terms: the printer, run backwards.

   FumolaPrint is the contract of the tile route, and until now it was checked
   by asking the real Fumola parser whether what we printed was acceptable and
   whether it meant the same. That catches a great deal, but it cannot catch a
   printer that is *self-consistently* wrong about a form it and the test
   corpus both spell the same way.

   A parser closes that: `parse(print(t)) == t` compares terms rather than
   text, so the two have to agree about structure and not merely about a
   string. Both use the one precedence ladder in FumolaPrint, so they cannot
   drift apart -- a level changed in one place changes both.

   This reads the M1 subset, which is what the editor can build. What it does
   not read is what the shipped `.fumola` corpus is made of: every file there
   is a `module { … }` of `public func`s with type annotations, and the type
   sublanguage is deliberately absent from this AST. Opening that corpus as
   tiles waits on types; see docs/fumola-tiles-design.md. */

open FumolaGrammar;

type token =
  | Ident(string)
  | Nat_(string)
  | Text_(string)
  | Tag(string) /* #tag */
  | Quoted(string) /* `name */
  | Sym(string) /* an operator or a delimiter */
  | End;

type error = {
  at: int,
  message: string,
};

exception Bad(error);

let fail = (at: int, message: string) =>
  raise(
    Bad({
      at,
      message,
    }),
  );

/* --- the lexer ---

   Fumola's `#` is the one character that means three things -- a variant
   prefix, the concatenation operator, and the opener of an attribute -- and
   the lexer is where they are told apart: `#` followed by a name is a tag,
   and `#` alone is the operator. lalrpop needs the same distinction and gets
   it the same way. */

let is_digit = c => c >= '0' && c <= '9';
let is_name_start = c =>
  c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_';
let is_name = c => is_name_start(c) || is_digit(c);

/* Longest first, so that `**` is not read as two `*` and `<=` not as `<`. */
let symbols = [
  "**%",
  "<<>",
  "<>>",
  "**",
  "+%",
  "-%",
  "*%",
  ":=",
  "==",
  "!=",
  "<=",
  ">=",
  "<<",
  ">>",
  "+",
  "-",
  "*",
  "/",
  "%",
  "#",
  "|",
  "&",
  "^",
  "<",
  ">",
  "(",
  ")",
  "{",
  "}",
  "[",
  "]",
  ",",
  ";",
  ".",
  "?",
  "@",
  "!",
  "~",
  "=",
  ":",
];

let lex = (src: string): list((int, token)) => {
  let n = String.length(src);
  let rec go = (i: int, acc: list((int, token))) =>
    if (i >= n) {
      List.rev([(i, End), ...acc]);
    } else {
      let c = src.[i];
      if (c == ' ' || c == '\n' || c == '\t' || c == '\r') {
        go(i + 1, acc);
      } else if (is_digit(c)) {
        let j = ref(i);
        while (j^ < n && (is_digit(src.[j^]) || src.[j^] == '_')) {
          incr(j);
        };
        go(j^, [(i, Nat_(String.sub(src, i, j^ - i))), ...acc]);
      } else if (is_name_start(c)) {
        let j = ref(i);
        while (j^ < n && is_name(src.[j^])) {
          incr(j);
        };
        go(j^, [(i, Ident(String.sub(src, i, j^ - i))), ...acc]);
      } else if (c == '"') {
        let j = ref(i + 1);
        while (j^ < n && src.[j^] != '"') {
          incr(j);
        };
        if (j^ >= n) {
          fail(i, "a text literal with no closing quote");
        };
        go(j^ + 1, [(i, Text_(String.sub(src, i, j^ + 1 - i))), ...acc]);
      } else if (c == '`') {
        let j = ref(i + 1);
        while (j^ < n && is_name(src.[j^])) {
          incr(j);
        };
        go(j^, [(i, Quoted(String.sub(src, i + 1, j^ - i - 1))), ...acc]);
      } else if (c == '#' && i + 1 < n && is_name_start(src.[i + 1])) {
        let j = ref(i + 1);
        while (j^ < n && is_name(src.[j^])) {
          incr(j);
        };
        go(j^, [(i, Tag(String.sub(src, i + 1, j^ - i - 1))), ...acc]);
      } else {
        switch (
          List.find_opt(
            s =>
              String.length(s) <= n
              - i
              && String.sub(src, i, String.length(s)) == s,
            symbols,
          )
        ) {
        | Some(s) => go(i + String.length(s), [(i, Sym(s)), ...acc])
        | None =>
          fail(
            i,
            "there is no Fumola token starting with " ++ String.make(1, c),
          )
        };
      };
    };
  go(0, []);
};

/* --- the parser --- */

type state = {mutable rest: list((int, token))};

let peek = (st: state) =>
  switch (st.rest) {
  | [(_, t), ..._] => t
  | [] => End
  };

let position = (st: state) =>
  switch (st.rest) {
  | [(i, _), ..._] => i
  | [] => 0
  };

let advance = (st: state) =>
  switch (st.rest) {
  | [_, ...tl] => st.rest = tl
  | [] => ()
  };

let eat = (st: state, s: string) =>
  switch (peek(st)) {
  | Sym(t) when t == s => advance(st)
  | Ident(t) when t == s => advance(st)
  | _ => fail(position(st), "expected " ++ s)
  };

let looking_at = (st: state, s: string) =>
  switch (peek(st)) {
  | Sym(t)
  | Ident(t) => t == s
  | _ => false
  };

let node = t => IdTagged.fresh(t);

/* The binary operators, by the level FumolaPrint gives them. One table, read
   by both directions. */
let bin_of_symbol = (s: string): option((bin, int)) =>
  switch (s) {
  | "+" => Some((Add, FumolaPrint.p_add))
  | "-" => Some((Sub, FumolaPrint.p_add))
  | "#" => Some((Cat, FumolaPrint.p_add))
  | "*" => Some((Mul, FumolaPrint.p_mul))
  | "/" => Some((Div, FumolaPrint.p_mul))
  | "%" => Some((Mod, FumolaPrint.p_mul))
  | "|" => Some((BitOr, FumolaPrint.p_bitor))
  | "&" => Some((BitAnd, FumolaPrint.p_bitand))
  | "^" => Some((Xor, FumolaPrint.p_xor))
  | "<<" => Some((ShL, FumolaPrint.p_shift))
  | ">>" => Some((ShR, FumolaPrint.p_shift))
  | "<<>" => Some((RotL, FumolaPrint.p_shift))
  | "<>>" => Some((RotR, FumolaPrint.p_shift))
  | "**" => Some((Pow, FumolaPrint.p_pow))
  | _ => None
  };

let rel_of_symbol = (s: string): option(rel) =>
  switch (s) {
  | "==" => Some(Eq)
  | "!=" => Some(Neq)
  | "<" => Some(Lt)
  | ">" => Some(Gt)
  | "<=" => Some(Le)
  | ">=" => Some(Ge)
  | _ => None
  };

let rec parse_exp = (st: state): FumolaTermBase.t => {
  /* ExpNonDec: everything looser than the binary chain. */
  switch (peek(st)) {
  | Ident("force") =>
    advance(st);
    node(Force(parse_atom(st)));
  | Ident("thunk") =>
    advance(st);
    node(Thunk(parse_block(st)));
  | Ident("assert") =>
    advance(st);
    node(Assert(parse_nest(st)));
  | Ident("ignore") =>
    advance(st);
    node(Ignore(parse_nest(st)));
  | Ident("return") =>
    advance(st);
    node(Return(Some(parse_exp(st))));
  | Ident("if") =>
    advance(st);
    let c = parse_atom(st);
    let t = parse_nest(st);
    if (looking_at(st, "else")) {
      advance(st);
      node(If(c, t, Some(parse_nest(st))));
    } else {
      node(If(c, t, None));
    };
  | Ident("switch") =>
    advance(st);
    let scrutinee = parse_atom(st);
    eat(st, "{");
    let rec cases = acc =>
      if (looking_at(st, "case")) {
        advance(st);
        let pat = parse_pat(st);
        let body = parse_exp(st);
        let acc = [
          {
            pat,
            body,
          },
          ...acc,
        ];
        if (looking_at(st, ";")) {
          advance(st);
          cases(acc);
        } else {
          acc;
        };
      } else {
        acc;
      };
    let cs = List.rev(cases([]));
    eat(st, "}");
    node(Switch(scrutinee, cs));
  | Sym("@") =>
    advance(st);
    node(Get(parse_atom(st)));
  | Ident("do") =>
    advance(st);
    switch (peek(st)) {
    | Sym("@") =>
      advance(st);
      let target = parse_atom(st);
      node(DoPutForce(target, parse_nest(st)));
    | Ident("goto")
    | Ident("within") =>
      let nav =
        switch (peek(st)) {
        | Ident("goto") => Goto
        | _ => Within
        };
      advance(st);
      let dim = parse_atom(st);
      let e = parse_atom(st);
      node(DoNav(nav, dim, e, parse_block(st)));
    | _ => node(Block(parse_block(st)))
    };
  | _ =>
    /* `e := e` takes an ExpBin0 on the left and a whole Exp on the right. */
    let left = parse_bin(st, FumolaPrint.p_or);
    if (looking_at(st, ":=")) {
      advance(st);
      node(Put(left, parse_exp(st)));
    } else {
      left;
    };
  };
}

/* An ExpNest position: a brace block is a block here, and anything else is an
   ordinary expression. */
and parse_nest = (st: state): FumolaTermBase.t =>
  if (looking_at(st, "{")) {
    node(Block(parse_block(st)));
  } else {
    parse_exp(st);
  }

and parse_block = (st: state): list(FumolaTermBase.dec) => {
  eat(st, "{");
  let rec decs = acc =>
    if (looking_at(st, "}")) {
      acc;
    } else {
      let d = parse_dec(st);
      let acc = [d, ...acc];
      if (looking_at(st, ";")) {
        advance(st);
        decs(acc);
      } else {
        acc;
      };
    };
  let ds = List.rev(decs([]));
  eat(st, "}");
  ds;
}

and parse_dec = (st: state): FumolaTermBase.dec =>
  switch (peek(st)) {
  | Ident("let") =>
    advance(st);
    let p = parse_pat(st);
    eat(st, "=");
    node(DLet(p, parse_exp(st)));
  | Ident("var") =>
    advance(st);
    let p = parse_pat(st);
    eat(st, "=");
    node(DVar(p, parse_exp(st)));
  | Ident("func") =>
    advance(st);
    let name =
      switch (peek(st)) {
      | Ident(x) =>
        advance(st);
        x;
      | _ => fail(position(st), "a func needs a name")
      };
    let p = parse_pat(st);
    node(DFunc(name, p, parse_block(st)));
  | _ => node(DExp(parse_exp(st)))
  }

/* Precedence climbing over FumolaPrint's ladder. Left-associative
   throughout, except the shifts, which the grammar makes non-associative by
   drawing both operands from the tighter nonterminal. */
and parse_bin = (st: state, level: int): FumolaTermBase.t =>
  if (level > FumolaPrint.p_pow) {
    parse_un(st);
  } else {
    let left = ref(parse_bin(st, level + 1));
    let continue_ = ref(true);
    while (continue_^) {
      switch (peek(st)) {
      | Sym(s) =>
        switch (bin_of_symbol(s)) {
        | Some((op, l)) when l == level =>
          advance(st);
          let right = parse_bin(st, level + 1);
          left := node(Bin(left^, op, right));
          /* Non-associative: one shift per level, then stop. */
          if (level == FumolaPrint.p_shift) {
            continue_ := false;
          };
        | _ =>
          switch (rel_of_symbol(s)) {
          | Some(op) when level == FumolaPrint.p_rel =>
            advance(st);
            left := node(Rel(left^, op, parse_bin(st, level + 1)));
          | _ => continue_ := false
          }
        }
      | Ident("and") when level == FumolaPrint.p_and =>
        advance(st);
        left := node(And(left^, parse_bin(st, level + 1)));
      | Ident("or") when level == FumolaPrint.p_or =>
        advance(st);
        left := node(Or(left^, parse_bin(st, level + 1)));
      | _ => continue_ := false
      };
    };
    left^;
  }

and parse_un = (st: state): FumolaTermBase.t =>
  switch (peek(st)) {
  | Sym("?") =>
    advance(st);
    node(Opt(parse_un(st)));
  | Sym("-") =>
    advance(st);
    node(Un(Neg, parse_un(st)));
  | Sym("+") =>
    advance(st);
    node(Un(Pos, parse_un(st)));
  | Sym("^") =>
    advance(st);
    node(Un(BitNot, parse_un(st)));
  | Sym("~") =>
    advance(st);
    node(Unquote(parse_un(st)));
  | Ident("not") =>
    advance(st);
    node(Not(parse_un(st)));
  | Tag(t) =>
    advance(st);
    /* A variant's payload is an atom, and is optional. */
    if (starts_atom(st)) {
      node(Variant(t, Some(parse_atom(st))));
    } else {
      node(Variant(t, None));
    };
  | _ => parse_post(st)
  }

/* Whether what comes next could begin an atom -- which is how juxtaposed
   application is told from the end of an expression, since neither has a
   token of its own. */
and starts_atom = (st: state): bool =>
  switch (peek(st)) {
  | Ident(x) =>
    !
      List.mem(
        x,
        [
          "and",
          "or",
          "not",
          "else",
          "case",
          "in",
          "let",
          "var",
          "func",
          "do",
          "if",
          "switch",
          "force",
          "thunk",
          "assert",
          "ignore",
          "return",
          "end",
        ],
      )
  | Nat_(_)
  | Text_(_)
  | Quoted(_)
  | Tag(_) => true
  | Sym("(")
  | Sym("[") => true
  | _ => false
  }

and parse_post = (st: state): FumolaTermBase.t => {
  let e = ref(parse_atom(st));
  let continue_ = ref(true);
  while (continue_^) {
    switch (peek(st)) {
    | Sym(".") =>
      advance(st);
      switch (peek(st)) {
      | Ident(f) =>
        advance(st);
        e := node(Proj(e^, f));
      | Nat_(i) =>
        advance(st);
        e := node(Proj(e^, i));
      | _ => fail(position(st), "a projection needs a name or an index")
      };
    | Sym("[") =>
      advance(st);
      let i = parse_exp(st);
      eat(st, "]");
      e := node(Index(e^, i));
    | Sym("!") =>
      advance(st);
      e := node(Bang(e^));
    | _ =>
      /* Juxtaposition: an atom right here is an argument. */
      if (starts_atom(st)) {
        e := node(Ap(e^, parse_atom(st)));
      } else {
        continue_ := false;
      }
    };
  };
  e^;
}

and parse_atom = (st: state): FumolaTermBase.t =>
  switch (peek(st)) {
  | Nat_(n) =>
    advance(st);
    node(Lit(Nat(n)));
  | Text_(s) =>
    advance(st);
    node(Lit(Text(s)));
  | Quoted(x) =>
    advance(st);
    node(QuotedId(x));
  | Tag(t) =>
    advance(st);
    node(Variant(t, None));
  | Ident("true") =>
    advance(st);
    node(Lit(Bool(true)));
  | Ident("false") =>
    advance(st);
    node(Lit(Bool(false)));
  | Ident("null") =>
    advance(st);
    node(Lit(Null));
  | Ident("prim") =>
    advance(st);
    switch (peek(st)) {
    | Text_(s) =>
      advance(st);
      /* The name, without its quotes. */
      node(Prim(String.sub(s, 1, String.length(s) - 2)));
    | _ => fail(position(st), "prim needs the name of one, in quotes")
    };
  | Ident(x) =>
    advance(st);
    node(Var(x));
  | Sym("(") =>
    advance(st);
    if (looking_at(st, ")")) {
      advance(st);
      node(Lit(Unit));
    } else {
      let first = parse_exp(st);
      if (looking_at(st, ",")) {
        let rec more = acc =>
          if (looking_at(st, ",")) {
            advance(st);
            more([parse_exp(st), ...acc]);
          } else {
            acc;
          };
        let parts = List.rev(more([first]));
        eat(st, ")");
        node(Tuple(parts));
      } else {
        eat(st, ")");
        node(Paren(first));
      };
    };
  | Sym("[") =>
    advance(st);
    let is_var = looking_at(st, "var");
    if (is_var) {
      advance(st);
    };
    let rec items = acc =>
      if (looking_at(st, "]")) {
        acc;
      } else {
        let e = parse_exp(st);
        let acc = [e, ...acc];
        if (looking_at(st, ",")) {
          advance(st);
          items(acc);
        } else {
          acc;
        };
      };
    let es = List.rev(items([]));
    eat(st, "]");
    node(Array(is_var, es));
  | Sym("{") => node(Block(parse_block(st)))
  | _ => fail(position(st), "expected an expression")
  }

and parse_pat = (st: state): FumolaTermBase.pat =>
  switch (peek(st)) {
  | Ident("_") =>
    advance(st);
    node(PWild);
  | Ident(x) =>
    advance(st);
    node(PVar(x));
  | Nat_(n) =>
    advance(st);
    node(PLit(Nat(n)));
  | Tag(t) =>
    advance(st);
    if (starts_atom(st)) {
      node(PVariant(t, Some(parse_pat(st))));
    } else {
      node(PVariant(t, None));
    };
  | Sym("?") =>
    advance(st);
    node(POpt(parse_pat(st)));
  | Sym("(") =>
    advance(st);
    if (looking_at(st, ")")) {
      advance(st);
      node(PLit(Unit));
    } else {
      let first = parse_pat(st);
      if (looking_at(st, ",")) {
        let rec more = acc =>
          if (looking_at(st, ",")) {
            advance(st);
            more([parse_pat(st), ...acc]);
          } else {
            acc;
          };
        let parts = List.rev(more([first]));
        eat(st, ")");
        node(PTuple(parts));
      } else {
        eat(st, ")");
        node(PParen(first));
      };
    };
  | _ => fail(position(st), "expected a pattern")
  };

/* A whole program: declarations separated by `;`, as Fumola's Prog is. */
let program = (src: string): result(list(FumolaTermBase.dec), error) =>
  switch (
    {
      let st = {rest: lex(src)};
      let rec decs = acc => {
        let d = parse_dec(st);
        let acc = [d, ...acc];
        if (looking_at(st, ";")) {
          advance(st);
          peek(st) == End ? acc : decs(acc);
        } else {
          acc;
        };
      };
      let ds = List.rev(decs([]));
      switch (peek(st)) {
      | End => ds
      | _ => fail(position(st), "there is more here than one program")
      };
    }
  ) {
  | ds => Ok(ds)
  | exception (Bad(e)) => Error(e)
  };

let exp = (src: string): result(FumolaTermBase.t, error) =>
  switch (
    {
      let st = {rest: lex(src)};
      let e = parse_exp(st);
      switch (peek(st)) {
      | End => e
      | _ => fail(position(st), "there is more here than one expression")
      };
    }
  ) {
  | e => Ok(e)
  | exception (Bad(e)) => Error(e)
  };
