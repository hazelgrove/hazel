/* hazel-jit (^ticket(694); Matthew, 2026-11-05 and 06: "Reason code (hazel-jit let's call this component and code for it)
   compile some hazel ast into a mostly-equivalent JS code and call that instead"): an elaborated Hazel term compiled to the
   source text of a JavaScript function, for the best case of how much faster JS could run some Hazel code. Numbers are JS's
   (overflow and rounding may differ from Hazel's: allowed, for the best case). A form it does not compile raises
   Unsupported(reason), and the caller runs the term on the interpreter as before.

   Values in JS:
     Int, SInt, Nat, Float  a JS number
     Bool, String           a JS boolean, a JS string
     a tuple                a JS array of its components; a labeled component is also a property of the array, by its label
     a list                 null for [], {h, t} for a cons, so :: is O(1)
     a constructor          {c: name} or {c: name, v: payload}
     a function             a JS function of one argument (a tuple argument is an array)

   The code is expressions all the way down: a let's (or a match's) bindings are assigned, in a comma expression, to
   variables declared at the top of the enclosing JS function, so no closure is made for a let. Every binding gets a JS
   name of its own (x$12), so shadowing and the hoisting cannot clash. A free variable is the builtins' (the environment
   given): an OCaml builtin is the runtime's JS version of it, by name; a builtin written in Hazel (List.map, say) is
   compiled once, as a definition before the program's code. */

exception Unsupported(string);

let unsupported = reason => raise(Unsupported(reason));

/* What a compiled term needs at run time, passed to the compiled function as $R (Runtime.source is its JS text). */
module Runtime = {
  let source = {|{
  append(a, b) { if (a === null) return b; const xs = []; for (let p = a; p !== null; p = p.t) xs.push(p.h);
    let r = b; for (let i = xs.length - 1; i >= 0; i--) r = {h: xs[i], t: r}; return r; },
  tup(vs, ls) { for (let i = 0; i < ls.length; i++) if (ls[i] !== null) vs[ls[i]] = vs[i]; return vs; },
  eq(a, b) {
    if (a === b) return true;
    if (a === null || b === null || typeof a !== "object" || typeof b !== "object") return false;
    if (Array.isArray(a)) { if (!Array.isArray(b) || a.length !== b.length) return false;
      for (let i = 0; i < a.length; i++) if (!this.eq(a[i], b[i])) return false; return true; }
    if ("c" in a) return ("c" in b) && a.c === b.c && this.eq(a.v, b.v);
    let p = a, q = b;
    while (p !== null && q !== null) { if (!this.eq(p.h, q.h)) return false; p = p.t; q = q.t; }
    return p === q;
  },
  nomatch(where) { throw new Error("hazel-jit: no pattern matched in " + where); },
  b: {
    string_of_int: n => String(n), string_of_sint: n => String(n), string_of_nat: n => String(n),
    string_of_bool: b => String(b),
    float_of_int: n => n, float_of_sint: n => n, float_of_nat: n => n,
    int_of_float: f => Math.trunc(f), sint_of_float: f => Math.trunc(f),
    int_of_sint: n => n, sint_of_int: n => n,
    int_of_string: s => { const n = Number(s); if (!Number.isInteger(n)) throw new Error("int_of_string"); return n; },
    float_of_string: s => Number(s),
    string_length: s => s.length, string_uppercase: s => s.toUpperCase(), string_lowercase: s => s.toLowerCase(),
    string_trim: s => s.trim(),
    abs: Math.abs, abs_float: Math.abs, ceil: Math.ceil, floor: Math.floor, round: Math.round,
    sqrt: Math.sqrt, exp: Math.exp, log: Math.log, log10: Math.log10,
    sin: Math.sin, cos: Math.cos, tan: Math.tan, asin: Math.asin, acos: Math.acos, atan: Math.atan,
    atan2: ([y, x]) => Math.atan2(y, x),
    float_min: ([a, b]) => Math.min(a, b), float_max: ([a, b]) => Math.max(a, b),
    int_mod: ([a, b]) => a % b, sint_mod: ([a, b]) => a % b, float_mod: ([a, b]) => a % b,
    fst: p => p[0], snd: p => p[1],
    is_nan: Number.isNaN, is_finite: Number.isFinite,
    pi: Math.PI, infinity: Infinity, neg_infinity: -Infinity, nan: NaN
  }
}|};
  /* the builtins Runtime.source has, by name */
  let builtins = [
    "string_of_int",
    "string_of_sint",
    "string_of_nat",
    "string_of_bool",
    "float_of_int",
    "float_of_sint",
    "float_of_nat",
    "int_of_float",
    "sint_of_float",
    "int_of_sint",
    "sint_of_int",
    "int_of_string",
    "float_of_string",
    "string_length",
    "string_uppercase",
    "string_lowercase",
    "string_trim",
    "abs",
    "abs_float",
    "ceil",
    "floor",
    "round",
    "sqrt",
    "exp",
    "log",
    "log10",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "atan2",
    "float_min",
    "float_max",
    "int_mod",
    "sint_mod",
    "float_mod",
    "fst",
    "snd",
    "is_nan",
    "is_finite",
  ];
  /* constants, not functions: what the builtins' environment binds them to is their value */
  let constants = ["pi", "infinity", "neg_infinity", "nan"];
};

/* A JS string literal: quotes, backslashes and control characters escaped (bytes from 0x80 up are UTF-8, kept) */
let js_string = (s: string): string => {
  let b = Buffer.create(String.length(s) + 2);
  Buffer.add_char(b, '"');
  String.iter(
    c =>
      switch (c) {
      | '"' => Buffer.add_string(b, "\\\"")
      | '\\' => Buffer.add_string(b, "\\\\")
      | '\n' => Buffer.add_string(b, "\\n")
      | c when Char.code(c) < 0x20 =>
        Buffer.add_string(b, Printf.sprintf("\\u%04x", Char.code(c)))
      | c => Buffer.add_char(b, c)
      },
    s,
  );
  Buffer.add_char(b, '"');
  Buffer.contents(b);
};

/* A JS number literal, in parentheses when negative */
let js_float = (f: float): string =>
  if (Float.is_nan(f)) {
    "NaN";
  } else if (f == Float.infinity) {
    "Infinity";
  } else if (f == Float.neg_infinity) {
    "(-Infinity)";
  } else {
    let s = Printf.sprintf("%.17g", f);
    f < 0. ? "(" ++ s ++ ")" : s;
  };
let js_bigint = (n: Bigint.t): string => {
  let s = Bigint.to_string(n);
  String.length(s) > 0 && s.[0] == '-' ? "(" ++ s ++ ")" : s;
};

/* An identifier made from a Hazel name: letters, digits and _ kept, the rest as _ */
let sanitize = (name: string): string =>
  String.map(
    c =>
      switch (c) {
      | 'a'..'z'
      | 'A'..'Z'
      | '0'..'9'
      | '_' => c
      | _ => '_'
      },
    name,
  );

/* The compiler's state for one term: the builtins' environment (for free variables), a counter for fresh names, and the
   definitions compiled before the program's code (the Hazel-written builtins it uses), in the order they must run. */
type state = {
  env: Environment.t(Exp.t),
  mutable counter: int,
  mutable globals: list(string), /* newest first */
  global_names: Hashtbl.t(string, string),
};

/* The variables declared at the top of the JS function being compiled */
type scope = {mutable decls: list(string)};

let fresh = (st: state, scope: scope, base: string): string => {
  st.counter = st.counter + 1;
  let name = sanitize(base) ++ "$" ++ string_of_int(st.counter);
  scope.decls = [name, ...scope.decls];
  name;
};

/* A function's body: its declarations, then its value */
let body_with_decls = (scope: scope, expr: string): string =>
  switch (scope.decls) {
  | [] => "return " ++ expr ++ ";"
  | ds =>
    "let " ++ String.concat(", ", List.rev(ds)) ++ "; return " ++ expr ++ ";"
  };

let label_of = (e: Exp.t): option(string) =>
  switch (Exp.term_of(e)) {
  | Label(name) => Some(name)
  | _ => None
  };
let rec pat_label_of = (p: Pat.t): option(string) =>
  switch (Pat.term_of(p)) {
  | Label(name) => Some(name)
  | Parens(p) => pat_label_of(p)
  | _ => None
  };

/* A pattern, matched against the JS value at the expression `v`: a JS test (true when it matches), and the assignments
   that bind its variables, each to a fresh variable of the scope, and the lexical environment with them. The test only
   reads `v`, so the bindings can come after it. */
let rec pat =
        (
          st: state,
          scope: scope,
          lex: list((string, string)),
          p: Pat.t,
          v: string,
        )
        : (list(string), list(string), list((string, string))) =>
  switch (Pat.term_of(p)) {
  | Wild => ([], [], lex)
  | Var(x) =>
    let j = fresh(st, scope, x);
    ([], [j ++ " = " ++ v], [(x, j), ...lex]);
  | Parens(p)
  | Asc(p, _) => pat(st, scope, lex, p, v)
  | Atom(Int(n))
  | Atom(Nat(n)) => ([v ++ " === " ++ js_bigint(n)], [], lex)
  | Atom(SInt(n)) => ([v ++ " === " ++ string_of_int(n)], [], lex)
  | Atom(Float(f)) => ([v ++ " === " ++ js_float(f)], [], lex)
  | Atom(Bool(b)) => ([v ++ " === " ++ string_of_bool(b)], [], lex)
  | Atom(String(s)) => ([v ++ " === " ++ js_string(s)], [], lex)
  | ListLit(ps) =>
    let rec go = (ps, v, (tests, binds, lex)) =>
      switch (ps) {
      | [] => (tests @ [v ++ " === null"], binds, lex)
      | [p, ...rest] =>
        let (t, b, lex) = pat(st, scope, lex, p, v ++ ".h");
        go(rest, v ++ ".t", (tests @ [v ++ " !== null"] @ t, binds @ b, lex));
      };
    go(ps, v, ([], [], lex));
  | Cons(ph, pt) =>
    let (t1, b1, lex) = pat(st, scope, lex, ph, v ++ ".h");
    let (t2, b2, lex) = pat(st, scope, lex, pt, v ++ ".t");
    ([v ++ " !== null"] @ t1 @ t2, b1 @ b2, lex);
  | Tuple(ps) =>
    List.fold_left(
      ((tests, binds, lex, i), p: Pat.t) => {
        let (key, p) =
          switch (Pat.term_of(p)) {
          | TupLabel(l, p') =>
            switch (pat_label_of(l)) {
            | Some(name) => (v ++ "[" ++ js_string(name) ++ "]", p')
            | None => unsupported("a tuple label that is not a name")
            }
          | _ => (v ++ "[" ++ string_of_int(i) ++ "]", p)
          };
        let (t, b, lex) = pat(st, scope, lex, p, key);
        (tests @ t, binds @ b, lex, i + 1);
      },
      ([], [], lex, 0),
      ps,
    )
    |> (((t, b, lex, _)) => (t, b, lex))
  | TupLabel(l, p') =>
    switch (pat_label_of(l)) {
    | Some(name) => pat(st, scope, lex, p', v ++ "[" ++ js_string(name) ++ "]")
    | None => unsupported("a tuple label that is not a name")
    }
  | Constructor(name, _) => ([v ++ ".c === " ++ js_string(name)], [], lex)
  | Ap(c, arg) =>
    switch (Pat.term_of(c)) {
    | Constructor(name, _) =>
      let (t, b, lex) = pat(st, scope, lex, arg, v ++ ".v");
      ([v ++ ".c === " ++ js_string(name)] @ t, b, lex);
    | _ => unsupported("an applied pattern that is not a constructor")
    }
  | EmptyHole
  | MultiHole(_)
  | Invalid(_) => unsupported("a hole in a pattern")
  | ExplicitNonlabel
  | Label(_)
  | Projector(_)
  | Splice(_) => unsupported("this kind of pattern")
  };

let conj = (tests: list(string)): string =>
  switch (tests) {
  | [] => "true"
  | ts => String.concat(" && ", List.map(t => "(" ++ t ++ ")", ts))
  };

/* A term as a JS expression, in a scope (the enclosing JS function's declarations) and a lexical environment (Hazel
   names to JS names) */
let rec exp =
        (st: state, scope: scope, lex: list((string, string)), e: Exp.t)
        : string => {
  let go = exp(st, scope, lex);
  switch (Exp.term_of(e)) {
  | Atom(Int(n))
  | Atom(Nat(n)) => js_bigint(n)
  | Atom(SInt(n)) => n < 0 ? "(" ++ string_of_int(n) ++ ")" : string_of_int(n)
  | Atom(Float(f)) => js_float(f)
  | Atom(Bool(b)) => string_of_bool(b)
  | Atom(String(s)) => js_string(s)
  | Parens(e)
  | Asc(e, _)
  | TypAp(e, _)
  | TypFun(_, e, _)
  | TyAlias(_, _, e)
  | Use(_, e) => go(e)
  | Var(x) =>
    switch (List.assoc_opt(x, lex)) {
    | Some(j) => j
    | None => global(st, x)
    }
  | ListLit(es) =>
    List.fold_right(
      (e, acc) => "{h: " ++ go(e) ++ ", t: " ++ acc ++ "}",
      es,
      "null",
    )
  | Cons(h, t) => "{h: " ++ go(h) ++ ", t: " ++ go(t) ++ "}"
  | ListConcat(a, b) => "$R.append(" ++ go(a) ++ ", " ++ go(b) ++ ")"
  | Tuple(es) =>
    let parts =
      List.map(
        (e: Exp.t) =>
          switch (Exp.term_of(e)) {
          | TupLabel(l, e') =>
            switch (label_of(l)) {
            | Some(name) => (Some(name), go(e'))
            | None => unsupported("a tuple label that is not a name")
            }
          | _ => (None, go(e))
          },
        es,
      );
    let arr = "[" ++ String.concat(", ", List.map(snd, parts)) ++ "]";
    List.exists(((l, _)) => l != None, parts)
      ? "$R.tup("
        ++ arr
        ++ ", ["
        ++ String.concat(
             ", ",
             List.map(
               ((l, _)) =>
                 switch (l) {
                 | Some(name) => js_string(name)
                 | None => "null"
                 },
               parts,
             ),
           )
        ++ "])"
      : arr;
  | TupLabel(l, e') =>
    switch (label_of(l)) {
    | Some(name) => "$R.tup([" ++ go(e') ++ "], [" ++ js_string(name) ++ "])"
    | None => unsupported("a tuple label that is not a name")
    }
  | Dot(e1, l) =>
    switch (label_of(l)) {
    | Some(name) => "(" ++ go(e1) ++ ")[" ++ js_string(name) ++ "]"
    | None => unsupported("a projection by something other than a label")
    }
  | Constructor(name, _) => "{c: " ++ js_string(name) ++ "}"
  | Ap(Forward, f, arg) =>
    switch (Exp.term_of(f)) {
    | Constructor(name, _) =>
      "{c: " ++ js_string(name) ++ ", v: " ++ go(arg) ++ "}"
    | _ => "(" ++ go(f) ++ ")(" ++ go(arg) ++ ")"
    }
  | Ap(Reverse, _, _) => unsupported("a reverse application")
  | Fun(p, body, _, _) => func(st, lex, p, body)
  | FixF(p, body, _) =>
    let rec name_of = (p: Pat.t) =>
      switch (Pat.term_of(p)) {
      | Var(x) => x
      | Parens(p)
      | Asc(p, _) => name_of(p)
      | _ => unsupported("a recursive definition of more than one name")
      };
    let x = name_of(p);
    let inner = {decls: []};
    let j = fresh(st, inner, x);
    let body = exp(st, inner, [(x, j), ...lex], body);
    "(() => { "
    ++ body_with_decls(inner, "(" ++ j ++ " = " ++ body ++ ", " ++ j ++ ")")
    ++ " })()";
  | Let(p, e1, e2) =>
    let tmp = fresh(st, scope, "let");
    let v1 = go(e1);
    let (tests, binds, lex') = pat(st, scope, lex, p, tmp);
    let check =
      tests == [] ? [] : ["(" ++ conj(tests) ++ ") || $R.nomatch(\"let\")"];
    "("
    ++ String.concat(
         ", ",
         [tmp ++ " = " ++ v1] @ check @ binds @ [exp(st, scope, lex', e2)],
       )
    ++ ")";
  | If(c, a, b) => "(" ++ go(c) ++ " ? " ++ go(a) ++ " : " ++ go(b) ++ ")"
  | Seq(a, b) => "(" ++ go(a) ++ ", " ++ go(b) ++ ")"
  | Match(scrut, rules) =>
    let tmp = fresh(st, scope, "m");
    let arms =
      List.fold_right(
        ((p, body), rest) => {
          let (tests, binds, lex') = pat(st, scope, lex, p, tmp);
          let then_ =
            "(" ++ String.concat(", ", binds @ [exp(st, scope, lex', body)]) ++ ")";
          tests == []
            ? then_ : "(" ++ conj(tests) ++ " ? " ++ then_ ++ " : " ++ rest ++ ")";
        },
        rules,
        "$R.nomatch(\"case\")",
      );
    "(" ++ tmp ++ " = " ++ go(scrut) ++ ", " ++ arms ++ ")";
  | UnOp(Int(Minus) | SInt(Minus) | Nat(Minus) | Float(Minus), e) =>
    "(-(" ++ go(e) ++ "))"
  | UnOp(Bool(Not), e) => "(!(" ++ go(e) ++ "))"
  | BinOp(op, a, b) =>
    let (a, b) = (go(a), go(b));
    let infix = o => "(" ++ a ++ " " ++ o ++ " " ++ b ++ ")";
    switch (op) {
    | Int(op)
    | SInt(op)
    | Nat(op) =>
      switch (op) {
      | Plus => infix("+")
      | Minus => infix("-")
      | Times => infix("*")
      | Power => infix("**")
      | Divide => "Math.trunc(" ++ a ++ " / " ++ b ++ ")"
      | LessThan => infix("<")
      | LessThanOrEqual => infix("<=")
      | GreaterThan => infix(">")
      | GreaterThanOrEqual => infix(">=")
      }
    | Float(op) =>
      switch (op) {
      | Plus => infix("+")
      | Minus => infix("-")
      | Times => infix("*")
      | Power => infix("**")
      | Divide => infix("/")
      | LessThan => infix("<")
      | LessThanOrEqual => infix("<=")
      | GreaterThan => infix(">")
      | GreaterThanOrEqual => infix(">=")
      | Equals => infix("===")
      | NotEquals => infix("!==")
      }
    | Bool(And) => infix("&&")
    | Bool(Or) => infix("||")
    | String(Concat) => infix("+")
    | Poly(Equals) => "$R.eq(" ++ a ++ ", " ++ b ++ ")"
    | Poly(NotEquals) => "(!$R.eq(" ++ a ++ ", " ++ b ++ "))"
    };
  | BuiltinFun(name) => builtin(name)
  | Closure(env, e) when Environment.to_list(env) == [] => go(e)
  | Closure(_) => unsupported("a closure with its own environment")
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_) => unsupported("a hole")
  | Test(_)
  | HintedTest(_) => unsupported("a test")
  | Filter(_) => unsupported("a stepper filter")
  | Quote(_)
  | Unquote(_)
  | Splice(_) => unsupported("a quotation or a splice")
  | Projector(_) => unsupported("a projector")
  | Module(_)
  | ModuleExp(_) => unsupported("a module")
  | Bind(_)
  | Theorem(_)
  | ProofObject(_)
  | Forall(_)
  | DrvQuote(_) => unsupported("this kind of term")
  | Deferral(_)
  | DeferredAp(_) => unsupported("a partial application")
  | Undefined => unsupported("undefined")
  | Label(_)
  | ExplicitNonlabel
  | LivelitName(_)
  | TupleExtension(_) => unsupported("this kind of term")
  };
}
/* A function: its own JS function, its parameter matched against its pattern */
and func = (st: state, lex, p: Pat.t, body: Exp.t): string => {
  let inner = {decls: []};
  st.counter = st.counter + 1;
  let arg = "a$" ++ string_of_int(st.counter);
  let (tests, binds, lex') = pat(st, inner, lex, p, arg);
  let check =
    tests == [] ? [] : ["(" ++ conj(tests) ++ ") || $R.nomatch(\"fun\")"];
  let b = exp(st, inner, lex', body);
  let value =
    check == [] && binds == []
      ? b : "(" ++ String.concat(", ", check @ binds @ [b]) ++ ")";
  "((" ++ arg ++ ") => { " ++ body_with_decls(inner, value) ++ " })";
}
/* A free variable: the builtins' */
and global = (st: state, x: string): string =>
  switch (Hashtbl.find_opt(st.global_names, x)) {
  | Some(j) => j
  | None =>
    switch (Environment.lookup(st.env, x)) {
    | None => unsupported("the free variable " ++ x)
    | Some(d) =>
      switch (Exp.term_of(d)) {
      | BuiltinFun(name) => builtin(name)
      | _ when List.mem(x, Runtime.constants) => "$R.b." ++ x
      | _ =>
        /* a builtin written in Hazel: compiled once, before the program's code; its name is known before its body is
           compiled, so a body that refers to it finds it */
        let j = "g$" ++ sanitize(x);
        Hashtbl.add(st.global_names, x, j);
        let scope = {decls: []};
        let v = exp(st, scope, [], d);
        st.globals = [
          "const "
          ++ j
          ++ " = (() => { "
          ++ body_with_decls(scope, v)
          ++ " })();",
          ...st.globals,
        ];
        j;
      }
    }
  }
and builtin = (name: string): string =>
  List.mem(name, Runtime.builtins)
    ? "$R.b." ++ name : unsupported("the builtin " ++ name);

/* An elaborated term as the source of a JS function's body, run as new Function("$R", source)(runtime): the builtins it
   uses first, then its value. Raises Unsupported when it holds a form hazel-jit does not compile. */
let compile = (~env: Environment.t(Exp.t), e: Exp.t): string => {
  let st = {
    env,
    counter: 0,
    globals: [],
    global_names: Hashtbl.create(8),
  };
  let scope = {decls: []};
  let v = exp(st, scope, [], e);
  "\"use strict\"; "
  ++ String.concat(" ", List.rev(st.globals))
  ++ " "
  ++ body_with_decls(scope, v);
};

/* The same, or the reason it is not compiled */
let try_compile =
    (~env: Environment.t(Exp.t), e: Exp.t): result(string, string) =>
  switch (compile(~env, e)) {
  | src => Ok(src)
  | exception (Unsupported(reason)) => Error(reason)
  };
