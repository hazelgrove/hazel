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
  /* (^ticket(722)) a slide's run with its livelits' models as parameters: params, when set, maps a model's id to its
     value, in place of the literal the program holds; samples, when set, gets each livelit use's view answer, by its
     projector's id, as the interpreter samples it */
  params: null,
  samples: null,
  param(id, v) { const p = this.params; return p !== null && Object.prototype.hasOwnProperty.call(p, id) ? p[id] : v; },
  sample(id, v) { if (this.samples !== null) this.samples[id] = v; return v; },
  reach(why) { throw new Error("hazel-jit: not compiled, and reached: " + why); },
  missing(why) { const f = () => { throw new Error("hazel-jit: not compiled: " + why); }; f.$missing = why; return f; },
  fn(f, k, e) { f.$k = k; f.$e = e; return f; },
  b: {
    /* (^ticket(722)) %splice_value((model, id)): the value inside the model's SpliceRef((id, v)), found as the
       interpreter's BuiltinsADT.splice_value finds it, first in a walk from the root, left to right */
    "%splice_value": ([model, id]) => {
      const find = x => {
        if (x === null || typeof x !== "object") return undefined;
        if (Array.isArray(x)) { for (const y of x) { const r = find(y); if (r !== undefined) return r; } return undefined; }
        if ("c" in x) {
          if (x.c === "SpliceRef" && Array.isArray(x.v) && x.v[0] === id) return x.v[1];
          return "v" in x ? find(x.v) : undefined;
        }
        if ("h" in x) { for (let p = x; p !== null; p = p.t) { const r = find(p.h); if (r !== undefined) return r; } }
        return undefined;
      };
      const r = find(model);
      if (r === undefined) throw new Error("hazel-jit: %splice_value: no ref " + id);
      return r;
    },
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
    to_fixed: ([x, n]) => x.toFixed(Math.min(Math.max(n, 0), 20)),
    fst: p => p[0], snd: p => p[1],
    is_nan: Number.isNaN, is_finite: Number.isFinite,
    pi: Math.PI, infinity: Infinity, neg_infinity: -Infinity, nan: NaN
  }
}|};
  /* the builtins Runtime.source has, by name */
  let builtins = [
    "%splice_value",
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
    "to_fixed",
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
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
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
  /* (slice 3, the way back) each constructor the code builds, by name: its type as elaboration gave it, the same for
     every use of the name; None when the name has no type, or two (two sum types that share a constructor's name) */
  ctors: Hashtbl.t(string, option(Typ.t)),
  /* the type aliases the term defines (type T = ... in, and a module's type members), so the way back can see through
     a constructor's argument type when it names one */
  aliases: Hashtbl.t(string, Typ.t),
  /* (slice 3, functions back) each JS function the code makes, by its index: the Hazel function it compiles (its Fun
     term), and the Hazel names it captures, in the order of the values it carries ($R.fn(f, k, [values])); and the
     functions being compiled, innermost first: how many bindings were in scope where each began, and what it captures */
  funs: Hashtbl.t(int, (Exp.t, list(string))),
  mutable frames: list((int, ref(list((string, string))))),
};

let note_alias = (st: state, tp: TPat.t, ty: Typ.t): unit =>
  switch (tp.term) {
  | Var(x) => Hashtbl.replace(st.aliases, x, ty)
  | _ => ()
  };

let note_ctor = (st: state, name: string, t: option(option(Typ.t))): unit => {
  let t =
    switch (t) {
    | Some(Some(t)) => Some(t)
    | _ => None
    };
  switch (Hashtbl.find_opt(st.ctors, name)) {
  | None => Hashtbl.replace(st.ctors, name, t)
  | Some(Some(t0)) =>
    switch (t) {
    | Some(t) when Typ.fast_equal(t0, t) => ()
    | _ => Hashtbl.replace(st.ctors, name, None)
    }
  | Some(None) => ()
  };
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
        go(
          rest,
          v ++ ".t",
          (tests @ [v ++ " !== null"] @ t, binds @ b, lex),
        );
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
    | Some(name) =>
      pat(st, scope, lex, p', v ++ "[" ++ js_string(name) ++ "]")
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
/* (^ticket(722)) a survey of a whole program: when [survey] holds a table, a form hazel-jit does not compile is counted
   there by its reason and compiled as $R.reach(reason), which throws if the run evaluates it, instead of failing
   the whole term. Off (None) everywhere but the CLI's [jit] command. */
let survey: ref(option(Hashtbl.t(string, int))) = ref(None);

/* (^ticket(722)) when on, a livelit use's model (UserLivelit.instrument_view's %model) that is a literal compiles as
   $R.param(its id, the literal), so a run can be given a new model without new code; and the use's view call as
   $R.sample(the projector's id, the answer). Off but for a slide's compiled run. */
let params_on: ref(bool) = ref(false);
/* and, as it compiles, each livelit use's projector id with its model's parameter (both as Id.to_string), for the caller
   to give a projector's new model to the right parameter; a use whose model is not a literal has none */
let param_of_projector: Hashtbl.t(string, string) = Hashtbl.create(64);
let last_param: ref(option(string)) = ref(None);

let rec model_literal = (e: Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | Atom(_)
  | Label(_)
  | Constructor(_) => true
  | ListLit(es)
  | Tuple(es) => List.for_all(model_literal, es)
  | TupLabel(a, b) => model_literal(a) && model_literal(b)
  | Ap(Forward, {term: Constructor(_), _}, arg) => model_literal(arg)
  | Parens(e)
  | Asc(e, _) => model_literal(e)
  | _ => false
  };

let rec pat_is = (x: string, p: Pat.t): bool =>
  switch (Pat.term_of(p)) {
  | Var(y) => x == y
  | Parens(p)
  | Asc(p, _) => pat_is(x, p)
  | _ => false
  };
/* and, for each reason, the first term that gave it (Exp.show, cut short), for the CLI to print */
let survey_examples: Hashtbl.t(string, string) = Hashtbl.create(8);

let rec exp =
        (st: state, scope: scope, lex: list((string, string)), e: Exp.t)
        : string =>
  switch (survey^) {
  | None => exp_inner(st, scope, lex, e)
  | Some(tbl) =>
    switch (exp_inner(st, scope, lex, e)) {
    | src => src
    | exception (Unsupported(why)) =>
      Hashtbl.replace(
        tbl,
        why,
        1 + Option.value(Hashtbl.find_opt(tbl, why), ~default=0),
      );
      if (!Hashtbl.mem(survey_examples, why)) {
        let shown = Exp.show(e);
        Hashtbl.replace(
          survey_examples,
          why,
          String.sub(shown, 0, min(String.length(shown), 700)),
        );
      };
      "$R.reach(" ++ js_string(why) ++ ")";
    }
  }
and exp_inner =
    (st: state, scope: scope, lex: list((string, string)), e: Exp.t): string => {
  let go = exp(st, scope, lex);
  switch (Exp.term_of(e)) {
  | Atom(Int(n))
  | Atom(Nat(n)) => js_bigint(n)
  | Atom(SInt(n)) =>
    n < 0 ? "(" ++ string_of_int(n) ++ ")" : string_of_int(n)
  | Atom(Float(f)) => js_float(f)
  | Atom(Bool(b)) => string_of_bool(b)
  | Atom(String(s)) => js_string(s)
  | TyAlias(tp, ty, e) =>
    note_alias(st, tp, ty);
    go(e);
  | Parens(e)
  | Asc(e, _)
  | TypAp(e, _)
  | TypFun(_, e, _)
  | Use(_, e) => go(e)
  | Var(x) =>
    let rec find = (i, l) =>
      switch (l) {
      | [] => None
      | [(y, j), ..._] when y == x => Some((j, List.length(lex) - i))
      | [_, ...rest] => find(i + 1, rest)
      };
    switch (find(0, lex)) {
    | Some((j, depth)) =>
      /* the binding, counted from the oldest: a function that began after it captures it */
      List.iter(
        ((entry, caps)) =>
          if (depth <= entry && !List.mem_assoc(x, caps^)) {
            caps := caps^ @ [(x, j)];
          },
        st.frames,
      );
      j;
    | None => global(st, x)
    };
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
    | Some(name) =>
      "$R.tup([" ++ go(e') ++ "], [" ++ js_string(name) ++ "])"
    | None => unsupported("a tuple label that is not a name")
    }
  | Dot(e1, l) =>
    switch (label_of(l)) {
    | Some(name) => "(" ++ go(e1) ++ ")[" ++ js_string(name) ++ "]"
    | None => unsupported("a projection by something other than a label")
    }
  | Constructor(name, t) =>
    note_ctor(st, name, t);
    "{c: " ++ js_string(name) ++ "}";
  | Ap(Forward, f, arg) =>
    switch (Exp.term_of(f), Exp.term_of(arg)) {
    | (Constructor(name, t), _) =>
      note_ctor(st, name, t);
      "{c: " ++ js_string(name) ++ ", v: " ++ go(arg) ++ "}";
    | (Dot(_, l), Var("%model"))
        when params_on^ && label_of(l) == Some("view") =>
      let pid = Id.to_string(Exp.rep_id(e));
      switch (last_param^) {
      | Some(mid) => Hashtbl.replace(param_of_projector, pid, mid)
      | None => ()
      };
      last_param := None;
      "$R.sample("
      ++ js_string(Id.to_string(Exp.rep_id(e)))
      ++ ", ("
      ++ go(f)
      ++ ")("
      ++ go(arg)
      ++ "))";
    | _ => "(" ++ go(f) ++ ")(" ++ go(arg) ++ ")"
    }
  | Ap(Reverse, _, _) => unsupported("a reverse application")
  | Fun(p, body, _, _) => func(st, lex, ~term=e, p, body)
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
    let v1 =
      switch (params_on^ && pat_is("%model", p), model_literal(e1)) {
      | (true, true) =>
        let mid = Id.to_string(Exp.rep_id(e1));
        let v = "$R.param(" ++ js_string(mid) ++ ", " ++ go(e1) ++ ")";
        last_param := Some(mid);
        v;
      | (true, false) =>
        last_param := None;
        go(e1);
      | (false, _) => go(e1)
      };
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
            "("
            ++ String.concat(", ", binds @ [exp(st, scope, lex', body)])
            ++ ")";
          tests == []
            ? then_
            : "(" ++ conj(tests) ++ " ? " ++ then_ ++ " : " ++ rest ++ ")";
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
  | Unquote(_) => unsupported("a quotation")
  /* (^ticket(722)) a livelit use and a splice are transparent to the run, as in the interpreter (Transition: each
     steps to what it holds); what a livelit's projector draws comes from its view, sampled apart from this */
  | Splice(d)
  | Projector(_, d) => go(d)
  | Module(items) => module_(st, lex, items)
  | ModuleExp(mp, e1, e2) =>
    /* `module M = e1 in e2`, its name bound as a let's would be */
    let rec name_of = (mp: MPat.t) =>
      switch (mp.term) {
      | Var(x) => x
      | Asc(mp, _) => name_of(mp)
      | _ => unsupported("a module bound by a pattern that is not a name")
      };
    let x = name_of(mp);
    let j = fresh(st, scope, x);
    "("
    ++ j
    ++ " = "
    ++ go(e1)
    ++ ", "
    ++ exp(st, scope, [(x, j), ...lex], e2)
    ++ ")";
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
and func = (st: state, lex, ~term: Exp.t, p: Pat.t, body: Exp.t): string => {
  let inner = {decls: []};
  st.counter = st.counter + 1;
  let arg = "a$" ++ string_of_int(st.counter);
  let caps = ref([]);
  st.frames = [(List.length(lex), caps), ...st.frames];
  let (tests, binds, lex') = pat(st, inner, lex, p, arg);
  let check =
    tests == [] ? [] : ["(" ++ conj(tests) ++ ") || $R.nomatch(\"fun\")"];
  let b =
    try(exp(st, inner, lex', body)) {
    | exn =>
      st.frames = List.tl(st.frames);
      raise(exn);
    };
  st.frames = List.tl(st.frames);
  let value =
    check == [] && binds == []
      ? b : "(" ++ String.concat(", ", check @ binds @ [b]) ++ ")";
  /* tagged with its term and its captured values, so the way back can make it a Hazel closure (HazelJitValue) */
  let k = Hashtbl.length(st.funs);
  Hashtbl.replace(st.funs, k, (term, List.map(fst, caps^)));
  "$R.fn(("
  ++ arg
  ++ ") => { "
  ++ body_with_decls(inner, value)
  ++ " }, "
  ++ string_of_int(k)
  ++ ", "
  ++ (
    caps^ == []
      ? "null" : "[" ++ String.concat(", ", List.map(snd, caps^)) ++ "]"
  )
  ++ ")";
}
/* A module: its members, in order, each seeing the ones before it, gathered into one value whose properties are their
   names (as a labeled tuple's), so `M.x` is a property read. Types are erased. A name defined twice is its last. */
and module_ = (st: state, lex, items: list(Mod.t)): string => {
  let inner = {decls: []};
  let name_of_mpat = (mp: MPat.t) => {
    let rec go = (mp: MPat.t) =>
      switch (mp.term) {
      | Var(x) => x
      | Asc(mp, _) => go(mp)
      | _ => unsupported("a module bound by a pattern that is not a name")
      };
    go(mp);
  };
  /* steps: the JS assignments, in order; exports: each member's name and its JS variable, newest first */
  let (steps, exports, _) =
    List.fold_left(
      ((steps, exports, lex), item: Mod.t) =>
        switch (item.term) {
        | ModLet(p, e) =>
          let tmp = fresh(st, inner, "def");
          /* a member with a form hazel-jit does not compile is a stub that says why if it is called: a view uses
             some of its module's members, not all (a livelit's expand, say, may be a quotation) */
          let v =
            try(exp(st, inner, lex, e)) {
            | Unsupported(why) => "$R.missing(" ++ js_string(why) ++ ")"
            };
          let (tests, binds, lex') = pat(st, inner, lex, p, tmp);
          let check =
            tests == []
              ? [] : ["(" ++ conj(tests) ++ ") || $R.nomatch(\"module\")"];
          /* the names this member binds: the front of lex' that lex does not have */
          let n = List.length(lex') - List.length(lex);
          let bound = List.filteri((i, _) => i < n, lex');
          (
            steps @ [tmp ++ " = " ++ v] @ check @ binds,
            bound @ exports,
            lex',
          );
        | ModVal(x, e) =>
          let j = fresh(st, inner, x);
          (
            steps @ [j ++ " = " ++ exp(st, inner, lex, e)],
            [(x, j), ...exports],
            [(x, j), ...lex],
          );
        | ModuleMod(mp, e) =>
          let x = name_of_mpat(mp);
          let j = fresh(st, inner, x);
          (
            steps @ [j ++ " = " ++ exp(st, inner, lex, e)],
            [(x, j), ...exports],
            [(x, j), ...lex],
          );
        | ModType(tp, ty) =>
          note_alias(st, tp, ty);
          (steps, exports, lex);
        | ModExp(_) => unsupported("an expression as a module member")
        | Invalid(_)
        | EmptyHole
        | MultiHole(_) => unsupported("a hole in a module")
        },
      ([], [], lex),
      items,
    );
  /* each name once, at its newest definition, in the order first defined */
  let names =
    List.fold_left(
      (acc, (x, _)) => List.mem(x, acc) ? acc : [x, ...acc],
      [],
      exports,
    );
  let value =
    "$R.tup(["
    ++ String.concat(", ", List.map(x => List.assoc(x, exports), names))
    ++ "], ["
    ++ String.concat(", ", List.map(js_string, names))
    ++ "])";
  "(() => { "
  ++ body_with_decls(
       inner,
       "(" ++ String.concat(", ", steps @ [value]) ++ ")",
     )
  ++ " })()";
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
        /* a global is compiled on its own: the functions being compiled where it is first used do not capture its
           variables */
        let frames = st.frames;
        st.frames = [];
        let v =
          try(exp(st, scope, [], d)) {
          | Unsupported(_) as exn =>
            st.frames = frames;
            Hashtbl.remove(st.global_names, x);
            raise(exn);
          };
        st.frames = frames;
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
    ? "$R.b[" ++ js_string(name) ++ "]" : unsupported("the builtin " ++ name);

/* An elaborated term as the source of a JS function's body, run as new Function("$R", source)(runtime): the builtins it
   uses first, then its value. Raises Unsupported when it holds a form hazel-jit does not compile. */
/* What compiling a term gives: the JS source, and what the way back needs (HazelJitValue.of_js): the constructors'
   types and the type aliases */
type compiled = {
  src: string,
  ctors: Hashtbl.t(string, option(Typ.t)),
  aliases: Hashtbl.t(string, Typ.t),
  funs: Hashtbl.t(int, (Exp.t, list(string))),
};

let compile_full = (~env: Environment.t(Exp.t), e: Exp.t): compiled => {
  let st = {
    env,
    counter: 0,
    globals: [],
    global_names: Hashtbl.create(8),
    ctors: Hashtbl.create(16),
    aliases: Hashtbl.create(8),
    funs: Hashtbl.create(16),
    frames: [],
  };
  let scope = {decls: []};
  let v = exp(st, scope, [], e);
  {
    src:
      "\"use strict\"; "
      ++ String.concat(" ", List.rev(st.globals))
      ++ " "
      ++ body_with_decls(scope, v),
    ctors: st.ctors,
    aliases: st.aliases,
    funs: st.funs,
  };
};

let compile = (~env: Environment.t(Exp.t), e: Exp.t): string =>
  compile_full(~env, e).src;

/* The same, or the reason it is not compiled */
let try_compile =
    (~env: Environment.t(Exp.t), e: Exp.t): result(string, string) =>
  switch (compile(~env, e)) {
  | src => Ok(src)
  | exception (Unsupported(reason)) => Error(reason)
  };
