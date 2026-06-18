open Language;

/* Test input generation: translate a core Hazel expression into an
 * SMT-LIB2 expression string.
 *
 * This is the web-free, solver-agnostic heart of the feature. It lives in
 * haz3lcore (which compiles both natively and under js_of_ocaml) and has NO
 * dependency on any Z3 binding. The resulting SMT-LIB2 text is fed to a
 * solver backend (the system `z3` binary natively, or the `z3-solver` WASM
 * package in the browser/node) by TestGen.
 *
 * This is a direct descendant of the `tigen` prototype's
 * ConstraintGeneration.re, but operates on the real core `Exp.t` and
 * `Operators` rather than the toy menhir AST, and emits SMT-LIB2 text
 * instead of building Z3 expression objects directly.
 *
 * Only the predicate-friendly fragment of the language is supported:
 * literals, variables, the boolean/numeric/string operators, `if`, and
 * `let` with a simple variable binder. Anything else raises Unsupported,
 * which TestGen catches and surfaces to the user. */

exception Unsupported(string);

let unsupported = (msg: string) => raise(Unsupported(msg));

/* SMT-LIB2 integer literal. Negative numerals must be written as
 * `(- 5)` rather than `-5`. */
let smt_int = (s: string): string =>
  if (String.length(s) > 0 && s.[0] == '-') {
    "(- " ++ String.sub(s, 1, String.length(s) - 1) ++ ")";
  } else {
    s;
  };

/* SMT-LIB2 real literal. We mirror how Hazel serializes floats (`%f`, which
 * always includes a decimal point, as required for SMT reals) and wrap
 * negatives in `(- ...)`. */
let smt_real = (f: float): string =>
  if (!Float.is_finite(f)) {
    unsupported("non-finite float literal");
  } else {
    let s = Printf.sprintf("%f", Float.abs(f));
    f < 0.0 ? "(- " ++ s ++ ")" : s;
  };

/* SMT-LIB2 string literal: wrap in double quotes, doubling embedded quotes
 * per the SMT-LIB2 string standard. */
let smt_string = (s: string): string => {
  let escaped =
    String.to_seq(s)
    |> Seq.fold_left(
         (acc, c) => acc ++ (c == '"' ? "\"\"" : String.make(1, c)),
         "",
       );
  "\"" ++ escaped ++ "\"";
};

let smt_atom = (a: Atom.t): string =>
  switch (a) {
  | Int(n)
  | Nat(n) => smt_int(Bigint.to_string(n))
  | SInt(n) => smt_int(string_of_int(n))
  | Float(f) => smt_real(f)
  | Bool(b) => b ? "true" : "false"
  | String(s) => smt_string(s)
  };

/* SMT operator symbol for a numeric (Int-sorted) binary op. */
let smt_num_op = (op: Operators.op_bin_num): string =>
  switch (op) {
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Power => "^"
  | Divide => "div"
  | LessThan => "<"
  | LessThanOrEqual => "<="
  | GreaterThan => ">"
  | GreaterThanOrEqual => ">="
  };

/* SMT operator symbol for a Real-sorted (float) binary op. Division and
 * comparison differ from the integer forms. Equality is handled by the
 * caller (it negates for NotEquals). */
let smt_float_op = (op: Operators.op_bin_float): string =>
  switch (op) {
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Power => "^"
  | Divide => "/"
  | LessThan => "<"
  | LessThanOrEqual => "<="
  | GreaterThan => ">"
  | GreaterThanOrEqual => ">="
  | Equals => "="
  | NotEquals => "distinct"
  };

let app = (f: string, args: list(string)): string =>
  "(" ++ f ++ " " ++ String.concat(" ", args) ++ ")";

/* The element expressions of a tuple (looking through parens), if `e` is one. */
let rec tuple_elems = (e: Exp.t): option(list(Exp.t)) =>
  switch (e.term) {
  | Tuple(es) => Some(es)
  | Parens(inner) => tuple_elems(inner)
  | _ => None
  };

/* SMT condition for a value `s` matching a pattern. Only literal and wildcard
 * patterns are supported (constructor/tuple/list/var-binding patterns would
 * need SMT datatypes or scope handling). */
let rec pat_match_cond = (s: string, p: Pat.t): string =>
  switch (p.term) {
  | Atom(a) => app("=", [s, smt_atom(a)])
  | Wild => "true"
  | Parens(inner) => pat_match_cond(s, inner)
  | _ => unsupported("unsupported match pattern")
  };

let rec smt_of_exp = (e: Exp.t): string =>
  switch (e.term) {
  | Parens(inner) => smt_of_exp(inner)
  | Atom(a) => smt_atom(a)
  | Var(x) => x
  | UnOp(Bool(Not), e) => app("not", [smt_of_exp(e)])
  | UnOp(Int(Minus) | Nat(Minus) | SInt(Minus) | Float(Minus), e) =>
    app("-", [smt_of_exp(e)])
  /* Equality is component-wise on tuples: (a, b) == (c, d) ⇒ a == c ∧ b == d. */
  | BinOp(Poly(Equals), l, r) => eq_smt(l, r)
  | BinOp(Poly(NotEquals), l, r) => app("not", [eq_smt(l, r)])
  | BinOp(op, l, r) =>
    let l = smt_of_exp(l);
    let r = smt_of_exp(r);
    switch (op) {
    | Int(o)
    | SInt(o)
    | Nat(o) => app(smt_num_op(o), [l, r])
    | Float(NotEquals) => app("not", [app("=", [l, r])])
    | Float(o) => app(smt_float_op(o), [l, r])
    | Bool(And) => app("and", [l, r])
    | Bool(Or) => app("or", [l, r])
    | String(Concat) => app("str.++", [l, r])
    | Poly(Equals) => app("=", [l, r])
    | Poly(NotEquals) => app("not", [app("=", [l, r])])
    };
  | If(c, t, f) =>
    app("ite", [smt_of_exp(c), smt_of_exp(t), smt_of_exp(f)])
  | Match(scrut, rules) =>
    /* Desugar to nested ite. The last arm is the fallthrough (its pattern is
     * treated as a catch-all — fine for the common `_`/total-match case). */
    let s = smt_of_exp(scrut);
    let rec build = (
      fun
      | [] => unsupported("empty match")
      | [(_p, body)] => smt_of_exp(body)
      | [(p, body), ...rest] =>
        app("ite", [pat_match_cond(s, p), smt_of_exp(body), build(rest)])
    );
    build(rules);
  | Let(
      {term: Var(x), _} | {term: Parens({term: Var(x), _}), _},
      def,
      body,
    ) =>
    /* SMT-LIB2 let-binding for a simple variable binder. */
    "(let (("
    ++ x
    ++ " "
    ++ smt_of_exp(def)
    ++ ")) "
    ++ smt_of_exp(body)
    ++ ")"
  | Let(_) => unsupported("let with non-variable pattern")
  | _ => unsupported(Exp.show_cls(Exp.cls_of_term(e.term)))
  }
/* Component-wise equality of two expressions, recursing through tuples. */
and eq_smt = (l: Exp.t, r: Exp.t): string =>
  switch (tuple_elems(l), tuple_elems(r)) {
  | (Some(ls), Some(rs)) when List.length(ls) == List.length(rs) =>
    switch (List.map2(eq_smt, ls, rs)) {
    | [] => "true"
    | conds => app("and", conds)
    }
  | (Some(_), _)
  | (_, Some(_)) => unsupported("tuple equality with mismatched shape")
  | (None, None) => app("=", [smt_of_exp(l), smt_of_exp(r)])
  };
