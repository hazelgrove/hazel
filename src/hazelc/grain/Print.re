/*
   Printing is a relatively straight-foward 1-1 conversion.

   TODO: Pretty-printing for readability of the final result.
 */
let sprintf = Printf.sprintf;

module Consts = {
  let truelit = "true";
  let falselit = "false";
  let voidlit = "void";

  let prim_and_op = "&&";
  let prim_or_op = "||";

  let prim_equals_op = "==";
  let prim_not_equals_op = "!=";

  let pat_wild = "_";
  let pat_list_nil = "[]";
  let pat_triv = "void";
};

/* Some utilities for printing things. */
let print_infix = (o1, op, o2) => sprintf("%s %s %s", o1, op, o2);
let print_lines = ss => ss |> String.concat("\n");
let print_sep = (delim, ss) => ss |> String.concat(delim);
let print_comma_sep = print_sep(", ");

let rec print = ((decls, b): Module.t) => {
  let decls = decls |> print_decls;
  let b = b |> print_block_unwrapped;
  if (decls != "") {
    [decls, "", b] |> print_lines;
  } else {
    [b] |> print_lines;
  };
}

and print_decls = (decls: list(Decl.t)) =>
  decls |> List.map(print_decl) |> print_lines

and print_decl = (decl: Decl.t) =>
  switch (decl) {
  | DEnum(en) => en |> print_enum
  | DStmt(stmt) => stmt |> print_stmt
  | DImport(imp) => imp |> print_import
  }

and print_import = (imp: Import.t) => {
  let name = imp |> Import.name |> print_ident;
  let path = imp |> Import.path |> print_import_path;
  sprintf("import %s from \"%s\"", name, path);
}

and print_import_path = (path: ImportPath.t) =>
  path |> ImportPath.to_path |> print_path

and print_path = (path: Path.t) => path |> Path.to_string

and print_enum = ({name, type_vars, variants}: Enum.t) => {
  let name = name |> print_ident;
  let type_idents =
    List.length(type_vars) == 0
      ? ""
      : type_vars
        |> List.map(print_ident)
        |> print_comma_sep
        |> sprintf("<%s>");
  let variants =
    variants
    |> List.map(({ctor, params}: Enum.variant) => {
         let ctor = ctor |> print_ident;
         if (List.length(params) == 0) {
           ctor;
         } else {
           params
           |> List.map(print_ident)
           |> print_comma_sep
           |> sprintf("%s(%s)", ctor);
         };
       })
    |> print_comma_sep;

  sprintf("enum %s%s { %s }", name, type_idents, variants);
}

and print_expr = (e: Expr.t) =>
  switch (e) {
  | EBoolLit(b) => print_bool_lit(b)
  | EInt32Lit(n) => print_int32_lit(n)
  | EInt64Lit(n) => print_int64_lit(n)
  | EFloat32Lit(f) => print_float32_lit(f)
  | EFloat64Lit(f) => print_float64_lit(f)
  | ECharLit(c) => print_char_lit(c)
  | EStringLit(s) => print_string_lit(s)
  | EBinOp(op, e1, e2) => print_bin_op(op, e1, e2)
  | EList(es) =>
    es |> List.map(print_expr) |> String.concat(", ") |> sprintf("[%s]")
  | ETriv => Consts.voidlit
  | ECons(e1, e2) => print_cons(e1, e2)
  | ETuple(els) => print_tuple(els)
  | EVar(ident) => print_ident(ident)
  | ELam(params, e') => print_fn(params, e')
  | EAp(fn, args) => print_ap(fn, args)
  | ECtor(ctor, args) => print_ctor(ctor, args)
  | EMatch(scrut, rules) => print_match(scrut, rules)
  | EBlock(b) => print_block_wrapped(b)
  }

and print_block_unwrapped = (b: Expr.block) =>
  b |> List.map(print_stmt) |> print_lines

and print_block_wrapped = (b: Expr.block) =>
  b |> print_block_unwrapped |> sprintf("{ %s }")

and print_stmt = (stmt: Expr.stmt) =>
  switch (stmt) {
  | SLet(p, e) =>
    let p = p |> print_pat;
    let e = e |> print_expr;
    sprintf("let %s = %s", p, e);
  | SLetRec(p, e) =>
    let p = p |> print_pat;
    let e = e |> print_expr;
    sprintf("let rec %s = %s", p, e);
  | SExpr(e) => e |> print_expr
  }

and print_bool_lit = (b: bool) => b ? Consts.truelit : Consts.falselit
and print_int32_lit = (n: int) => string_of_int(n) ++ "l"
and print_int64_lit = (n: int) => string_of_int(n) ++ "L"
/* TODO: NaN? */
and print_float32_lit = (f: float) => string_of_float(f) ++ "f"
and print_float64_lit = (f: float) => string_of_float(f) ++ "d"
and print_char_lit = (c: char) => sprintf("'%c'", c)
and print_string_lit = (s: string) => sprintf("\"%s\"", s)

and print_args = (args: Expr.args) =>
  args |> List.map(print_expr) |> print_comma_sep

and print_bin_op = (op: Expr.bin_op, e1: Expr.t, e2: Expr.t) =>
  switch (op) {
  | OpAnd => print_infix(print_expr(e1), Consts.prim_and_op, print_expr(e2))
  | OpOr => print_infix(print_expr(e1), Consts.prim_or_op, print_expr(e2))

  | OpEquals =>
    print_infix(print_expr(e1), Consts.prim_equals_op, print_expr(e2))
  | OpNotEquals =>
    print_infix(print_expr(e1), Consts.prim_not_equals_op, print_expr(e2))
  }

and print_cons = (e1: Expr.t, e2: Expr.t) => {
  let e1 = e1 |> print_expr;
  let e2 = e2 |> print_expr;
  sprintf("[%s, ...%s]", e1, e2);
}

and print_tuple = (els: list(Expr.t)) =>
  els |> List.map(print_expr) |> print_comma_sep |> sprintf("(%s)")

and print_ident = (ident: Ident.t) => ident |> Ident.to_string

and print_fn = (params: Expr.params, e': Expr.t) => {
  let params = params |> print_params;
  let e' = e' |> print_expr;
  sprintf("((%s) => { %s })", params, e');
}

and print_ap = (fn: Expr.t, args: Expr.args) => {
  let fn = fn |> print_expr;
  let args = args |> print_args;
  sprintf("%s(%s)", fn, args);
}

and print_ctor = (ctor: Ident.t, args: Expr.args) => {
  let ctor = ctor |> print_ident;
  let args = args |> print_args;
  sprintf("%s(%s)", ctor, args);
}

and print_match = (scrut: Expr.t, rules: list(Expr.rule)) => {
  let scrut = scrut |> print_expr;
  let rules = rules |> List.map(print_rule) |> print_comma_sep;
  sprintf("match (%s) { %s }", scrut, rules);
}
and print_rule = (rule: Expr.rule) =>
  switch (rule) {
  | RRule(p, rhs) =>
    let p = p |> print_pat;
    let rhs = rhs |> print_expr;
    sprintf("%s => %s", p, rhs);
  }

and print_params = (ps: Expr.params) =>
  ps |> List.map(print_pat) |> String.concat(", ")

and print_pat = (p: Pat.t) => {
  switch (p) {
  | PWild => Consts.pat_wild
  // TODO: Check if ident conflicts with a keyword?
  | PVar(ident) => ident |> print_ident
  | PInt(i) => string_of_int(i)
  | PFloat(f) => string_of_float(f)
  | PBool(b) => string_of_bool(b)
  | PNil => Consts.pat_list_nil
  | PTriv => Consts.pat_triv
  // TODO: Optimize this?
  | PCons(p1, p2) =>
    let p1 = p1 |> print_pat;
    let p2 = p2 |> print_pat;
    sprintf("[%s, ...%s]", p1, p2);
  | PTuple(ps) =>
    ps |> List.map(print_pat) |> print_comma_sep |> sprintf("(%s)")
  | PCtor(ctor, ps) =>
    let ctor = ctor |> print_ident;
    let ps = ps |> List.map(print_pat) |> print_comma_sep;
    sprintf("%s(%s)", ctor, ps);
  };
};
