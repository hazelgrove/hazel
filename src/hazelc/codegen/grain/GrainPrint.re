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
let print_surround = (s1, o, s2) => sprintf("%s%s%s", s1, o, s2);
let print_lines = ss => ss |> String.concat("\n");
let print_sep = (delim, ss) => ss |> String.concat(delim);
let print_comma_sep = print_sep(", ");

let rec print = ((tb, b): GrainIR.prog) => {
  let tb = print_top_block(tb);
  let b = print_block_nowrap(b);
  if (tb != "") {
    [tb, "", b] |> print_lines;
  } else {
    [b] |> print_lines;
  };
}

and print_top_block = (tb: GrainIR.top_block) => {
  tb |> List.map(print_top_statement) |> print_lines;
}

and print_top_statement = (tstmt: GrainIR.top_stmt) =>
  switch (tstmt) {
  | TSImport(name, path) => print_import(name, path)
  | TSDecl(decl) => print_decl(decl)
  }

and print_import = (name: Var.t, path: GrainIR.import_path) => {
  let path =
    switch (path) {
    | ImportStd(path) => path
    // TODO: Pass lib base path as argument.
    | ImportRel(path) => sprintf("./%s", path)
    };
  sprintf("import %s from \"%s\"", name, path);
}

and print_decl = (decl: GrainIR.decl) =>
  switch (decl) {
  | DEnum(en) => print_enum(en)
  }

and print_enum = ({name, type_vars, variants}: GrainIR.enum) => {
  let type_vars =
    List.length(type_vars) == 0
      ? "" : type_vars |> print_comma_sep |> sprintf("<%s>");
  let variants =
    variants
    |> List.map((variant: GrainIR.enum_variant) =>
         if (List.length(variant.params) == 0) {
           variant.ctor;
         } else {
           variant.params
           |> print_comma_sep
           |> sprintf("%s(%s)", variant.ctor);
         }
       )
    |> print_comma_sep;

  sprintf("enum %s%s { %s }", name, type_vars, variants);
}

and print_block_nowrap = (b: GrainIR.block) => {
  b |> List.map(print_statement) |> print_lines;
}
and print_block = (b: GrainIR.block) => {
  let s = print_block_nowrap(b);
  sprintf("{ %s }", s);
}

and print_statement = (stmt: GrainIR.stmt) =>
  switch (stmt) {
  | SLet(params, e) =>
    let params = print_params(params);
    let e = print_expr(e);
    sprintf("let %s = %s", params, e);
  | SLetRec(params, e) =>
    let params = print_params(params);
    let e = print_expr(e);
    sprintf("let rec %s = %s", params, e);
  | SExpr(e) => print_expr(e)
  }

and print_expr = (e: GrainIR.expr) =>
  switch (e) {
  | EBoolLit(b) => b ? Consts.truelit : Consts.falselit
  | EInt32Lit(n) => string_of_int(n) ++ "l"
  | EInt64Lit(n) => string_of_int(n) ++ "L"
  // TODO: NaN?
  | EFloat32Lit(f) => string_of_float(f) ++ "f"
  | EFloat64Lit(f) => string_of_float(f) ++ "d"
  | ECharLit(c) => print_char(c)
  | EStringLit(s) => print_string(s)
  | EBinOp(op, e1, e2) => print_bin_op(op, e1, e2)
  | EList(es) =>
    let es = es |> List.map(print_expr) |> String.concat(", ");
    sprintf("[%s]", es);
  | ETriv => Consts.voidlit
  | ECons(e1, e2) => print_cons(e1, e2)
  | ETuple(els) => print_tuple(els)
  | EVar(var) => print_var(var)
  | Efn(params, e') => print_fn(params, e')
  | EAp(fn, args) => print_ap(fn, args)
  | ECtor(ctor, args) => print_ctor(ctor, args)
  | EMatch(scrut, rules) => print_match(scrut, rules)
  | EBlock(b) => print_block(b)
  }

and print_char = (c: char) => print_surround("'", String.make(1, c), "'")

and print_string = (s: string) => print_surround("\"", s, "\"")

and print_args = (args: GrainIR.args) =>
  args |> List.map(print_expr) |> print_comma_sep

and print_bin_op = (op: GrainIR.bin_op, e1: GrainIR.expr, e2: GrainIR.expr) =>
  switch (op) {
  | OpAnd => print_infix(print_expr(e1), Consts.prim_and_op, print_expr(e2))
  | OpOr => print_infix(print_expr(e1), Consts.prim_or_op, print_expr(e2))

  | OpEquals =>
    print_infix(print_expr(e1), Consts.prim_equals_op, print_expr(e2))
  | OpNotEquals =>
    print_infix(print_expr(e1), Consts.prim_not_equals_op, print_expr(e2))
  }

and print_cons = (e1: GrainIR.expr, e2: GrainIR.expr) => {
  let e1 = print_expr(e1);
  let e2 = print_expr(e2);
  sprintf("[%s, ...%s]", e1, e2);
}

and print_tuple = (els: list(GrainIR.expr)) => {
  let els = els |> List.map(print_expr) |> print_comma_sep;
  sprintf("(%s)", els);
}

and print_var = (var: Var.t) => var
and print_params = (ps: GrainIR.params) =>
  ps |> List.map(print_pat) |> String.concat(", ")

and print_fn = (params: GrainIR.params, e': GrainIR.expr) => {
  let params = print_params(params);
  let e' = print_expr(e');
  sprintf("(%s) => { %s }", params, e');
}

and print_ap = (fn: GrainIR.expr, args: GrainIR.args) => {
  let fn = print_expr(fn);
  let args = print_args(args);
  sprintf("%s(%s)", fn, args);
}

and print_ctor = (ctor: Var.t, args: GrainIR.args) => {
  let ctor = print_var(ctor);
  let args = print_args(args);
  sprintf("%s(%s)", ctor, args);
}

and print_match = (scrut: GrainIR.expr, rules: list(GrainIR.rule)) => {
  let scrut = print_expr(scrut);
  let rules = rules |> List.map(print_rule) |> print_comma_sep;
  sprintf("match (%s) { %s }", scrut, rules);
}
and print_rule = (rule: GrainIR.rule) => {
  switch (rule) {
  | RRule(p, rhs) =>
    let p = print_pat(p);
    let rhs = print_expr(rhs);
    sprintf("%s => %s", p, rhs);
  };
}

and print_pat = (p: GrainIR.pat) => {
  switch (p) {
  | PWild => Consts.pat_wild
  // TODO: Check if var conflicts with a keyword?
  | PVar(var) => var
  | PInt(i) => string_of_int(i)
  | PFloat(f) => string_of_float(f)
  | PBool(b) => string_of_bool(b)
  | PNil => Consts.pat_list_nil
  | PTriv => Consts.pat_triv
  // TODO: Optimize this?
  | PCons(p1, p2) =>
    let p1 = print_pat(p1);
    let p2 = print_pat(p2);
    sprintf("[%s, ...%s]", p1, p2);
  | PTuple(ps) =>
    let ps = ps |> List.map(print_pat) |> print_comma_sep;
    sprintf("(%s)", ps);
  | PCtor(ctor, ps) =>
    let ps = ps |> List.map(print_pat) |> print_comma_sep;
    sprintf("%s(%s)", ctor, ps);
  };
};
