open Grain;

type fnn = list(expr) => expr;
type fn1 = expr => expr;
type fn2 = (expr, expr) => expr;
type fn3 = (expr, expr, expr) => expr;
type fn4 = (expr, expr, expr, expr) => expr;
type fn5 = (expr, expr, expr, expr, expr) => expr;

module type UseS = {let imp: import;};

module type S = {
  let path: Path.t;
  let impl: unit => FileModule.t;

  let with_import: import => unit;

  let with_fn: (string, list(string), fnn) => fnn;
  let with_fn1: (string, string, fn1) => fn1;
  let with_fn2: (string, string, string, fn2) => fn2;
  let with_fn3: (string, string, string, string, fn3) => fn3;
  let with_fn4: (string, string, string, string, string, fn4) => fn4;
  let with_fn5: (string, string, string, string, string, string, fn5) => fn5;

  let with_alias: (string, ident, 'f) => 'f;

  module Use: (I: FileModuleStatic.I) => UseS;
};

/**
  Functor to create implementation utilities for implementing a given stub
  module.
 */
module Make = (M: FileModuleStatic.Stub.S) : S => {
  let path = FileModule.Stub.path(M.fmodl);

  let decls = ref([]);
  let add_decl = decl => decls := [decl, ...decls^];
  let add_import = imp => Decl.DImport(imp) |> add_decl;
  let add_stmt = stmt => Decl.DStmt(stmt) |> add_decl;

  let with_import = imp => add_import(imp);

  let fail_argument_count = () => failwith("bad argument count");
  let fn_lam = (xs, body) => {
    let xs = xs |> List.map(Ident.v);
    let pats = xs |> List.map(Pat.var);
    let vars = xs |> List.map(Expr.var);
    Expr.ELam(pats, body(vars));
  };

  let with_fn_ = (name, xs, body) => {
    let name = name |> Ident.v;

    /* Make function implementation. */
    let impl = fn_lam(xs, body);
    let stmt = Expr.SLet(Pat.var(name), impl);

    /* Register declaration. */
    add_stmt(stmt);

    name |> Expr.var;
  };

  let with_fn = (name, xs, body) => with_fn_(name, xs, body) |> Expr.ap;

  let with_fn1 = (name, x1, body) => {
    let fn =
      with_fn_(
        name,
        [x1],
        fun
        | [v1] => body(v1)
        | _ => fail_argument_count(),
      );
    e1 => Expr.ap(fn, [e1]);
  };

  let with_fn2 = (name, v1, v2, body) => {
    let fn =
      with_fn_(
        name,
        [v1, v2],
        fun
        | [v1, v2] => body(v1, v2)
        | _ => fail_argument_count(),
      );
    (e1, e2) => Expr.ap(fn, [e1, e2]);
  };

  let with_fn3 = (name, v1, v2, v3, body) => {
    let fn =
      with_fn_(
        name,
        [v1, v2, v3],
        fun
        | [v1, v2, v3] => body(v1, v2, v3)
        | _ => fail_argument_count(),
      );
    (e1, e2, e3) => Expr.ap(fn, [e1, e2, e3]);
  };

  let with_fn4 = (name, v1, v2, v3, v4, body) => {
    let fn =
      with_fn_(
        name,
        [v1, v2, v3, v4],
        fun
        | [v1, v2, v3, v4] => body(v1, v2, v3, v4)
        | _ => fail_argument_count(),
      );
    (e1, e2, e3, e4) => Expr.ap(fn, [e1, e2, e3, e4]);
  };

  let with_fn5 = (name, v1, v2, v3, v4, v5, body) => {
    let fn =
      with_fn_(
        name,
        [v1, v2, v3, v4, v5],
        fun
        | [v1, v2, v3, v4, v5] => body(v1, v2, v3, v4, v5)
        | _ => fail_argument_count(),
      );
    (e1, e2, e3, e4, e5) => Expr.ap(fn, [e1, e2, e3, e4, e5]);
  };

  let with_alias = (alias, original, f) => {
    let alias = alias |> Ident.v;
    add_stmt(SLet(Pat.var(alias), Expr.var(original)));

    f;
  };

  open M;
  module Use = (I: FileModuleStatic.I) => {
    open Use(I);
    let imp = imp;
  };

  let impl = () => {
    let decls = List.rev(decls^);
    let modl = (decls, []);
    FileModule.mk(path, modl);
  };
};
