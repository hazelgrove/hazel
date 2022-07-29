open Grain;

type fnn = list(expr) => expr;
type fn1 = expr => expr;
type fn2 = (expr, expr) => expr;
type fn3 = (expr, expr, expr) => expr;
type fn4 = (expr, expr, expr, expr) => expr;
type fn5 = (expr, expr, expr, expr, expr) => expr;

module type S = {
  let path: Path.t;
  let impl: unit => FileModule.t;

  module H: {
    let with_import: import => unit;

    let with_fn: (string, list(string), fnn) => ident;
    let with_fn1: (string, string, fn1) => ident;
    let with_fn2: (string, string, string, fn2) => ident;
    let with_fn3: (string, string, string, string, fn3) => ident;
    let with_fn4: (string, string, string, string, string, fn4) => ident;
    let with_fn5:
      (string, string, string, string, string, string, fn5) => ident;

    let with_alias: (string, ident) => ident;
  };

  module Use: (I: FileModuleStatic.I) => ImportStatic.S;
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
  let add_stmt = stmt => Decl.DStmt(ExPublic, stmt) |> add_decl;

  module H = {
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

      name;
    };

    let with_fn = (name, xs, body) => with_fn_(name, xs, body);

    let with_fn1 = (name, x1, body) =>
      with_fn_(
        name,
        [x1],
        fun
        | [v1] => body(v1)
        | _ => fail_argument_count(),
      );

    let with_fn2 = (name, v1, v2, body) =>
      with_fn_(
        name,
        [v1, v2],
        fun
        | [v1, v2] => body(v1, v2)
        | _ => fail_argument_count(),
      );

    let with_fn3 = (name, v1, v2, v3, body) =>
      with_fn_(
        name,
        [v1, v2, v3],
        fun
        | [v1, v2, v3] => body(v1, v2, v3)
        | _ => fail_argument_count(),
      );

    let with_fn4 = (name, v1, v2, v3, v4, body) =>
      with_fn_(
        name,
        [v1, v2, v3, v4],
        fun
        | [v1, v2, v3, v4] => body(v1, v2, v3, v4)
        | _ => fail_argument_count(),
      );

    let with_fn5 = (name, v1, v2, v3, v4, v5, body) =>
      with_fn_(
        name,
        [v1, v2, v3, v4, v5],
        fun
        | [v1, v2, v3, v4, v5] => body(v1, v2, v3, v4, v5)
        | _ => fail_argument_count(),
      );

    let with_alias = (alias, original) => {
      let alias = alias |> Ident.v;
      add_stmt(SLet(Pat.var(alias), Expr.var(original)));

      alias;
    };
  };

  open M;
  module Use = (I: FileModuleStatic.I) => Use(I);

  let impl = () => {
    let decls = List.rev(decls^);
    let modl = (decls, []);
    FileModule.mk(path, modl);
  };
};
