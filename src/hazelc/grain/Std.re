/*
   Abstractions for modules in the Expr standard library.

   To create an interface for a module:

      module ModuleName = {
        include Inner({
          let name = "ModuleName";
          let path = Expr.ImportStd("module-path");
        })

        let do_something = (x, y) => mk_ap("doSomething", [x, y]);

        ...
      }

   Then, the following contains a call to the "doSomething" function in the
   "ModuleName" module:

      let do_something_call: Expr.expr = ExprStd.ModuleName.do_something(x, y);

   See existing modules for examples.
 */
module type M = {
  let name: Expr.var;
  let path: Module.import_path;
};

/* FIXME: Rename to MakeInner. */
module Make = (X: M) => {
  /* Construct an identifier within the module. */
  let ident = (x: Expr.var): Expr.var => X.name ++ "." ++ x;

  /* Return import metadata for the module. */
  let import = (X.name, X.path);

  /* Construct a call to a function in the module. */
  let mk_nary_ap = (x: Expr.var): (list(Expr.expr) => Expr.expr) =>
    args => EAp(EVar(ident(x)), args);
  let mk_unary_ap = (x: Expr.var): (Expr.expr => Expr.expr) =>
    arg => mk_nary_ap(x, [arg]);
  let mk_binary_ap = (x: Expr.var): ((Expr.expr, Expr.expr) => Expr.expr) =>
    (arg1, arg2) => mk_nary_ap(x, [arg1, arg2]);

  /* Reference a variable in the module. */
  let mk_var = (x: Expr.var): Expr.expr => EVar(ident(x));

  /* Construct a call to a constructor in the module. */
  let mk_nary_ctor = (x: Expr.var): (list(Expr.expr) => Expr.expr) =>
    args => ECtor(ident(x), args);

  /* Construct a call to a pattern constructor in the module. */
  let mk_nary_ctor_pat = (x: Expr.var): (list(Expr.pat) => Expr.pat) =>
    pats => PCtor(ident(x), pats);
};

/* Below are utilities for abstracting Int/Float32/64 modules. */
type num_ty =
  | Int
  | Float;
type num_sz =
  | S32
  | S64;

module type SizedNumType = {
  let ty: num_ty;
  let sz: num_sz;
};

module SizedNum = (X: SizedNumType) => {
  include Make({
    let name =
      (
        switch (X.ty) {
        | Int => "Int"
        | Float => "Float"
        }
      )
      ++ (
        switch (X.sz) {
        | S32 => "32"
        | S64 => "64"
        }
      );

    let path = Module.ImportStd(String.lowercase_ascii(name));
  });

  let add = mk_binary_ap("add");
  let sub = mk_binary_ap("sub");
  let mul = mk_binary_ap("mul");
  let div = mk_binary_ap("div");

  let lt = mk_binary_ap("lt");
  let gt = mk_binary_ap("gt");
  let lte = mk_binary_ap("lte");
  let gte = mk_binary_ap("gte");
};

module type SizedIntType = {let sz: num_sz;};
module SizedInt = (X: SizedIntType) => {
  include SizedNum({
    let ty = Int;
    let sz = X.sz;
  });

  let incr = mk_unary_ap("incr");
  let decr = mk_unary_ap("decr");

  let div_u = mk_binary_ap("divU");

  let eq = mk_binary_ap("eq");
};

module type SizedFloatType = SizedIntType;
module SizedFloat = (X: SizedFloatType) => {
  include SizedNum({
    let ty = Float;
    let sz = X.sz;
  });

  let eq = (n1, n2) => Expr.EBinOp(OpEquals, n1, n2);
};

/* Int32 module. */
module Int32 =
  SizedInt({
    let sz = S32;
  });

/* Int64 module. */
module Int64 =
  SizedInt({
    let sz = S64;
  });

/* Float32 module. */
module Float32 =
  SizedFloat({
    let sz = S32;
  });

/* Float64 module. */
module Float64 =
  SizedFloat({
    let sz = S64;
  });

/* Map module. */
module Map = {
  include Make({
    let name = "Map";
    let path = Module.ImportStd("map");
  });

  /* Map.fromList */
  let from_list = mk_unary_ap("fromList");
};
