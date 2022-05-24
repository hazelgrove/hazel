/*
   Abstractions for modules in the Grain standard library.

   To create an interface for a module:

      module ModuleName = {
        include Inner({
          let name = "ModuleName";
          let path = GrainIR.ImportStd("module-path");
        })

        let do_something = (x, y) => mk_ap("doSomething", [x, y]);

        ...
      }

   Then, the following contains a call to the "doSomething" function in the
   "ModuleName" module:

      let do_something_call: GrainIR.expr = GrainStd.ModuleName.do_something(x, y);

   See existing modules for examples.
 */

module type InnerMeta = {
  let name: Var.t;
  let path: GrainIR.import_path;
};

module Inner = (X: InnerMeta) => {
  /* Construct an identifier within the module. */
  let ident = (x: Var.t): Var.t => X.name ++ "." ++ x;

  /* Return import metadata for the module. */
  let import = (X.name, X.path);

  /* Construct a call to a function in the module. */
  let mk_ap = (x: Var.t): (list(GrainIR.expr) => GrainIR.expr) => {
    args => EAp(EVar(ident(x)), args);
  };

  /* Reference a variable in the module. */
  let mk_var = (x: Var.t): GrainIR.expr => {
    EVar(ident(x));
  };

  /* Construct a call to a constructor in the module. */
  let mk_ctor = (x: Var.t): (list(GrainIR.expr) => GrainIR.expr) => {
    args => ECtor(ident(x), args);
  };

  /* Construct a call to a pattern constructor in the module. */
  let mk_ctor_pat = (x: Var.t): (list(GrainIR.pat) => GrainIR.pat) => {
    pats => PCtor(ident(x), pats);
  };
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
  include Inner({
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

    let path = GrainIR.ImportStd(String.lowercase_ascii(name));
  });

  let add = (n1, n2) => mk_ap("add", [n1, n2]);
  let sub = (n1, n2) => mk_ap("sub", [n1, n2]);
  let mul = (n1, n2) => mk_ap("mul", [n1, n2]);
  let div = (n1, n2) => mk_ap("div", [n1, n2]);

  let lt = (n1, n2) => mk_ap("lt", [n1, n2]);
  let gt = (n1, n2) => mk_ap("gt", [n1, n2]);
  let lte = (n1, n2) => mk_ap("lte", [n1, n2]);
  let gte = (n1, n2) => mk_ap("gte", [n1, n2]);
};

module type SizedIntType = {let sz: num_sz;};
module SizedInt = (X: SizedIntType) => {
  include SizedNum({
    let ty = Int;
    let sz = X.sz;
  });

  let incr = n => mk_ap("incr", [n]);
  let decr = n => mk_ap("decr", [n]);

  let div_u = (n1, n2) => mk_ap("divU", [n1, n2]);

  let eq = (n1, n2) => mk_ap("eq", [n1, n2]);
};

module type SizedFloatType = SizedIntType;
module SizedFloat = (X: SizedFloatType) => {
  include SizedNum({
    let ty = Float;
    let sz = X.sz;
  });

  let eq = (n1, n2) => GrainIR.EBinOp(OpEquals, n1, n2);
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
  include Inner({
    let name = "Map";
    let path = GrainIR.ImportStd("map");
  });

  /* Map.fromList */
  let from_list = xs => mk_ap("fromList", [xs]);
};
