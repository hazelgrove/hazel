/**
   Abstractions for modules in the Grain standard library.

   To create an interface for a module:

      module ModuleName = {
        let name = "ModuleName";
        let path = ImportStd("module-path");

        let fmd = FileModule.mk_lib(path);
        let file_mod_import = FileModuleImport.mk(fmd, name);

        let do_something = (x, y) => ap("doSomething", [x, y]);

        ...
      }

   Then, the following contains a call to the "doSomething" function in the
   "ModuleName" module:

      let do_something_call: Expr.expr = Std.ModuleName.do_something(x, y);

   See existing modules below for examples.
 */
open Expr;
open Module;

module type M = {
  let name: ident;
  let path: string;
};

module Make = (M: M) => {
  include Lib.Make({
    let name = M.name;
    let path = ImportStd(M.path);
  });
};

module Num_ = {
  [@deriving sexp]
  type num_ty =
    | Int
    | Float;

  [@deriving sexp]
  type num_sz =
    | S32
    | S64;

  [@deriving sexp]
  type sized = {
    ty: num_ty,
    sz: num_sz,
  };

  module type SIZED = {
    let ty: num_ty;
    let sz: num_sz;
  };

  module SizedNum = (X: SIZED) => {
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

      let path = String.lowercase_ascii(name);
    });

    open FileModuleImport;
    let add = ap2(file_mod_import, "add");
    let sub = ap2(file_mod_import, "sub");
    let mul = ap2(file_mod_import, "mul");
    let div = ap2(file_mod_import, "div");

    let lt = ap2(file_mod_import, "lt");
    let gt = ap2(file_mod_import, "gt");
    let lte = ap2(file_mod_import, "lte");
    let gte = ap2(file_mod_import, "gte");
  };

  module type SizedIntType = {let sz: num_sz;};
  module SizedInt = (X: SizedIntType) => {
    include SizedNum({
      let ty = Int;
      let sz = X.sz;
    });

    open FileModuleImport;
    let incr = ap1(file_mod_import, "incr");
    let decr = ap1(file_mod_import, "decr");

    let div_u = ap2(file_mod_import, "divU");

    let eq = ap2(file_mod_import, "eq");
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
};

module Int32 = Num_.Int32;
module Int64 = Num_.Int64;
module Float32 = Num_.Float32;
module Float64 = Num_.Float64;

module Map = {
  include Make({
    let name = "Map";
    let path = "map";
  });

  open FileModuleImport;
  let from_list = ap1(file_mod_import, "fromList");
};
