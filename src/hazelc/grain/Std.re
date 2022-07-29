/**
   Abstractions for modules in the Grain standard library.

   To create an interface for a module:

      module ModuleName = {
        include Make({
          let path = "module_path";
        });

        module Use = (I: I) => {
          include Use(I);

          let do_something = (x, y) => ap2("doSomething", x, y);
          ...
        };
      }

   Then, the following contains a call to the "doSomething" function in the
   "ModuleName" module:

      let here = ...;
      module ModuleName = Std.ModuleName.Use({ let name = Ident.v("ModuleName"); let path = here; });

      let do_something_call: Expr.expr = ModuleName.do_something(x, y);

   See existing modules below for examples.
 */
let v = Ident.v;

module type I = {
  let name: Ident.t;
  let from: Path.t;
};

module type USE_META = {let imp: Import.t;};
module type META = {let fmodl: FileModule.stub;};

module type M = {let path: string;};
module Make = (M: M) => {
  let path = "/" ++ M.path |> Path.v;
  let fmodl = FileModule.Stub.mk(path);

  include FileModuleStatic.Stub.Make({
    let fmodl = fmodl;
  });
};

module Num_ = {
  module type NUM = {let ty: [> | `Int | `Float];};
  module type SIZED = {let sz: [> | `S32 | `S64];};

  module SizedNum = (N: NUM, S: SIZED) => {
    let name_ =
      (
        switch (N.ty) {
        | `Int => "Int"
        | `Float => "Float"
        }
      )
      ++ (
        switch (S.sz) {
        | `S32 => "32"
        | `S64 => "64"
        }
      );

    let path = name_ |> String.lowercase_ascii;

    include Make({
      let path = path;
    });

    module Use = (I: I) => {
      include Use(I);

      let add = ap2("add" |> v);
      let sub = ap2("sub" |> v);
      let mul = ap2("mul" |> v);
      let div = ap2("div" |> v);

      let lt = ap2("lt" |> v);
      let gt = ap2("gt" |> v);
      let lte = ap2("lte" |> v);
      let gte = ap2("gte" |> v);
    };
  };

  module SizedInt = (S: SIZED) => {
    include SizedNum(
              {
                let ty = `Int;
              },
              S,
            );

    module Use = (I: I) => {
      include Use(I);

      let incr = ap1("incr" |> v);
      let decr = ap1("decr" |> v);

      let div_u = ap2("divU" |> v);

      let eq = ap2("eq" |> v);
    };
  };

  module SizedFloat = (S: SIZED) => {
    include SizedNum(
              {
                let ty = `Float;
              },
              S,
            );

    module Use = (I: I) => {
      include Use(I);

      let eq = (n1, n2) => Expr.EBinOp(OpEquals, n1, n2);
    };
  };

  /* Int32 module. */
  module Int32 =
    SizedInt({
      let sz = `S32;
    });

  /* Int64 module. */
  module Int64 =
    SizedInt({
      let sz = `S64;
    });

  /* Float32 module. */
  module Float32 =
    SizedFloat({
      let sz = `S32;
    });

  /* Float64 module. */
  module Float64 =
    SizedFloat({
      let sz = `S64;
    });
};

module Int32 = Num_.Int32;
module Int64 = Num_.Int64;
module Float32 = Num_.Float32;
module Float64 = Num_.Float64;

module Map = {
  include Make({
    let path = "map";
  });

  module Use = (I: I) => {
    include Use(I);

    let from_list = ap1("fromList" |> v);
  };
};

/* Pervasives */
let print = x => Expr.(ap(var(v("print")), [x]));

let and_ = (b1, b2) => Expr.(EBinOp(OpAnd, b1, b2));
let or_ = (b1, b2) => Expr.(EBinOp(OpOr, b1, b2));

let eq = (x1, x2) => Expr.(EBinOp(OpEquals, x1, x2));
let neq = (x1, x2) => Expr.(EBinOp(OpNotEquals, x1, x2));
