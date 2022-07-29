open Grain;
open Rt.Hazel.Rt;

/* Dummy path since we include the lib directory. */
let here = "." |> Path.v;

module Ast =
  Ast.Use({
    let name = "Ast" |> Ident.v;
    let from = here;
  });
module AstMk =
  AstMk.Use({
    let name = "AstMk" |> Ident.v;
    let from = here;
  });
module AstPrint =
  AstPrint.Use({
    let name = "AstPrint" |> Ident.v;
    let from = here;
  });
module AstSexp =
  AstSexp.Use({
    let name = "AstSexp" |> Ident.v;
    let from = here;
  });
module Ops =
  Ops.Use({
    let name = "Ops" |> Ident.v;
    let from = here;
  });
module MaybeIndet =
  MaybeIndet.Use({
    let name = "MaybeIndet" |> Ident.v;
    let from = here;
  });
module Sum =
  Sum.Use({
    let name = "Sum" |> Ident.v;
    let from = here;
  });

module Std = {
  let print = Std.print;

  module Int32 =
    Std.Int32.Use({
      let name = "Int32" |> Ident.v;
      let from = here;
    });

  module Int64 =
    Std.Int64.Use({
      let name = "Int64" |> Ident.v;
      let from = here;
    });

  module Float32 =
    Std.Float32.Use({
      let name = "Float32" |> Ident.v;
      let from = here;
    });

  module Float64 =
    Std.Float64.Use({
      let name = "Float64" |> Ident.v;
      let from = here;
    });

  module Map =
    Std.Map.Use({
      let name = "Map" |> Ident.v;
      let from = here;
    });
};
