open Grain;
open Rt;

let list =
  [
    Hazel.Rt.Ast.impl,
    Hazel.Rt.AstMk.impl,
    Hazel.Rt.AstPrint.impl,
    Hazel.Rt.AstPrint.impl,
    Hazel.Rt.MaybeIndet.impl,
    Hazel.Rt.Sum.impl,
    Hazel.Rt.Ops.impl,
  ]
  |> List.map(f => f())
  |> List.filter_map(FileModule.to_full);
