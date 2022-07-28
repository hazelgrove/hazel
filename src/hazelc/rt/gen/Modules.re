open Rt;

let list =
  [
    Hazel.Rt.Ast.impl_md,
    Hazel.Rt.AstMk.impl_md,
    Hazel.Rt.AstPrint.impl_md,
    Hazel.Rt.AstPrint.impl_md,
    Hazel.Rt.Sum.impl_md,
  ]
  |> List.filter_map(f => f());
