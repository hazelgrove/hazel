let ctx: Context.t =
  VarMap.to_list(Builtins.vars)
  |> List.map(((x, ty)) => Context.VarEntry(x, ty))
  |> Context.of_entries;
