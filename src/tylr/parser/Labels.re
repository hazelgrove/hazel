open Util;

let all =
  {
    open ListUtil.Syntax;
    let* (_, tbl) = Sort.Map.bindings(Grammar.v);
    let* (_, r) = tbl;
    Regex.atoms(r) |> List.filter_map(Sym.get_t);
  }
  |> Label.Set.of_list;

let const = Label.Set.(elements(filter(Label.is_const, all)));

let completions = (lbl: Label.t) =>
  switch (lbl) {
  | Space
  | Id_lower
  | Id_upper
  | Int_lit
  | Float_lit => [lbl]
  | Const(_, prefix) =>
    const
    |> List.filter(
         fun
         | Label.Const(_, t) => String.starts_with(~prefix, t)
         | _ => false,
       )
  };
