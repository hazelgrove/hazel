open Util;

let all =
  {
    open ListUtil.Syntax;
    let* (_, tbl) = Sort.Map.bindings(Grammar.v);
    let* (_, r) = tbl;
    Regex.atoms(r) |> List.filter_map(Atom.get_tok);
  }
  |> Label.Set.of_list;

let const = Label.Set.(elements(filter(Label.is_const, all)));

let with_prefix = prefix =>
  const
  |> List.filter(
       fun
       | Label.Const(t) when String.starts_with(~prefix, t) => true
       | _ => false,
     );
