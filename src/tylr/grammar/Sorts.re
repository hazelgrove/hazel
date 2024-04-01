let kids = (s: Sort.t): Sort.Set.t =>
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.mapi(_ => Fun.id)
  |> List.concat_map(Regex.atoms)
  |> List.filter_map(Sym.get_nt)
  |> List.map(snd)
  |> List.fold_left(Fun.flip(Sort.Set.add), Sort.Set.empty);
let kids = Core.Memo.general(kids);

let deps = (s: Sort.t): Sort.Set.t => {
  let rec go = (deps, s) => {
    let kids = kids(s);
    let deps' = Sort.Set.union(deps, kids);
    Sort.Set.diff(kids, deps)
    |> Sort.Set.elements
    |> List.map(new_kid => go(deps', new_kid))
    |> List.fold_left(Sort.Set.union, Sort.Set.empty);
  };
  go(Sort.Set.empty, s);
};
let deps = Core.Memo.general(deps);
