type t('sort) = list(('sort, list((Grex.t('sort), Assoc.t))));

// let hazel: t =
//   [
//     (Typ, failwith("todo typ")),
//     (Pat, failwith("todo pat")),
//     (Exp, [(Seq([Kid(Exp), Tok("+"), Kid(Exp)]), Some(L))]),
//   ]
//   |> List.to_seq
//   |> Sort.Map.of_seq;
