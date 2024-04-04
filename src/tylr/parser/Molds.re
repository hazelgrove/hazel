let map: Mtrl.Labeled.Map.t(list(Mold.t)) =
  Walker.walk_into(~from=L, Root)
  |> Walk.Index.to_list
  |> List.rev_map(fst)
  |> List.fold_left(
       map =>
         fun
         | Bound.Root => map
         | Node((mtrl, mold)) =>
           map
           |> Mtrl.Labeled.Map.update(
                mtrl,
                fun
                | None => Some([mold])
                | Some(ms) => Some([mold, ...ms]),
              ),
       Mtrl.Labeled.Map.empty,
     );

let with_label = lbl =>
  switch (Mtrl.Labeled.Map.find_opt(lbl, map)) {
  | None => []
  | Some(ms) => ms
  };
