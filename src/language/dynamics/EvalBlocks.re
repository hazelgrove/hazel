open Util;

let of_evaluations =
    (~info_map: StaticsBase.Map.t, evaluations: Id.Map.t(int))
    : list(Blocks.t) => {
  let counted: Id.Map.t((int, Info.ancestors)) =
    Id.Map.fold(
      (id, info, acc) =>
        switch (info, Id.Map.find_opt(id, evaluations)) {
        | (Info.InfoExp(e), Some(count))
            when Id.equal(id, Info.id_of(info)) =>
          Id.Map.add(id, (count, e.ancestors), acc)
        | _ => acc
        },
      info_map,
      Id.Map.empty,
    );
  let most =
    Id.Map.fold((_, (count, _), acc) => max(count, acc), counted, 0);
  counted
  |> Id.Map.map(((count, ancestors)) =>
       (Blocks.heat(~most, count), ancestors)
     )
  |> Blocks.nest;
};
