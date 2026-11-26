open Util;
open OptUtil.Syntax;

/* A definition's uses, for the editor's "Goto first use" (on a
   definition) and "Goto next use" (on a use): the terms whose binding
   site, as "Goto definition" finds it (Info.get_binding_site), is that
   definition, in the order they appear in the program. So "Goto
   definition" from each use leads back to the definition, and those
   uses are the definition's: a use where the name is bound again
   (shadowed) belongs to the nearer definition. */

/* Each piece's place in the program, in the order the pieces appear */
let order = (z: Zipper.t): Id.Map.t(int) =>
  Zipper.unselect_and_zip(z)
  |> Segment.ids
  |> List.fold_left(
       ((i, m), id) =>
         (i + 1, Id.Map.mem(id, m) ? m : Id.Map.add(id, i, m)),
       (0, Id.Map.empty),
     )
  |> snd;

let of_binding =
    (~order: Id.Map.t(int), info_map: Language.Statics.Map.t, def: Id.t)
    : list(Id.t) =>
  Id.Map.fold(
    (id, info, acc) =>
      /* (each term once: under its own id, not its other pieces') */
      id == Language.Info.id_of(info)
      && Language.Info.get_binding_site(info) == Some(def)
        ? switch (Id.Map.find_opt(id, order)) {
          | Some(i) => [(i, id), ...acc]
          | None => acc
          }
        : acc,
    info_map,
    [],
  )
  |> List.sort(((i, _), (j, _)) => Int.compare(i, j))
  |> List.map(snd);

/* The first use of the indicated definition */
let first =
    (~order: Id.Map.t(int), info_map: Language.Statics.Map.t, ci)
    : option(Id.t) =>
  ListUtil.hd_opt(of_binding(~order, info_map, Language.Info.id_of(ci)));

/* The use after the indicated one, of the same definition: none from
   the last */
let next =
    (~order: Id.Map.t(int), info_map: Language.Statics.Map.t, ci)
    : option(Id.t) => {
  let* def = Language.Info.get_binding_site(ci);
  let here = Language.Info.id_of(ci);
  let rec after =
    fun
    | [u, v, ..._] when u == here => Some(v)
    | [_, ...rest] => after(rest)
    | [] => None;
  after(of_binding(~order, info_map, def));
};
