open Language;
open Language.Statics;

/* Items-mode statics for the tool path. DefStatics analyzes every item
   alone, so an item's infos know no ancestors beyond the item root — but
   the tools reason about scope through ancestors (enclosing node of the
   cursor, capture checks on rename), and in the program an item IS
   nested in every item before it in its chain. Appending those spine
   ancestors to the merged view restores what monolithic statics reports. */

let with_ancestors = (extra: list(Id.t), info: Info.t): Info.t =>
  switch (info) {
  | InfoExp(i) =>
    InfoExp({
      ...i,
      ancestors: i.ancestors @ extra,
    })
  | InfoPat(i) =>
    InfoPat({
      ...i,
      ancestors: i.ancestors @ extra,
    })
  | InfoTyp(i) =>
    InfoTyp({
      ...i,
      ancestors: i.ancestors @ extra,
    })
  | InfoTPat(i) =>
    InfoTPat({
      ...i,
      ancestors: i.ancestors @ extra,
    })
  | InfoMod(i) =>
    InfoMod({
      ...i,
      ancestors: i.ancestors @ extra,
    })
  | InfoSig(i) =>
    InfoSig({
      ...i,
      ancestors: i.ancestors @ extra,
    })
  | InfoMPat(i) =>
    InfoMPat({
      ...i,
      ancestors: i.ancestors @ extra,
    })
  | InfoDrv(_)
  | _ => info
  };

/* MakeTerm folds a module literal's member separators (`;`) into the
   literal's ids, and monolithic statics stamps the literal's info on all
   of them. The items view only knows the literal's rep id (the member
   pass analyzes a surrogate def), so a caret on a separator found no
   info. Stamp the literal's info on its remaining ids. */
let rec stamp_module_ids = (m: Map.t, e: Exp.t): Map.t =>
  switch (Exp.term_of(e)) {
  | Module(items) =>
    let m =
      switch (Id.Map.find_opt(Exp.rep_id(e), m)) {
      | Some(info) =>
        List.fold_left(
          (m, id) => Id.Map.mem(id, m) ? m : Id.Map.add(id, info, m),
          m,
          IdTagged.ids(e),
        )
      | None => m
      };
    List.fold_left(
      (m, it: Mod.t) =>
        switch (it.term) {
        | ModLet(_, e)
        | ModExp(e)
        | ModuleMod(_, e) => stamp_module_ids(m, e)
        | _ => m
        },
      m,
      items,
    );
  | _ =>
    List.fold_left(
      stamp_module_ids,
      m,
      HighLevelNodeMap.Utils.child_expressions_of_exp(e),
    )
  };

let merged_with_spine = (ds: DefStatics.t): Map.t => {
  /* [spine]: nearest preceding root first. Members sit in their module
     root (then its spine); a module item's own map is patched minus the
     member maps, which the member pass handles with the longer spine. */
  let rec go =
          (spine: list(Id.t), items: list(DefStatics.item), m: Map.t): Map.t =>
    List.fold_left(
      ((m, spine), it: DefStatics.item) => {
        let skip =
          List.fold_left(
            (acc, mem: DefStatics.item) =>
              Id.Map.union((_, a, _) => Some(a), acc, mem.d_map),
            Id.Map.empty,
            it.d_members,
          );
        let m =
          spine == []
            ? m
            : Id.Map.fold(
                (id, _, m) =>
                  Id.Map.mem(id, skip)
                    ? m
                    : (
                      switch (Id.Map.find_opt(id, m)) {
                      | Some(info) =>
                        Id.Map.add(id, with_ancestors(spine, info), m)
                      | None => m
                      }
                    ),
                it.d_map,
                m,
              );
        let m = go([it.d_id, ...spine], it.d_members, m);
        (m, [it.d_id, ...spine]);
      },
      (m, spine),
      items,
    )
    |> fst;
  stamp_module_ids(go([], ds.items, ds.merged), ds.term);
};
