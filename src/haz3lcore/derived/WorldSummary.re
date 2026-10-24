open Util;

/* what a slide is, known without opening it (plans/world-tree.md,
   summaries): its type errors, the names it defines and the names it
   uses, from its members analyzed in [ctx_in] (the library's) */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  errors: int,
  defines: list(string),
  uses: list(string),
};

let entry_name = (e: Language.Ctx.entry): option(string) =>
  switch (e) {
  | VarEntry({name, _})
  | ConstructorEntry({name, _})
  | TVarEntry({name, _}) => Some(name)
  | LivelitEntry(_) => None
  };

let of_statics = (ds: DefStatics.t): t => {
  let defines =
    List.concat_map(
      (it: DefStatics.item) => List.filter_map(entry_name, it.d_exports),
      ds.items,
    )
    |> ListUtil.dedup;
  let uses =
    List.concat_map((it: DefStatics.item) => it.d_free, ds.items)
    |> List.filter(x => !List.mem(x, defines))
    |> ListUtil.dedup;
  {
    errors: List.length(DefStatics.all_error_ids(ds)),
    defines,
    uses,
  };
};

let of_members = (~settings, ~ctx_in: Language.Ctx.t, members: Segment.t): t =>
  of_statics(
    DefStatics.calc(
      ~settings,
      ~ctx_in,
      MakeTerm.Incr.term_of_root(~root=Mod, members),
    ),
  );
