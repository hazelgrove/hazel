/* The standard library's source checked and elaborated into
   Library.baked (plans/stdlib.md §2): at build time (src/stdlib/bake) and
   when the library is edited in the app. */
open Language;

/* names bound by top-level items, last binding last and once */
let rec top_names = (items: list(Mod.t)): list(Var.t) =>
  switch (items) {
  | [] => []
  | [item, ...rest] =>
    let later = top_names(rest);
    let here =
      switch (item.term) {
      | ModLet(p, _) => Pat.bound_vars(p)
      | ModuleMod(mp, _) => ModuleHelpers.mpat_names(mp)
      | _ => []
      };
    List.filter(x => !List.mem(x, later), here) @ later;
  };

/* the items as the let/module/type chain a slide would write, ending in
   [tail]; None when an item is not a definition */
let rec chain = (items: list(Mod.t), tail: Exp.t): option(Exp.t) =>
  switch (items) {
  | [] => Some(tail)
  | [item, ...rest] =>
    Option.bind(chain(rest, tail), body =>
      switch (item.term) {
      | ModLet(p, def) => Some(Exp.fresh(Let(p, def, body)))
      | ModType(tp, ty) => Some(Exp.fresh(TyAlias(tp, ty, body)))
      | ModuleMod(mp, def) => Some(Exp.fresh(ModuleExp(mp, def, body)))
      | _ => None
      }
    )
  };

/* every annotation one shared id (Marshal stores it once) and no
   secondary: install re-ids the elab, as evaluation results are */
let compact: Exp.t => Exp.t = {
  let shared = Id.mk();
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) =>
      {
        ...x,
        annotation: {
          ...IdTagged.IdTag.mk_internal([shared]),
          lexeme: x.annotation.lexeme,
        },
      }
      |> continue;
  e =>
    Exp.map_term(
      ~f_exp=f,
      ~f_pat=f,
      ~f_typ=f,
      ~f_tpat=f,
      ~f_rul=f,
      ~f_mod=f,
      ~f_sig=f,
      ~f_mpat=f,
      e,
    );
};

/* the elaborated chain's definitions in order: a loop, not recursion,
   down a chain as long as the library */
let defs = (elab: Exp.t): result(list((Var.t, Exp.t)), string) => {
  let rec go = (e: Exp.t, acc) =>
    switch (e.term) {
    | Let(p, def, body) =>
      switch (Pat.bound_vars(p)) {
      | [x] => go(body, [(x, def), ...acc])
      | _ => Error("a top-level definition must bind exactly one name")
      }
    | TyAlias(_, _, body) => go(body, acc)
    | Tuple(_) => Ok(List.rev(acc))
    | _ => Error("the library elaborated to an unexpected shape")
    };
  go(elab, []);
};

/* each definition with the earlier ones it mentions (the latest of each
   name), compacted */
let baked_defs = (defs: list((Var.t, Exp.t))): array(Library.def) => {
  let latest = Hashtbl.create(64);
  defs
  |> List.mapi((i, (name, def)) => {
       let deps =
         Builtins.mentioned(~keep=Hashtbl.mem(latest), def)
         |> List.map(Hashtbl.find(latest))
         |> List.sort_uniq(compare);
       Hashtbl.replace(latest, name, i);
       Library.{
         name,
         deps,
         elab: Marshal.to_string(compact(def), []),
       };
     })
  |> Array.of_list;
};

/* [items] checked against the builtins alone: the library never sees
   itself */
let of_items =
    (~hash: string, items: list(Mod.t)): result(Library.baked, string) => {
  let names = top_names(items);
  let tail = Exp.fresh(Tuple(List.map(x => Exp.fresh(Var(x)), names)));
  let base = Builtins.ctx_base(Some(Operators.default_mode));
  switch (chain(items, tail)) {
  | None => Error("the library's top level may only hold definitions")
  | Some(_) when List.length(names) < 2 =>
    Error("the library must bind at least two names")
  | Some(exp) =>
    /* per item, as a slide's statics: a monolithic pass down a chain this
       long overflows a browser's stack, and the app re-bakes an edited
       library */
    let ds = DefStatics.calc(~settings=CoreSettings.on, ~ctx_in=base, exp);
    switch (
      DefStatics.all_error_ids(ds),
      Statics.Map.lookup(Exp.rep_id(tail), ds.merged),
      ds.elab,
    ) {
    | ([_, ..._] as ids, _, _) =>
      Error(
        Printf.sprintf("%d static errors in the library", List.length(ids)),
      )
    | ([], None, _) => Error("no statics for the library's tail")
    | ([], _, None) => Error("the library did not elaborate")
    | ([], Some(info), Some(elab)) =>
      switch (Ctx.subtract_prefix(Info.ctx_of(info), base)) {
      | None => Error("the library's context does not extend the builtins'")
      | Some(added) =>
        defs(elab)
        |> Result.map((defs): Library.baked =>
             {
               hash,
               entries: added.entries,
               defs: baked_defs(defs),
             }
           )
      }
    };
  };
};

/* the source text parsed on the fast path, then baked */
let of_text = (text: string): result((Library.baked, Segment.t), string) =>
  switch (FastParse.parsed_of_text(~root=Mod, text)) {
  | Error(note) => Error("the library is off the fast parser: " ++ note)
  | Ok({segment, _}) =>
    switch (MakeTerm.Incr.term_of_mod(segment).term) {
    | Module(items) =>
      of_items(~hash=Digest.to_hex(Digest.string(text)), items)
      |> Result.map(b => (b, segment))
    | _ => Error("the library did not parse as module items")
    }
  };
