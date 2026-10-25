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
   [tail]: examples and tests (expression items) as `e; …`, checked but
   no export and never run in a slide (definitions evaluate on demand);
   unfinished items (holes, mid-edit) skipped. Each link takes its item's
   id, so a chain built again after an edit keeps the unchanged items'
   analyses (DefStatics ~prev) */
let rec chain = (items: list(Mod.t), tail: Exp.t): option(Exp.t) =>
  switch (items) {
  | [] => Some(tail)
  | [item, ...rest] =>
    Option.bind(
      chain(rest, tail),
      body => {
        let link = (term: Exp.term): Exp.t =>
          IdTagged.mk_internal([Mod.rep_id(item)], term);
        switch (item.term) {
        | ModLet(p, def) => Some(link(Let(p, def, body)))
        | ModType(tp, ty) => Some(link(TyAlias(tp, ty, body)))
        | ModuleMod(mp, def) => Some(link(ModuleExp(mp, def, body)))
        | ModExp(e) => Some(link(Seq(e, body)))
        | EmptyHole
        | MultiHole(_)
        | Invalid(_)
        | ModVal(_) => Some(body)
        };
      },
    )
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
    | TyAlias(_, _, body)
    | Seq(_, body) => go(body, acc)
    | Tuple(_) => Ok(List.rev(acc))
    | _ => Error("the library elaborated to an unexpected shape")
    };
  go(elab, []);
};

/* each definition with the earlier ones it mentions (the latest of each
   name), keeping its source's ids */
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
         elab: Marshal.to_string(def, []),
       };
     })
  |> Array.of_list;
};

/* a bake: the library, its static errors, and its analysis (for the next
   bake after an edit) */
type baked = {
  baked: Library.baked,
  errors: int,
  statics: DefStatics.t,
};

/* [items] checked against the builtins alone (the library never sees
   itself). Static errors refuse it when [strict] (the shipped library);
   otherwise it applies with them, as any program with errors runs (an
   edit in the app). The definitions keep the items' ids, so the code that
   runs is the code an editor of [items] shows */
let of_items =
    (
      ~strict: bool,
      ~prev: option(DefStatics.t)=?,
      ~hash: string,
      items: list(Mod.t),
    )
    : result(baked, string) => {
  let names = top_names(items);
  let tail = Exp.fresh(Tuple(List.map(x => Exp.fresh(Var(x)), names)));
  let base = Builtins.ctx_base(Some(Operators.default_mode));
  switch (chain(items, tail)) {
  | None => Error("the library did not chain")
  | Some(_) when List.length(names) < 2 =>
    Error("the library must bind at least two names")
  | Some(exp) =>
    /* per item, as a slide's statics: a monolithic pass down a chain this
       long overflows a browser's stack, and the app re-bakes an edited
       library */
    let ds =
      DefStatics.calc(~settings=CoreSettings.on, ~ctx_in=base, ~prev?, exp);
    switch (
      DefStatics.all_error_ids(ds),
      Statics.Map.lookup(Exp.rep_id(tail), ds.merged),
      ds.elab,
    ) {
    | ([_, ..._] as ids, _, _) when strict =>
      Error(
        Printf.sprintf("%d static errors in the library", List.length(ids)),
      )
    | (_, None, _) => Error("no statics for the library's tail")
    | (_, _, None) => Error("the library did not elaborate")
    | (errors, Some(info), Some(elab)) =>
      switch (Ctx.subtract_prefix(Info.ctx_of(info), base)) {
      | None => Error("the library's context does not extend the builtins'")
      | Some(added) =>
        defs(elab)
        |> Result.map(defs =>
             {
               baked: {
                 hash,
                 entries: Marshal.to_string(added.entries, []),
                 defs: baked_defs(defs),
               },
               errors: List.length(errors),
               statics: ds,
             }
           )
      }
    };
  };
};

/* the library's text parsed on the fast path with ids seeded by the text:
   the same text gets the same ids at build time and in the app, so the
   shipped library's code is the code its editor shows */
let parse = (text: string) =>
  Id.with_seed(text, () => FastParse.parsed_of_text(~root=Mod, text));

/* the source text parsed, then baked */
let of_text =
    (~strict: bool, text: string)
    : result((Library.baked, int, Segment.t), string) =>
  switch (parse(text)) {
  | Error(note) => Error("the library is off the fast parser: " ++ note)
  | Ok({segment, _}) =>
    switch (MakeTerm.Incr.term_of_mod(segment).term) {
    | Module(items) =>
      of_items(~strict, ~hash=Digest.to_hex(Digest.string(text)), items)
      |> Result.map(({baked, errors, _}) => (baked, errors, segment))
    | _ => Error("the library did not parse as module items")
    }
  };
