open Haz3lcore;
open Language;

/* Renaming from the outline: the binder, every reference statics
   resolves to it, and `M.x` labels for a member of module M. Ids are
   kept; a rename that would capture or be captured is refused. */

type kind =
  | KValue
  | KType
  | KModule;

type item = {
  binder: Id.t, /* the binding token: its ctx entry's id */
  old: string,
  kind,
  owner: option(Id.t) /* a member's: its module's binding token */
};

/* the item whose outline row is [row] */
let find_item = (row: Id.t, term: Exp.t): option(item) =>
  switch (OutlineTree.find(row, term)) {
  | Some((above, {o_binder: Some((binder, sort)), o_label: old, _})) =>
    Some({
      binder,
      old,
      kind:
        switch (sort) {
        | TPat => KType
        | MPat => KModule
        | _ => KValue
        },
      owner:
        switch (Util.ListUtil.last_opt(above)) {
        | Some({o_kind: OutlineTree.KModule, o_binder: Some((m, _)), _}) =>
          Some(m)
        | _ => None
        },
    })
  | _ => None
  };

/* tokens naming the item: its references, and `M.x` labels */
let references = (~info_map: Statics.Map.t, it: item): list(Id.t) =>
  Rename.references(~info_map, it.binder);

let uses_memo = Util.Slot.mk();
/* where [name] is used, as a value or a type */
let uses = (~info_map: Statics.Map.t, name: string): list(Info.t) =>
  Hashtbl.find_all(
    Util.Slot.get(
      uses_memo,
      info_map,
      () => {
        let found = Hashtbl.create(1024);
        Id.Map.iter(
          (_, info: Info.t) =>
            switch (info) {
            | InfoExp({user_term: {term: Var(x), _}, _})
            | InfoTyp({user_term: {term: Var(x), _}, _}) =>
              Hashtbl.add(found, x, info)
            | _ => ()
            },
          info_map,
        );
        found;
      },
    ),
    name,
  );

let bound = (kind, ctx: Ctx.t, name: string): option(Id.t) =>
  switch (kind) {
  | KType => Ctx.lookup_tvar_id(ctx, name)
  | KValue
  | KModule =>
    Option.map((v: Ctx.var_entry) => v.id, Ctx.lookup_var(ctx, name))
  };

/* why renaming [it] to [name] would change what some name refers to */
let capture =
    (~info_map: Statics.Map.t, it: item, refs: list(Id.t), name: string)
    : option(string) => {
  let ctx_at = id => Option.map(Info.ctx_of, Id.Map.find_opt(id, info_map));
  /* a reference would now find another [name] first */
  let shadowed =
    List.exists(
      id =>
        switch (Option.bind(ctx_at(id), ctx => bound(it.kind, ctx, name))) {
        | Some(other) => other != it.binder
        | None => false
        },
      refs,
    );
  /* an existing [name] in the item's scope, not one of its own, would now
     find the item */
  let captured =
    List.exists(
      (info: Info.t) =>
        bound(it.kind, Info.ctx_of(info), it.old) == Some(it.binder)
        && Info.get_binding_site(info) != Some(it.binder),
      uses(~info_map, name),
    );
  /* another member of the item's module so named: `M.name` would read
     one of the two */
  let clash =
    switch (Option.bind(it.owner, Info.binder_items(~info_map))) {
    | Some(items) =>
      Rename.member_clash(~typ=it.kind == KType, items, it.binder, name)
    | None => false
    };
  shadowed || captured || clash
    ? Some(name ++ " is already in use here") : None;
};

/* a plain name as the language reads one: not `_`, a literal, any
   form's keyword, `$`-prefixed or qualified. Short reasons: the outline
   is narrow */
let check_name = (kind, name: string): option(string) => {
  let spaced = String.exists(c => String.contains(" \t\n\r", c), name);
  let plain =
    name != ""
    && name.[0] != '$'
    && !String.contains(name, '.')
    && !List.mem(name, FormId.delims);
  let (value, upper) = (Token.is_var(name), Token.is_ctr(name));
  switch (kind) {
  | _ when spaced => Some("names have no spaces")
  | _ when Token.is_keyword(name) => Some(name ++ " is a keyword")
  | _ when !plain || !value && !upper => Some("a name is one identifier")
  | KValue when !value => Some("value names start lowercase")
  | KType
  | KModule when !upper => Some("type and module names start with a capital")
  | _ => None
  };
};

let relabel = Rename.relabel;

/* the references to relabel as [it] is named [name], as a [kind]'s (its
   own by default): none when nothing changes; else why not */
let prepare =
    (~info_map: Lazy.t(Statics.Map.t), ~kind=?, it: item, name: string)
    : result(option(list(Id.t)), string) => {
  let kind = Option.value(kind, ~default=it.kind);
  switch (check_name(kind, name)) {
  | _ when it.old == name && kind == it.kind => Ok(None)
  | Some(why) => Error(why)
  | None =>
    let info_map = Lazy.force(info_map);
    let refs = references(~info_map, it);
    switch (capture(~info_map, it, refs, name)) {
    | Some(why) => Error(why)
    | None => Ok(Some(refs))
    };
  };
};

let rename =
    (~info_map, ~term, row: Id.t, name: string, seg: Segment.t)
    : result(Segment.t, string) =>
  switch (find_item(row, term)) {
  | None => Error("this row has no name to change")
  | Some(it) =>
    switch (prepare(~info_map=Lazy.from_val(info_map), it, name)) {
    | Error(why) => Error(why)
    | Ok(None) => Ok(seg)
    | Ok(Some(refs)) => Ok(relabel([it.binder, ...refs], name, seg))
    }
  };
