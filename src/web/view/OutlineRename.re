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
  owner: option(Id.t) /* the enclosing module's binder */
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
      /* a member of `module M = {…}`: `M.x` names it too */
      owner:
        switch (Util.ListUtil.last_opt(above)) {
        | Some({o_kind: OutlineTree.KModule, o_binder: Some((m, MPat)), _}) =>
          Some(m)
        | _ => None
        },
    })
  | _ => None
  };

/* tokens naming the item: its references, and `M.x` labels */
let references = (~info_map: Statics.Map.t, it: item): list(Id.t) => {
  /* a path's base in a type (`U` of `U.Meters`) names the module's value */
  let module_site = (info: Info.t) =>
    switch (info) {
    | InfoTyp({user_term: {term: Var(name), _}, ctx, _}) =>
      Option.map((v: Ctx.var_entry) => v.id, Ctx.lookup_var(ctx, name))
    | _ => Info.get_binding_site(info)
    };
  /* `M.x` or `U.Meters`: the label, when its module is the item's owner */
  let qualified = (owner, base, l, label, acc) =>
    l == it.old
    && Option.bind(Id.Map.find_opt(base, info_map), module_site)
    == Some(owner)
      ? [label, ...acc] : acc;
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (Info.get_binding_site(info), it.owner, info) {
      | (Some(b), _, _) when b == it.binder && id != it.binder => [
          id,
          ...acc,
        ]
      | (
          _,
          Some(owner),
          InfoExp({
            user_term: {term: Dot(e1, {term: Label(l), _} as e2), _},
            _,
          }),
        ) =>
        qualified(owner, Exp.rep_id(e1), l, Exp.rep_id(e2), acc)
      | (
          _,
          Some(owner),
          InfoTyp({
            user_term:
              {term: ProdProjection(t1, {term: Label(l), _} as t2), _},
            _,
          }),
        ) =>
        qualified(owner, Typ.rep_id(t1), l, Typ.rep_id(t2), acc)
      | _ => acc
      },
    info_map,
    [],
  );
};

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
  /* an existing [name] in the item's scope would now find the item */
  let captured =
    Id.Map.exists(
      (_, info: Info.t) =>
        switch (info) {
        | InfoExp({user_term: {term: Var(x), _}, ctx, _})
        | InfoTyp({user_term: {term: Var(x), _}, ctx, _}) when x == name =>
          bound(it.kind, ctx, it.old) == Some(it.binder)
        | _ => false
        },
      info_map,
    );
  shadowed || captured
    ? Some(name ++ " is already used where " ++ it.old ++ " is in scope")
    : None;
};

/* a plain name as the language reads one: not `_`, a literal, any
   form's keyword, `$`-prefixed or qualified */
let check_name = (kind, name: string): option(string) => {
  let plain =
    name != ""
    && name.[0] != '$'
    && !String.contains(name, '.')
    && !Token.is_keyword(name)
    && !List.mem(name, FormId.delims);
  let (value, upper) = (Token.is_var(name), Token.is_ctr(name));
  switch (kind) {
  | _ when !plain || !value && !upper => Some("a name is one identifier")
  | KValue when !value => Some("value names start lowercase")
  | KType
  | KModule when !upper => Some("type and module names start with a capital")
  | _ => None
  };
};

/* each tile in [ids] now reads [name]; untouched subtrees stay the
   same values (the parse caches compare by identity) */
let rec relabel = (ids: list(Id.t), name: string, seg: Segment.t): Segment.t =>
  ScratchFocus.map_sharing(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        let children =
          ScratchFocus.map_sharing(relabel(ids, name), t.children);
        switch (t.form) {
        | Tok(_) when List.mem(t.id, ids) =>
          Piece.Tile({
            ...t,
            form: Tok(name),
            children,
          })
        | _ => ScratchFocus.tile_sharing(p, t, children)
        };
      | Projector(pr) =>
        switch (relabel(ids, name, [pr.syntax])) {
        | [syntax] when syntax !== pr.syntax =>
          Piece.Projector({
            ...pr,
            syntax,
          })
        | _ => p
        }
      | p => p
      },
    seg,
  );

let rename =
    (~info_map, ~term, row: Id.t, name: string, seg: Segment.t)
    : result(Segment.t, string) =>
  switch (find_item(row, term)) {
  | None => Error("this row has no name to change")
  | Some(it) when it.old == name => Ok(seg)
  | Some(it) =>
    switch (check_name(it.kind, name)) {
    | Some(why) => Error(why)
    | None =>
      let refs = references(~info_map, it);
      switch (capture(~info_map, it, refs, name)) {
      | Some(why) => Error(why)
      | None => Ok(relabel([it.binder, ...refs], name, seg))
      };
    }
  };
