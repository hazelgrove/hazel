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

let rec pat_var = (p: Pat.t): option((string, Id.t)) =>
  switch (p.term) {
  | Var(x) => Some((x, Pat.rep_id(p)))
  | Parens(p)
  | Asc(p, _)
  | Projector(_, p)
  | TupLabel(_, p) => pat_var(p)
  | Ap(f, _) => pat_var(f)
  | Tuple([p, ..._]) => pat_var(p)
  | _ => None
  };

let tpat_var = (tp: TPat.t): option((string, Id.t)) =>
  switch (tp.term) {
  | Var(x) => Some((x, TPat.rep_id(tp)))
  | _ => None
  };

let mpat_var = (mp: MPat.t): option((string, Id.t)) =>
  switch (mp.term) {
  | Var(x) => Some((x, MPat.rep_id(mp)))
  | _ => None
  };

let rec strip = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e)
  | Projector(_, e)
  | Filter(_, e) => strip(e)
  | _ => e
  };

let module_items = (def: Exp.t): option(list(Mod.t)) =>
  switch (strip(def).term) {
  | Module(items) => Some(items)
  | _ => None
  };

/* the item whose outline row is [row] */
let find_item = (row: Id.t, term: Exp.t): option(item) => {
  let found = ref(None);
  let set = (kind, owner, var) =>
    switch (found^, var) {
    | (None, Some((old, binder))) =>
      found :=
        Some({
          binder,
          old,
          kind,
          owner,
        })
    | _ => ()
    };
  let members = (~owner, items: list(Mod.t)) =>
    List.iter(
      (m: Mod.t) =>
        if (Mod.rep_id(m) == row) {
          switch (m.term) {
          | ModLet(p, _) => set(KValue, owner, pat_var(p))
          | ModType(tp, _) => set(KType, owner, tpat_var(tp))
          | ModuleMod(mp, _) => set(KModule, owner, mpat_var(mp))
          | _ => ()
          };
        },
      items,
    );
  let f_exp = (continue, e: Exp.t) => {
    let here = Exp.rep_id(e) == row;
    switch (e.term) {
    | Let(p, _, _) when here => set(KValue, None, pat_var(p))
    | TyAlias(tp, _, _) when here => set(KType, None, tpat_var(tp))
    | ModuleExp(mp, def, _) =>
      if (here) {
        set(KModule, None, mpat_var(mp));
      };
      switch (module_items(def)) {
      | Some(items) => members(~owner=Option.map(snd, mpat_var(mp)), items)
      | None => ()
      };
    | Module(items) => members(~owner=None, items)
    | _ => ()
    };
    continue(e);
  };
  let f_mod = (continue, m: Mod.t) => {
    switch (m.term) {
    | ModuleMod(mp, def) =>
      switch (module_items(def)) {
      | Some(items) => members(~owner=Option.map(snd, mpat_var(mp)), items)
      | None => ()
      }
    | _ => ()
    };
    continue(m);
  };
  ignore(TermBase.Exp.map_term(~f_exp, ~f_mod, term));
  found^;
};

/* tokens naming the item: its references, and `M.x` labels */
let references = (~info_map: Statics.Map.t, it: item): list(Id.t) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (Info.get_binding_site(info), it.owner, info) {
      | (Some(b), _, _) when b == it.binder && id != it.binder => [
          id,
          ...acc,
        ]
      | (_, Some(owner), InfoExp({user_term: {term: Dot(e1, e2), _}, _})) =>
        switch (e2.term) {
        | Label(l) when l == it.old =>
          switch (
            Option.bind(
              Id.Map.find_opt(Exp.rep_id(e1), info_map),
              Info.get_binding_site,
            )
          ) {
          | Some(b) when b == owner => [Exp.rep_id(e2), ...acc]
          | _ => acc
          }
        | _ => acc
        }
      | _ => acc
      },
    info_map,
    [],
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

let is_ident = (s: string): bool => {
  let ok = c =>
    c == '_'
    || c == '\''
    || c >= 'a'
    && c <= 'z'
    || c >= 'A'
    && c <= 'Z'
    || c >= '0'
    && c <= '9';
  String.length(s) > 0
  && s.[0] != '\''
  && !(s.[0] >= '0' && s.[0] <= '9')
  && String.for_all(ok, s);
};

let keywords = [
  "let",
  "in",
  "fun",
  "type",
  "module",
  "case",
  "end",
  "if",
  "then",
  "else",
  "test",
  "true",
  "false",
];

let check_name = (kind, name: string): option(string) =>
  if (!is_ident(name) || List.mem(name, keywords)) {
    Some("a name is one identifier");
  } else {
    let upper = name.[0] >= 'A' && name.[0] <= 'Z';
    switch (kind) {
    | KValue when upper => Some("value names start lowercase")
    | KType
    | KModule when !upper =>
      Some("type and module names start with a capital")
    | _ => None
    };
  };

/* each tile in [ids] now reads [name]; untouched subtrees stay the
   same values (the parse caches compare by identity) */
let rec relabel = (ids: list(Id.t), name: string, seg: Segment.t): Segment.t => {
  let changed = ref(false);
  let out =
    List.map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          let kids = List.map(relabel(ids, name), t.children);
          let kids_same = List.for_all2((a, b) => a === b, kids, t.children);
          let hit = List.mem(t.id, ids);
          if (!hit && kids_same) {
            p;
          } else {
            changed := true;
            Piece.Tile({
              ...t,
              form:
                switch (hit, t.form) {
                | (true, Tok(_)) => Tok(name)
                | _ => t.form
                },
              children: kids_same ? t.children : kids,
            });
          };
        | p => p
        },
      seg,
    );
  changed^ ? out : seg;
};

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
