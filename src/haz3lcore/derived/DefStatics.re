open Language;

/* whole-program statics per top-level item (body replaced by a hole)
   with chained ctxs: an edit recomputes the edited item plus downstream
   items that mention a name whose export changed (d_free for expression
   names, d_tfree for type-side ones). unused-binding warnings are
   computed across items, since an item alone can't see its later uses */

type item = {
  d_id: Id.t, /* the item's rep id (the outline id domain) */
  d_node: Exp.t,
  d_ctx_in: Ctx.t,
  d_map: Statics.Map.t,
  d_error_ids: list(Id.t),
  d_warning_ids: list(Id.t), /* raw; all_warning_ids redoes unused binders */
  d_exports: list(Ctx.entry),
  d_free: list(string), /* free expression vars of pat+def */
  d_tfree: list(string), /* type-side names the item depends on */
  d_ctx_out: Ctx.t,
  d_elab: Exp.t, /* elaboration of the hollow item */
  d_hole: option(Id.t), /* the body hole's id (None: trailing exp) */
  /* when the def is a module literal, its members (+ exports tail)
     are analyzed as a nested item chain, memoized per member */
  d_members: list(item),
};

type t = {
  items: list(item),
  term: Exp.t, /* the whole term these items were computed from */
  probe_ids: Id.Map.t(unit),
  merged: Statics.Map.t /* union of the items' maps, kept incrementally */
};

let rec strip = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e)
  | Projector(_, e) => strip(e)
  | _ => e
  };

/* the top-level item chain; the trailing expression is its own item.
   a chain inside parens or a projector (a folded section) stays whole
   in the tail: the wrapper has info and elaboration of its own */
let rec chain = (e: Exp.t): list(Exp.t) =>
  switch (e.term) {
  | Let(_, _, body)
  | TyAlias(_, _, body)
  | ModuleExp(_, _, body) => [e, ...chain(body)]
  | Seq(_, body)
  | Filter(_, body) => [e, ...chain(body)]
  | _ => [e]
  };

/* a Module root itemizes like the monolithic lowering: each mod item
   becomes a Let/TyAlias wrapper, plus a tail tuple of the exports.
   head_equal gates cleanliness on wrapper and tail ids, so they must be
   stable: wrappers that can't reuse the item's rep id get derived ids */
let derived_id = (tag: string, rep: Id.t): Id.t =>
  Id.mk_str(tag ++ Id.to_string(rep));

let lower_mod_item = (item: Mod.t): Exp.t => {
  let hole = Exp.fresh(EmptyHole);
  let rep = Mod.rep_id(item);
  let stable_wild_let = (tag: string, e: Exp.t): Exp.t =>
    IdTagged.fast_copy(
      derived_id(tag ++ "let:", rep),
      Exp.fresh(
        Let(
          IdTagged.fast_copy(
            derived_id(tag ++ "pat:", rep),
            Pat.fresh(Wild),
          ),
          e,
          hole,
        ),
      ),
    );
  switch (item.term) {
  | ModLet(pat, def) =>
    IdTagged.fast_copy(rep, Exp.fresh(Let(pat, def, hole)))
  | ModType(tpat, typ) =>
    IdTagged.fast_copy(rep, Exp.fresh(TyAlias(tpat, typ, hole)))
  | ModuleMod(mp, def) =>
    IdTagged.fast_copy(
      rep,
      Exp.fresh(Let(ModuleHelpers.mpat_to_pat(mp), def, hole)),
    )
  | ModExp(e) => stable_wild_let("modexp-", e)
  | EmptyHole =>
    stable_wild_let(
      "modhole-",
      IdTagged.fast_copy(rep, Exp.fresh(EmptyHole)),
    )
  | Invalid(s) =>
    stable_wild_let(
      "modinv-",
      IdTagged.fast_copy(rep, Exp.fresh(Invalid(s))),
    )
  | MultiHole(es) =>
    stable_wild_let(
      "modmh-",
      IdTagged.fast_copy(rep, Exp.fresh(MultiHole(es))),
    )
  };
};

/* the module-value tail, with ids derived from the root's rep and export
   names so it stays clean unless the export name set changes; it
   mentions every export, so any export delta re-analyzes it */
let exports_tail = (root_rep: Id.t, items: list(Mod.t)): Exp.t => {
  let sid = (tag: string, name: string) =>
    derived_id(tag ++ name ++ ":", root_rep);
  let fields =
    ModuleHelpers.value_exports(items)
    |> List.map(({name, _}: ModuleHelpers.value_export) =>
         IdTagged.fast_copy(
           sid("modtail-f-", name),
           Exp.fresh(
             TupLabel(
               IdTagged.fast_copy(
                 sid("modtail-l-", name),
                 Exp.fresh(Label(name)),
               ),
               IdTagged.fast_copy(
                 sid("modtail-v-", name),
                 Exp.fresh(Var(name)),
               ),
             ),
           ),
         )
       );
  IdTagged.fast_copy(
    derived_id("modtail:", root_rep),
    Exp.fresh(Tuple(fields)),
  );
};

/* item equality across versions: the head only, ids included (an
   id-preserving rebuild compares equal) */
let head_equal = (a: Exp.t, b: Exp.t): bool =>
  switch (a.term, b.term) {
  | (Let(p1, d1, _), Let(p2, d2, _)) => compare((p1, d1), (p2, d2)) == 0
  | (TyAlias(t1, y1, _), TyAlias(t2, y2, _)) =>
    compare((t1, y1), (t2, y2)) == 0
  | (ModuleExp(m1, d1, _), ModuleExp(m2, d2, _)) =>
    compare((m1, d1), (m2, d2)) == 0
  | (Seq(e1, _), Seq(e2, _)) => compare(e1, e2) == 0
  | (Filter(f1, _), Filter(f2, _)) => compare(f1, f2) == 0
  | (t1, t2) => compare(t1, t2) == 0 /* trailing exp: whole term */
  };

let entry_name = (e: Ctx.entry): string =>
  switch (e) {
  | VarEntry({name, _})
  | ConstructorEntry({name, _}) => name
  | TVarEntry({name, _}) => name
  | LivelitEntry({name, _}) => name
  };

/* the names an export binds as values, the namespace co_ctx tracks
   (CoCtx.mk filters on VarEntry): an alias or constructor of the same
   name leaves a value binding in scope */
let var_names = (exports: list(Ctx.entry)): list(string) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({name, _}) => Some(name)
    | _ => None,
    exports,
  );

let entry_equal = (a: Ctx.entry, b: Ctx.entry): bool =>
  switch (a, b) {
  | (VarEntry(v1), VarEntry(v2))
  | (ConstructorEntry(v1), ConstructorEntry(v2)) =>
    v1.name == v2.name
    && v1.id == v2.id
    && Typ.fast_equal(v1.typ, v2.typ)
    && v1.custom_statics == v2.custom_statics
  | (TVarEntry(t1), TVarEntry(t2)) =>
    t1.name == t2.name
    && t1.id == t2.id
    && (
      switch (t1.kind, t2.kind) {
      /* not ids: a recursive alias gets a fresh binder each analysis */
      | (Ctx.Singleton(a), Ctx.Singleton(b)) => Typ.fast_equal(a, b)
      | (Abstract, Abstract) => true
      | _ => false
      }
    )
  | (LivelitEntry(l1), LivelitEntry(l2)) => l1 === l2 /* closures */
  | _ => false
  };

/* type-side dependencies: co_ctx records only expression variables, so
   type names get parallel tracking (d_tfree, export deltas, a dirty set
   down the chain) with transitive closure through alias definitions,
   since users of an alias never mention the names it expands to */

/* constructors share the type side's name lists (d_tfree, dirty_tnames)
   with types, tagged: each namespace shadows only its own names */
let ctor_key = (c: string): string => "#" ++ c;
let untag = (n: string): string =>
  String.starts_with(~prefix="#", n)
    ? String.sub(n, 1, String.length(n) - 1) : n;

/* type-side names an export ENTRY involves (the ctor's typ mentions
   its sum name — case scrutinee infos mention the sum, not the ctor) */
let tnames_of_entry = (e: Ctx.entry): list(string) =>
  switch (e) {
  | TVarEntry({name, _}) => [name]
  | ConstructorEntry({name, typ, _}) => [
      ctor_key(name),
      ...Typ.free_vars(typ),
    ]
  | VarEntry(_)
  | LivelitEntry(_) => []
  };

let is_type_entry = (e: Ctx.entry): bool =>
  switch (e) {
  | TVarEntry(_)
  | ConstructorEntry(_) => true
  | _ => false
  };

/* type-side names an item depends on: those it writes (types,
   constructors) plus those in its infos' stored types, since using
   x : T depends on T without writing it */
let tfree_of_item = (node: Exp.t, map: Statics.Map.t): list(string) => {
  let acc = ref([]);
  let add = names =>
    switch (names) {
    | [] => ()
    | _ => acc := names @ acc^
    };
  let f_typ = (cont, ty: Typ.t) => {
    switch (Typ.term_of(ty)) {
    | Var(v) => add([v])
    | _ => ()
    };
    cont(ty);
  };
  let f_exp = (cont, e: Exp.t) => {
    switch (e.term) {
    | Constructor(c, _) => add([ctor_key(c)])
    | _ => ()
    };
    cont(e);
  };
  let f_pat = (cont, p: Pat.t) => {
    switch (p.term) {
    | Constructor(c, _) => add([ctor_key(c)])
    | _ => ()
    };
    cont(p);
  };
  switch (Exp.map_term(~f_typ, ~f_exp, ~f_pat, node)) {
  | _ => ()
  | exception _ => add(["*"])
  };
  Id.Map.iter(
    (_, info: Info.t) =>
      switch (info) {
      | InfoExp({ty, _})
      | InfoPat({ty, _}) => add(Typ.free_vars(ty))
      | _ => ()
      },
    map,
  );
  List.sort_uniq(compare, acc^);
};

/* drop dirty type-side names this item's exports rebind in the same
   namespace: an alias shadows a type, a constructor a constructor */
let tshadow = (exports: list(Ctx.entry), dirty: list(string)) => {
  let bound =
    List.filter_map(
      fun
      | Ctx.TVarEntry({name, _}) => Some(name)
      | ConstructorEntry({name, _}) => Some(ctor_key(name))
      | _ => None,
      exports,
    );
  List.filter(n => !List.mem(n, bound), dirty);
};

/* transitive closure step: aliases exported here whose DEFINITION
   mentions a dirty type name are dirty for everything downstream */
let ttransit = (exports: list(Ctx.entry), dirty: list(string)) =>
  dirty == []
    ? []
    : List.concat_map(
        e =>
          switch (e) {
          | Ctx.TVarEntry({name, kind: Singleton(def), _}) =>
            List.exists(v => List.mem(v, dirty), Typ.free_vars(def))
              ? [name] : []
          | _ => []
          },
        exports,
      );

type export_delta =
  | Unchanged
  | Changed({
      vars: list(string),
      tnames: list(string),
    });

let export_delta =
    (old: list(Ctx.entry), new_: list(Ctx.entry)): export_delta => {
  let mk = (vars, tnames) =>
    vars == [] && tnames == []
      ? Unchanged
      : Changed({
          vars: List.sort_uniq(compare, vars),
          tnames: List.sort_uniq(compare, tnames),
        });
  let of_pair = (o, n, vars, tnames) => {
    let vside = e => is_type_entry(e) ? [] : [entry_name(e)];
    (
      vside(o) @ vside(n) @ vars,
      tnames_of_entry(o) @ tnames_of_entry(n) @ tnames,
    );
  };
  if (List.length(old) != List.length(new_)) {
    let (vars, tnames) =
      List.fold_left(
        ((vars, tnames), e) =>
          (
            (is_type_entry(e) ? [] : [entry_name(e)]) @ vars,
            tnames_of_entry(e) @ tnames,
          ),
        ([], []),
        old @ new_,
      );
    mk(vars, tnames);
  } else {
    let rec go = (os, ns, vars, tnames) =>
      switch (os, ns) {
      | ([], []) => mk(vars, tnames)
      | ([o, ...os], [n, ...ns]) =>
        entry_equal(o, n)
          ? go(os, ns, vars, tnames)
          : {
            let (vars, tnames) = of_pair(o, n, vars, tnames);
            go(os, ns, vars, tnames);
          }
      | _ => mk(["*"], ["*"]) /* unreachable: same length */
      };
    go(old, new_, [], []);
  };
};

/* likewise for dirty value names: only a value binding shadows one */
let shadow_filter = (exports: list(Ctx.entry), dirty: list(string)) => {
  let bound = var_names(exports);
  List.filter(v => !List.mem(v, bound), dirty);
};

/* "*" is the unknown-free-vars sentinel: depends on anything dirty */
let depends = (free: list(string), dirty: list(string)): bool =>
  dirty != []
  && (List.mem("*", free) || List.exists(v => List.mem(v, free), dirty));

/* whether [q] uses a dirty name, on either side: a capitalized name may
   resolve to a module or a constructor, an annotation's module ref reads
   the module's value, and `let n = m` copies m's type exports (module
   names needn't be capitalized) */
let stale = (q: item, dirty_vars, dirty_tnames): bool =>
  depends(q.d_free, dirty_vars @ List.map(untag, dirty_tnames))
  || depends(
       q.d_tfree,
       dirty_tnames @ dirty_vars @ List.map(ctor_key, dirty_vars),
     );

let names_of = (exports: list(Ctx.entry)): list(string) =>
  List.sort_uniq(compare, List.map(entry_name, exports));

let seed_delta = (delta: export_delta, dirty_vars, dirty_tnames) =>
  switch (delta) {
  | Unchanged => (dirty_vars, dirty_tnames)
  | Changed({vars, tnames}) => (
      List.sort_uniq(compare, vars @ dirty_vars),
      List.sort_uniq(compare, tnames @ dirty_tnames),
    )
  };

/* observability: how many items the last calc actually re-analyzed */
let last_analyzed: ref(int) = ref(0);

let rec graft_at = (hole_id: Id.t, acc: Exp.t, e: Exp.t): option(Exp.t) =>
  if (List.mem(hole_id, e.annotation.ids)) {
    Some(acc);
  } else {
    let re = (term: Exp.term) => {
      ...e,
      term,
    };
    switch (e.term) {
    | Let(p, d, b) =>
      graft_at(hole_id, acc, b) |> Option.map(b => re(Let(p, d, b)))
    | Seq(a, b) =>
      graft_at(hole_id, acc, b) |> Option.map(b => re(Seq(a, b)))
    | TyAlias(tp, ty, b) =>
      graft_at(hole_id, acc, b) |> Option.map(b => re(TyAlias(tp, ty, b)))
    | Filter(f, b) =>
      graft_at(hole_id, acc, b) |> Option.map(b => re(Filter(f, b)))
    | Parens(b) =>
      graft_at(hole_id, acc, b) |> Option.map(b => re(Parens(b)))
    | _ => None
    };
  };

/* item statics hollows the continuation, so a spine root's info misses
   later items. the evaluator's reuse gating reads a root's elab_term,
   probe_targets and co_ctx, so each non-tail root gets them extended
   over the suffix (its co_ctx minus the root's value bindings), or cached
   runs replay stale */
let fix_spine_infos_full =
    (~probe_ids: Id.Map.t(unit), items: list(item), merged: Statics.Map.t)
    : (Statics.Map.t, SubexpProbeTargets.t, CoCtx.t) => {
  let probes_in = (it: item): SubexpProbeTargets.t =>
    Id.Map.fold(
      (pid, (), acc) =>
        Id.Map.mem(pid, it.d_map)
          ? SubexpProbeTargets.add_self(~is_probed=true, pid, acc) : acc,
      probe_ids,
      SubexpProbeTargets.empty,
    );
  let (merged, top_wit, top_co, _) =
    List.fold_right(
      (it: item, (m, below_wit, below_co, below_elab)) => {
        let bound = var_names(it.d_exports);
        let below_co_scoped =
          CoCtx.filter_names(name => !List.mem(name, bound), below_co);
        /* the root's elab_term is the whole suffix: a hollow one reads
           as unchanged whatever happens downstream */
        let suffix_elab =
          switch (it.d_hole, below_elab) {
          | (Some(h), Some(below)) => graft_at(h, below, it.d_elab)
          | (None, _) => Some(it.d_elab)
          | (Some(_), None) => None
          };
        /* read the RAW info from d_map, never [m]: [m] may hold the last
           calc's patched entry, and re-patching doubles the co_ctx
           use-lists every calc */
        switch (it.d_hole, Statics.Map.lookup_exp(it.d_id, it.d_map)) {
        | (Some(_), Some(raw)) =>
          let co_ctx = CoCtx.union([raw.co_ctx, below_co_scoped]);
          let m =
            Id.Map.add(
              it.d_id,
              Info.InfoExp({
                ...raw,
                elab_term:
                  switch (suffix_elab) {
                  | Some(e) => e
                  | None => raw.elab_term
                  },
                probe_targets:
                  SubexpProbeTargets.union(raw.probe_targets, below_wit),
                co_ctx,
              }),
              m,
            );
          (
            m,
            SubexpProbeTargets.union(probes_in(it), below_wit),
            co_ctx,
            suffix_elab,
          );
        | _ =>
          /* tail item (already accurate) or no InfoExp at the root
             (module forms): thread upward without patching */
          let own_co =
            switch (Statics.Map.lookup_exp(it.d_id, it.d_map)) {
            | Some(raw) => raw.co_ctx
            | None => CoCtx.empty
            };
          (
            m,
            SubexpProbeTargets.union(probes_in(it), below_wit),
            CoCtx.union([own_co, below_co_scoped]),
            suffix_elab,
          );
        };
      },
      items,
      (merged, SubexpProbeTargets.empty, CoCtx.empty, None),
    );
  (merged, top_wit, top_co);
};

let fix_spine_infos =
    (~probe_ids: Id.Map.t(unit), items: list(item), merged: Statics.Map.t)
    : Statics.Map.t => {
  let (merged, _, _) = fix_spine_infos_full(~probe_ids, items, merged);
  merged;
};

let map_union = (a: Statics.Map.t, b: Statics.Map.t): Statics.Map.t =>
  Id.Map.union((_, _x, y) => Some(y), a, b);

let graft_elabs = (items: list(item)): option(Exp.t) => {
  let rec go = (items: list(item)): option(Exp.t) =>
    switch (items) {
    | [] => None
    | [last] =>
      switch (last.d_hole) {
      | None => Some(last.d_elab) /* trailing exp: real elab */
      | Some(_) => None /* items with holes need a successor */
      }
    | [it, ...rest] =>
      switch (go(rest), it.d_hole) {
      | (Some(acc), Some(h)) => graft_at(h, acc, it.d_elab)
      | _ => None
      }
    };
  go(items);
};

/* a top-level export is used iff a later item mentions it before a
   value rebinding, or a hole below could ("$hole" in d_free: real holes
   only, since synthetic body holes aren't in any def) */
let unused_binders = (items: list(item)): list(Id.t) => {
  let hole_below = rest =>
    List.exists(it => List.mem("$hole", it.d_free), rest);
  let rec used_below = (name: string, rest: list(item)): bool =>
    switch (rest) {
    | [] => false
    | [it, ...rest] =>
      List.mem(name, it.d_free)
      || (
        List.mem(name, var_names(it.d_exports))
          ? false  /* shadowed from here on */
          : used_below(name, rest)
      )
    };
  let rec go = (items: list(item)) =>
    switch (items) {
    | [] => []
    | [it, ...rest] =>
      List.filter_map(
        e =>
          switch (e) {
          | Ctx.VarEntry({name, id, _}) =>
            used_below(name, rest)
            || hole_below(rest)
            || String.length(name) > 0
            && name.[0] == '_'
              ? None : Some(id)
          | _ => None
          },
        it.d_exports,
      )
      @ go(rest)
    };
  go(items);
};

/* compute one item's statics in isolation: body swapped for a hole */
let rec calc_item =
        (
          ~settings,
          ~probe_ids=Id.Map.empty,
          ~probe_dirty: item => bool=_ => false,
          ~prev: option(item)=?,
          /* dirty names incoming here: a module item passes them to its
             members, or members using a changed name reuse stale maps */
          ~dirty_vars: list(string)=[],
          ~dirty_tnames: list(string)=[],
          ~ctx_in: Ctx.t,
          node: Exp.t,
        )
        : item =>
  switch (module_literal_members(node)) {
  | Some((bind_pat, def, members)) =>
    calc_module_item(
      ~settings,
      ~probe_ids,
      ~probe_dirty,
      ~prev,
      ~dirty_vars,
      ~dirty_tnames,
      ~ctx_in,
      ~bind_pat,
      ~def,
      ~members,
      node,
    )
  | None => calc_plain_item(~settings, ~probe_ids, ~ctx_in, node)
  }

and calc_plain_item =
    (~settings, ~probe_ids=Id.Map.empty, ~ctx_in: Ctx.t, node: Exp.t): item => {
  incr(last_analyzed);
  let hole = Exp.fresh(EmptyHole);
  let is_tail =
    switch (node.term) {
    | Let(_)
    | TyAlias(_)
    | ModuleExp(_)
    | Seq(_)
    | Filter(_) => false
    | _ => true
    };
  let hollow_term: Exp.term =
    switch (node.term) {
    | Let(p, d, _) => Let(p, d, hole)
    | TyAlias(tp, ty, _) => TyAlias(tp, ty, hole)
    | ModuleExp(mp, d, _) => ModuleExp(mp, d, hole)
    | Seq(e, _) => Seq(e, hole)
    | Filter(f, _) => Filter(f, hole)
    | t => t /* trailing expression: type as-is */
    };
  let hollow = {
    ...node,
    term: hollow_term,
  };
  let (map, elab) =
    Statics.mk_unmemoized(~probe_ids, settings, ctx_in, hollow);
  let ctx_out =
    switch (Statics.Map.lookup_exp(Exp.rep_id(hole), map)) {
    | Some(info) => info.ctx
    | None => ctx_in /* trailing exp: no hole in the map */
    };
  /* read the DEF's co_ctx: the item node may lack an InfoExp (e.g.
     ModuleExp), and a silent [] would make items falsely clean */
  let free = {
    let src =
      switch (node.term) {
      | Let(_, d, _)
      | ModuleExp(_, d, _) => Some(d)
      | Seq(e, _)
      | Filter(Filter({pat: e, _}), _) => Some(e)
      | TyAlias(_)
      | Filter(Residue(_), _) => None
      | _ => Some(node)
      };
    switch (src) {
    | None => []
    | Some(d) =>
      switch (Statics.Map.lookup_exp(Exp.rep_id(d), map)) {
      | Some(info) => CoCtx.names(info.co_ctx)
      | None =>
        /* refuse to fail silent: treat as depending on everything */
        ["*"]
      }
    };
  };
  /* module refs in the item's own annotations are uses too, as in the
     monolithic Let/TyAlias co_ctx */
  let free =
    free
    @ CoCtx.names(
        switch (node.term) {
        | Let(p, _, _) => ModuleHelpers.collect_pat_type_refs(ctx_in, p)
        | ModuleExp(mp, _, _) =>
          ModuleHelpers.collect_pat_type_refs(
            ctx_in,
            ModuleHelpers.mpat_to_pat(mp),
          )
        | TyAlias(_, ty, _) =>
          ModuleHelpers.collect_module_refs_in_typ(
            ctx_in,
            Typ.rep_id(ty),
            ty,
          )
        | _ => CoCtx.empty
        },
      );
  /* the hole is scaffolding, not program: keep it out of the merged
     whole-program view (ctx_out was already read above) */
  let map = is_tail ? map : Id.Map.remove(Exp.rep_id(hole), map);
  {
    d_id: Exp.rep_id(node),
    d_node: node,
    d_ctx_in: ctx_in,
    d_map: map,
    d_error_ids: Statics.Map.error_ids(map),
    d_warning_ids: Statics.Map.warning_ids(map),
    d_exports: Ctx.added_bindings(ctx_out, ctx_in).entries,
    d_free: free,
    d_tfree: tfree_of_item(hollow, map),
    d_ctx_out: ctx_out,
    d_elab: elab,
    d_hole: is_tail ? None : Some(Exp.rep_id(hole)),
    d_members: [],
  };
}

/* member granularity only for simple bindings of a module literal:
   ascribed signatures push ana_labels the member path doesn't replicate */
and module_literal_members =
    (node: Exp.t): option((Pat.t, Exp.t, list(Mod.t))) => {
  let simple = (p: Pat.t): bool =>
    switch (p.term) {
    | Var(_)
    | Wild => true
    | _ => false
    };
  switch (node.term) {
  | Let(p, def, _) when simple(p) =>
    switch (def.term) {
    | Module(members) => Some((p, def, members))
    | _ => None
    }
  | ModuleExp(mp, def, _) =>
    let p = ModuleHelpers.mpat_to_pat(mp);
    switch (def.term, simple(p)) {
    | (Module(members), true) => Some((p, def, members))
    | _ => None
    };
  | _ => None
  };
}

/* members (+ exports tail) run as a nested memoized chain; the wrapper
   runs on a surrogate def (hole : actual_ty), which skips two effects
   replicated here: the M.T type-export alias and the def's co_ctx/probe
   view of its members */
and calc_module_item =
    (
      ~settings,
      ~probe_ids,
      ~probe_dirty: item => bool,
      ~prev: option(item),
      ~dirty_vars: list(string),
      ~dirty_tnames: list(string),
      ~ctx_in: Ctx.t,
      ~bind_pat: Pat.t,
      ~def: Exp.t,
      ~members: list(Mod.t),
      node: Exp.t,
    )
    : item => {
  incr(last_analyzed);
  let prev_members =
    switch (prev) {
    | Some(q) when q.d_id == Exp.rep_id(node) => q.d_members
    | _ => []
    };
  let member_nodes =
    List.map(lower_mod_item, members)
    @ [exports_tail(Exp.rep_id(def), members)];
  let items_m =
    calc_members(
      ~settings,
      ~probe_ids,
      ~probe_dirty,
      ~prev_members,
      ~dirty_vars,
      ~dirty_tnames,
      ~ctx_in,
      member_nodes,
    );
  let member_merged =
    List.fold_left(
      (m, it) => map_union(m, it.d_map),
      Id.Map.empty,
      items_m,
    );
  /* member roots need the top spine's suffix patch too */
  let (member_merged, top_wit, top_co) =
    fix_spine_infos_full(~probe_ids, items_m, member_merged);
  let value_exports = ModuleHelpers.value_exports(members);
  let type_exports = ModuleHelpers.collect_type_exports(ctx_in, members);
  let actual_ty =
    ModuleHelpers.module_actual_type(
      ~local_names=List.map(fst, type_exports),
      value_exports,
      member_merged,
    );
  let sur_hole = Exp.fresh(EmptyHole);
  let sur_def =
    IdTagged.fast_copy(
      Exp.rep_id(def),
      Exp.fresh(Asc(sur_hole, actual_ty)),
    );
  let body_hole = Exp.fresh(EmptyHole);
  let hollow_term: Exp.term =
    switch (node.term) {
    | ModuleExp(mp, _, _) => ModuleExp(mp, sur_def, body_hole)
    | _ => Let(bind_pat, sur_def, body_hole)
    };
  let hollow = {
    ...node,
    term: hollow_term,
  };
  let (map_sur, elab_sur) =
    Statics.mk_unmemoized(~probe_ids, settings, ctx_in, hollow);
  /* surrogate scaffolding ids (inner hole, synthesized annotation), minus
     the def's rep id, whose entry stands in for the module node */
  let sur_ids = {
    let acc = ref([]);
    let grab = (cont, x) => {
      acc := IdTagged.ids(x) @ acc^;
      cont(x);
    };
    ignore(
      Exp.map_term(~f_exp=grab, ~f_typ=(cont, x) => grab(cont, x), sur_def),
    );
    List.filter(id => id != Exp.rep_id(def), acc^);
  };
  let ctx_out = {
    let base =
      switch (Statics.Map.lookup_exp(Exp.rep_id(body_hole), map_sur)) {
      | Some(info) => info.ctx
      | None => ctx_in
      };
    /* the M.T type-export alias the surrogate skips */
    switch (
      ModuleHelpers.single_bound_var(bind_pat),
      ModuleHelpers.type_exports_alias_type(type_exports),
    ) {
    | (Some(name), Some(exports_ty)) =>
      Ctx.extend_alias(base, name, Pat.rep_id(bind_pat), exports_ty)
    | _ => base
    };
  };
  let map_sur =
    List.fold_left((m, id) => Id.Map.remove(id, m), map_sur, sur_ids);
  let map_sur = Id.Map.remove(Exp.rep_id(body_hole), map_sur);
  let map =
    ModuleHelpers.reclassify_expanded_module_items(
      members,
      map_union(member_merged, map_sur),
    );
  /* member elabs grafted, finished like monolithic Module statics */
  let module_value =
    graft_elabs(items_m)
    |> Option.map(g =>
         ModuleHelpers.module_elab(~module_exp_id=Exp.rep_id(def), g)
       );
  /* the literal's info must look monolithic too: the evaluator reuses
     the module value recorded at this id while its elab is unchanged */
  let map =
    switch (module_value, Statics.Map.lookup_exp(Exp.rep_id(def), map)) {
    | (Some(v), Some(raw)) =>
      let info =
        Info.InfoExp({
          ...raw,
          elab_term: v,
          co_ctx: CoCtx.union([raw.co_ctx, top_co]),
          probe_targets: SubexpProbeTargets.union(raw.probe_targets, top_wit),
        });
      List.fold_left(
        (m, id) => Id.Map.add(id, info, m),
        map,
        IdTagged.ids(def),
      );
    | _ => map
    };
  /* the item root likewise takes its members' co_ctx/witnesses (from
     this fresh map, so the patch stays idempotent) */
  let map =
    switch (Statics.Map.lookup_exp(Exp.rep_id(node), map)) {
    | Some(raw) =>
      Id.Map.add(
        Exp.rep_id(node),
        Info.InfoExp({
          ...raw,
          co_ctx: CoCtx.union([raw.co_ctx, top_co]),
          probe_targets: SubexpProbeTargets.union(raw.probe_targets, top_wit),
        }),
        map,
      )
    | None => map
    };
  /* the item's free names = the members' frees minus module-internal
     bindings (the surrogate def's co_ctx is empty, so compose) */
  let compose_free = (~get, ~shadow) =>
    List.fold_right(
      (m: item, below) =>
        List.sort_uniq(compare, get(m) @ shadow(m.d_exports, below)),
      items_m,
      [],
    );
  let free = compose_free(~get=m => m.d_free, ~shadow=shadow_filter);
  let tfree = compose_free(~get=m => m.d_tfree, ~shadow=tshadow);
  let d_elab =
    switch (module_value) {
    | Some(v) => ModuleHelpers.moduleexp_elab(~def_elab_direct=v, elab_sur)
    | None => elab_sur /* shape gap: keep the surrogate's */
    };
  {
    d_id: Exp.rep_id(node),
    d_node: node,
    d_ctx_in: ctx_in,
    d_map: map,
    d_error_ids:
      List.concat_map((m: item) => m.d_error_ids, items_m)
      @ Statics.Map.error_ids(map_sur),
    /* members' unused bindings (a shadowed one), as the top chain's */
    d_warning_ids:
      List.concat_map((m: item) => m.d_warning_ids, items_m)
      @ unused_binders(items_m)
      @ Statics.Map.warning_ids(map_sur),
    d_exports: Ctx.added_bindings(ctx_out, ctx_in).entries,
    d_free: free,
    d_tfree: tfree,
    d_ctx_out: ctx_out,
    d_elab,
    d_hole: Some(Exp.rep_id(body_hole)),
    d_members: items_m,
  };
}

/* the member chain, aligned with the previous one by id as the top chain
   is: a deleted member's names go dirty where it was, and a moved one is
   popped aside there and recomputed where it lands */
and calc_members =
    (
      ~settings,
      ~probe_ids,
      ~probe_dirty: item => bool,
      ~prev_members: list(item),
      ~dirty_vars: list(string),
      ~dirty_tnames: list(string),
      ~ctx_in: Ctx.t,
      nodes: list(Exp.t),
    )
    : list(item) => {
  let id_set = List.fold_left((s, id) => Id.Set.add(id, s), Id.Set.empty);
  let node_ids = id_set(List.map(Exp.rep_id, nodes));
  let prev_ids = id_set(List.map((q: item) => q.d_id, prev_members));
  let moved: ref(Id.Map.t(item)) = ref(Id.Map.empty);
  /* q's exports leave the chain here */
  let vacate = (q: item, dirty_vars, dirty_tnames) =>
    seed_delta(export_delta(q.d_exports, []), dirty_vars, dirty_tnames);
  let rec go = (ps, ns, ctx, dirty_vars, dirty_tnames, acc) =>
    switch (ps, ns) {
    | (_, []) => List.rev(acc)
    | ([q, ...pt], _) when !Id.Set.mem(q.d_id, node_ids) =>
      let (dirty_vars, dirty_tnames) = vacate(q, dirty_vars, dirty_tnames);
      go(pt, ns, ctx, dirty_vars, dirty_tnames, acc);
    | ([q, ...pt], [n, ..._])
        when
          q.d_id != Exp.rep_id(n)
          && Id.Set.mem(Exp.rep_id(n), prev_ids)
          && !Id.Map.mem(Exp.rep_id(n), moved^) =>
      /* the node sits deeper in prev, so q moved later */
      moved := Id.Map.add(q.d_id, q, moved^);
      let (dirty_vars, dirty_tnames) = vacate(q, dirty_vars, dirty_tnames);
      go(pt, ns, ctx, dirty_vars, dirty_tnames, acc);
    | (ps, [n, ...nt]) =>
      let nid = Exp.rep_id(n);
      let (prev_it, moved_in, ps) =
        switch (ps) {
        | [q, ...pt] when q.d_id == nid => (Some(q), false, pt)
        | _ =>
          switch (Id.Map.find_opt(nid, moved^)) {
          | Some(q) => (Some(q), true, ps)
          | None => (None, false, ps) /* inserted */
          }
        };
      let clean =
        switch (prev_it) {
        | Some(q) =>
          !moved_in
          && head_equal(q.d_node, n)
          && !stale(q, dirty_vars, dirty_tnames)
          && !probe_dirty(q)
        | None => false
        };
      switch (clean, prev_it) {
      | (true, Some(q)) =>
        let (it, ctx_out) =
          ctx === q.d_ctx_in
            ? (q, q.d_ctx_out)
            : {
              let ctx_out = Ctx.prepend_entries(ctx, q.d_exports);
              (
                {
                  ...q,
                  d_ctx_in: ctx,
                  d_ctx_out: ctx_out,
                },
                ctx_out,
              );
            };
        let incoming_t = tshadow(it.d_exports, dirty_tnames);
        go(
          ps,
          nt,
          ctx_out,
          shadow_filter(it.d_exports, dirty_vars),
          List.sort_uniq(
            compare,
            ttransit(it.d_exports, incoming_t) @ incoming_t,
          ),
          [it, ...acc],
        );
      | _ =>
        let it =
          calc_item(
            ~settings,
            ~probe_ids,
            ~probe_dirty,
            ~prev=?prev_it,
            ~dirty_vars,
            ~dirty_tnames,
            ~ctx_in=ctx,
            n,
          );
        let p_exports =
          switch (prev_it) {
          | Some(q) => q.d_exports
          | None => []
          };
        let delta = export_delta(p_exports, it.d_exports);
        let incoming = shadow_filter(it.d_exports, dirty_vars);
        let incoming_t = tshadow(it.d_exports, dirty_tnames);
        let (dirty_vars, dirty_tnames) =
          seed_delta(delta, incoming, incoming_t);
        /* landed after a move: its names may resolve to it anew below */
        let (dirty_vars, dirty_tnames) =
          moved_in
            ? (
              List.sort_uniq(compare, names_of(it.d_exports) @ dirty_vars),
              List.sort_uniq(
                compare,
                List.concat_map(tnames_of_entry, it.d_exports) @ dirty_tnames,
              ),
            )
            : (dirty_vars, dirty_tnames);
        let dirty_tnames =
          List.sort_uniq(
            compare,
            ttransit(it.d_exports, dirty_tnames) @ dirty_tnames,
          );
        go(ps, nt, it.d_ctx_out, dirty_vars, dirty_tnames, [it, ...acc]);
      };
    };
  go(prev_members, nodes, ctx_in, dirty_vars, dirty_tnames, []);
};

/* the seed ctx must be PHYSICALLY stable across calc calls: reuse
   gating chains on pointer identity in the clean case */
let ctx0: Ctx.t = Builtins.ctx_init(Some(Operators.default_mode));

/* [chain], except a Module root itemizes via the lowering (chain never
   descends into defs, so only a root Module is seen here) */
let chain_root = (e: Exp.t): list(Exp.t) => {
  let s = strip(e);
  switch (s.term) {
  | Module(items) =>
    List.map(lower_mod_item, items) @ [exports_tail(Exp.rep_id(s), items)]
  | _ => chain(e)
  };
};

/* INVARIANT down the fold: [ctx] differs from the previous run's only at
   dirty names. a clean item (same head, d_free/d_tfree avoid the dirty
   sets) is reused; its map may keep stale ctx entries for dirty names it
   doesn't use, sound for typing though its Γ display can lag */
let calc =
    (~settings, ~prev: option(t)=?, ~probe_ids=Id.Map.empty, whole: Exp.t): t => {
  last_analyzed := 0;
  let nodes = chain_root(whole);
  /* probe ids are an analysis input (witness stamping): only items whose
     maps contain a toggled probe id re-analyze */
  let probe_delta =
    switch (prev) {
    | Some(p) =>
      Id.Map.merge(
        (_, a, b) =>
          switch (a, b) {
          | (Some (), Some ())
          | (None, None) => None
          | _ => Some()
          },
        p.probe_ids,
        probe_ids,
      )
    | None => Id.Map.empty
    };
  let probe_dirty = (p: item): bool =>
    !Id.Map.is_empty(probe_delta)
    && Id.Map.exists((pid, ()) => Id.Map.mem(pid, p.d_map), probe_delta);
  let (items, merged) =
    switch (prev) {
    | None =>
      /* cold: compute every item in chain order */
      let (items_rev, _) =
        List.fold_left(
          ((acc, ctx), node) => {
            let it = calc_item(~settings, ~probe_ids, ~ctx_in=ctx, node);
            ([it, ...acc], it.d_ctx_out);
          },
          ([], ctx0),
          nodes,
        );
      let items = List.rev(items_rev);
      (
        items,
        List.fold_left(
          (m, it) => map_union(m, it.d_map),
          Id.Map.empty,
          items,
        ),
      );
    | Some(p) =>
      /* align old and new items BY ID, so a restructure costs the changed
         item plus downstream users of its exports. a moved item is popped
         aside where it was and recomputed where it lands */
      let prev_items = p.items;
      let prev_merged = p.merged;
      let node_ids =
        List.fold_left(
          (s, n) => Id.Set.add(Exp.rep_id(n), s),
          Id.Set.empty,
          nodes,
        );
      let prev_ids =
        List.fold_left(
          (s, q: item) => Id.Set.add(q.d_id, s),
          Id.Set.empty,
          prev_items,
        );
      let moved: ref(Id.Map.t(item)) = ref(Id.Map.empty);
      /* keys written by items analyzed so far in this pass: a member moved
         out of a module ahead of it keeps its ids, and removing the
         module's old keys must not drop them */
      let claimed: ref(Statics.Map.t) = ref(Id.Map.empty);
      let remove_stale = (old: Statics.Map.t, m: Statics.Map.t) =>
        Id.Map.fold(
          (k, _, m) => Id.Map.mem(k, claimed^) ? m : Id.Map.remove(k, m),
          old,
          m,
        );
      /* (re)compute one node; [prev_it] is its previous version if any */
      let run_dirty =
          (
            ~moved_in=false,
            prev_it,
            node,
            ctx,
            dirty_vars,
            dirty_tnames,
            merged,
          ) => {
        let it =
          calc_item(
            ~settings,
            ~probe_ids,
            ~probe_dirty,
            ~prev=?prev_it,
            ~dirty_vars,
            ~dirty_tnames,
            ~ctx_in=ctx,
            node,
          );
        let (p_exports, p_map) =
          switch (prev_it) {
          | Some(q) => (q.d_exports, q.d_map)
          | None => ([], Id.Map.empty)
          };
        let delta = export_delta(p_exports, it.d_exports);
        let (it, ctx_out) =
          switch (prev_it, delta) {
          | (Some(q), Unchanged) when ctx === q.d_ctx_in => (
              {
                ...it,
                d_ctx_out: q.d_ctx_out,
              },
              q.d_ctx_out,
            )
          | _ => (it, it.d_ctx_out)
          };
        /* this item's exports shadow INCOMING dirty names; its own
           delta is added after (it must not filter itself) */
        let incoming = shadow_filter(it.d_exports, dirty_vars);
        let incoming_t = tshadow(it.d_exports, dirty_tnames);
        let (dirty_vars, dirty_tnames) =
          seed_delta(delta, incoming, incoming_t);
        /* a move changes shadowing order, so its names may resolve to a
           different binder downstream: all its exports go dirty */
        let (dirty_vars, dirty_tnames) =
          if (moved_in) {
            (
              List.sort_uniq(compare, names_of(it.d_exports) @ dirty_vars),
              List.sort_uniq(
                compare,
                List.concat_map(tnames_of_entry, it.d_exports) @ dirty_tnames,
              ),
            );
          } else {
            (dirty_vars, dirty_tnames);
          };
        let dirty_tnames =
          List.sort_uniq(
            compare,
            ttransit(it.d_exports, dirty_tnames) @ dirty_tnames,
          );
        let merged = remove_stale(p_map, merged);
        claimed := map_union(claimed^, it.d_map);
        (it, ctx_out, dirty_vars, dirty_tnames, map_union(merged, it.d_map));
      };
      let rec go = (ps, ns, acc, ctx, dirty_vars, dirty_tnames, merged) =>
        switch (ps, ns) {
        | (ps, []) =>
          /* remaining prev items were deleted */
          let merged =
            List.fold_left(
              (m, q: item) =>
                Id.Set.mem(q.d_id, node_ids) ? m : remove_stale(q.d_map, m),
              merged,
              ps,
            );
          (List.rev(acc), merged);
        | ([q, ...pt], _) when !Id.Set.mem(q.d_id, node_ids) =>
          /* deleted: downstream loses its exports */
          let (dirty_vars, dirty_tnames) =
            seed_delta(
              export_delta(q.d_exports, []),
              dirty_vars,
              dirty_tnames,
            );
          go(
            pt,
            ns,
            acc,
            ctx,
            dirty_vars,
            dirty_tnames,
            remove_stale(q.d_map, merged),
          );
        | ([q, ...pt], [n, ..._])
            when
              q.d_id != Exp.rep_id(n)
              && Id.Set.mem(Exp.rep_id(n), prev_ids)
              && !Id.Map.mem(Exp.rep_id(n), moved^) =>
          /* the node sits deeper in prev, so q moved later: pop it aside.
             its exports go dirty for the span it crosses; its old map
             stays in merged until the move-in replaces it */
          moved := Id.Map.add(q.d_id, q, moved^);
          let (dirty_vars, dirty_tnames) =
            seed_delta(
              export_delta(q.d_exports, []),
              dirty_vars,
              dirty_tnames,
            );
          go(pt, ns, acc, ctx, dirty_vars, dirty_tnames, merged);
        | (ps, [n, ...nt]) =>
          let nid = Exp.rep_id(n);
          switch (ps) {
          | [q, ...pt] when q.d_id == nid =>
            /* aligned head */
            let clean =
              head_equal(q.d_node, n)
              && !stale(q, dirty_vars, dirty_tnames)
              && !probe_dirty(q);
            if (clean) {
              let (it, ctx_out) =
                ctx === q.d_ctx_in
                  ? (q, q.d_ctx_out)
                  /* upstream changed only names this item doesn't use:
                     re-chain its exports without re-running statics */
                  : {
                    let ctx_out = Ctx.prepend_entries(ctx, q.d_exports);
                    (
                      {
                        ...q,
                        d_ctx_in: ctx,
                        d_ctx_out: ctx_out,
                      },
                      ctx_out,
                    );
                  };
              let incoming_t = tshadow(it.d_exports, dirty_tnames);
              go(
                pt,
                nt,
                [it, ...acc],
                ctx_out,
                shadow_filter(it.d_exports, dirty_vars),
                List.sort_uniq(
                  compare,
                  ttransit(it.d_exports, incoming_t) @ incoming_t,
                ),
                merged,
              );
            } else {
              let (it, ctx_out, dirty_vars, dirty_tnames, merged) =
                run_dirty(Some(q), n, ctx, dirty_vars, dirty_tnames, merged);
              go(
                pt,
                nt,
                [it, ...acc],
                ctx_out,
                dirty_vars,
                dirty_tnames,
                merged,
              );
            };
          | _ =>
            let (prev_it, moved_in) =
              switch (Id.Map.find_opt(nid, moved^)) {
              | Some(q) => (Some(q), true)
              | None => (None, false) /* inserted */
              };
            let (it, ctx_out, dirty_vars, dirty_tnames, merged) =
              run_dirty(
                ~moved_in,
                prev_it,
                n,
                ctx,
                dirty_vars,
                dirty_tnames,
                merged,
              );
            go(
              ps,
              nt,
              [it, ...acc],
              ctx_out,
              dirty_vars,
              dirty_tnames,
              merged,
            );
          };
        };
      go(prev_items, nodes, [], ctx0, [], [], prev_merged);
    };
  let merged = fix_spine_infos(~probe_ids, items, merged);
  {
    items,
    term: whole,
    probe_ids,
    merged,
  };
};

/* the whole-program elaboration, each item's elab grafted into its
   predecessor's body hole; None on an unexpected elab shape. recursion
   depth is per item, so it fits the browser stack where a monolithic
   elaboration doesn't */
let whole_elab = (t: t): option(Exp.t) => {
  let grafted = graft_elabs(t.items);
  switch (strip(t.term).term) {
  | Module(_) =>
    /* mod root: the graft is the lowered expansion's elab; finish it
       the way monolithic Module statics does (marks the module value) */
    Option.map(
      ModuleHelpers.module_elab(~module_exp_id=Exp.rep_id(strip(t.term))),
      grafted,
    )
  | _ => grafted
  };
};

/* whole-program views over the per-item results */
let all_error_ids = (t: t): list(Id.t) =>
  List.concat_map(it => it.d_error_ids, t.items);

let all_warning_ids = (t: t): list(Id.t) => {
  let binder_ids =
    List.concat_map(
      it =>
        List.filter_map(
          fun
          | Ctx.VarEntry({id, _}) => Some(id)
          | _ => None,
          it.d_exports,
        ),
      t.items,
    );
  let engine_unused = unused_binders(t.items);
  List.concat_map(
    it => List.filter(id => !List.mem(id, binder_ids), it.d_warning_ids),
    t.items,
  )
  @ engine_unused;
};

/* set by the test runner to check each calc_auto result */
let after_calc: ref(option((CoreSettings.t, t) => unit)) = ref(None);

/* the last calc per document (LRU), so switching documents doesn't force
   a cold pass; keyed by the term's rep id, stable unless the first item
   is replaced */
let slots: Hashtbl.t(Id.t, t) = Hashtbl.create(8);
let slots_mru: ref(list(Id.t)) = ref([]);
let slots_cap = 8;

let calc_auto = (~settings, ~probe_ids=Id.Map.empty, whole: Exp.t): t => {
  /* a probe change keeps the slot: calc re-analyzes just the affected items */
  let key = Exp.rep_id(whole);
  let prev = Hashtbl.find_opt(slots, key);
  let t = calc(~settings, ~prev?, ~probe_ids, whole);
  Hashtbl.replace(slots, key, t);
  slots_mru := [key, ...List.filter(k => k != key, slots_mru^)];
  switch (Util.ListUtil.split_n_opt(slots_cap, slots_mru^)) {
  | Some((keep, evict)) when evict != [] =>
    List.iter(Hashtbl.remove(slots), evict);
    slots_mru := keep;
  | _ => ()
  };
  Option.iter(check => check(settings, t), after_calc^);
  t;
};

/* the last calc of [whole]'s document, if still cached */
let cached = (whole: Exp.t): option(t) =>
  Hashtbl.find_opt(slots, Exp.rep_id(whole));
