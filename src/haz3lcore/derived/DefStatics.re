open Language;

/* DefStatics — compositional whole-program statics
   (plans/modular-editors.md §8d): statics computed PER TOP-LEVEL ITEM
   (let / type alias / module / trailing expression) with chained
   ctxs, so an edit recomputes only the dirty set:
     - the edited item always;
     - downstream items, only when an upstream item's EXPORTS changed
       (name/id/type of a binding) AND they mention a changed name —
       expression names via their co_ctx (d_free), type-side names
       (aliases, constructors) via d_tfree;
     - top-level unused-binding warnings are computed by the ENGINE
       (an item in isolation can't see its downstream uses).
   Item statics runs on the item with its body replaced by a hole, so
   per-item info_maps compose into whole-program statics modulo the
   item nodes' own (chain-typed) infos. */

type item = {
  d_id: Id.t, /* the item's rep id (the outline id domain) */
  d_node: Exp.t,
  d_ctx_in: Ctx.t,
  d_map: Statics.Map.t,
  d_error_ids: list(Id.t),
  d_warning_ids: list(Id.t), /* engine-corrected (see unused pass) */
  d_exports: list(Ctx.entry),
  d_free: list(string), /* free expression vars of pat+def */
  d_tfree: list(string), /* type-side names the item depends on */
  d_ctx_out: Ctx.t,
  d_elab: Exp.t, /* elaboration of the hollow item */
  d_hole: option(Id.t), /* the body hole's id (None: trailing exp) */
  /* member-granular items (plans/mod-root.md phase 5): when the def
     is a module LITERAL, its members (+ exports tail) are analyzed as
     a nested item chain, memoized per member across calcs */
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
  | Projector(_, e)
  | Filter(_, e) => strip(e)
  | _ => e
  };

/* the top-level item chain; the trailing expression is its own item */
let rec chain = (e: Exp.t): list(Exp.t) => {
  let e = strip(e);
  switch (e.term) {
  | Let(_, _, body)
  | TyAlias(_, _, body)
  | ModuleExp(_, _, body) => [e, ...chain(body)]
  | Seq(_, body) => [e, ...chain(body)]
  | _ => [e]
  };
};

/* ---- Mod-rooted programs (plans/mod-root.md phase 2) ----
   A Module(items) ROOT itemizes exactly like the monolithic lowering
   (ModuleHelpers.wrap_item): each mod item becomes a hollow-able
   Let/TyAlias wrapper feeding the SAME per-item machinery, plus a
   trailing labeled tuple of the exports (the module value). Wrapper
   and tail ids must be STABLE across calcs — head_equal gates item
   cleanliness on them — so wrappers that can't reuse the item's rep
   id (ModExp/hole items: the rep belongs to the expression itself)
   get deterministic derived ids instead of the monolithic lowering's
   per-run fresh ones. */
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

/* the module-value tail: ModuleHelpers.labeled_tuple_exp with
   deterministic ids (derived from the root's rep + export name) so
   the tail stays head-equal — and thus clean — across calcs unless
   the export NAME SET changes. Its free vars are every export name,
   so any export delta re-analyzes it through the normal dirty path. */
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

/* item equality between program versions: the pat+def head, body
   excluded. Ids participate (they're stable across unrelated edits),
   so an id-preserving MakeTerm rebuild compares equal. */
let head_equal = (a: Exp.t, b: Exp.t): bool =>
  switch (a.term, b.term) {
  | (Let(p1, d1, _), Let(p2, d2, _)) => compare((p1, d1), (p2, d2)) == 0
  | (TyAlias(t1, y1, _), TyAlias(t2, y2, _)) =>
    compare((t1, y1), (t2, y2)) == 0
  | (ModuleExp(m1, d1, _), ModuleExp(m2, d2, _)) =>
    compare((m1, d1), (m2, d2)) == 0
  | (Seq(e1, _), Seq(e2, _)) => compare(e1, e2) == 0
  | (t1, t2) => compare(t1, t2) == 0 /* trailing exp: whole term */
  };

let entry_name = (e: Ctx.entry): string =>
  switch (e) {
  | VarEntry({name, _})
  | ConstructorEntry({name, _}) => name
  | TVarEntry({name, _}) => name
  | LivelitEntry({name, _}) => name
  };

let entry_equal = (a: Ctx.entry, b: Ctx.entry): bool =>
  switch (a, b) {
  | (VarEntry(v1), VarEntry(v2))
  | (ConstructorEntry(v1), ConstructorEntry(v2)) =>
    v1.name == v2.name
    && v1.id == v2.id
    && Typ.fast_equal(v1.typ, v2.typ)
    && v1.custom_statics == v2.custom_statics
  | (TVarEntry(t1), TVarEntry(t2)) =>
    t1.name == t2.name && t1.id == t2.id && compare(t1.kind, t2.kind) == 0
  | (LivelitEntry(l1), LivelitEntry(l2)) => l1 === l2 /* closures */
  | _ => false
  };

/* ---- type-side dependency tracking ----
   co_ctx records only EXPRESSION variables, so type-side names get a
   parallel treatment: d_tfree per item, type-name sets in export
   deltas, and a dirty type-name set down the chain folds — with
   shadowing by type-side rebinds and TRANSITIVE closure through
   alias definitions (an alias whose definition mentions a dirty name
   is itself dirty: normalization chases chains lazily at use sites,
   so users of the head alias never mention the tail name). */

/* type-side names an export ENTRY involves (the ctor's typ mentions
   its sum name — case scrutinee infos mention the sum, not the ctor) */
let tnames_of_entry = (e: Ctx.entry): list(string) =>
  switch (e) {
  | TVarEntry({name, _}) => [name]
  | ConstructorEntry({name, typ, _}) => [name, ...Typ.free_vars(typ)]
  | VarEntry(_)
  | LivelitEntry(_) => []
  };

let is_type_entry = (e: Ctx.entry): bool =>
  switch (e) {
  | TVarEntry(_)
  | ConstructorEntry(_) => true
  | _ => false
  };

/* type-side names an ITEM depends on: syntactic type-position names
   and constructor uses, plus names in the STORED (unnormalized)
   types of its infos — an item using a var x : T depends on T
   without ever writing it (normalization resolves lazily at use). */
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
    | Constructor(c, _) => add([c])
    | _ => ()
    };
    cont(e);
  };
  let f_pat = (cont, p: Pat.t) => {
    switch (p.term) {
    | Constructor(c, _) => add([c])
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

/* dirty type names SHADOWED by this item's own type-side exports
   (a rebinding serves downstream lookups; expression bindings never
   shadow the type namespace — lookup_tvar walks past them) */
let tshadow = (exports: list(Ctx.entry), dirty: list(string)) =>
  List.filter(
    n => !List.exists(e => is_type_entry(e) && entry_name(e) == n, exports),
    dirty,
  );

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

/* did the exports change, and how? */
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

let shadow_filter = (exports: list(Ctx.entry), dirty: list(string)) =>
  List.filter(v => !List.exists(e => entry_name(e) == v, exports), dirty);

/* "*" is the unknown-free-vars sentinel: depends on anything dirty */
let depends = (free: list(string), dirty: list(string)): bool =>
  dirty != []
  && (List.mem("*", free) || List.exists(v => List.mem(v, free), dirty));

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

/* Item statics runs with the continuation hollowed, so an item ROOT's
   info misses everything about LATER items. Two root fields feed the
   evaluator's incremental reuse_check, and both must look monolithic
   or the resident cache replays stale runs:
     - probe_targets (= probes under the node): left stale, adding a
       probe deep in the program looks like "nothing changed" at the
       top-level spine and the cached run replays sampleless;
     - co_ctx (= names used under the node): reuse-map Dirty flags
       (a re-bound definition dirtying its callers) are consulted
       through exactly this co_ctx, so without the suffix's names a
       spine root reuses right past a dirtied binding and the whole
       suffix — call sites included — replays from cache.
   Patch the merged view: each non-tail root's witness/co_ctx becomes
   its own unioned with the items below it (minus its own bindings,
   for co_ctx — the same scoping a monolithic analysis applies). */
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
  let (merged, top_wit, top_co) =
    List.fold_right(
      (it: item, (m, below_wit, below_co)) => {
        let bound = List.map(entry_name, it.d_exports);
        let below_co_scoped =
          CoCtx.filter_names(name => !List.mem(name, bound), below_co);
        /* CRITICAL: read the root's RAW info from the item's own
           d_map, never from [m]. The incremental calc feeds the
           previous run's PATCHED merged back in as the base — reading
           the patched entry and unioning the suffix again DOUBLES the
           co_ctx use-lists every calc (exponential memory: the mega
           editors died within a few edits). d_maps stay raw, so
           sourcing from them makes the patch idempotent. */
        switch (it.d_hole, Statics.Map.lookup_exp(it.d_id, it.d_map)) {
        | (Some(_), Some(raw)) =>
          let co_ctx = CoCtx.union([raw.co_ctx, below_co_scoped]);
          let m =
            Id.Map.add(
              it.d_id,
              Info.InfoExp({
                ...raw,
                probe_targets:
                  SubexpProbeTargets.union(raw.probe_targets, below_wit),
                co_ctx,
              }),
              m,
            );
          (m, SubexpProbeTargets.union(probes_in(it), below_wit), co_ctx);
        | _ =>
          /* tail item (info already whole-suffix accurate) or no
             InfoExp at the root (e.g. module forms): thread what we
             know upward without patching */
          let own_co =
            switch (Statics.Map.lookup_exp(it.d_id, it.d_map)) {
            | Some(raw) => raw.co_ctx
            | None => CoCtx.empty
            };
          (
            m,
            SubexpProbeTargets.union(probes_in(it), below_wit),
            CoCtx.union([own_co, below_co_scoped]),
          );
        };
      },
      items,
      (merged, SubexpProbeTargets.empty, CoCtx.empty),
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

let map_remove_keys = (keys: Statics.Map.t, m: Statics.Map.t): Statics.Map.t =>
  Id.Map.fold((k, _, m) => Id.Map.remove(k, m), keys, m);

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

/* graft a hollow-item chain's elabs into one expression */
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

/* compute one item's statics in isolation: body swapped for a hole */
let rec calc_item =
        (
          ~settings,
          ~probe_ids=Id.Map.empty,
          ~probe_dirty: item => bool=_ => false,
          ~prev: option(item)=?,
          /* dirty names INCOMING at this item's position: a module
             item re-analyzing because an UPSTREAM export changed must
             pass them into its member chain, or members using the
             changed name reuse stale maps */
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
    | Seq(_) => false
    | _ => true
    };
  let hollow_term: Exp.term =
    switch (node.term) {
    | Let(p, d, _) => Let(p, d, hole)
    | TyAlias(tp, ty, _) => TyAlias(tp, ty, hole)
    | ModuleExp(mp, d, _) => ModuleExp(mp, d, hole)
    | Seq(e, _) => Seq(e, hole)
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
  /* free expression vars: read the DEF's co_ctx (the item node itself
     may not carry an InfoExp — e.g. ModuleExp — and a silent [] here
     would make items falsely clean); type-side deps live in d_tfree */
  let free = {
    let src =
      switch (node.term) {
      | Let(_, d, _)
      | ModuleExp(_, d, _) => Some(d)
      | Seq(e, _) => Some(e)
      | TyAlias(_) => None
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

/* member granularity fires only for SIMPLE bindings of a module
   literal: ascribed signatures push ana_labels into the lowering,
   which the member path does not replicate (they keep the monolithic
   item analysis) */
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

/* the member-granular module item: members (+ exports tail) run as a
   nested memoized chain; the WRAPPER's statics run on a tiny
   surrogate where the def is (hole : actual_ty) — same Let machinery,
   member-sized cost. The surrogate skips two module-literal-only
   effects, replicated here: the M.T type-export alias injected into
   the body ctx, and the def-node root info's co_ctx/probe-witness
   view of the members (fix_spine discipline). */
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
  /* member roots need the same suffix co_ctx/witness patch the top
     spine gets, or the evaluator's reuse gating replays stale runs */
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
  /* wrapper surrogate: def := (hole : actual_ty) */
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
  /* surrogate-only scaffolding ids (the inner hole + the synthesized
     type annotation), minus the def's own rep id (its entry stands in
     for the module node) */
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
    /* replicate the Let-with-module-literal special case the
       surrogate skips: inject the M.T type-export alias */
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
  /* the item ROOT's info must look monolithic for the reuse gating:
     union the members' co_ctx/witnesses into it (raw source = this
     freshly built map, so the patch stays idempotent across calcs) */
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
  /* elab: graft the member elabs into the module value, finish like
     monolithic Module statics, and splice into the wrapper's elab */
  let d_elab =
    switch (graft_elabs(items_m)) {
    | Some(g) =>
      ModuleHelpers.moduleexp_elab(
        ~def_elab_direct=
          ModuleHelpers.module_elab(~module_exp_id=Exp.rep_id(def), g),
        elab_sur,
      )
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
    d_warning_ids:
      List.concat_map((m: item) => m.d_warning_ids, items_m)
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

/* the nested member chain: same clean/dirty discipline as the top
   chain, without move tracking (member reorders recompute from the
   change point on — the hot path is the in-place member edit) */
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
  let prev_tbl = Hashtbl.create(List.length(prev_members) + 1);
  List.iter(
    (q: item) => Hashtbl.replace(prev_tbl, q.d_id, q),
    prev_members,
  );
  let rec go = (ns, ctx, dirty_vars, dirty_tnames, acc) =>
    switch (ns) {
    | [] => List.rev(acc)
    | [n, ...nt] =>
      let nid = Exp.rep_id(n);
      let prev_it = Hashtbl.find_opt(prev_tbl, nid);
      let clean =
        switch (prev_it) {
        | Some(q) =>
          head_equal(q.d_node, n)
          && !depends(q.d_free, dirty_vars)
          && !depends(q.d_tfree, dirty_tnames)
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
        let dirty_tnames =
          List.sort_uniq(
            compare,
            ttransit(it.d_exports, dirty_tnames) @ dirty_tnames,
          );
        go(nt, it.d_ctx_out, dirty_vars, dirty_tnames, [it, ...acc]);
      };
    };
  go(nodes, ctx_in, dirty_vars, dirty_tnames, []);
};

/* engine-level unused-binding pass: a top-level export is used iff a
   DOWNSTREAM item (up to a re-binding of the same name) mentions it.
   Corrects the per-item maps' view, which sees a hole body and would
   call every top-level binder unused. */
let unused_binders = (items: list(item)): list(Id.t) => {
  let rec used_below = (name: string, rest: list(item)): bool =>
    switch (rest) {
    | [] => false
    | [it, ...rest] =>
      List.mem(name, it.d_free)
      || (
        List.exists(e => entry_name(e) == name, it.d_exports)
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

/* the seed ctx must be PHYSICALLY stable across calc calls: reuse
   gating chains on pointer identity in the clean case */
let ctx0: Ctx.t = Builtins.ctx_init(Some(Operators.default_mode));

/* incremental calculate. INVARIANT down the fold: [ctx] differs from
   the prev run's ctx at this position only at [dirty_vars] /
   [dirty_tnames] entries. A clean item is one whose head is
   unchanged and whose d_free/d_tfree avoid the dirty sets — its
   statics are reused; its embedded map keeps STALE ctx entries for
   dirty names it doesn't use (sound for typing; Γ display of
   unrelated names can lag until the item is next recomputed).
   Structural edits (item added/removed/reordered) align BY ID and
   cost the changed item plus downstream mentioners of its export
   names. */

let names_of = (exports: list(Ctx.entry)): list(string) =>
  List.sort_uniq(compare, List.map(entry_name, exports));

/* the top-level item chain of a whole-program term. A Module ROOT
   (mod-rooted editors) itemizes via the lowering; a Module literal
   anywhere else is an ordinary expression (chain never descends into
   defs, so only the root case can see one). */
let chain_root = (e: Exp.t): list(Exp.t) => {
  let s = strip(e);
  switch (s.term) {
  | Module(items) =>
    List.map(lower_mod_item, items) @ [exports_tail(Exp.rep_id(s), items)]
  | _ => chain(e)
  };
};

let calc =
    (~settings, ~prev: option(t)=?, ~probe_ids=Id.Map.empty, whole: Exp.t): t => {
  last_analyzed := 0;
  let nodes = chain_root(whole);
  /* probe ids are an ANALYSIS input (witness stamping): an item whose
     map contains a toggled probe id must re-analyze. Everything else
     stays clean — a probe toggle costs one item, not the program. */
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
      /* diff-walk alignment BY ITEM ID: restructures (insert / delete /
         duplicate / move a top-level item) cost the changed item plus
         downstream mentioners of its export names — not the program.
         Steps: heads match → the usual clean/dirty logic; prev head's
         id gone from the program → delete (exports go dirty); prev
         head matches a later node → move-out (pop it aside, exports go
         dirty across the span it crossed); node's id is a popped item
         → move-in (recompute unconditionally: its ctx here is
         unrelated to its old position's); unknown id → insert. */
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
        /* a moved item's names may resolve to a DIFFERENT binder
           downstream (ctx entry order = shadowing order changed):
           floor its delta at its own export names */
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
        /* aliases defined here whose definitions mention dirty names
           are dirty downstream (transitive chains) */
        let dirty_tnames =
          List.sort_uniq(
            compare,
            ttransit(it.d_exports, dirty_tnames) @ dirty_tnames,
          );
        (
          it,
          ctx_out,
          dirty_vars,
          dirty_tnames,
          map_union(map_remove_keys(p_map, merged), it.d_map),
        );
      };
      let rec go = (ps, ns, acc, ctx, dirty_vars, dirty_tnames, merged) =>
        switch (ps, ns) {
        | (ps, []) =>
          /* remaining prev items were deleted */
          let merged =
            List.fold_left(
              (m, q: item) =>
                Id.Set.mem(q.d_id, node_ids)
                  ? m : map_remove_keys(q.d_map, m),
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
            map_remove_keys(q.d_map, merged),
          );
        | ([q, ...pt], [n, ..._])
            when
              q.d_id != Exp.rep_id(n)
              && Id.Set.mem(Exp.rep_id(n), prev_ids)
              && !Id.Map.mem(Exp.rep_id(n), moved^) =>
          /* the NODE head sits deeper in prev (not an insert, not
             already popped): q matches a LATER node — a move. Its
             exports go dirty for the span it crosses; its old map
             stays in merged until the move-in replaces it. */
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
              && !depends(q.d_free, dirty_vars)
              && !depends(q.d_tfree, dirty_tnames)
              && !probe_dirty(q);
            if (clean) {
              let (it, ctx_out) =
                ctx === q.d_ctx_in
                  ? (q, q.d_ctx_out)
                  /* upstream entries changed for names this item
                     doesn't use: re-chain its exports onto the new
                     ctx without re-running statics */
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

/* Stitch the per-item elaborations into a whole-program elaboration:
   each hollow item's elab contains its body hole; graft the next
   item's (already-grafted) elab in its place. Descends only through
   body-position shapes — None means an unexpected elab shape (the
   caller degrades to no-eval rather than crashing). Depth is the
   ITEM's elab depth, not the program's, so this stays within the
   browser's stack where a monolithic elaboration doesn't. */
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

/* single-slot auto cache: the scratch/documentation master is the one
   whole-program editor; slide switches and structural edits fall back
   to a full recompute via calc's own alignment check */
let slot: ref(option(t)) = ref(None);

/* per-DOCUMENT slots, LRU-capped: a single global slot meant any
   alternation of statics consumers (slide switches; a second client)
   thrashed it into full cold re-analyses (measured: 891 members /
   ~650ms on returning to a mega slide). Keyed by the whole term's
   rep id — stable for a document unless its first item is replaced,
   which costs one cold pass. `slot` still tracks the ACTIVE document
   for current()/error_item_ids. */
let slots: Hashtbl.t(Id.t, t) = Hashtbl.create(8);
let slots_mru: ref(list(Id.t)) = ref([]);
let slots_cap = 8;

let calc_auto = (~settings, ~probe_ids=Id.Map.empty, whole: Exp.t): t => {
  /* probe changes no longer bust the slot: calc's probe-aware dirtying
     re-analyzes exactly the items whose maps contain a toggled id */
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
  slot := Some(t);
  t;
};

let current = (): option(t) => slot^;

/* item ids (outline id domain) currently carrying errors — the
   outline badge feed; reads the auto-cache slot */
let error_item_ids = (): list(Id.t) =>
  switch (slot^) {
  | None => []
  | Some(t) =>
    List.filter_map(
      it => it.d_error_ids == [] ? None : Some(it.d_id),
      t.items,
    )
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
