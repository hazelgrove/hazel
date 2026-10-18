open Language;

/* whole-program statics per top-level item (body replaced by a hole)
   with chained ctxs: an edit recomputes the edited item plus downstream
   items that mention a name whose export changed (d_free for expression
   names, d_tfree for type-side ones). unused-binding warnings are
   computed across items, since an item alone can't see its later uses.
   what the items add up to (a chain's merged map, its roots' suffix
   patches, its aggregates) is kept between calcs and updated where items
   changed, so an edit's bookkeeping is proportional to the edit */

module SSet = Set.Make(String);
module SMap = Map.Make(String);

type item = {
  d_id: Id.t, /* the item's rep id (the outline id domain) */
  d_node: Exp.t,
  d_ctx_in: Ctx.t,
  d_map: Statics.Map.t,
  d_error_ids: list(Id.t),
  d_warning_ids: list(Id.t), /* never its top-level binders (own_unused) */
  d_exports: list(Ctx.entry),
  d_free: list(string), /* free expression vars of pat+def */
  d_tfree: list(string), /* type-side names the item depends on */
  d_ctx_out: Ctx.t,
  d_elab: Exp.t, /* elaboration of the hollow item */
  d_hole: option(Id.t), /* the body hole's id (None: trailing exp) */
  /* a nested item chain, memoized per item: a module literal def's
     members (+ exports tail), or a let-chain ModExp member's links */
  d_members: list(item),
  /* what the next calc of this item updates rather than redoes */
  d_nest: option(nest),
}
/* an item and the chain below it, summed from the bottom up: the item's
   root info patched over the suffix, and the chain's aggregates from it
   down. a calc keeps the sums below its lowest changed item */
and sum = {
  s_item: item, /* as it was when summed */
  s_src: option(Mod.t), /* a module member's source, reclassified */
  s_wit: SubexpProbeTargets.t,
  s_co: CoCtx.t,
  s_elab: option(Exp.t), /* None on an unexpected elab shape */
  s_free: list(string),
  s_tfree: list(string),
  s_holes: bool, /* a real hole at or below */
  s_errors: list(Id.t),
  s_warnings: list(Id.t),
  s_unused: list(Id.t),
  /* written over the item's own entries: its patched root info and, for
     a module member, its ids reclassified */
  s_overlay: list((Id.t, Info.t)),
}
/* a module or ModExp item's chain sums (its map holds the chain's merged
   map), its own entries over the chain's, and a module's memos */
and nest = {
  n_sums: list(sum),
  n_wrap: Statics.Map.t,
  n_memo: option(memo),
}
and memo = {
  m_lowering: lowering,
  m_types: (list(Mod.t), Ctx.t, list((Var.t, Typ.t))),
  m_actual: actual,
  m_sur: sur,
}
/* members lowered (by rep id), and the exports tail by its module id and
   export names */
and lowering = {
  by_id: Id.Map.t(lowered),
  tail: option((Id.t, list(string), Exp.t)),
}
/* a member, the node its item holds, its own exports and bound names */
and lowered = {
  l_src: Mod.t,
  l_node: Exp.t,
  l_exports: list(ModuleHelpers.value_export),
  l_names: list(Var.t),
}
/* the module's type: per export, the pattern info read and its field */
and actual = {
  a_locals: list(string),
  a_fields: list(field),
  a_ty: Typ.t,
}
and field = {
  f_pat: Id.t,
  f_info: option(Info.t),
  f_ty: Typ.t,
}
/* the module item's surrogate analysis, with its inputs */
and sur = {
  u_settings: CoreSettings.t,
  u_ctx: Ctx.t,
  u_node: Exp.t,
  u_def: Id.t,
  u_ty: Typ.t,
  u_probes: Id.Map.t(unit),
  u_map: Statics.Map.t, /* scaffolding removed */
  u_elab: Exp.t,
  u_ctx_out: Ctx.t, /* before the M.T alias */
  u_hole: Id.t,
  u_errors: list(Id.t),
  u_warnings: list(Id.t),
};

type t = {
  items: list(item),
  term: Exp.t, /* the whole term these items were computed from */
  probe_ids: Id.Map.t(unit),
  merged: Statics.Map.t, /* union of the items' maps, kept incrementally */
  /* each item's elab grafted into its predecessor's body hole: recursion
     depth is per item, so it fits the browser stack where a monolithic
     elaboration doesn't */
  elab: option(Exp.t),
  sums: list(sum), /* the top chain's */
  lowering: option(lowering) /* a Module root's */
};

let rec strip = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e)
  | Projector(_, e) => strip(e)
  | _ => e
  };

/* a chain item's body, and the item rebuilt around another body */
let split_body = (item: Exp.t): option((Exp.t, Exp.t => Exp.term)) =>
  switch (item.term) {
  | Let(p, d, body) => Some((body, (b => Let(p, d, b))))
  | TyAlias(tp, ty, body) => Some((body, (b => TyAlias(tp, ty, b))))
  | ModuleExp(mp, d, body) => Some((body, (b => ModuleExp(mp, d, b))))
  | Seq(e, body) => Some((body, (b => Seq(e, b))))
  | Filter(f, body) => Some((body, (b => Filter(f, b))))
  | _ => None
  };

/* the top-level item chain; the trailing expression is its own item.
   a chain inside parens or a projector (a folded section) stays whole
   in the tail: the wrapper has info and elaboration of its own */
let rec chain = (e: Exp.t): list(Exp.t) =>
  switch (split_body(e)) {
  | Some((body, _)) => [e, ...chain(body)]
  | None => [e]
  };

/* a module item as a chain item, like the monolithic lowering: a
   Let/TyAlias wrapper (lower_members adds a tail tuple of the exports).
   head_equal gates cleanliness on wrapper and tail ids, so they must be
   stable: wrappers that can't reuse the item's rep id get derived ids */
let lower_item = (item: Mod.t): Exp.t =>
  ModuleHelpers.wrap_item(
    ~ana_labels=[],
    ~stable=true,
    item,
    Exp.fresh(EmptyHole),
  );

/* a let chain at an item position is a level of items; one inside a
   definition is not. so a ModExp member whose expression is a chain
   (`{ let a = 1 in a + 1 }`) splits, and `let _ = <chain>` doesn't. the
   two share a shape once lowered: the member's wrapper is told apart by
   its id, which lower_item derives from the member's rep id, and MakeTerm
   gives a ModExp(e) e's rep id (one minted around a broken parse's e has
   its own, and stays whole) */
let modexp_chain = (node: Exp.t): option((Pat.t, Exp.t)) =>
  switch (node.term) {
  | Let({term: Wild, _} as p, e, _) when Option.is_some(split_body(e)) =>
    let member = IdTagged.fast_copy(Exp.rep_id(e), Mod.fresh(ModExp(e)));
    Exp.rep_id(lower_item(member)) == Exp.rep_id(node)
      ? Some((p, e)) : None;
  | _ => None
  };

/* head parts compared structurally, a test counter: a clean item's
   parts are physically the same (MakeTerm's member memo, the lowering
   memo), which costs O(1) */
let head_compares = ref(0);

/* item equality across versions: the head only, ids included (an
   id-preserving rebuild compares equal) */
let head_equal = (a: Exp.t, b: Exp.t): bool => {
  let same = (x, y) =>
    x === y
    || {
      incr(head_compares);
      compare(x, y) == 0;
    };
  a === b
  || (
    switch (a.term, b.term) {
    | (Let(p1, d1, _), Let(p2, d2, _)) => same(p1, p2) && same(d1, d2)
    | (TyAlias(t1, y1, _), TyAlias(t2, y2, _)) =>
      same(t1, t2) && same(y1, y2)
    | (ModuleExp(m1, d1, _), ModuleExp(m2, d2, _)) =>
      same(m1, m2) && same(d1, d2)
    | (Seq(e1, _), Seq(e2, _)) => same(e1, e2)
    | (Filter(f1, _), Filter(f2, _)) => same(f1, f2)
    | (t1, t2) => same(t1, t2) /* trailing exp: whole term */
    }
  );
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
  let changed =
    List.length(old) != List.length(new_)
      ? old @ new_
      : List.concat(
          List.map2((o, n) => entry_equal(o, n) ? [] : [o, n], old, new_),
        );
  let vars =
    List.concat_map(e => is_type_entry(e) ? [] : [entry_name(e)], changed);
  let tnames = List.concat_map(tnames_of_entry, changed);
  vars == [] && tnames == []
    ? Unchanged
    : Changed({
        vars: List.sort_uniq(compare, vars),
        tnames: List.sort_uniq(compare, tnames),
      });
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

/* sums computed in the last calc, a test counter */
let summed: ref(int) = ref(0);

let map_union = (a: Statics.Map.t, b: Statics.Map.t): Statics.Map.t =>
  Id.Map.union((_, _x, y) => Some(y), a, b);

let union_maps = (items: list(item)): Statics.Map.t =>
  List.fold_left((m, it) => map_union(m, it.d_map), Id.Map.empty, items);

let keys = (m: Statics.Map.t, acc: list(Id.t)): list(Id.t) =>
  Id.Map.fold((k, _, acc) => [k, ...acc], m, acc);

/* how a walk changed an item's map: whole (None: dropped), or only at
   some keys, which a nested item's bookkeeping knows */
type delta =
  | Whole(Statics.Map.t, option(Statics.Map.t))
  | Keys(list(Id.t), Statics.Map.t);

/* the keys a walk's changes may have rebound */
let change_keys = (changes: list((Id.t, delta))): list(Id.t) =>
  List.fold_left(
    (acc, (_, d)) =>
      switch (d) {
      | Whole(old, None) => keys(old, acc)
      | Whole(old, Some(n)) => keys(old, keys(n, acc))
      | Keys(ks, _) => List.rev_append(ks, acc)
      },
    [],
    changes,
  );

/* [merged] under a walk's [changes] (newest first), replayed in walk
   order: an old map's keys leave unless a recomputed item claimed them
   earlier in the walk (a member moved out of a module ahead of it keeps
   its ids), and new maps join. also returns the keys claimed */
let remerge =
    (merged: Statics.Map.t, changes): (Statics.Map.t, Statics.Map.t) =>
  List.fold_left(
    ((merged, claimed), (_, d)) =>
      switch (d) {
      | Whole(old, new_) =>
        let merged =
          Id.Map.fold(
            (k, _, m) => Id.Map.mem(k, claimed) ? m : Id.Map.remove(k, m),
            old,
            merged,
          );
        switch (new_) {
        | Some(n) => (map_union(merged, n), map_union(claimed, n))
        | None => (merged, claimed)
        };
      | Keys(ks, n) =>
        List.fold_left(
          ((merged, claimed), k) =>
            switch (Id.Map.find_opt(k, n)) {
            | Some(v) => (
                Id.Map.add(k, v, merged),
                Id.Map.add(k, v, claimed),
              )
            | None => (
                Id.Map.mem(k, claimed) ? merged : Id.Map.remove(k, merged),
                claimed,
              )
            },
          (merged, claimed),
          ks,
        )
      },
    (merged, Id.Map.empty),
    List.rev(changes),
  );

/* CoCtx.union([a, b]) in a pass over each, where its name lookups are
   linear: b's names a lacks (newest first), then a's (b's uses after
   their own). [b] is a union's or a filter of one, so its names are
   unique; a repeated name in [a] takes CoCtx.union */
let co_union = (a: CoCtx.t, b: CoCtx.t): CoCtx.t => {
  let a = CoCtx.to_list(a);
  let names = List.fold_left((s, (n, _)) => SSet.add(n, s), SSet.empty, a);
  if (SSet.cardinal(names) < List.length(a)) {
    CoCtx.union([a, b]);
  } else {
    let (fresh, shared) =
      List.fold_left(
        ((fresh, shared), (n, es)) =>
          SSet.mem(n, names)
            ? (fresh, SMap.add(n, es, shared))
            : ([(n, es), ...fresh], shared),
        ([], SMap.empty),
        CoCtx.to_list(b),
      );
    fresh
    @ List.fold_left(
        (acc, (n, es)) =>
          [
            (
              n,
              switch (SMap.find_opt(n, shared)) {
              | Some(more) => es @ more
              | None => es
              },
            ),
            ...acc,
          ],
        [],
        a,
      );
  };
};

let probes_in = (~probe_ids, it: item): SubexpProbeTargets.t =>
  Id.Map.fold(
    (pid, (), acc) =>
      Id.Map.mem(pid, it.d_map)
        ? SubexpProbeTargets.add_self(~is_probed=true, pid, acc) : acc,
    probe_ids,
    SubexpProbeTargets.empty,
  );

/* the `f(x)` and `f(x) : T` wrappers of a function definition carry its
   name's info, unused warning included (FunctionSugar.add_binder_infos) */
let fun_wrapper_ids = (node: Exp.t, name_id: Id.t): list(Id.t) => {
  let names_it = (p: Pat.t) =>
    switch (p.term) {
    | Ap(fn, _) => Pat.rep_id(fn) == name_id
    | _ => false
    };
  switch (node.term) {
  | Let(p, _, _) =>
    switch (p.term) {
    | Asc(inner, _) when names_it(inner) => [
        Pat.rep_id(p),
        Pat.rep_id(inner),
      ]
    | _ when names_it(p) => [Pat.rep_id(p)]
    | _ => []
    }
  | _ => []
  };
};

/* a chain export is used iff a later item mentions it before a value
   rebinding ([used]: the free names of the items below, which is that),
   or a hole below could ([holes]: "$hole" in a d_free below, real holes
   only, since synthetic body holes aren't in any def) */
let own_unused = (it: item, ~used: list(string), ~holes: bool): list(Id.t) =>
  holes
    ? []
    : List.concat_map(
        fun
        | Ctx.VarEntry({name, id, _}) =>
          List.exists(String.equal(name), used)
          || String.length(name) > 0
          && name.[0] == '_'
            ? [] : [id, ...fun_wrapper_ids(it.d_node, id)]
        | _ => [],
        it.d_exports,
      );

/* [it] summed over the sum below. item statics hollows the continuation,
   so a spine root's info misses later items. the evaluator's reuse
   gating reads a root's elab_term, probe_targets and co_ctx, so each
   non-tail root gets them extended over the suffix (its co_ctx minus the
   root's value bindings), or cached runs replay stale */
let summarize =
    (~probe_ids, ~src: option(Mod.t), it: item, below: option(sum)): sum => {
  incr(summed);
  let (wit, co, elab, free, tfree, holes, errors, warnings, unused) =
    switch (below) {
    | Some(s) => (
        s.s_wit,
        s.s_co,
        s.s_elab,
        s.s_free,
        s.s_tfree,
        s.s_holes,
        s.s_errors,
        s.s_warnings,
        s.s_unused,
      )
    | None => (
        SubexpProbeTargets.empty,
        CoCtx.empty,
        None,
        [],
        [],
        false,
        [],
        [],
        [],
      )
    };
  let bound = var_names(it.d_exports);
  /* the root's elab_term is the whole suffix: a hollow one reads as
     unchanged whatever happens downstream */
  let s_elab =
    switch (it.d_hole, elab) {
    | (Some(h), Some(below)) =>
      MakeTerm.Incr.graft_spine(h, below, it.d_elab)
    | (None, _) => Some(it.d_elab)
    | (Some(_), None) => None
    };
  /* the RAW info, from the item's own map: the chain's holds the patch */
  let raw = Statics.Map.lookup_exp(it.d_id, it.d_map);
  let s_co =
    co_union(
      Option.fold(~none=CoCtx.empty, ~some=(r: Info.exp) => r.co_ctx, raw),
      bound == []
        ? co
        : CoCtx.filter_names(
            name => !List.exists(String.equal(name), bound),
            co,
          ),
    );
  /* a tail item is already accurate, and a module form has no InfoExp
     at the root: those thread upward without patching */
  let overlay =
    switch (it.d_hole, raw) {
    | (Some(_), Some(raw)) =>
      Id.Map.singleton(
        it.d_id,
        Info.InfoExp({
          ...raw,
          elab_term: Option.value(~default=raw.elab_term, s_elab),
          probe_targets: SubexpProbeTargets.union(raw.probe_targets, wit),
          co_ctx: s_co,
        }),
      )
    | _ => Id.Map.empty
    };
  /* a module's items read as module items, as monolithically */
  let overlay =
    switch (src) {
    | None => overlay
    | Some(m) =>
      let rep = Mod.rep_id(m);
      switch (Id.Map.find_opt(rep, overlay), Id.Map.find_opt(rep, it.d_map)) {
      | (Some(base), _)
      | (None, Some(base)) =>
        ModuleHelpers.reclassify_expanded_module_items(
          [m],
          Id.Map.add(rep, base, overlay),
        )
      | (None, None) => overlay
      };
    };
  {
    s_item: it,
    s_src: src,
    s_wit: SubexpProbeTargets.union(probes_in(~probe_ids, it), wit),
    s_co,
    s_elab,
    s_free:
      List.sort_uniq(
        String.compare,
        it.d_free @ shadow_filter(it.d_exports, free),
      ),
    s_tfree:
      List.sort_uniq(
        String.compare,
        it.d_tfree @ tshadow(it.d_exports, tfree),
      ),
    s_holes: holes || List.mem("$hole", it.d_free),
    s_errors: it.d_error_ids @ errors,
    s_warnings: it.d_warning_ids @ warnings,
    s_unused: own_unused(it, ~used=free, ~holes) @ unused,
    s_overlay: Id.Map.bindings(overlay),
  };
};

/* whether [s] still sums [it]: nothing it read has changed */
let sums_still = (s: sum, it: item, src: option(Mod.t)): bool => {
  let q = s.s_item;
  (
    q === it
    || Id.equal(q.d_id, it.d_id)
    && q.d_map === it.d_map
    && q.d_elab === it.d_elab
    && q.d_exports === it.d_exports
    && q.d_free === it.d_free
    && q.d_tfree === it.d_tfree
    && q.d_error_ids === it.d_error_ids
    && q.d_warning_ids === it.d_warning_ids
    && q.d_node === it.d_node
    && Option.equal(Id.equal, q.d_hole, it.d_hole)
  )
  && (
    switch (s.s_src, src) {
    | (None, None) => true
    | (Some(a), Some(b)) =>
      a === b
      || List.equal(Id.equal, IdTagged.ids(a), IdTagged.ids(b))
      && Mod.cls_of_term(a.term) == Mod.cls_of_term(b.term)
    | _ => false
    }
  );
};

let top_sum = (sums: list(sum)): option(sum) =>
  switch (sums) {
  | [s, ..._] => Some(s)
  | [] => None
  };

/* [items]' sums and merged map after a walk's [changes]: from the last
   run's ([prev]: its sums, and a map that holds its merged map), or
   afresh. sums below the lowest changed item are kept and the rest are
   summed again, their overlays rewritten. also returns the keys where
   the map may have changed (None: built afresh) */
let step_chain =
    (
      ~probe_ids,
      ~srcs: list(option(Mod.t)),
      ~prev: option((list(sum), Statics.Map.t)),
      items: list(item),
      changes: list((Id.t, delta)),
    )
    : (list(sum), Statics.Map.t, option(list(Id.t))) => {
  let changed =
    List.fold_left(
      (s, (id, _)) => Id.Set.add(id, s),
      Id.Set.empty,
      changes,
    );
  /* bottom first */
  let rec pair = (items, srcs, acc) =>
    switch (items, srcs) {
    | ([], _) => acc
    | ([it, ...items], [src, ...srcs]) =>
      pair(items, srcs, [(it, src), ...acc])
    | ([it, ...items], []) => pair(items, [], [(it, None), ...acc])
    };
  let rec keep = (up, old, kept) =>
    switch (up, old) {
    | ([(it, src), ...up'], [s, ...old'])
        when !Id.Set.mem(it.d_id, changed) && sums_still(s, it, src) =>
      keep(up', old', [s, ...kept])
    | _ => (up, old, kept)
    };
  let (up, replaced, kept) =
    keep(
      pair(items, srcs, []),
      switch (prev) {
      | Some((sums, _)) => List.rev(sums)
      | None => []
      },
      [],
    );
  let (fresh, _) =
    List.fold_left(
      ((acc, below), (it, src)) => {
        let s = summarize(~probe_ids, ~src, it, below);
        ([s, ...acc], Some(s));
      },
      ([], top_sum(kept)),
      up,
    );
  let sums = fresh @ kept;
  let overlays =
    List.fold_left(
      (m, s) =>
        List.fold_left((m, (k, v)) => Id.Map.add(k, v, m), m, s.s_overlay),
      Id.Map.empty,
      fresh,
    );
  switch (prev) {
  | None => (sums, map_union(union_maps(items), overlays), None)
  | Some((_, map)) =>
    let (map, claimed) = remerge(map, changes);
    /* a replaced overlay's keys fall back to their item's own entries;
       a dropped item's leave, unless an item that moved them in claimed
       them */
    let owners =
      List.fold_left(
        (m, (it, _)) => Id.Map.add(it.d_id, it, m),
        Id.Map.empty,
        up,
      );
    let (map, old_keys) =
      List.fold_left(
        (acc, s) =>
          List.fold_left(
            ((map, ks), (k, _)) =>
              if (Id.Map.mem(k, overlays)) {
                (map, [k, ...ks]);
              } else {
                let map =
                  switch (Id.Map.find_opt(s.s_item.d_id, owners)) {
                  | Some(it) =>
                    switch (Id.Map.find_opt(k, it.d_map)) {
                    | Some(v) => Id.Map.add(k, v, map)
                    | None => Id.Map.remove(k, map)
                    }
                  | None =>
                    Id.Map.mem(k, claimed) ? map : Id.Map.remove(k, map)
                  };
                (map, [k, ...ks]);
              },
            acc,
            s.s_overlay,
          ),
        (map, []),
        replaced,
      );
    (
      sums,
      map_union(map, overlays),
      Some(
        keys(overlays, List.rev_append(old_keys, change_keys(changes))),
      ),
    );
  };
};

/* [map] at [ks] from [wrap]: an item's own entries over its chain's */
let rewrap = (map: Statics.Map.t, wrap: Statics.Map.t, ks: list(Id.t)) =>
  List.fold_left(
    (m, k) =>
      switch (Id.Map.find_opt(k, wrap)) {
      | Some(v) => Id.Map.add(k, v, m)
      | None => Id.Map.remove(k, m)
      },
    map,
    ks,
  );

/* a nested item's map, its chain's under its own entries: updated where
   either changed when it was computed from its previous version, which
   [touched] (the chain's keys) says; also the keys it changed at */
let nest_map =
    (
      ~prev: option(nest),
      ~touched: option(list(Id.t)),
      chain_map: Statics.Map.t,
      wrap: Statics.Map.t,
    )
    : (Statics.Map.t, option(list(Id.t))) =>
  switch (prev, touched) {
  | (Some(n), Some(ts)) =>
    let ks = keys(n.n_wrap, keys(wrap, []));
    (rewrap(chain_map, wrap, ks), Some(List.rev_append(ks, ts)));
  | _ => (map_union(chain_map, wrap), None)
  };

/* a module's members as a chain, reusing [prev]'s wrappers and own
   exports for members it holds unchanged, plus the exports tail, which
   mentions every export (any export delta re-analyzes it) and takes its
   ids from [rep] and the export names. returns the nodes, the members'
   lowerings and the exports (ModuleHelpers.value_exports) */
let lower_members =
    (~prev: option(lowering), rep: Id.t, members: list(Mod.t)) => {
  let by_id =
    switch (prev) {
    | Some(l) => l.by_id
    | None => Id.Map.empty
    };
  let lowered =
    List.map(
      (m: Mod.t) =>
        switch (Id.Map.find_opt(Mod.rep_id(m), by_id)) {
        | Some(l) when l.l_src === m => l
        | _ => {
            l_src: m,
            l_node: lower_item(m),
            l_exports: ModuleHelpers.value_exports([m]),
            l_names: ModuleHelpers.item_bound_names(m),
          }
        },
      members,
    );
  /* each member's own exports, less the names a later member binds */
  let (_, exports) =
    List.fold_left(
      ((later, acc), l) =>
        (
          List.fold_left((s, n) => SSet.add(n, s), later, l.l_names),
          List.filter(
            (e: ModuleHelpers.value_export) => !SSet.mem(e.name, later),
            l.l_exports,
          )
          @ acc,
        ),
      (SSet.empty, []),
      List.rev(lowered),
    );
  let names = List.map((e: ModuleHelpers.value_export) => e.name, exports);
  let tail =
    switch (prev) {
    | Some({tail: Some((r, ns, node)), _})
        when Id.equal(r, rep) && List.equal(String.equal, ns, names) => node
    | _ => ModuleHelpers.labeled_tuple_exp(~stable=rep, exports)
    };
  (List.map(l => l.l_node, lowered) @ [tail], lowered, exports);
};

/* the lowering the next calc reuses: each member with the node its item
   holds (a clean item keeps its old node, so its head stays ===) */
let settle =
    (
      rep: Id.t,
      lowered: list(lowered),
      exports: list(ModuleHelpers.value_export),
      items: list(item),
    )
    : lowering => {
  let names = List.map((e: ModuleHelpers.value_export) => e.name, exports);
  let rec go = (ls, its, acc) =>
    switch (ls, its) {
    | ([l, ...ls], [it, ...its]) =>
      go(
        ls,
        its,
        Id.Map.add(
          Mod.rep_id(l.l_src),
          {
            ...l,
            l_node: it.d_node,
          },
          acc,
        ),
      )
    | ([], [tail]) => {
        by_id: acc,
        tail: Some((rep, names, tail.d_node)),
      }
    | _ => {
        by_id: acc,
        tail: None,
      }
    };
  go(lowered, items, Id.Map.empty);
};

/* ModuleHelpers.collect_type_exports reads the type and module members
   only, in order: unchanged while they and the ctx are */
let type_exports = (~prev, ctx_in: Ctx.t, members: list(Mod.t)) => {
  let typed =
    List.filter(
      (m: Mod.t) =>
        switch (m.term) {
        | ModType(_)
        | ModuleMod(_) => true
        | _ => false
        },
      members,
    );
  switch (prev) {
  | Some((ms, ctx, exports))
      when ctx === ctx_in && List.equal((===), ms, typed) => (
      ms,
      ctx,
      exports,
    )
  | _ => (typed, ctx_in, ModuleHelpers.collect_type_exports(ctx_in, typed))
  };
};

/* the module's type (ModuleHelpers.module_actual_type) by field: a field
   is reused while its export reads the same pattern info, and an equal
   one keeps the old type, so the surrogate keyed on it is reused too */
let actual_type =
    (
      ~prev: option(actual),
      ~locals: list(string),
      exports: list(ModuleHelpers.value_export),
      map: Statics.Map.t,
    )
    : actual => {
  let old =
    switch (prev) {
    | Some(a) when List.equal(String.equal, a.a_locals, locals) =>
      List.fold_left(
        (m, f) => Id.Map.add(f.f_pat, f, m),
        Id.Map.empty,
        a.a_fields,
      )
    | _ => Id.Map.empty
    };
  let fields =
    List.map(
      (e: ModuleHelpers.value_export) => {
        let f_pat = Pat.rep_id(e.pat);
        let f_info = Id.Map.find_opt(f_pat, map);
        let old = Id.Map.find_opt(f_pat, old);
        switch (old, f_info) {
        | (Some(f), Some(i)) when Option.equal((===), f.f_info, Some(i)) => f
        | _ =>
          let ty =
            switch (
              ModuleHelpers.module_actual_type(~local_names=locals, [e], map).
                term
            ) {
            | Prod([f]) => f
            | _ => failwith("module_actual_type: not a field")
            };
          {
            f_pat,
            f_info,
            f_ty:
              switch (old) {
              | Some(f) when Equality.syntactic.typ(f.f_ty, ty) => f.f_ty
              | _ => ty
              },
          };
        };
      },
      exports,
    );
  switch (prev) {
  | Some(a)
      when List.equal((f: field, g) => f.f_ty === g.f_ty, a.a_fields, fields) => {
      ...a,
      a_locals: locals,
      a_fields: fields,
    }
  | _ => {
      a_locals: locals,
      a_fields: fields,
      a_ty: Prod(List.map(f => f.f_ty, fields)) |> Typ.temp,
    }
  };
};

/* the module item's own analysis on a surrogate def (hole : ty), which
   skips two effects replicated by the caller: the M.T type-export alias
   and the def's co_ctx/probe view of its members. reused while its
   inputs are the same */
let surrogate =
    (
      ~prev: option(sur),
      ~settings: CoreSettings.t,
      ~probe_ids,
      ~ctx_in: Ctx.t,
      ~bind_pat: Pat.t,
      ~def: Exp.t,
      ty: Typ.t,
      node: Exp.t,
    )
    : sur => {
  let same_binding = (a: Exp.t, b: Exp.t) =>
    a === b
    || List.equal(Id.equal, IdTagged.ids(a), IdTagged.ids(b))
    && (
      switch (a.term, b.term) {
      | (ModuleExp(m1, _, _), ModuleExp(m2, _, _)) =>
        m1 === m2 || compare(m1, m2) == 0
      | (Let(p1, _, _), Let(p2, _, _)) => p1 === p2 || compare(p1, p2) == 0
      | _ => false
      }
    );
  switch (prev) {
  | Some(u)
      when
        u.u_ty === ty
        && u.u_ctx === ctx_in
        && (u.u_settings === settings || u.u_settings == settings)
        && Id.equal(u.u_def, Exp.rep_id(def))
        && same_binding(u.u_node, node)
        && Id.Map.equal((_, _) => true, u.u_probes, probe_ids) => u
  | _ =>
    let sur_hole = Exp.fresh(EmptyHole);
    let sur_def =
      IdTagged.fast_copy(Exp.rep_id(def), Exp.fresh(Asc(sur_hole, ty)));
    let body_hole = Exp.fresh(EmptyHole);
    let hollow_term: Exp.term =
      switch (node.term) {
      | ModuleExp(mp, _, _) => ModuleExp(mp, sur_def, body_hole)
      | _ => Let(bind_pat, sur_def, body_hole)
      };
    let (map, elab) =
      Statics.mk_unmemoized(
        ~probe_ids,
        settings,
        ctx_in,
        {
          ...node,
          term: hollow_term,
        },
      );
    /* surrogate scaffolding ids (inner hole, synthesized annotation),
       minus the def's rep id, whose entry stands in for the module node */
    let sur_ids = {
      let acc = ref([]);
      let grab = (cont, x) => {
        acc := IdTagged.ids(x) @ acc^;
        cont(x);
      };
      ignore(
        Exp.map_term(
          ~f_exp=grab,
          ~f_typ=(cont, x) => grab(cont, x),
          sur_def,
        ),
      );
      List.filter(id => id != Exp.rep_id(def), acc^);
    };
    let ctx_out =
      switch (Statics.Map.lookup_exp(Exp.rep_id(body_hole), map)) {
      | Some(info) => info.ctx
      | None => ctx_in
      };
    let map =
      List.fold_left((m, id) => Id.Map.remove(id, m), map, sur_ids)
      |> Id.Map.remove(Exp.rep_id(body_hole));
    {
      u_settings: settings,
      u_ctx: ctx_in,
      u_node: node,
      u_def: Exp.rep_id(def),
      u_ty: ty,
      u_probes: probe_ids,
      u_map: map,
      u_elab: elab,
      u_ctx_out: ctx_out,
      u_hole: Exp.rep_id(body_hole),
      u_errors: Statics.Map.error_ids(map),
      u_warnings: Statics.Map.warning_ids(map),
    };
  };
};

/* compute one item's statics in isolation: body swapped for a hole. a
   nested item computed from its previous version also returns the keys
   its map changed at */
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
        : (item, option(list(Id.t))) =>
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
  | None =>
    switch (modexp_chain(node)) {
    | Some((pat, def)) =>
      calc_modexp_item(
        ~settings,
        ~probe_ids,
        ~probe_dirty,
        ~prev,
        ~dirty_vars,
        ~dirty_tnames,
        ~ctx_in,
        ~pat,
        ~def,
        node,
      )
    | None => (calc_plain_item(~settings, ~probe_ids, ~ctx_in, node), None)
    }
  }

and calc_plain_item =
    (~settings, ~probe_ids=Id.Map.empty, ~ctx_in: Ctx.t, node: Exp.t): item => {
  incr(last_analyzed);
  let hole = Exp.fresh(EmptyHole);
  let (is_tail, hollow_term) =
    switch (split_body(node)) {
    | Some((_, rebuild)) => (false, rebuild(hole))
    | None => (true, node.term) /* trailing expression: type as-is */
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
    d_nest: None,
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

/* members (+ exports tail) run as a nested memoized chain, and the
   module's own binding on a surrogate. computed from its previous
   version, the chain's map, sums and the module's memos update where
   members changed */
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
    : (item, option(list(Id.t))) => {
  incr(last_analyzed);
  let (prev_members, prev_nest, prev_map) =
    switch (prev) {
    | Some(q) when q.d_id == Exp.rep_id(node) => (
        q.d_members,
        q.d_nest,
        q.d_map,
      )
    | _ => ([], None, Id.Map.empty)
    };
  let memo = Option.bind(prev_nest, n => n.n_memo);
  let rep = Exp.rep_id(def);
  let (nodes, lowered, exports) =
    lower_members(~prev=Option.map(m => m.m_lowering, memo), rep, members);
  let (items_m, changes) =
    calc_chain(
      ~settings,
      ~probe_ids,
      ~probe_dirty,
      ~keep_ctx=false,
      ~prevs=prev_members,
      ~dirty_vars,
      ~dirty_tnames,
      ~ctx_in,
      nodes,
    );
  let (sums, chain_map, touched) =
    step_chain(
      ~probe_ids,
      ~srcs=List.map(Option.some, members),
      ~prev=Option.map(n => (n.n_sums, prev_map), prev_nest),
      items_m,
      changes,
    );
  let top = top_sum(sums);
  let types =
    type_exports(~prev=Option.map(m => m.m_types, memo), ctx_in, members);
  let (_, _, type_exports) = types;
  let actual =
    actual_type(
      ~prev=Option.map(m => m.m_actual, memo),
      ~locals=List.map(fst, type_exports),
      exports,
      chain_map,
    );
  let sur =
    surrogate(
      ~prev=Option.map(m => m.m_sur, memo),
      ~settings,
      ~probe_ids,
      ~ctx_in,
      ~bind_pat,
      ~def,
      actual.a_ty,
      node,
    );
  /* the M.T type-export alias the surrogate skips */
  let ctx_out =
    switch (
      ModuleHelpers.single_bound_var(bind_pat),
      ModuleHelpers.type_exports_alias_type(type_exports),
    ) {
    | (Some(name), Some(exports_ty)) =>
      Ctx.extend_alias(sur.u_ctx_out, name, Pat.rep_id(bind_pat), exports_ty)
    | _ => sur.u_ctx_out
    };
  /* member elabs grafted, finished like monolithic Module statics */
  let module_value =
    Option.map(
      ModuleHelpers.module_elab(~module_exp_id=rep),
      Option.bind(top, s => s.s_elab),
    );
  /* the members' co_ctx/witnesses, which the surrogate def lacks */
  let with_members = (raw: Info.exp): Info.exp =>
    switch (top) {
    | Some(s) => {
        ...raw,
        co_ctx: co_union(raw.co_ctx, s.s_co),
        probe_targets: SubexpProbeTargets.union(raw.probe_targets, s.s_wit),
      }
    | None => raw
    };
  /* the literal's info must look monolithic too: the evaluator reuses
     the module value recorded at this id while its elab is unchanged */
  let wrap =
    switch (module_value, Statics.Map.lookup_exp(rep, sur.u_map)) {
    | (Some(v), Some(raw)) =>
      let info =
        Info.InfoExp({
          ...with_members(raw),
          /* the surrogate's are an ascription's */
          user_term: def,
          cls: Cls.Exp(Exp.cls_of_term(def.term)),
          elab_term: v,
        });
      List.fold_left(
        (m, id) => Id.Map.add(id, info, m),
        sur.u_map,
        IdTagged.ids(def),
      );
    | _ => sur.u_map
    };
  /* the item root likewise (from this fresh map, so the patch stays
     idempotent) */
  let wrap =
    switch (Statics.Map.lookup_exp(Exp.rep_id(node), wrap)) {
    | Some(raw) =>
      Id.Map.add(Exp.rep_id(node), Info.InfoExp(with_members(raw)), wrap)
    | None => wrap
    };
  let (d_map, touched) =
    nest_map(~prev=prev_nest, ~touched, chain_map, wrap);
  let (errors, warnings, unused, free, tfree) =
    switch (top) {
    | Some(s) => (s.s_errors, s.s_warnings, s.s_unused, s.s_free, s.s_tfree)
    | None => ([], [], [], [], [])
    };
  (
    {
      d_id: Exp.rep_id(node),
      d_node: node,
      d_ctx_in: ctx_in,
      d_map,
      d_error_ids: errors @ sur.u_errors,
      /* members' unused bindings (a shadowed one), as the top chain's */
      d_warning_ids: warnings @ unused @ sur.u_warnings,
      d_exports: Ctx.added_bindings(ctx_out, ctx_in).entries,
      /* the members' frees minus module-internal bindings */
      d_free: free,
      d_tfree: tfree,
      d_ctx_out: ctx_out,
      d_elab:
        switch (module_value) {
        | Some(v) =>
          ModuleHelpers.moduleexp_elab(~def_elab_direct=v, sur.u_elab)
        | None => sur.u_elab /* shape gap: keep the surrogate's */
        },
      d_hole: Some(sur.u_hole),
      d_members: items_m,
      d_nest:
        Some({
          n_sums: sums,
          n_wrap: wrap,
          n_memo:
            Some({
              m_lowering: settle(rep, lowered, exports, items_m),
              m_types: types,
              m_actual: actual,
              m_sur: sur,
            }),
        }),
    },
    touched,
  );
}

/* a ModExp member whose expression is a chain (modexp_chain): its links
   (+ the trailing exp) run as a nested memoized chain from the member's
   ctx, as the top level's do, and the wrapper `let _ = e in •` on a hole
   standing in for e (so `_`, which nothing binds or shows, types as a
   hole). the member still exports nothing, and its map, elab and frees
   compose to the whole member's, so the module sees no change */
and calc_modexp_item =
    (
      ~settings,
      ~probe_ids,
      ~probe_dirty: item => bool,
      ~prev: option(item),
      ~dirty_vars: list(string),
      ~dirty_tnames: list(string),
      ~ctx_in: Ctx.t,
      ~pat: Pat.t,
      ~def: Exp.t,
      node: Exp.t,
    )
    : (item, option(list(Id.t))) => {
  let (prev_links, prev_nest, prev_map) =
    switch (prev) {
    | Some(q) when q.d_id == Exp.rep_id(node) => (
        q.d_members,
        q.d_nest,
        q.d_map,
      )
    | _ => ([], None, Id.Map.empty)
    };
  let (links, changes) =
    calc_chain(
      ~settings,
      ~probe_ids,
      ~probe_dirty,
      ~keep_ctx=false,
      ~prevs=prev_links,
      ~dirty_vars,
      ~dirty_tnames,
      ~ctx_in,
      chain(def),
    );
  let (sums, chain_map, touched) =
    step_chain(
      ~probe_ids,
      ~srcs=[],
      ~prev=Option.map(n => (n.n_sums, prev_map), prev_nest),
      links,
      changes,
    );
  let top = top_sum(sums);
  let sur_hole = Exp.fresh(EmptyHole);
  let body_hole = Exp.fresh(EmptyHole);
  let (map_sur, elab_sur) =
    Statics.mk_unmemoized(
      ~probe_ids,
      settings,
      ctx_in,
      {
        ...node,
        term: Let(pat, sur_hole, body_hole),
      }: Exp.t,
    );
  switch (elab_sur.term, Option.bind(top, s => s.s_elab)) {
  | (Let(p_elab, _, body_elab), Some(def_elab)) =>
    incr(last_analyzed);
    let d_elab: Exp.t = {
      ...elab_sur,
      term: Let(p_elab, def_elab, body_elab),
    };
    let ctx_out =
      switch (Statics.Map.lookup_exp(Exp.rep_id(body_hole), map_sur)) {
      | Some(info) => info.ctx
      | None => ctx_in
      };
    let map_sur =
      map_sur
      |> Id.Map.remove(Exp.rep_id(sur_hole))
      |> Id.Map.remove(Exp.rep_id(body_hole));
    let (wit, co, errors, warnings, unused, free, tfree) =
      switch (top) {
      | Some(s) => (
          s.s_wit,
          s.s_co,
          s.s_errors,
          s.s_warnings,
          s.s_unused,
          s.s_free,
          s.s_tfree,
        )
      | None => (SubexpProbeTargets.empty, CoCtx.empty, [], [], [], [], [])
      };
    /* the wrapper's info as over the whole member: e's elab, uses and
       witnesses */
    let wrap =
      switch (Statics.Map.lookup_exp(Exp.rep_id(node), map_sur)) {
      | Some(raw) =>
        Id.Map.add(
          Exp.rep_id(node),
          Info.InfoExp({
            ...raw,
            elab_term: d_elab,
            co_ctx: co_union(raw.co_ctx, co),
            probe_targets: SubexpProbeTargets.union(raw.probe_targets, wit),
          }),
          map_sur,
        )
      | None => map_sur
      };
    let (d_map, touched) =
      nest_map(~prev=prev_nest, ~touched, chain_map, wrap);
    (
      {
        d_id: Exp.rep_id(node),
        d_node: node,
        d_ctx_in: ctx_in,
        d_map,
        d_error_ids: errors @ Statics.Map.error_ids(wrap),
        /* the links' binders are own_unused's call, as the top chain's */
        d_warning_ids: warnings @ unused @ Statics.Map.warning_ids(wrap),
        d_exports: Ctx.added_bindings(ctx_out, ctx_in).entries,
        d_free: free,
        d_tfree: tfree,
        d_ctx_out: ctx_out,
        d_elab,
        d_hole: Some(Exp.rep_id(body_hole)),
        d_members: links,
        d_nest:
          Some({
            n_sums: sums,
            n_wrap: wrap,
            n_memo: None,
          }),
      },
      touched,
    );
  /* an elab shape gap: the whole member, as before */
  | _ => (calc_plain_item(~settings, ~probe_ids, ~ctx_in, node), None)
  };
}

/* an item chain from [ctx_in], aligned with the previous run's [prevs] BY
   ID, so a restructure costs the changed item plus downstream users of
   its exports: a deleted item's names go dirty where it was, and a moved
   one is popped aside there and recomputed whole where it lands. also
   returns, newest first, each dropped or recomputed item's map change.
   [keep_ctx]: a recomputed item whose exports didn't change keeps its
   old ctx_out, so the items below stay reusable as is.
   lazy analysis (world-tree A3) would plug in here: an item no view needs
   and nothing reads (a sealed par member) stands pending, with an empty
   map and its declared exports, where it would be analyzed; forcing it
   is recomputing it as changed. its delta then flows through step_chain
   like any other, so a pending item costs a walk step and a kept sum */
and calc_chain =
    (
      ~settings,
      ~probe_ids,
      ~probe_dirty: item => bool,
      ~keep_ctx: bool,
      ~prevs: list(item),
      ~dirty_vars: list(string),
      ~dirty_tnames: list(string),
      ~ctx_in: Ctx.t,
      nodes: list(Exp.t),
    )
    : (list(item), list((Id.t, delta))) => {
  /* with no previous run nothing can be clean, so the dirty names (sorted
     lists grown per item) go untracked */
  let track = prevs != [];
  let id_set = ids =>
    List.fold_left((s, id) => Id.Set.add(id, s), Id.Set.empty, ids);
  let node_ids = track ? id_set(List.map(Exp.rep_id, nodes)) : Id.Set.empty;
  let prev_ids =
    track ? id_set(List.map((q: item) => q.d_id, prevs)) : Id.Set.empty;
  let moved: ref(Id.Map.t(item)) = ref(Id.Map.empty);
  /* q's exports leave the chain here */
  let vacate = (q: item, dirty_vars, dirty_tnames) =>
    seed_delta(export_delta(q.d_exports, []), dirty_vars, dirty_tnames);
  let rec go = (ps, ns, ctx, dirty_vars, dirty_tnames, acc, changes) =>
    switch (ps, ns) {
    | (ps, []) =>
      /* remaining prev items were deleted */
      let changes =
        List.fold_left(
          (cs, q: item) =>
            Id.Set.mem(q.d_id, node_ids)
              ? cs : [(q.d_id, Whole(q.d_map, None)), ...cs],
          changes,
          ps,
        );
      (List.rev(acc), changes);
    | ([q, ...pt], _) when !Id.Set.mem(q.d_id, node_ids) =>
      let (dirty_vars, dirty_tnames) = vacate(q, dirty_vars, dirty_tnames);
      let changes = [(q.d_id, Whole(q.d_map, None)), ...changes];
      go(pt, ns, ctx, dirty_vars, dirty_tnames, acc, changes);
    | ([q, ...pt], [n, ..._])
        when
          q.d_id != Exp.rep_id(n)
          && Id.Set.mem(Exp.rep_id(n), prev_ids)
          && !Id.Map.mem(Exp.rep_id(n), moved^) =>
      /* the node sits deeper in prev, so q moved later: pop it aside.
         its exports go dirty for the span it crosses; its old map stays
         until the move-in replaces it */
      moved := Id.Map.add(q.d_id, q, moved^);
      let (dirty_vars, dirty_tnames) = vacate(q, dirty_vars, dirty_tnames);
      go(pt, ns, ctx, dirty_vars, dirty_tnames, acc, changes);
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
          ps,
          nt,
          ctx_out,
          shadow_filter(it.d_exports, dirty_vars),
          List.sort_uniq(
            compare,
            ttransit(it.d_exports, incoming_t) @ incoming_t,
          ),
          [it, ...acc],
          changes,
        );
      | _ =>
        let (it, touched) =
          calc_item(
            ~settings,
            ~probe_ids,
            ~probe_dirty,
            /* a moved item's ctx gained or lost the names it crossed,
               which the dirty sets don't hold: nothing nested in it is
               reusable */
            ~prev=?moved_in ? None : prev_it,
            ~dirty_vars,
            ~dirty_tnames,
            ~ctx_in=ctx,
            n,
          );
        let (p_exports, p_map) =
          switch (prev_it) {
          | Some(q) => (q.d_exports, q.d_map)
          | None => ([], Id.Map.empty)
          };
        let changes = [
          (
            it.d_id,
            switch (touched) {
            | Some(ks) => Keys(ks, it.d_map)
            | None => Whole(p_map, Some(it.d_map))
            },
          ),
          ...changes,
        ];
        if (!track) {
          go(
            ps,
            nt,
            it.d_ctx_out,
            dirty_vars,
            dirty_tnames,
            [it, ...acc],
            changes,
          );
        } else {
          let delta = export_delta(p_exports, it.d_exports);
          let (it, ctx_out) =
            switch (prev_it, delta) {
            | (Some(q), Unchanged) when keep_ctx && ctx === q.d_ctx_in => (
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
            moved_in
              ? (
                List.sort_uniq(compare, names_of(it.d_exports) @ dirty_vars),
                List.sort_uniq(
                  compare,
                  List.concat_map(tnames_of_entry, it.d_exports)
                  @ dirty_tnames,
                ),
              )
              : (dirty_vars, dirty_tnames);
          let dirty_tnames =
            List.sort_uniq(
              compare,
              ttransit(it.d_exports, dirty_tnames) @ dirty_tnames,
            );
          go(
            ps,
            nt,
            ctx_out,
            dirty_vars,
            dirty_tnames,
            [it, ...acc],
            changes,
          );
        };
      };
    };
  go(prevs, nodes, ctx_in, dirty_vars, dirty_tnames, [], []);
};

/* the seed ctx must be PHYSICALLY stable across calc calls: reuse
   gating chains on pointer identity in the clean case */
let ctx0: Ctx.t = Builtins.ctx_init(Some(Operators.default_mode));

/* [chain], except a Module root itemizes via the lowering (chain never
   descends into defs, so only a root Module is seen here): its nodes,
   and the members and lowering a Module root has */
let chain_root = (~prev: option(lowering), e: Exp.t) => {
  let s = strip(e);
  switch (s.term) {
  | Module(items) =>
    let (nodes, lowered, exports) =
      lower_members(~prev, Exp.rep_id(s), items);
    (nodes, Some((Exp.rep_id(s), items, lowered, exports)));
  | _ => (chain(e), None)
  };
};

/* INVARIANT down the walk: [ctx] differs from the previous run's only at
   dirty names. a clean item (same head, d_free/d_tfree avoid the dirty
   sets) is reused; its map may keep stale ctx entries for dirty names it
   doesn't use, sound for typing though its Γ display can lag */
let calc =
    (~settings, ~prev: option(t)=?, ~probe_ids=Id.Map.empty, whole: Exp.t): t => {
  last_analyzed := 0;
  summed := 0;
  let (nodes, root) =
    chain_root(~prev=Option.bind(prev, (p: t) => p.lowering), whole);
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
  let (items, changes) =
    calc_chain(
      ~settings,
      ~probe_ids,
      ~probe_dirty,
      ~keep_ctx=true,
      ~prevs=Option.fold(~none=[], ~some=(p: t) => p.items, prev),
      ~dirty_vars=[],
      ~dirty_tnames=[],
      ~ctx_in=ctx0,
      nodes,
    );
  /* a Module root's items read as module items, as monolithically */
  let (sums, merged, _) =
    step_chain(
      ~probe_ids,
      ~srcs=
        switch (root) {
        | Some((_, members, _, _)) => List.map(Option.some, members)
        | None => []
        },
      ~prev=Option.map((p: t) => (p.sums, p.merged), prev),
      items,
      changes,
    );
  {
    items,
    term: whole,
    probe_ids,
    merged,
    elab: Option.bind(top_sum(sums), s => s.s_elab),
    sums,
    lowering:
      Option.map(
        ((rep, _, lowered, exports)) =>
          settle(rep, lowered, exports, items),
        root,
      ),
  };
};

/* the whole-program elaboration; None on an unexpected elab shape */
let whole_elab = (t: t): option(Exp.t) =>
  switch (strip(t.term).term) {
  | Module(_) =>
    /* mod root: the graft is the lowered expansion's elab; finish it
       the way monolithic Module statics does (marks the module value) */
    Option.map(
      ModuleHelpers.module_elab(~module_exp_id=Exp.rep_id(strip(t.term))),
      t.elab,
    )
  | _ => t.elab
  };

/* whole-program views over the per-item results */
let all_error_ids = (t: t): list(Id.t) =>
  switch (t.sums) {
  | [s, ..._] => s.s_errors
  | [] => []
  };

/* an item never flags its own top-level binders (its body hole could use
   them): unused ones are own_unused's call, wrappers included */
let all_warning_ids = (t: t): list(Id.t) =>
  switch (t.sums) {
  | [s, ..._] => s.s_warnings @ s.s_unused
  | [] => []
  };

/* set by the test runner to check each calc_auto result */
let after_calc: ref(option((CoreSettings.t, t) => unit)) = ref(None);

/* the last calc per document, most recent first, so switching documents
   doesn't force a cold pass; keyed by the term's rep id, stable unless
   the first item is replaced */
let slots: ref(list((Id.t, t))) = ref([]);
let slots_cap = 8;

let calc_auto = (~settings, ~probe_ids=Id.Map.empty, whole: Exp.t): t => {
  /* a probe change keeps the slot: calc re-analyzes just the affected items */
  let key = Exp.rep_id(whole);
  let prev = List.assoc_opt(key, slots^);
  let t = calc(~settings, ~prev?, ~probe_ids, whole);
  slots :=
    [
      (key, t),
      ...Util.ListUtil.take(slots_cap - 1, List.remove_assoc(key, slots^)),
    ];
  Option.iter(check => check(settings, t), after_calc^);
  t;
};

/* the last calc of [whole]'s document, if still cached */
let cached = (whole: Exp.t): option(t) =>
  List.assoc_opt(Exp.rep_id(whole), slots^);
