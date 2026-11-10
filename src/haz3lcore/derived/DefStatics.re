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
  d_elab: Exp.t, /* elaboration of the hollow item; a member's, its module of it */
  d_hole: option(Id.t), /* the body hole's id (None: trailing exp, member) */
  /* a member's signature items, each typed outside its module */
  d_sig: list(Sig.t),
  /* the names a member exports, which its module uses by exporting them */
  d_used: list(string),
  /* a nested item chain, memoized per item: a module literal def's
     members, or a let-chain ModExp member's links */
  d_members: list(item),
  /* what the next calc of this item updates rather than redoes */
  d_nest: option(nest),
}
/* an item and the chain below it, summed from the bottom up: the item's
   root info patched over the suffix, and the chain's aggregates from it
   down. a calc keeps the sums below its lowest changed item */
and sum = {
  s_item: item, /* as it was when summed */
  s_src: option(Mod.t), /* a module member's source */
  /* the root's parent: the item above, or what holds the chain */
  s_parent: list(Id.t),
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
  m_stands: stands,
  m_sig: Typ.t,
  m_sur: sur,
}
/* members' chain nodes (stand_in) by rep id, each with its source and
   what its analysis read from the module (member_input) */
and stands = Id.Map.t((Mod.t, Exp.t, member_input))
/* what a member's analysis reads from its module beyond its ctx: the
   names it exports (those no later member rebinds), the signature's
   expectations that can bear on it, and the module's declared types */
and member_input = {
  i_exported: list(Var.t),
  i_expected: list(Sig.member),
  i_declared: list(Var.t),
}
/* the module item's surrogate analysis, with its inputs */
and sur = {
  u_settings: CoreSettings.t,
  u_ctx: Ctx.t,
  u_hollow: Exp.t, /* the item's head on the surrogate */
  u_def: Id.t,
  u_ty: Typ.t,
  u_probes: Id.Map.t(unit),
  u_input: option(member_input), /* a member's */
  u_map: Statics.Map.t, /* scaffolding removed */
  u_elab: Exp.t,
  u_ctx_out: Ctx.t,
  u_hole: Id.t,
  /* the holes standing for the def and what follows, whose uses are none */
  u_scaffold: list(Id.t),
  u_sig: list(Sig.t), /* a member's signature items */
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
  /* the program's elaboration (whole_elab): [elab], a Module root's
     refolded into its value. None on an unexpected elab shape */
  program_elab: option(Exp.t),
  sums: list(sum), /* the top chain's */
  stands: option(stands) /* a Module root's members' */
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

/* a module member as a chain node: a module of it under the member's
   own ids, so the chain keys and compares it as an item. a clean member
   keeps its node (stands), so its head stays === */
let stand_in = (item: Mod.t): Exp.t => {
  annotation: item.annotation,
  term: Module([item]),
};

let member_of = (node: Exp.t): option(Mod.t) =>
  switch (node.term) {
  | Module([item]) when Id.equal(Mod.rep_id(item), Exp.rep_id(node)) =>
    Some(item)
  | _ => None
  };

/* a module elaboration's items */
let items_of = (e: Exp.t): list(Mod.t) =>
  switch (e.term) {
  | Module(items) => items
  | _ => []
  };

/* the module value [node] elaborates to, from its members' summed elab */
let module_elab = (node: Exp.t, top: option(Exp.t)): Exp.t => {
  ...node,
  term: Module(Option.fold(~none=[], ~some=items_of, top)),
};

/* a let chain at an item position is a level of items; one inside a
   definition is not. so a ModExp member whose expression is a chain
   (`{ let a = 1 in a + 1 }`) splits */
let member_chain = (item: Mod.t): option(Exp.t) =>
  switch (item.term) {
  | ModExp(e) when Option.is_some(split_body(e)) => Some(e)
  | _ => None
  };

/* head parts compared structurally, a test counter: a clean item's
   parts are physically the same (MakeTerm's member memo, the stand-in
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
    /* members' stand-ins: the item's binder and definition */
    | (Module([i1]), Module([i2]))
        when List.equal(Id.equal, IdTagged.ids(i1), IdTagged.ids(i2)) =>
      switch (i1.term, i2.term) {
      | (ModLet(p1, d1), ModLet(p2, d2)) => same(p1, p2) && same(d1, d2)
      | (ModuleMod(m1, d1), ModuleMod(m2, d2)) =>
        same(m1, m2) && same(d1, d2)
      | (ModType(t1, y1), ModType(t2, y2)) => same(t1, t2) && same(y1, y2)
      | (t1, t2) => same(t1, t2)
      }
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
  /* the synthesized type too: an inconsistent use's [ty] is what was
     expected, and its own type is what goes stale */
  Id.Map.iter(
    (_, info: Info.t) =>
      switch (info) {
      | InfoExp({ty, elab_syn_ty, _})
      | InfoPat({ty, elab_syn_ty, _}) =>
        add(Typ.free_vars(ty));
        add(Typ.free_vars(elab_syn_ty));
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
   mentions a dirty name are dirty for everything downstream. a dirty
   value counts: `type U = M.T` reads module M's signature */
let ttransit =
    (exports: list(Ctx.entry), ~dirty_vars: list(string), dirty_tnames) => {
  let dirty = dirty_tnames @ dirty_vars;
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
};

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

/* the `f(x)`, `f(x)(y)` and `f(x) : T` wrappers of a function
   definition carry its name's info, unused warning included
   (FunctionSugar.add_binder_infos) */
let fun_wrapper_ids = (node: Exp.t, name_id: Id.t): list(Id.t) => {
  /* the Aps down the spine to the name, outermost first */
  let rec spine = (p: Pat.t): option(list(Id.t)) =>
    switch (p.term) {
    | Ap(fn, _) when Pat.rep_id(fn) == name_id => Some([Pat.rep_id(p)])
    | Ap(fn, _) => Option.map(ids => [Pat.rep_id(p), ...ids], spine(fn))
    | _ => None
    };
  let of_binder = (p: Pat.t) =>
    switch (p.term) {
    | Asc(inner, _) =>
      Option.fold(
        ~none=[],
        ~some=ids => [Pat.rep_id(p), ...ids],
        spine(inner),
      )
    | _ => Option.value(~default=[], spine(p))
    };
  switch (node.term) {
  | Let(p, _, _)
  | Module([{term: ModLet(p, _), _}]) => of_binder(p)
  | _ => []
  };
};

/* a chain export is used iff a later item mentions it before a value
   rebinding ([used]: the free names of the items below, which is that),
   its module exports it, or a hole below could ([holes]: "$hole" in a
   d_free below, real holes only, since synthetic body holes aren't in
   any def) */
let own_unused = (it: item, ~used: list(string), ~holes: bool): list(Id.t) =>
  holes
    ? []
    : List.concat_map(
        fun
        | Ctx.VarEntry({name, id, _}) =>
          List.exists(String.equal(name), used)
          || List.exists(String.equal(name), it.d_used)
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
   root's value bindings), or cached runs replay stale. and every root
   reads as in a whole-program analysis: its term continues into the item
   below, and its ancestors are its [parent] (the item above, or what
   holds the chain), whose own ancestors go on from there
   (Info.ancestors) */
let summarize =
    (
      ~probe_ids,
      ~src: option(Mod.t),
      ~parent: list(Id.t),
      it: item,
      below: option(sum),
    )
    : sum => {
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
     unchanged whatever happens downstream. a member's, its module of the
     members from it down */
  let s_elab =
    switch (src, it.d_hole, elab) {
    | (Some(_), _, _) =>
      Some({
        ...it.d_elab,
        term: (
          Module(
            items_of(it.d_elab) @ Option.fold(~none=[], ~some=items_of, elab),
          ): Exp.term
        ),
      })
    | (None, Some(h), Some(below)) =>
      MakeTerm.Incr.graft_spine(h, below, it.d_elab)
    | (None, None, _) => Some(it.d_elab)
    | (None, Some(_), None) => None
    };
  /* the RAW info, from the item's own map: the chain's holds the patch.
     a member's is its own, its parent the module: it is not patched */
  let raw =
    switch (src) {
    | None => Statics.Map.lookup_exp(it.d_id, it.d_map)
    | Some(_) => None
    };
  let raw_co =
    switch (Id.Map.find_opt(it.d_id, it.d_map)) {
    | Some(InfoExp({co_ctx, _}))
    | Some(InfoMod({co_ctx, _})) => co_ctx
    | _ => CoCtx.empty
    };
  let s_co =
    co_union(
      raw_co,
      bound == []
        ? co
        : CoCtx.filter_names(
            name => !List.exists(String.equal(name), bound),
            co,
          ),
    );
  /* the item's node over the item below, as the program holds it */
  let node =
    switch (split_body(it.d_node), below) {
    | (Some((_, rebuild)), Some(s)) => {
        ...it.d_node,
        term: rebuild(s.s_item.d_node),
      }
    | _ => it.d_node
    };
  /* at each of the root's ids (a case has several); a tail item's suffix
     is itself */
  let overlay =
    switch (raw) {
    | Some(raw) =>
      let raw = {
        ...raw,
        user_term: node,
        ancestors: parent,
      };
      let info =
        Info.InfoExp(
          switch (it.d_hole) {
          | Some(_) => {
              ...raw,
              elab_term: Option.value(~default=raw.elab_term, s_elab),
              probe_targets: SubexpProbeTargets.union(raw.probe_targets, wit),
              co_ctx: s_co,
            }
          | None => raw
          },
        );
      List.fold_left(
        (m, id) => Id.Map.add(id, info, m),
        Id.Map.empty,
        IdTagged.ids(it.d_node),
      );
    | None => Id.Map.empty
    };
  {
    s_item: it,
    s_src: src,
    s_parent: parent,
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
let sums_still =
    (s: sum, it: item, src: option(Mod.t), parent: list(Id.t)): bool => {
  let q = s.s_item;
  List.equal(Id.equal, s.s_parent, parent)
  && (
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
   afresh. sums below the lowest changed item are kept, unless the item
   above one changed (its root's parent did), and the rest are summed
   again, their overlays rewritten. [container]: the first item's parent.
   also returns the keys where the map may have changed (None: built
   afresh) */
let step_chain =
    (
      ~probe_ids,
      ~container: list(Id.t),
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
  /* bottom first, each with its source and its parent */
  let rec pair = (items, srcs, parent, acc) =>
    switch (items, srcs) {
    | ([], _) => acc
    | ([it, ...items], [src, ...srcs]) =>
      pair(items, srcs, [it.d_id], [(it, src, parent), ...acc])
    | ([it, ...items], []) =>
      pair(items, [], [it.d_id], [(it, None, parent), ...acc])
    };
  let rec keep = (up, old, kept) =>
    switch (up, old) {
    | ([(it, src, parent), ...up'], [s, ...old'])
        when !Id.Set.mem(it.d_id, changed) && sums_still(s, it, src, parent) =>
      keep(up', old', [s, ...kept])
    | _ => (up, old, kept)
    };
  let (up, replaced, kept) =
    keep(
      pair(items, srcs, container, []),
      switch (prev) {
      | Some((sums, _)) => List.rev(sums)
      | None => []
      },
      [],
    );
  let (fresh, _) =
    List.fold_left(
      ((acc, below), (it, src, parent)) => {
        let s = summarize(~probe_ids, ~src, ~parent, it, below);
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
        (m, (it, _, _)) => Id.Map.add(it.d_id, it, m),
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

/* [map] at [ks] from [wrap]: an item's own entries over its chain's. A
   key [wrap] lost stays where the chain's walk set it ([walked]): an id
   that moved into a member, as a module's `;` does when completion
   closes a module before it no longer */
let rewrap =
    (
      ~walked: Id.t => bool,
      map: Statics.Map.t,
      wrap: Statics.Map.t,
      ks: list(Id.t),
    ) =>
  List.fold_left(
    (m, k) =>
      switch (Id.Map.find_opt(k, wrap)) {
      | Some(v) => Id.Map.add(k, v, m)
      | None => walked(k) ? m : Id.Map.remove(k, m)
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
    /* read only when a key leaves [wrap] */
    let walked =
      lazy(List.fold_left((s, k) => Id.Set.add(k, s), Id.Set.empty, ts));
    (
      rewrap(
        ~walked=k => Id.Set.mem(k, Lazy.force(walked)),
        chain_map,
        wrap,
        ks,
      ),
      Some(List.rev_append(ks, ts)),
    );
  | _ => (map_union(chain_map, wrap), None)
  };

/* what each of a module's members reads from it, aligned with [members]
   ([expected]: the signature's expectations, read in the module's ctx as
   ModuleHelpers.expected_members does). a binding member is expected to
   be what the signature says of the names it exports; a type member, what
   it says of its name, unless a later member declares that again */
let member_inputs =
    (~expected: list(Sig.member), members: list(Mod.t)): list(member_input) => {
  let declared = ModuleHelpers.declared_type_names(members);
  /* the expectations by name (a signature declares each member once) */
  let (vals, types) =
    List.fold_right(
      (m: Sig.member, (vs, ts)) =>
        switch (m) {
        | Val(x, _) => (
            SMap.update(
              x,
              l => Some([m, ...Option.value(~default=[], l)]),
              vs,
            ),
            ts,
          )
        | TypeManifest(t, _) => (
            vs,
            SMap.update(
              t,
              l => Some([m, ...Option.value(~default=[], l)]),
              ts,
            ),
          )
        },
      expected,
      (SMap.empty, SMap.empty),
    );
  let find = (map, x) => Option.value(~default=[], SMap.find_opt(x, map));
  /* right to left: a member exports the names no later member rebinds */
  let (_, _, inputs) =
    List.fold_right(
      (item: Mod.t, (rebound, later_types, acc)) => {
        let names = Mod.bound_names(item);
        let exported = List.filter(x => !SSet.mem(x, rebound), names);
        let (i_expected, later_types) =
          switch (item.term) {
          | ModType({term: Var(n), _}, _) => (
              SSet.mem(n, later_types) ? [] : find(types, n),
              SSet.add(n, later_types),
            )
          | ModLet(_)
          | ModuleMod(_) => (
              List.concat_map(find(vals), exported),
              later_types,
            )
          | _ => ([], later_types)
          };
        (
          List.fold_left((s, n) => SSet.add(n, s), rebound, names),
          later_types,
          [
            {
              i_exported: exported,
              i_expected,
              i_declared: declared,
            },
            ...acc,
          ],
        );
      },
      members,
      (SSet.empty, SSet.empty, []),
    );
  inputs;
};

let input_equal = (a: member_input, b: member_input): bool => {
  let names = List.equal(String.equal);
  let member = (x: Sig.member, y: Sig.member) =>
    switch (x, y) {
    | (Val(n1, t1), Val(n2, t2))
    | (TypeManifest(n1, t1), TypeManifest(n2, t2)) =>
      String.equal(n1, n2) && Typ.fast_equal(t1, t2)
    | _ => false
    };
  names(a.i_exported, b.i_exported)
  && names(a.i_declared, b.i_declared)
  && List.equal(member, a.i_expected, b.i_expected);
};

/* the module a member chain belongs to: its literal, whose ids are the
   members' parent, and each member's input by rep id */
type member_level = {
  mc_module: Exp.t,
  mc_inputs: Id.Map.t(member_input),
};

/* a module's members as chain nodes, reusing [prev]'s for members it
   holds unchanged; also the members whose input differs from [prev]'s,
   which re-analyze though their nodes are clean */
let member_level =
    (
      ~prev: option(stands),
      ~expected: list(Sig.member),
      module_: Exp.t,
      members: list(Mod.t),
    ) => {
  let (nodes, stands, changed) =
    List.fold_left2(
      ((nodes, stands, changed), m: Mod.t, input) => {
        let id = Mod.rep_id(m);
        let prior = Option.bind(prev, Id.Map.find_opt(id));
        let node =
          switch (prior) {
          | Some((src, node, _)) when src === m => node
          | _ => stand_in(m)
          };
        let changed =
          switch (prior) {
          | Some((_, _, i)) when !input_equal(i, input) =>
            Id.Set.add(id, changed)
          | _ => changed
          };
        (
          [node, ...nodes],
          Id.Map.add(id, (m, node, input), stands),
          changed,
        );
      },
      ([], Id.Map.empty, Id.Set.empty),
      members,
      member_inputs(~expected, members),
    );
  (
    List.rev(nodes),
    stands,
    changed,
    {
      mc_module: module_,
      mc_inputs: Id.Map.map(((_, _, i)) => i, stands),
    },
  );
};

/* the module's signature from its members' (Statics' `Module` arm), the
   previous object while it is syntactically equal, so the surrogate keyed
   on it is reused too */
let sig_type = (~prev: option(Typ.t), sig_items: list(Sig.t)): Typ.t => {
  let ty = Typ.temp(Sig(Closed(ClosedSig.of_items(sig_items))));
  switch (prev) {
  | Some(p) when Equality.syntactic.typ(p, ty) => p
  | _ => ty
  };
};

/* whether a member's type member is defined otherwise than the
   signature's, which the whole check marks on the module's type too */
let type_member_unmet = (it: item): bool =>
  List.exists(
    id =>
      switch (Id.Map.find_opt(id, it.d_map)) {
      | Some(InfoTyp({marks, _})) =>
        List.exists(
          fun
          | Mark.ModuleTypeMemberMismatch(_) => true
          | _ => false,
          marks,
        )
      | _ => false
      },
    it.d_error_ids,
  );

/* the members checked against the signature on their own item, whose
   types the sealed module takes from it (ModuleHelpers.sealed) */
let checked_names =
    (
      ~inputs: Id.Map.t(member_input),
      items: list(item),
      members: list(Mod.t),
    )
    : list(Var.t) =>
  List.concat(
    List.map2(
      (it: item, m: Mod.t) => {
        let binder =
          switch (m.term) {
          | ModLet(p, def) => Some(fst(FunctionSugar.binding(p, def)))
          | ModuleMod(mp, _) => Some(MPat.to_pat(mp))
          | _ => None
          };
        switch (binder, Id.Map.find_opt(Mod.rep_id(m), inputs)) {
        | (Some(p), Some(input)) =>
          ModuleHelpers.expected_pat_type(
            ~ctx=it.d_ctx_in,
            input.i_expected,
            p,
          )
          |> Option.fold(~none=[], ~some=(e: ModuleHelpers.pat_expectation) =>
               e.names
             )
        | _ => []
        };
      },
      items,
      members,
    ),
  );

/* whether [items] define just the signature's members by name: none
   missing and none extra. a module's member names are syntactic */
let names_meet = (~ana_items: list(Sig.t), items: list(Mod.t)): bool => {
  let (values, types) =
    List.fold_left(
      ((vs, ts), item: Mod.t) =>
        switch (item.term) {
        | ModType({term: Var(n), _}, _) => (vs, SSet.add(n, ts))
        | _ => (
            List.fold_left(
              (vs, x) => SSet.add(x, vs),
              vs,
              Mod.bound_names(item),
            ),
            ts,
          )
        },
      (SSet.empty, SSet.empty),
      items,
    );
  let want = Sig.members(ana_items);
  SSet.equal(values, SSet.of_list(Sig.value_names(want)))
  && SSet.equal(types, SSet.of_list(Sig.type_names(want)));
};

/* where a module literal is bound: a top-level item binding [pat], or a
   member of the module [level] */
type binding_site =
  | Top(Pat.t)
  | Member(member_level, Mod.t);

/* the binding [node] makes, compared across versions: its binder, and a
   member's input from its module */
let same_binding = (a: Exp.t, b: Exp.t): bool => {
  let same = (x, y) => x === y || compare(x, y) == 0;
  a === b
  || List.equal(Id.equal, IdTagged.ids(a), IdTagged.ids(b))
  && (
    switch (a.term, b.term) {
    | (ModuleExp(m1, _, _), ModuleExp(m2, _, _)) => same(m1, m2)
    | (Let(p1, _, _), Let(p2, _, _)) => same(p1, p2)
    | (
        Module([{term: ModLet(p1, _), _}]),
        Module([{term: ModLet(p2, _), _}]),
      ) =>
      same(p1, p2)
    | (
        Module([{term: ModuleMod(m1, _), _}]),
        Module([{term: ModuleMod(m2, _), _}]),
      ) =>
      same(m1, m2)
    | _ => false
    }
  );
};

/* [co] without the uses [ids] made: scaffolding standing for terms */
let without_uses = (ids: list(Id.t), co: CoCtx.t): CoCtx.t =>
  List.filter_map(
    ((n, es)) =>
      switch (
        List.filter(
          (e: CoCtx.entry) => !List.exists(Id.equal(e.id), ids),
          es,
        )
      ) {
      | [] => None
      | es => Some((n, es))
      },
    CoCtx.to_list(co),
  );

/* the module item's own analysis on a surrogate def (hole : ty), which
   skips the def's co_ctx/probe view of its members, replicated by the
   caller. a member's is its module's analysis of it (Statics.mk_segment),
   with a hole for the members after it. reused while its inputs are the
   same */
let surrogate =
    (
      ~prev: option(sur),
      ~settings: CoreSettings.t,
      ~probe_ids,
      ~ctx_in: Ctx.t,
      ~site: binding_site,
      ~def: Exp.t,
      ty: Typ.t,
      node: Exp.t,
    )
    : sur => {
  let input =
    switch (site) {
    | Top(_) => None
    | Member(mc, item) => Id.Map.find_opt(Mod.rep_id(item), mc.mc_inputs)
    };
  switch (prev) {
  | Some(u)
      when
        u.u_ty === ty
        && u.u_ctx === ctx_in
        && (u.u_settings === settings || u.u_settings == settings)
        && Id.equal(u.u_def, Exp.rep_id(def))
        && same_binding(u.u_hollow, node)
        && Option.equal(input_equal, u.u_input, input)
        && Id.Map.equal((_, _) => true, u.u_probes, probe_ids) => u
  | _ =>
    let sur_hole = Exp.fresh(EmptyHole);
    /* the type with ids of its own: a sealed member's type is the
       signature's, whose nodes the program holds */
    let ty_copy =
      Grammar.map_typ_annotation(
        (tag: IdTagged.IdTag.t) =>
          {
            ...tag,
            ids: IdTagged.IdTag.fresh().ids,
          },
        ty,
      );
    let sur_def =
      IdTagged.fast_copy(
        Exp.rep_id(def),
        Exp.fresh(Asc(sur_hole, ty_copy)),
      );
    let body_hole = Exp.fresh(EmptyHole);
    let (hollow, map, elab, scaffold) =
      switch (site) {
      | Top(bind_pat) =>
        let hollow_term: Exp.term =
          switch (node.term) {
          | ModuleExp(mp, _, _) => ModuleExp(mp, sur_def, body_hole)
          | _ => Let(bind_pat, sur_def, body_hole)
          };
        let hollow = {
          ...node,
          term: hollow_term,
        };
        let (map, elab) =
          Statics.mk_unmemoized(~probe_ids, settings, ctx_in, hollow);
        (hollow, map, elab, []);
      | Member(mc, item) =>
        let term: Mod.term =
          switch (item.term) {
          | ModuleMod(mp, _) => ModuleMod(mp, sur_def)
          | ModLet(p, _) => ModLet(p, sur_def)
          | t => t
          };
        let module_ = {
          ...mc.mc_module,
          term: (
            Module([
              {
                ...item,
                term,
              },
            ]): Exp.term
          ),
        };
        let input =
          Option.value(
            ~default={
              i_exported: Mod.bound_names(item),
              i_expected: [],
              i_declared: [],
            },
            input,
          );
        let (map, elab) =
          Statics.mk_segment(
            ~probe_ids,
            settings,
            ctx_in,
            ~segment={
              seg_declared: input.i_declared,
              seg_expected: input.i_expected,
              seg_exported: input.i_exported,
              seg_tail: body_hole,
            },
            module_,
          );
        (node, map, elab, IdTagged.ids(mc.mc_module));
      };
    /* surrogate scaffolding ids (inner hole, the signature annotation,
       which may carry member annotations' ids), minus the def's rep id,
       whose entry stands in for the module node */
    let sur_ids = {
      let acc = ref([]);
      let grab = (cont, x) => {
        acc := IdTagged.ids(x) @ acc^;
        cont(x);
      };
      ignore(
        Exp.map_term(
          ~f_exp=grab,
          ~f_pat=grab,
          ~f_typ=grab,
          ~f_tpat=grab,
          ~f_sig=grab,
          ~f_mpat=grab,
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
    let u_sig =
      switch (site) {
      | Top(_) => []
      | Member(mc, _) =>
        switch (Statics.Map.lookup_exp(Exp.rep_id(mc.mc_module), map)) {
        | Some(info) =>
          switch (Typ.term_of(info.elab_syn_ty)) {
          | Sig(Closed(cs)) => ClosedSig.items(cs)
          | _ => []
          }
        | None => []
        }
      };
    let map =
      List.fold_left(
        (m, id) => Id.Map.remove(id, m),
        map,
        [Exp.rep_id(body_hole), ...scaffold @ sur_ids],
      );
    {
      u_settings: settings,
      u_ctx: ctx_in,
      u_hollow: hollow,
      u_def: Exp.rep_id(def),
      u_ty: ty,
      u_probes: probe_ids,
      u_input: input,
      u_map: map,
      u_elab: elab,
      u_ctx_out: ctx_out,
      u_hole: Exp.rep_id(body_hole),
      u_scaffold: [Exp.rep_id(body_hole), Exp.rep_id(sur_hole)],
      u_sig,
      u_errors: Statics.Map.error_ids(map),
      u_warnings: Statics.Map.warning_ids(map),
    };
  };
};

/* an alias that would capture a name the types before it use is renamed
   with its scope (Typ.unshadow): the scope is the items below, renamed
   here as the whole program's analysis renames its body */
let rename_tvars = (renames: list((string, string)), node: Exp.t): Exp.t =>
  List.fold_right(
    ((x, x'), n) => Exp.rename_tvar(x, x', n),
    renames,
    node,
  );

/* [renames] past [node], whose exports say whether its alias was renamed */
let renames_after =
    (
      renames: list((string, string)),
      node: Exp.t,
      exports: list(Ctx.entry),
    ) =>
  switch (node.term) {
  | TyAlias({term: Var(x), _}, _, _) =>
    let rest = List.filter(((y, _)) => !String.equal(x, y), renames);
    switch (
      List.find_map(
        fun
        | Ctx.TVarEntry({name, _}) when !String.equal(name, x) => Some(name)
        | _ => None,
        exports,
      )
    ) {
    | Some(x') => [(x, x'), ...rest]
    | None => rest
    };
  | _ => renames
  };

/* compute one item's statics in isolation: body swapped for a hole. a
   nested item computed from its previous version also returns the keys
   its map changed at. a module's member ([member]: its level) is
   analyzed as its module analyzes it */
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
          ~member: option(member_level)=?,
          ~ctx_in: Ctx.t,
          node: Exp.t,
        )
        : (item, option(list(Id.t))) =>
  switch (member, Option.bind(member, _ => member_of(node))) {
  | (Some(mc), Some(item)) =>
    switch (member_chain(item), member_module(~ctx=ctx_in, ~mc, item)) {
    | (_, Some((def, members, ana_items))) =>
      calc_module_item(
        ~settings,
        ~probe_ids,
        ~probe_dirty,
        ~prev,
        ~dirty_vars,
        ~dirty_tnames,
        ~ctx_in,
        ~site=Member(mc, item),
        ~def,
        ~members,
        ~ana_items,
        node,
      )
    | (Some(e), None) =>
      calc_member_chain(
        ~settings,
        ~probe_ids,
        ~probe_dirty,
        ~prev,
        ~dirty_vars,
        ~dirty_tnames,
        ~ctx_in,
        ~mc,
        item,
        e,
        node,
      )
    | (None, None) => (
        calc_member(~settings, ~probe_ids, ~ctx_in, ~mc, item, node),
        None,
      )
    }
  | _ =>
    switch (module_literal_members(~ctx=ctx_in, node)) {
    | Some((bind_pat, def, members, ana_items)) =>
      calc_module_item(
        ~settings,
        ~probe_ids,
        ~probe_dirty,
        ~prev,
        ~dirty_vars,
        ~dirty_tnames,
        ~ctx_in,
        ~site=Top(bind_pat),
        ~def,
        ~members,
        ~ana_items,
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
     ModuleExp), and a silent [] would make items falsely clean. a
     function definition's is its desugared Fun's, which its parameters
     bind */
  let free = {
    let src =
      switch (node.term) {
      | Let(p, d, _) =>
        switch (Pat.function_sugar(p)) {
        | Some((_, [args, ..._], _)) => Some(FunctionSugar.fun_id(args))
        | _ => Some(Exp.rep_id(d))
        }
      | ModuleExp(_, d, _) => Some(Exp.rep_id(d))
      | Seq(e, _)
      | Filter(Filter({pat: e, _}), _) => Some(Exp.rep_id(e))
      | TyAlias(_)
      | Filter(Residue(_), _) => None
      | _ => Some(Exp.rep_id(node))
      };
    switch (src) {
    | None => []
    | Some(d) =>
      switch (Statics.Map.lookup_exp(d, map)) {
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
          ModuleHelpers.collect_pat_type_refs(ctx_in, MPat.to_pat(mp))
        | TyAlias(_, ty, _) =>
          ModuleHelpers.collect_module_refs_in_typ(
            ctx_in,
            Typ.rep_id(ty),
            ty,
          )
        | _ => CoCtx.empty
        },
      );
  let exports = Ctx.added_bindings(ctx_out, ctx_in).entries;
  /* a recursive definition's info drops its own name (the FixF patch),
     but the Let's co_ctx keeps it, and monolithic statics reads that one
     from above: a copy's recursive call uses the binding above it */
  let free =
    switch (node.term, Statics.Map.lookup_exp(Exp.rep_id(node), map)) {
    | (Let(_), Some(root)) =>
      let bound = var_names(exports);
      free
      @ List.filter(
          n => List.mem(n, bound) && !List.mem(n, free),
          CoCtx.names(root.co_ctx),
        );
    | _ => free
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
    d_exports: exports,
    d_free: free,
    d_tfree: tfree_of_item(hollow, map),
    d_ctx_out: ctx_out,
    d_elab: elab,
    d_hole: is_tail ? None : Some(Exp.rep_id(hole)),
    d_sig: [],
    d_used: [],
    d_members: [],
    d_nest: None,
  };
}

/* a module's member, checked as its whole module checks it, with a hole
   for the members after it (Statics.mk_segment): what it exports, its
   signature items, its elaboration (its module of it), and its uses,
   the stand-in's for later members left out */
and calc_member =
    (
      ~settings,
      ~probe_ids,
      ~ctx_in: Ctx.t,
      ~mc: member_level,
      item: Mod.t,
      node: Exp.t,
    )
    : item => {
  incr(last_analyzed);
  let rep = Mod.rep_id(item);
  let input =
    switch (Id.Map.find_opt(rep, mc.mc_inputs)) {
    | Some(i) => i
    | None => {
        i_exported: Mod.bound_names(item),
        i_expected: [],
        i_declared: [],
      }
    };
  let tail = Exp.fresh(EmptyHole);
  let tail_id = Exp.rep_id(tail);
  let module_ = {
    ...mc.mc_module,
    term: (Module([item]): Exp.term),
  };
  let (map, elab) =
    Statics.mk_segment(
      ~probe_ids,
      settings,
      ctx_in,
      ~segment={
        seg_declared: input.i_declared,
        seg_expected: input.i_expected,
        seg_exported: input.i_exported,
        seg_tail: tail,
      },
      module_,
    );
  let ctx_out =
    switch (Statics.Map.lookup_exp(tail_id, map)) {
    | Some(info) => info.ctx
    | None => ctx_in
    };
  let d_sig =
    switch (Statics.Map.lookup_exp(Exp.rep_id(module_), map)) {
    | Some(info) =>
      switch (Typ.term_of(info.elab_syn_ty)) {
      | Sig(Closed(cs)) => ClosedSig.items(cs)
      | _ => []
      }
    | None => []
    };
  /* the tail and the module are scaffolding: the module's info is its
     item's */
  let map =
    List.fold_left(
      (m, id) => Id.Map.remove(id, m),
      map,
      [tail_id, ...IdTagged.ids(mc.mc_module)],
    );
  let untail = (co: CoCtx.t): CoCtx.t =>
    List.filter_map(
      ((n, es)) =>
        switch (
          List.filter((e: CoCtx.entry) => !Id.equal(e.id, tail_id), es)
        ) {
        | [] => None
        | es => Some((n, es))
        },
      CoCtx.to_list(co),
    );
  let map =
    switch (Id.Map.find_opt(rep, map)) {
    | Some(InfoMod(i)) =>
      Statics.Map.add_info(
        IdTagged.ids(item),
        InfoMod({
          ...i,
          co_ctx: untail(i.co_ctx),
          scope_co_ctx: untail(i.scope_co_ctx),
        }),
        map,
      )
    | _ => map
    };
  /* a bare expression item's info is its expression's */
  let free =
    switch (Id.Map.find_opt(rep, map)) {
    | Some(InfoMod({co_ctx, _}))
    | Some(InfoExp({co_ctx, _})) => CoCtx.names(co_ctx)
    | _ => ["*"]
    };
  {
    d_id: rep,
    d_node: node,
    d_ctx_in: ctx_in,
    d_map: map,
    d_error_ids: Statics.Map.error_ids(map),
    d_warning_ids: Statics.Map.warning_ids(map),
    d_exports: Ctx.added_bindings(ctx_out, ctx_in).entries,
    d_free: free,
    d_tfree: tfree_of_item(node, map),
    d_ctx_out: ctx_out,
    d_elab: elab,
    d_hole: None,
    d_sig,
    d_used: input.i_exported,
    d_members: [],
    d_nest: None,
  };
}

/* member granularity for a module literal bound to a name, plain or
   ascribed with a signature whose members it defines (and against whose
   items the members are checked, as the whole check does). one missing
   or extra is reported on the module, as the whole check does: such an
   item is analyzed whole */
and module_literal_members =
    (~ctx: Ctx.t, node: Exp.t)
    : option((Pat.t, Exp.t, list(Mod.t), option(list(Sig.t)))) => {
  let literal = (p, def: Exp.t) =>
    Option.map(
      ((members, ana_items)) => (p, def, members, ana_items),
      bound_literal(~ctx, ~expected=None, p, def),
    );
  switch (node.term) {
  | Let(p, def, _) => literal(p, def)
  | ModuleExp(mp, def, _) => literal(MPat.to_pat(mp), def)
  | _ => None
  };
}

/* a module literal [def] bound to [p], a name plain or ascribed (or, a
   member's, given a type by its module's signature, [expected]): its
   members and the signature they are checked against */
and bound_literal =
    (~ctx: Ctx.t, ~expected: option(Typ.t), p: Pat.t, def: Exp.t)
    : option((list(Mod.t), option(list(Sig.t)))) => {
  let simple = (p: Pat.t): bool =>
    switch (p.term) {
    | Var(_)
    | Wild => true
    | _ => false
    };
  let sig_of = ty =>
    switch (Typ.as_sig(ctx, ty)) {
    | Some(items) => Some(Some(ClosedSig.items(items)))
    | None => None
    | exception _ => None
    };
  let ana =
    switch (p.term, expected) {
    | (_, None) when simple(p) => Some(None)
    | (_, Some(ty)) when simple(p) => sig_of(ty)
    | (Asc(inner, ty), _) when simple(inner) => sig_of(Typ.close(ctx, ty))
    | _ => None
    };
  switch (def.term, ana) {
  | (Module(members), Some(None)) => Some((members, None))
  | (Module(members), Some(Some(sig_items)))
      when names_meet(~ana_items=sig_items, members) =>
    Some((members, Some(sig_items)))
  | _ => None
  };
}

/* a member binding a module literal, as module_literal_members */
and member_module =
    (~ctx: Ctx.t, ~mc: member_level, item: Mod.t)
    : option((Exp.t, list(Mod.t), option(list(Sig.t)))) => {
  let literal = (p: Pat.t, def: Exp.t) => {
    let expected =
      Option.bind(Id.Map.find_opt(Mod.rep_id(item), mc.mc_inputs), input =>
        ModuleHelpers.expected_pat_type(~ctx, input.i_expected, p)
      )
      |> Option.map((e: ModuleHelpers.pat_expectation) => e.ty);
    Option.map(
      ((members, ana_items)) => (def, members, ana_items),
      bound_literal(~ctx, ~expected, p, def),
    );
  };
  switch (item.term) {
  | ModLet(p, def) when Option.is_none(Pat.function_sugar(p)) =>
    literal(p, def)
  | ModuleMod(mp, def) => literal(MPat.to_pat(mp), def)
  | _ => None
  };
}

/* members run as a nested memoized chain, and the module's own binding
   on a surrogate. computed from its previous version, the chain's map,
   sums and the module's memos update where members changed */
and calc_module_item =
    (
      ~settings,
      ~probe_ids,
      ~probe_dirty: item => bool,
      ~prev: option(item),
      ~dirty_vars: list(string),
      ~dirty_tnames: list(string),
      ~ctx_in: Ctx.t,
      ~site: binding_site,
      ~def: Exp.t,
      ~members: list(Mod.t),
      ~ana_items: option(list(Sig.t)),
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
  let declared = ModuleHelpers.declared_type_names(members);
  let expected =
    Option.fold(
      ~none=[],
      ~some=ModuleHelpers.expected_members(~ctx=ctx_in, ~declared),
      ana_items,
    );
  let (nodes, stands, changed, mc) =
    member_level(
      ~prev=Option.map(m => m.m_stands, memo),
      ~expected,
      def,
      members,
    );
  let (items_m, changes) =
    calc_chain(
      ~settings,
      ~probe_ids,
      ~probe_dirty=q => probe_dirty(q) || Id.Set.mem(q.d_id, changed),
      ~member=mc,
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
      ~container=[rep],
      ~srcs=List.map(Option.some, members),
      ~prev=Option.map(n => (n.n_sums, prev_map), prev_nest),
      items_m,
      changes,
    );
  let top = top_sum(sums);
  if (Option.is_some(ana_items) && List.exists(type_member_unmet, items_m)) {
    (
      switch (site) {
      | Top(_) => calc_plain_item(~settings, ~probe_ids, ~ctx_in, node)
      | Member(mc, item) =>
        calc_member(~settings, ~probe_ids, ~ctx_in, ~mc, item, node)
      },
      None,
    );
  } else {
    let have =
      ModuleHelpers.exports(List.concat_map(it => it.d_sig, items_m));
    let sig_items =
      switch (ana_items) {
      | None => have
      | Some(_) =>
        ModuleHelpers.sealed(
          ~ctx=ctx_in,
          ~expected,
          ~checked=checked_names(~inputs=mc.mc_inputs, items_m, members),
          ~mismatched_types=[],
          have,
        )
      };
    let sig_ty = sig_type(~prev=Option.map(m => m.m_sig, memo), sig_items);
    let sur =
      surrogate(
        ~prev=Option.map(m => m.m_sur, memo),
        ~settings,
        ~probe_ids,
        ~ctx_in,
        ~site,
        ~def,
        sig_ty,
        node,
      );
    let ctx_out = sur.u_ctx_out;
    /* member elabs in order, as Statics' `Module` arm builds the value */
    let module_value = module_elab(def, Option.bind(top, s => s.s_elab));
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
    /* the literal's info must look whole too: the evaluator reuses the
       module value recorded at this id while its elab is unchanged */
    let wrap =
      switch (Statics.Map.lookup_exp(rep, sur.u_map)) {
      | Some(raw) =>
        let info =
          Info.InfoExp({
            ...with_members(raw),
            /* the surrogate's are an ascription's */
            user_term: def,
            cls: Cls.Exp(Exp.cls_of_term(def.term)),
            elab_term: module_value,
          });
        List.fold_left(
          (m, id) => Id.Map.add(id, info, m),
          sur.u_map,
          IdTagged.ids(def),
        );
      | None => sur.u_map
      };
    /* the item root likewise (from this fresh map, so the patch stays
       idempotent); a member's without the surrogate's holes */
    let wrap =
      switch (Id.Map.find_opt(Exp.rep_id(node), wrap)) {
      | Some(InfoExp(raw)) =>
        Id.Map.add(Exp.rep_id(node), Info.InfoExp(with_members(raw)), wrap)
      | Some(InfoMod(raw)) =>
        let below = Option.fold(~none=CoCtx.empty, ~some=s => s.s_co, top);
        Statics.Map.add_info(
          IdTagged.ids(node),
          InfoMod({
            ...raw,
            /* the surrogate's item holds the hole for its definition */
            user_term:
              switch (site) {
              | Member(_, item) => item
              | Top(_) => raw.user_term
              },
            co_ctx:
              co_union(without_uses(sur.u_scaffold, raw.co_ctx), below),
            scope_co_ctx: without_uses(sur.u_scaffold, raw.scope_co_ctx),
          }),
          wrap,
        );
      | _ => wrap
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
        /* the binding of the module's value, as the `let` or the member
           elaborates it */
        d_elab:
          switch (sur.u_elab.term) {
          | Let(p, _, body) => {
              ...sur.u_elab,
              term: Let(p, module_value, body),
            }
          | Module([{term: ModLet(p, _), _} as b]) => {
              ...sur.u_elab,
              term:
                Module([
                  {
                    ...b,
                    term: ModLet(p, module_value),
                  },
                ]),
            }
          | _ => sur.u_elab /* shape gap: keep the surrogate's */
          },
        d_hole:
          switch (site) {
          | Top(_) => Some(sur.u_hole)
          | Member(_) => None
          },
        d_sig: sur.u_sig,
        d_used:
          switch (sur.u_input) {
          | Some(i) => i.i_exported
          | None => []
          },
        d_members: items_m,
        d_nest:
          Some({
            n_sums: sums,
            n_wrap: wrap,
            n_memo:
              Some({
                m_stands: stands,
                m_sig: sig_ty,
                m_sur: sur,
              }),
          }),
      },
      touched,
    );
  };
}

/* a ModExp member whose expression is a chain (member_chain): its links
   (+ the trailing exp) run as a nested memoized chain from the member's
   ctx, as the top level's do, the module their parent, as the whole
   check has it. the member exports nothing, and its map, elab and frees
   compose to the whole member's, so the module sees no change */
and calc_member_chain =
    (
      ~settings,
      ~probe_ids,
      ~probe_dirty: item => bool,
      ~prev: option(item),
      ~dirty_vars: list(string),
      ~dirty_tnames: list(string),
      ~ctx_in: Ctx.t,
      ~mc: member_level,
      item: Mod.t,
      e: Exp.t,
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
      chain(e),
    );
  let (sums, chain_map, touched) =
    step_chain(
      ~probe_ids,
      ~container=[Exp.rep_id(mc.mc_module)],
      ~srcs=[],
      ~prev=Option.map(n => (n.n_sums, prev_map), prev_nest),
      links,
      changes,
    );
  let top = top_sum(sums);
  switch (top) {
  | Some({s_elab: Some(e_elab), _} as s) =>
    incr(last_analyzed);
    let (d_map, touched) =
      nest_map(~prev=prev_nest, ~touched, chain_map, Id.Map.empty);
    (
      {
        d_id: Exp.rep_id(node),
        d_node: node,
        d_ctx_in: ctx_in,
        d_map,
        d_error_ids: s.s_errors,
        /* the links' binders are own_unused's call, as the top chain's */
        d_warning_ids: s.s_warnings @ s.s_unused,
        d_exports: [],
        d_free: s.s_free,
        d_tfree: s.s_tfree,
        d_ctx_out: ctx_in,
        d_elab: {
          ...mc.mc_module,
          term: (
            Module([
              {
                ...item,
                term: ModExp(e_elab),
              },
            ]): Exp.term
          ),
        },
        d_hole: None,
        d_sig: [],
        d_used: [],
        d_members: links,
        d_nest:
          Some({
            n_sums: sums,
            n_wrap: Id.Map.empty,
            n_memo: None,
          }),
      },
      touched,
    );
  /* an elab shape gap: the whole member, as before */
  | _ => (calc_member(~settings, ~probe_ids, ~ctx_in, ~mc, item, node), None)
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
      ~member: option(member_level)=?,
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
  let rec go = (ps, ns, ctx, renames, dirty_vars, dirty_tnames, acc, changes) =>
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
      go(pt, ns, ctx, renames, dirty_vars, dirty_tnames, acc, changes);
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
      go(pt, ns, ctx, renames, dirty_vars, dirty_tnames, acc, changes);
    | (ps, [n, ...nt]) =>
      let n = rename_tvars(renames, n);
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
        let incoming = shadow_filter(it.d_exports, dirty_vars);
        let incoming_t = tshadow(it.d_exports, dirty_tnames);
        go(
          ps,
          nt,
          ctx_out,
          renames_after(renames, n, it.d_exports),
          incoming,
          List.sort_uniq(
            compare,
            ttransit(it.d_exports, ~dirty_vars=incoming, incoming_t)
            @ incoming_t,
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
            ~member?,
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
            renames_after(renames, n, it.d_exports),
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
              ttransit(it.d_exports, ~dirty_vars, dirty_tnames) @ dirty_tnames,
            );
          go(
            ps,
            nt,
            ctx_out,
            renames_after(renames, n, it.d_exports),
            dirty_vars,
            dirty_tnames,
            [it, ...acc],
            changes,
          );
        };
      };
    };
  go(prevs, nodes, ctx_in, [], dirty_vars, dirty_tnames, [], []);
};

/* the seed ctx must be PHYSICALLY stable across calc calls: reuse
   gating chains on pointer identity in the clean case */
let ctx0: Ctx.t = Builtins.ctx_init(Some(Operators.default_mode));

/* [chain], except a Module root's members are its items (chain never
   descends into defs, so only a root Module is seen here): its nodes, and
   the members and their level a Module root has */
let chain_root = (~prev: option(t), e: Exp.t) => {
  let s = strip(e);
  switch (s.term) {
  | Module(members) =>
    let (nodes, stands, changed, mc) =
      member_level(
        ~prev=Option.bind(prev, (p: t) => p.stands),
        ~expected=[],
        s,
        members,
      );
    /* the members are the root's children: a new root id (its first item
       replaced) is a new parent for each */
    let changed =
      switch (prev) {
      | Some(p) when !Id.equal(Exp.rep_id(strip(p.term)), Exp.rep_id(s)) =>
        List.fold_left(
          (c, m) => Id.Set.add(Mod.rep_id(m), c),
          changed,
          members,
        )
      | _ => changed
      };
    (nodes, Some((members, stands, changed, mc)));
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
  let (nodes, root) = chain_root(~prev, whole);
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
  let probe_dirty =
    switch (root) {
    | Some((_, _, changed, _)) when !Id.Set.is_empty(changed) => (
        p => probe_dirty(p) || Id.Set.mem(p.d_id, changed)
      )
    | _ => probe_dirty
    };
  let (items, changes) =
    calc_chain(
      ~settings,
      ~probe_ids,
      ~probe_dirty,
      ~member=?Option.map(((_, _, _, mc)) => mc, root),
      ~keep_ctx=true,
      ~prevs=Option.fold(~none=[], ~some=(p: t) => p.items, prev),
      ~dirty_vars=[],
      ~dirty_tnames=[],
      ~ctx_in=ctx0,
      nodes,
    );
  /* a Module root's members list it as their parent */
  let (sums, merged, _) =
    step_chain(
      ~probe_ids,
      ~container=[],
      ~srcs=
        switch (root) {
        | Some((members, _, _, _)) => List.map(Option.some, members)
        | None => []
        },
      ~prev=Option.map((p: t) => (p.sums, p.merged), prev),
      items,
      changes,
    );
  let elab = Option.bind(top_sum(sums), s => s.s_elab);
  let program_elab =
    switch (strip(whole).term) {
    | Module(_) =>
      /* mod root: its value of the members' elabs */
      Some(module_elab(strip(whole), elab))
    | _ => elab
    };
  /* a Module root's own info, as a whole analysis gives it: the module
     of its members, read in the empty program */
  let merged =
    switch (root, program_elab) {
    | (Some(_), Some(value)) =>
      let s = strip(whole);
      let ty =
        Typ.temp(
          Sig(
            Closed(
              ClosedSig.of_items(
                ModuleHelpers.exports(
                  List.concat_map((it: item) => it.d_sig, items),
                ),
              ),
            ),
          ),
        );
      let (co_ctx, wit) =
        switch (top_sum(sums)) {
        | Some(t) => (t.s_co, t.s_wit)
        | None => (CoCtx.empty, SubexpProbeTargets.empty)
        };
      let info: Info.exp = {
        user_term: s,
        elab_term: value,
        ancestors: [],
        ctx: ctx0,
        ana: Typ.temp(Unknown(SynSwitch)),
        elab_syn_ty: ty,
        marks: [],
        co_ctx,
        probe_targets:
          SubexpProbeTargets.add_self(
            ~is_probed=Id.Map.mem(Exp.rep_id(s), probe_ids),
            Exp.rep_id(s),
            wit,
          ),
        cls: Cls.Exp(Exp.cls_of_term(s.term)),
        message: Message.Exp(Default),
        warnings: [],
        ty,
        label_inference: None,
        inferred_label: None,
        label_sort: false,
        dot_labels: [],
      };
      Statics.Map.add_info(IdTagged.ids(s), InfoExp(info), merged);
    | _ => merged
    };
  {
    items,
    term: whole,
    probe_ids,
    merged,
    elab,
    program_elab,
    sums,
    stands: Option.map(((_, stands, _, _)) => stands, root),
  };
};

/* the whole-program elaboration, the same object on every call; None
   on an unexpected elab shape */
let whole_elab = (t: t): option(Exp.t) => t.program_elab;

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
