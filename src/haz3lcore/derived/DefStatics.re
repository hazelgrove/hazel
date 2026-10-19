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
  d_warning_ids: list(Id.t), /* never its top-level binders (unused_binders) */
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
  merged: Statics.Map.t, /* union of the items' maps, kept incrementally */
  /* each item's elab grafted into its predecessor's body hole: recursion
     depth is per item, so it fits the browser stack where a monolithic
     elaboration doesn't */
  elab: option(Exp.t),
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

/* stable ids for synthesized wrappers: head_equal gates cleanliness on
   their ids */
let derived_id = (tag: string, rep: Id.t): Id.t =>
  Id.mk_str(tag ++ Id.to_string(rep));

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
  /* a use records the livelit it applies as `^name` in its co_ctx (the
     LivelitName case of Statics), so a changed livelit must dirty that
     name: as `name` it dirtied nothing, and a use kept the expansion of
     the definition it was last analyzed with */
  | LivelitEntry({name, _}) => "^" ++ name
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

/* item statics hollows the continuation, so a spine root's info misses
   later items. the evaluator's reuse gating reads a root's elab_term,
   probe_targets and co_ctx, so each non-tail root gets them extended
   over the suffix (its co_ctx minus the root's value bindings), or cached
   runs replay stale. also returns the top root's witnesses, co_ctx and
   elab_term (the whole chain's elaboration; None on an unexpected elab
   shape) */
/* An item's root, patched, is a function of the item and what is below it.
   When those are the very objects of the last calc, the last answer is
   reused, and so is the entry merged already holds. Kept by the item's
   id, with its elaboration, which is new whenever the item is analyzed
   again; emptied past 4096 items. */
type spine_memo = {
  m_elab: Exp.t,
  m_id: Id.t,
  m_map: Statics.Map.t,
  m_hole: option(Id.t),
  m_exports: list(Ctx.entry),
  m_probe_ids: Id.Map.t(unit),
  m_below_wit: SubexpProbeTargets.t,
  m_below_co: CoCtx.t,
  m_below_elab: option(Exp.t),
  out_info: option(Info.t),
  out_wit: SubexpProbeTargets.t,
  out_co: CoCtx.t,
  out_elab: option(Exp.t),
};
let spine_memo: Hashtbl.t(Id.t, spine_memo) = Hashtbl.create(256);
/* roots reused, and patched; and, for tests, a check that recomputes
   each reused root and counts those that differ */
let spine_reused = ref(0);
let spine_patched = ref(0);
let spine_check = ref(false);
let spine_mismatches = ref(0);

let fix_spine_infos =
    (~probe_ids: Id.Map.t(unit), items: list(item), merged: Statics.Map.t)
    : (Statics.Map.t, SubexpProbeTargets.t, CoCtx.t, option(Exp.t)) => {
  let same_probes = (a, b) => a === b || Id.Map.equal((_, _) => true, a, b);
  let probes_in = (it: item): SubexpProbeTargets.t =>
    Id.Map.fold(
      (pid, (), acc) =>
        Id.Map.mem(pid, it.d_map)
          ? SubexpProbeTargets.add_self(~is_probed=true, pid, acc) : acc,
      probe_ids,
      SubexpProbeTargets.empty,
    );
  let patch_root = (it: item, (m, below_wit, below_co, below_elab)) => {
    let bound = var_names(it.d_exports);
    let below_co_scoped =
      CoCtx.filter_names(name => !List.mem(name, bound), below_co);
    /* the root's elab_term is the whole suffix: a hollow one reads
       as unchanged whatever happens downstream */
    let suffix_elab =
      switch (it.d_hole, below_elab) {
      | (Some(h), Some(below)) =>
        MakeTerm.Incr.graft_spine(h, below, it.d_elab)
      | (None, _) => Some(it.d_elab)
      | (Some(_), None) => None
      };
    /* read the RAW info from d_map, never [m]: [m] may hold the last
       calc's patched entry, and re-patching doubles the co_ctx
       use-lists every calc */
    let raw = Statics.Map.lookup_exp(it.d_id, it.d_map);
    let co_ctx =
      CoCtx.union([
        Option.fold(~none=CoCtx.empty, ~some=(r: Info.exp) => r.co_ctx, raw),
        below_co_scoped,
      ]);
    /* a tail item is already accurate, and a module form has no
       InfoExp at the root: those thread upward without patching */
    let m =
      switch (it.d_hole, raw) {
      | (Some(_), Some(raw)) =>
        Id.Map.add(
          it.d_id,
          Info.InfoExp({
            ...raw,
            elab_term: Option.value(~default=raw.elab_term, suffix_elab),
            probe_targets:
              SubexpProbeTargets.union(raw.probe_targets, below_wit),
            co_ctx,
          }),
          m,
        )
      | _ => m
      };
    (
      m,
      SubexpProbeTargets.union(probes_in(it), below_wit),
      co_ctx,
      suffix_elab,
    );
  };
  List.fold_right(
    (it: item, (m, below_wit, below_co, below_elab)) =>
      switch (Hashtbl.find_opt(spine_memo, it.d_id)) {
      | Some(c)
          when
            c.m_elab === it.d_elab
            && c.m_map === it.d_map
            && c.m_id == it.d_id
            && c.m_hole == it.d_hole
            && c.m_exports === it.d_exports
            && same_probes(c.m_probe_ids, probe_ids)
            && c.m_below_wit === below_wit
            && c.m_below_co === below_co
            && c.m_below_elab === below_elab =>
        incr(spine_reused);
        if (spine_check^) {
          let (m', w, co_, e) =
            patch_root(it, (m, below_wit, below_co, below_elab));
          let fresh = m' === m ? None : Id.Map.find_opt(it.d_id, m');
          /* by printed form, ids and all (no ValueHash on this branch) */
          let same = (f, a, b) => Sexplib.Sexp.equal(f(a), f(b));
          let exp = Exp.sexp_of_t
          and co = CoCtx.sexp_of_t
          and wit = SubexpProbeTargets.sexp_of_t;
          let same_info =
            switch (fresh, c.out_info) {
            | (None, None) => true
            | (Some(Info.InfoExp(a)), Some(Info.InfoExp(b))) =>
              same(exp, a.elab_term, b.elab_term)
              && same(co, a.co_ctx, b.co_ctx)
              && same(wit, a.probe_targets, b.probe_targets)
            | _ => false
            };
          if (!(
                same_info
                && same(wit, w, c.out_wit)
                && same(co, co_, c.out_co)
                && same(Sexplib.Std.sexp_of_option(exp), e, c.out_elab)
              )) {
            incr(spine_mismatches);
          };
        };
        let m =
          switch (c.out_info, Id.Map.find_opt(it.d_id, m)) {
          | (Some(info), Some(held)) when held === info => m
          | (Some(info), _) => Id.Map.add(it.d_id, info, m)
          | (None, _) => m
          };
        (m, c.out_wit, c.out_co, c.out_elab);
      | _ =>
        incr(spine_patched);
        let (m', out_wit, out_co, out_elab) as out =
          patch_root(it, (m, below_wit, below_co, below_elab));
        if (Hashtbl.length(spine_memo) > 4096) {
          Hashtbl.reset(spine_memo);
        };
        Hashtbl.replace(
          spine_memo,
          it.d_id,
          {
            m_elab: it.d_elab,
            m_id: it.d_id,
            m_map: it.d_map,
            m_hole: it.d_hole,
            m_exports: it.d_exports,
            m_probe_ids: probe_ids,
            m_below_wit: below_wit,
            m_below_co: below_co,
            m_below_elab: below_elab,
            out_info: m' === m ? None : Id.Map.find_opt(it.d_id, m'),
            out_wit,
            out_co,
            out_elab,
          },
        );
        out;
      },
    items,
    (merged, SubexpProbeTargets.empty, CoCtx.empty, None),
  );
};

let map_union = (a: Statics.Map.t, b: Statics.Map.t): Statics.Map.t =>
  Id.Map.union((_, _x, y) => Some(y), a, b);

let union_maps = (items: list(item)): Statics.Map.t =>
  List.fold_left((m, it) => map_union(m, it.d_map), Id.Map.empty, items);

/* [merged] under a walk's [changes] (newest first), replayed in walk
   order: an old map's keys leave unless a recomputed item claimed them
   earlier in the walk (a member moved out of a module ahead of it keeps
   its ids), and new maps join */
let remerge = (merged: Statics.Map.t, changes): Statics.Map.t =>
  List.fold_left(
    ((merged, claimed), (old, new_)) => {
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
    },
    (merged, Id.Map.empty),
    List.rev(changes),
  )
  |> fst;

/* a top-level export is used iff a later item mentions it before a
   value rebinding, or a hole below could ("$hole" in d_free: real holes
   only, since synthetic body holes aren't in any def) */
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
      List.concat_map(
        e =>
          switch (e) {
          | Ctx.VarEntry({name, id, _}) =>
            used_below(name, rest)
            || hole_below(rest)
            || String.length(name) > 0
            && name.[0] == '_'
              ? [] : [id, ...fun_wrapper_ids(it.d_node, id)]
          | _ => []
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
        : item => {
  /* Module literals are analyzed whole on this branch: per-member
     granularity was built on dev's module statics, and Modules II's
     (signatures, sealing, abstract members) is not replicated member by
     member yet. The member chain's inputs go unused. */

  ignore((probe_dirty, prev, dirty_vars, dirty_tnames));
  calc_plain_item(~settings, ~probe_ids, ~ctx_in, node);
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
  };
}

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
    : (list(item), list((Statics.Map.t, option(Statics.Map.t)))) => {
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
            Id.Set.mem(q.d_id, node_ids) ? cs : [(q.d_map, None), ...cs],
          changes,
          ps,
        );
      (List.rev(acc), changes);
    | ([q, ...pt], _) when !Id.Set.mem(q.d_id, node_ids) =>
      let (dirty_vars, dirty_tnames) = vacate(q, dirty_vars, dirty_tnames);
      let changes = [(q.d_map, None), ...changes];
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
        let (p_exports, p_map) =
          switch (prev_it) {
          | Some(q) => (q.d_exports, q.d_map)
          | None => ([], Id.Map.empty)
          };
        let changes = [(p_map, Some(it.d_map)), ...changes];
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
   descends into defs, so only a root Module is seen here) */
let chain_root = (e: Exp.t): list(Exp.t) =>
  switch (strip(e).term) {
  /* a Module root is one item, analyzed by the monolithic Module statics
     (see calc_item) */
  | Module(_) => [e]
  | _ => chain(e)
  };

/* INVARIANT down the walk: [ctx] differs from the previous run's only at
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
  let merged =
    switch (prev) {
    | Some(p) => remerge(p.merged, changes)
    | None => union_maps(items)
    };
  let (merged, _, _, elab) = fix_spine_infos(~probe_ids, items, merged);
  /* what changed from the last calc's merged, for EvalInfo's projection:
     the keys of the walk's changes, old and new, and each item's root
     (fix_spine_infos). (A Module root is one item here, analyzed whole.) */
  switch (prev) {
  | Some(p) =>
    let keys = (m: Statics.Map.t, acc) =>
      Id.Map.fold((k, _, acc) => [k, ...acc], m, acc);
    let changed =
      List.fold_left(
        (acc, (old, new_)) =>
          keys(old, Option.fold(~none=acc, ~some=n => keys(n, acc), new_)),
        List.map((it: item) => it.d_id, items),
        changes,
      );
    EvalInfo.note_delta(~base=p.merged, ~changed, merged);
  | _ => ()
  };
  {
    items,
    term: whole,
    probe_ids,
    merged,
    elab,
  };
};

/* the whole-program elaboration, each item's elab grafted into its
   predecessor's body hole (fix_spine_infos); None on an unexpected elab
   shape. A Module root is one item, finished by the monolithic Module
   statics */
let whole_elab = (t: t): option(Exp.t) => t.elab;

/* whole-program views over the per-item results */
let all_error_ids = (t: t): list(Id.t) =>
  List.concat_map(it => it.d_error_ids, t.items);

/* an item never flags its own top-level binders (its body hole could use
   them): unused ones are unused_binders' call, wrappers included */
let all_warning_ids = (t: t): list(Id.t) =>
  List.concat_map(it => it.d_warning_ids, t.items) @ unused_binders(t.items);

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
