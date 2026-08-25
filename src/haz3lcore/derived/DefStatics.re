open Language;

/* DefStatics — compositional whole-program statics
   (plans/modular-editors.md §8d): statics computed PER TOP-LEVEL ITEM
   (let / type alias / module / trailing expression) with chained
   ctxs, so an edit recomputes only the dirty set:
     - the edited item always;
     - downstream items, only when an upstream item's EXPORTS changed
       (name/id/type of a binding) AND they mention a changed name
       (their co_ctx); type-side exports (aliases, constructors)
       cascade to all downstream items for now — co_ctx only tracks
       expression variables;
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
  d_ctx_out: Ctx.t,
};

type t = {
  items: list(item),
  term: Exp.t /* the whole term these items were computed from */
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

/* did the exports change, and how? Type-side changes (aliases,
   constructors) can be referenced from type positions that co_ctx
   doesn't track, so they invalidate everything downstream. */
type export_delta =
  | Unchanged
  | VarsChanged(list(string))
  | TypesChanged;

let export_delta =
    (old: list(Ctx.entry), new_: list(Ctx.entry)): export_delta =>
  if (List.length(old) != List.length(new_)) {
    List.exists(
      fun
      | Ctx.TVarEntry(_)
      | ConstructorEntry(_) => true
      | _ => false,
      old @ new_,
    )
      ? TypesChanged
      : VarsChanged(
          List.sort_uniq(compare, List.map(entry_name, old @ new_)),
        );
  } else {
    let rec go = (os, ns, vars, typs) =>
      switch (os, ns) {
      | ([], []) =>
        typs ? TypesChanged : vars == [] ? Unchanged : VarsChanged(vars)
      | ([o, ...os], [n, ...ns]) =>
        entry_equal(o, n)
          ? go(os, ns, vars, typs)
          : (
            switch (o, n) {
            | (Ctx.TVarEntry(_) | ConstructorEntry(_), _)
            | (_, Ctx.TVarEntry(_) | ConstructorEntry(_)) =>
              go(os, ns, vars, true)
            | _ =>
              go(
                os,
                ns,
                List.sort_uniq(
                  compare,
                  [entry_name(o), entry_name(n), ...vars],
                ),
                typs,
              )
            }
          )
      | _ => TypesChanged /* unreachable: same length */
      };
    go(old, new_, [], false);
  };

/* observability: how many items the last calc actually re-analyzed */
let last_analyzed: ref(int) = ref(0);

/* compute one item's statics in isolation: body swapped for a hole */
let calc_item = (~settings, ~ctx_in: Ctx.t, node: Exp.t): item => {
  incr(last_analyzed);
  let hole = Exp.fresh(EmptyHole);
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
  let (map, _) = Statics.mk_unmemoized(settings, ctx_in, hollow);
  let ctx_out =
    switch (Statics.Map.lookup_exp(Exp.rep_id(hole), map)) {
    | Some(info) => info.ctx
    | None => ctx_in /* trailing exp: no hole in the map */
    };
  /* free expression vars: read the DEF's co_ctx (the item node itself
     may not carry an InfoExp — e.g. ModuleExp — and a silent [] here
     would make items falsely clean). Type-side deps aren't in co_ctx;
     they cascade via TypesChanged instead. */
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
      | Some(info) => List.map(fst, info.co_ctx)
      | None =>
        /* refuse to fail silent: treat as depending on everything */
        ["*"]
      }
    };
  };
  {
    d_id: Exp.rep_id(node),
    d_node: node,
    d_ctx_in: ctx_in,
    d_map: map,
    d_error_ids: Statics.Map.error_ids(map),
    d_warning_ids: Statics.Map.warning_ids(map),
    d_exports: Ctx.added_bindings(ctx_out, ctx_in).entries,
    d_free: free,
    d_ctx_out: ctx_out,
  };
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

let shadow_filter = (exports: list(Ctx.entry), dirty: list(string)) =>
  List.filter(v => !List.exists(e => entry_name(e) == v, exports), dirty);

/* "*" is the unknown-free-vars sentinel: depends on anything dirty */
let depends = (free: list(string), dirty: list(string)): bool =>
  dirty != []
  && (List.mem("*", free) || List.exists(v => List.mem(v, free), dirty));

/* incremental calculate. INVARIANT down the fold: [ctx] differs from
   the prev run's ctx at this position only at [dirty_vars] entries
   (or arbitrarily, if [dirty_types]). A clean item is one whose head
   is unchanged and whose free vars avoid the dirty set — its statics
   are reused; its embedded map keeps STALE ctx entries for dirty
   names it doesn't use (sound for typing; Γ display of unrelated
   names can lag until the item is next recomputed). Structural edits
   (item added/removed/reordered) fall back to a full recompute. */
let calc = (~settings, ~prev: option(t)=?, whole: Exp.t): t => {
  last_analyzed := 0;
  let nodes = chain(whole);
  let aligned =
    switch (prev) {
    | Some(p)
        when
          List.map(Exp.rep_id, nodes)
          == List.map((it: item) => it.d_id, p.items) =>
      Some(p.items)
    | _ => None
    };
  let items =
    switch (aligned) {
    | None =>
      /* cold / structural edit: compute every item in chain order */
      let (items_rev, _) =
        List.fold_left(
          ((acc, ctx), node) => {
            let it = calc_item(~settings, ~ctx_in=ctx, node);
            ([it, ...acc], it.d_ctx_out);
          },
          ([], ctx0),
          nodes,
        );
      List.rev(items_rev);
    | Some(prev_items) =>
      let (items_rev, _, _, _) =
        List.fold_left2(
          ((acc, ctx, dirty_vars, dirty_types), node, p: item) => {
            let clean =
              !dirty_types
              && head_equal(p.d_node, node)
              && !depends(p.d_free, dirty_vars);
            if (clean) {
              let (it, ctx_out) =
                ctx === p.d_ctx_in
                  ? (p, p.d_ctx_out)
                  /* upstream entries changed for names this item
                     doesn't use: re-chain its exports onto the new
                     ctx without re-running statics */
                  : {
                    let ctx_out = {
                      ...ctx,
                      Ctx.entries: p.d_exports @ ctx.entries,
                    };
                    (
                      {
                        ...p,
                        d_ctx_in: ctx,
                        d_ctx_out: ctx_out,
                      },
                      ctx_out,
                    );
                  };
              (
                [it, ...acc],
                ctx_out,
                shadow_filter(it.d_exports, dirty_vars),
                dirty_types,
              );
            } else {
              let it = calc_item(~settings, ~ctx_in=ctx, node);
              let delta = export_delta(p.d_exports, it.d_exports);
              let (it, ctx_out) =
                switch (delta) {
                | Unchanged when ctx === p.d_ctx_in => (
                    {
                      ...it,
                      d_ctx_out: p.d_ctx_out,
                    },
                    p.d_ctx_out,
                  )
                | _ => (it, it.d_ctx_out)
                };
              /* this item's exports shadow INCOMING dirty names; its
                 own delta is added after (it must not filter itself) */
              let incoming = shadow_filter(it.d_exports, dirty_vars);
              let (dirty_vars, dirty_types) =
                switch (delta) {
                | Unchanged => (incoming, dirty_types)
                | VarsChanged(vs) => (vs @ incoming, dirty_types)
                | TypesChanged => (incoming, true)
                };
              ([it, ...acc], ctx_out, dirty_vars, dirty_types);
            };
          },
          ([], ctx0, [], false),
          nodes,
          prev_items,
        );
      List.rev(items_rev);
    };
  {
    items,
    term: whole,
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
