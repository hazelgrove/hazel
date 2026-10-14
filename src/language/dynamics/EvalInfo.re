open Util;

/* Witness for incremental-eval cache invalidation when probing changes.
 * `ProbeAll` means every probeable subexpression is targeted (settings.probe_all). */
[@deriving (show({with_path: false}), sexp, yojson)]
type probe_targets =
  | ProbeAll
  | ProbeTargets(SubexpProbeTargets.t);

let equal_probe_targets = (a: probe_targets, b: probe_targets): bool =>
  switch (a, b) {
  | (ProbeAll, ProbeAll) => true
  | (ProbeTargets(a), ProbeTargets(b)) => SubexpProbeTargets.equal(a, b)
  | (ProbeAll, ProbeTargets(_))
  | (ProbeTargets(_), ProbeAll) => false
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  elab_term: Exp.t,
  co_ctx: CoCtx.t,
  probe_targets,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: Id.Map.t(entry),
  targets: Sample.targets /* IDs of expressions/patterns to sample */
};

let empty: t = {
  statics: Id.Map.empty,
  targets: Sample.no_targets,
};

let find_opt = (id: Id.t, map: t): option(entry) =>
  Id.Map.find_opt(id, map.statics);

/* Observation and stream publication key only on PROPER program
 * structure — ids of the elaborated program the user wrote. Stepped
 * intermediates (administrative structure: Id.invalid temps, fresh ids
 * minted mid-evaluation) are never program nodes. */
let is_program_node = (id: Id.t, map: t): bool =>
  Id.Map.mem(id, map.statics);

/* Statics presence (not targets) is what enables incr-entry snapshots
 * and reuse_check hits. */
let has_statics = (map: t): bool => !Id.Map.is_empty(map.statics);

/* For callers (CLI, tests) that evaluate without statics. */
let of_targets = (targets: Sample.targets): t => {
  ...empty,
  targets,
};

let project = (~probe_all: bool, info: Info.t): option(entry) =>
  switch (info) {
  | Info.InfoExp({elab_term, co_ctx, probe_targets, _}) =>
    Some({
      elab_term,
      co_ctx,
      probe_targets: probe_all ? ProbeAll : ProbeTargets(probe_targets),
    })
  | _ => None
  };

let project_all =
    (~probe_all: bool, info_map: StaticsBase.Map.t): Id.Map.t(entry) =>
  Id.Map.filter_map((_id, info) => project(~probe_all, info), info_map);

/* what an entry holds, by identity */
let same_entry = (a: entry, b: entry): bool =>
  a.elab_term === b.elab_term
  && a.co_ctx === b.co_ctx
  && (
    switch (a.probe_targets, b.probe_targets) {
    | (ProbeAll, ProbeAll) => true
    | (ProbeTargets(x), ProbeTargets(y)) => x === y
    | (ProbeAll, ProbeTargets(_))
    | (ProbeTargets(_), ProbeAll) => false
    }
  );

/* [acc] with [k]'s entry projected from [info]; an entry that holds what
   the old one held is kept, the object itself */
let set_entry =
    (~probe_all: bool, k: Id.t, info: Info.t, acc: Id.Map.t(entry)) =>
  switch (project(~probe_all, info)) {
  | None => Id.Map.remove(k, acc)
  | Some(e) =>
    switch (Id.Map.find_opt(k, acc)) {
    | Some(old) when same_entry(old, e) => acc
    | _ => Id.Map.add(k, e, acc)
    }
  };

/* The projection of [info_map], given [prev], the projection of
   [prev_map]: one walk over both maps in key order, comparing infos by
   identity, and only the entries that changed are added or removed. So
   [prev]'s entries, and its subtrees, are kept wherever nothing in them
   changed, and the request that carries the projection hashes only what
   did (Util.ShareCodec). The same answer as project_all; or None when
   the maps turn out to share nothing (statics rebuilt whole, or another
   program): once [sample] infos are compared, more than half of them
   different. Ids are spread over the key order, so a commit's changes
   are too (a moved item, re-analyzed downstream: 7%). */
exception Unshared;
let project_from =
    (
      ~probe_all: bool,
      ~sample=256,
      prev_map: StaticsBase.Map.t,
      prev: Id.Map.t(entry),
      info_map: StaticsBase.Map.t,
    )
    : option(Id.Map.t(entry)) => {
  let compared = ref(0);
  let differ = ref(0);
  let count = (same: bool) => {
    incr(compared);
    if (!same) {
      incr(differ);
      if (compared^ >= sample && 2 * differ^ > compared^) {
        raise(Unshared);
      };
    };
  };
  let set = set_entry(~probe_all);
  let rec go =
          (acc, a: Seq.node((Id.t, Info.t)), b: Seq.node((Id.t, Info.t))) =>
    switch (a, b) {
    | (Nil, Nil) => acc
    | (Cons((ka, _), a'), Nil) =>
      count(false);
      go(Id.Map.remove(ka, acc), a'(), Nil);
    | (Nil, Cons((kb, ib), b')) =>
      count(false);
      go(set(kb, ib, acc), Nil, b'());
    | (Cons((ka, ia), a'), Cons((kb, ib), b')) =>
      let c = Id.compare(ka, kb);
      if (c == 0) {
        count(ia === ib);
        go(ia === ib ? acc : set(kb, ib, acc), a'(), b'());
      } else if (c < 0) {
        count(false);
        go(Id.Map.remove(ka, acc), a'(), b);
      } else {
        count(false);
        go(set(kb, ib, acc), a, b'());
      };
    };
  switch (
    go(prev, Id.Map.to_seq(prev_map, ()), Id.Map.to_seq(info_map, ()))
  ) {
  | statics => Some(statics)
  | exception Unshared => None
  };
};

/* The keys at which a statics map may differ from the map it was built
   from, as its builder says (DefStatics.calc: the items it analyzed again,
   the ones it dropped, the spine roots it patched). Given that, a
   projection of the old map is updated at those keys alone, where
   project_from walks both maps whole: 13-14 ms a release on Full Body,
   though some 180 entries change. A note is found by the identity of
   both maps, so one about other maps is never used; without one,
   project_from does as before. */
let deltas: ref(list((StaticsBase.Map.t, StaticsBase.Map.t, list(Id.t)))) =
  ref([]);
let deltas_cap = 8;

let note_delta =
    (~base: StaticsBase.Map.t, ~changed: list(Id.t), map: StaticsBase.Map.t)
    : unit =>
  deltas :=
    [
      (map, base, changed),
      ...ListUtil.take(
           deltas_cap - 1,
           List.filter(((m, _, _)) => m !== map, deltas^),
         ),
    ];

let delta_from =
    (prev_map: StaticsBase.Map.t, info_map: StaticsBase.Map.t)
    : option(list(Id.t)) =>
  List.find_map(
    ((m, b, changed)) =>
      m === info_map && b === prev_map ? Some(changed) : None,
    deltas^,
  );

/* how many projections a delta updated (tests) */
let delta_hits = ref(0);

let project_delta =
    (
      ~probe_all: bool,
      prev: Id.Map.t(entry),
      changed: list(Id.t),
      info_map: StaticsBase.Map.t,
    )
    : Id.Map.t(entry) =>
  List.fold_left(
    (acc, k) =>
      switch (Id.Map.find_opt(k, info_map)) {
      | None => Id.Map.remove(k, acc)
      | Some(info) => set_entry(~probe_all, k, info, acc)
      },
    prev,
    changed,
  );

/* The last projection per document (LRU, as DefStatics keeps its last
   calc): a commit changes a few infos of a statics map that shares the
   rest with the last one, so its projection is updated, not rebuilt. The
   key only guesses which projection to start from; any one gives the
   same answer. */
let slots: Hashtbl.t(Id.t, (bool, StaticsBase.Map.t, Id.Map.t(entry))) =
  Hashtbl.create(8);
let slots_mru: ref(list(Id.t)) = ref([]);
let slots_cap = 8;

let of_info_map =
    (
      ~key: option(Id.t)=?,
      ~probe_all: bool,
      ~targets: Sample.targets,
      info_map: StaticsBase.Map.t,
    )
    : t => {
  let statics =
    switch (Option.bind(key, Hashtbl.find_opt(slots))) {
    | Some((pa, prev_map, prev)) when pa == probe_all =>
      if (prev_map === info_map) {
        prev;
      } else {
        switch (delta_from(prev_map, info_map)) {
        | Some(changed) =>
          incr(delta_hits);
          project_delta(~probe_all, prev, changed, info_map);
        | None =>
          switch (project_from(~probe_all, prev_map, prev, info_map)) {
          | Some(statics) => statics
          | None => project_all(~probe_all, info_map)
          }
        };
      }
    | _ => project_all(~probe_all, info_map)
    };
  Option.iter(
    k => {
      Hashtbl.replace(slots, k, (probe_all, info_map, statics));
      slots_mru := [k, ...List.filter(k' => k' != k, slots_mru^)];
      switch (ListUtil.split_n_opt(slots_cap, slots_mru^)) {
      | Some((keep, evict)) when evict != [] =>
        List.iter(Hashtbl.remove(slots), evict);
        slots_mru := keep;
      | _ => ()
      };
    },
    key,
  );
  {
    statics,
    targets,
  };
};
