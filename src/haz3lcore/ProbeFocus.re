/* Per-calculate probe upkeep (`editor_effects`, from Editor.calculate):
 * collision cleanup, dead-pin drop, ephemeral regeneration, and
 * sample-focus resolution/alignment against the latest dynamics. */
open Util;
open Language;

let resolve_pending_focus = (~dynamics: Dynamics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.sample_focus.pending_focus) {
  | None => z
  | Some({probe_id, target_stack}) =>
    switch (Dynamics.Map.lookup(probe_id, dynamics)) {
    | None => z
    | Some(samples) =>
      let z' =
        SampleFocusPerform.resolve_pending_focus(z, samples, target_stack);
      if (z'.refractors.sample_focus.pending_focus == None) {
        FocusEffect.schedule(probe_id);
      };
      z';
    }
  };

let cursor_is_aligned_uncached =
    (~dynamics: Dynamics.Map.t, z: Zipper.t): bool => {
  let cursor = z.refractors.sample_focus;
  if (cursor.call_stack == []) {
    true;
  } else {
    let all_probe_ids =
      List.map(fst, Id.Map.bindings(z.refractors.multis.ephemerals))
      @ List.map(fst, z.refractors.manuals);
    List.exists(
      id =>
        switch (Dynamics.Map.lookup(id, dynamics)) {
        | Some([_, ..._] as samples) =>
          Sample.Selection.most_aligned_index(~ap_id=None, cursor, samples)
          != None
        | _ => false
        },
      all_probe_ids,
    );
  };
};

/* Memoize the O(probes x samples) verdict on physical identity (runs every Editor.calculate incl. caret moves; inputs ref-stable). */
let cia_key:
  ref(
    option(
      (
        Dynamics.Map.t,
        Id.Map.t(Refractors.entry),
        list((Id.t, Refractors.entry)),
        Sample.Focus.t,
        bool,
      ),
    ),
  ) =
  ref(None);

let cursor_is_aligned = (~dynamics: Dynamics.Map.t, z: Zipper.t): bool => {
  let cursor = z.refractors.sample_focus;
  let ephemerals = z.refractors.multis.ephemerals;
  let manuals = z.refractors.manuals;
  switch (cia_key^) {
  | Some((d, e, m, c, verdict))
      when d === dynamics && e === ephemerals && m === manuals && c == cursor => verdict
  | _ =>
    let verdict = cursor_is_aligned_uncached(~dynamics, z);
    cia_key := Some((dynamics, ephemerals, manuals, cursor, verdict));
    verdict;
  };
};

let caret_nearest_ephemeral =
    (~syntax: CachedSyntax.t, z: Zipper.t): option(Id.t) => {
  switch (Indicated.index(z)) {
  | Some(piece_id) when Id.Map.mem(piece_id, z.refractors.multis.ephemerals) =>
    Some(piece_id)
  | _ =>
    let caret_pt = Zipper.Caret.point(syntax.measured, z);
    Id.Map.bindings(z.refractors.multis.ephemerals)
    |> List.find_map(((id, _)) =>
         switch (
           TermData.extreme_measures(id, syntax.term_data, syntax.measured)
         ) {
         | Some((start_pt, end_pt))
             when
               start_pt.row == caret_pt.row
               && caret_pt.col >= start_pt.col
               && caret_pt.col <= end_pt.col
               + 1 =>
           Some(id)
         | _ => None
         }
       );
  };
};

/* Whether the probe already shows a sample under the current focus, rather
 * than ⊖: the Single-mode test of ProbeProj.select_samples. Only asked while
 * unpinned, so the pin filter just drops print samples. */
let shows_aligned_sample =
    (~ap_id: option(Id.t), z: Zipper.t, samples: list(Sample.t)): bool =>
  Sample.Selection.most_aligned_index(
    ~ap_id,
    z.refractors.sample_focus,
    Sample.Selection.filter_by_pin(~ap_id, ~pinned=None, samples),
  )
  != None;

let clear_pending_probe_cursor = (z: Zipper.t): Zipper.t =>
  Zipper.update_refractors(z, r =>
    {
      ...r,
      pending_probe_cursor: None,
    }
  );

/* Case 1: a pending request → its first probe with samples, caret-nearest
 * first. Automatic requests (Refractors.pending_probe_cursor) move the focus
 * only to fill a gap: when that probe already shows an aligned sample, nothing
 * moves. The caret-nearest probe is automatic even when injected into an
 * explicit request, which then falls through to its own probes. Case 2: no
 * pending but the cursor went stale (structural edit) → caret-nearest probe.
 * A pin skips case 2 (preserve pinned context) and drops automatic requests,
 * but still resolves explicit ones. */
let resolve_pending_probe_cursor =
    (
      ~dynamics: Dynamics.Map.t,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* A pending cursor whose ids no longer name a live probe can never resolve; clear it, else it wedges (suppresses alignment, forces the double-calculate pass every action). */
  let z =
    switch (z.refractors.pending_probe_cursor) {
    | Some({ids, _})
        when !List.exists(id => ProbePerform.has_probe(id, z), ids) =>
      clear_pending_probe_cursor(z)
    | Some({only_if_not_aligned: true, _}) when !ProbePerform.auto_focus(z) =>
      clear_pending_probe_cursor(z)
    | _ => z
    };
  let request =
    switch (z.refractors.pending_probe_cursor) {
    | Some({ids, only_if_not_aligned}) => Some((ids, only_if_not_aligned))
    | None =>
      if (cursor_is_aligned(~dynamics, z) || !ProbePerform.auto_focus(z)) {
        None;
      } else {
        /* Nothing is aligned, so there is always a gap to fill. */
        let all_ids =
          List.map(fst, Id.Map.bindings(z.refractors.multis.ephemerals))
          @ List.map(fst, z.refractors.manuals);
        switch (all_ids) {
        | [] => None
        | _ => Some((all_ids, false))
        };
      }
    };

  switch (request) {
  | None => z
  | Some((ids, only_if_not_aligned)) =>
    /* Prioritize caret-nearest probe; each candidate carries whether it
     * moves the focus only into a gap. */
    let tag = id => (id, only_if_not_aligned);
    let candidates =
      switch (caret_nearest_ephemeral(~syntax, z)) {
      | Some(nearest) when List.mem(nearest, ids) => [
          tag(nearest),
          ...List.map(tag, List.filter(i => i != nearest, ids)),
        ]
      | Some(nearest) => [(nearest, true), ...List.map(tag, ids)]
      | None => List.map(tag, ids)
      };

    let rec resolve = candidates =>
      switch (candidates) {
      | [] => z /* nothing has samples yet; stay pending */
      | [(probe_id, gap_only), ...rest] =>
        switch (Dynamics.Map.lookup(probe_id, dynamics)) {
        | None
        | Some([]) => resolve(rest)
        | Some(samples) =>
          let ap_id =
            switch (Statics.Map.lookup(probe_id, info_map)) {
            | Some(statics) => Sample.Focus.cur_var_ap(statics)
            | None => None
            };
          if (gap_only && shows_aligned_sample(~ap_id, z, samples)) {
            only_if_not_aligned
              ? clear_pending_probe_cursor(z) : resolve(rest);
          } else {
            switch (
              Sample.Selection.most_aligned_sample(
                ~ap_id,
                ~cursor=z.refractors.sample_focus,
                samples,
              )
            ) {
            | Some(sample) =>
              SampleFocusPerform.capture(
                z,
                Sample.capture_of_sample(sample),
                ap_id,
              )
              |> clear_pending_probe_cursor
            | None => clear_pending_probe_cursor(z)
            };
          };
        }
      };
    resolve(candidates);
  };
};

/* When grout ID preservation keeps the same id across a structural edit,
 * add_ids_from_multi_term won't set pending_probe_cursor; align to the
 * caret-nearest ephemeral instead. Indicated.index gives a piece id, ephemerals
 * key on term ids — try a direct match, then fall back to spatial proximity.
 * Automatic: the edited line moves the focus only if it shows ⊖. */
let align_to_indicated_probe =
    (~is_edited: bool, ~syntax: CachedSyntax.t, z: Zipper.t): Zipper.t =>
  if (!is_edited
      || z.refractors.pending_probe_cursor != None
      || !ProbePerform.auto_focus(z)) {
    z;
  } else {
    switch (caret_nearest_ephemeral(~syntax, z)) {
    | Some(id) =>
      ProbePerform.set_pending_probe(~only_if_not_aligned=true, [id], z)
    | None => z
    };
  };

/* Drop a pinned call stack once no sample matches it (call site deleted/
 * unreached) — a dead pin darkens every probe (⍟), since recovery is gated on
 * auto_focus. Checked against eval RESULTS, not statics: pinned stacks contain
 * library (builtin) frame ids absent from UI statics, and samples+pins both
 * come from the worker. Skipped on empty dynamics. */
let drop_dead_pin = (~dynamics: Dynamics.Map.t, z: Zipper.t): Zipper.t =>
  SampleFocusPerform.update_pinned_call(z, p =>
    switch (p) {
    | Some(stack) when !Id.Map.is_empty(dynamics) =>
      let pinned_ids = CallStack.ids_of_stack(stack);
      let (head_id, tail_ids) =
        switch (pinned_ids) {
        | [hd, ...tl] => (Some(hd), tl)
        | [] => (None, [])
        };
      let alive = (s: Sample.t) => {
        let s_ids = CallStack.ids_of_stack(s.call_stack);
        ListUtil.is_suffix_of(pinned_ids, s_ids)
        || Some(s.syntax_id) == head_id
        && s_ids == tail_ids;
      };
      Id.Map.exists((_, samples) => List.exists(alive, samples), dynamics)
        ? Some(stack) : None;
    | x => x
    }
  );

let editor_effects =
    (
      ~is_edited: bool,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      ~dynamics: Dynamics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t =>
  z
  |> ProbePerform.remove_colliding_probes(~syntax)
  |> drop_dead_pin(~dynamics)
  |> ProbePerform.add_ids_from_multi_term(~syntax, ~info_map)
  |> align_to_indicated_probe(~is_edited, ~syntax)
  |> resolve_pending_focus(~dynamics)
  |> resolve_pending_probe_cursor(~dynamics, ~syntax, ~info_map)
  |> ProbePerform.maybe_reset_cursor;
