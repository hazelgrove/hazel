open Util;

/* W2a SHADOW MODE (plans/w2-worker-residency.md §4.6): main remains
 * authoritative and computes its own statics; each master-statics
 * update ALSO syncs segments to the worker, whose ResidentProgram
 * answers a summary compared against main's out-of-band. No UI
 * consumer reads the worker's answer — a mismatch bumps a counter and
 * logs. Consumers flip (W2b) only after the counter stays zero across
 * the corpus + fuzz.
 *
 * Coherence: shipping is gated on the DefStatics slot changing
 * IDENTITY, which happens on Force frames — the one moment the
 * editor's segment and the slot's statics describe the same program
 * (between Force frames the segment runs ahead of the debounced
 * statics, and shipping there would manufacture false mismatches). */

let enabled: ref(bool) = ref(true);

/* the resident key must MATCH the eval-request key for the same
   editor, or Resident payloads resolve against nothing: ScratchMode
   queues the master eval under "" (ScratchMode.calculate) */
let master_key = "";

type mirror = {
  m_key: string,
  m_root: Haz3lcore.Sort.t,
  /* settings cross only on FULL syncs; a change forces one (the
     worker would otherwise analyze under stale settings — permanent
     mismatch) */
  m_settings: Language.CoreSettings.t,
  /* slice objects as last shipped — the per-edit diff is pointer
     equality against these (identity-restore keeps unchanged items
     physically intact across edits) */
  m_slices: list(Haz3lcore.Segment.t),
  /* roster as the worker now holds it */
  m_roster: list((Id.t, int)),
  /* probe set as last shipped (toggles can change with no edit) */
  m_probes: list(Id.t),
};

let mirror: ref(option(mirror)) = ref(None);
let generation: ref(int) = ref(0);
let last_ds: ref(option(Haz3lcore.DefStatics.t)) = ref(None);

/* main summaries awaiting the worker's echo (bounded; superseded
   generations just never match and age out) */
let pending: ref(list((int, Haz3lcore.ResidentProgram.Summary.t))) =
  ref([]);

let oks = ref(0);
let mismatches = ref(0);
let resyncs = ref(0);
let full_ships = ref(0);
let item_ships = ref(0);
let counters = (): (int, int, int) => (oks^, mismatches^, resyncs^);

/* ===== W2b FLIP ===== when on: main clamps statics propagation to
   the edited item (DefStatics.clamp), evals run from the worker's
   resident program, and worker summaries GRAFT fresh cross-item
   error/warning ids into the main slot (stale info elsewhere, per the
   staleness contract). The shadow COMPARISON is off while flipped
   (main's summary is stale-by-design). Turning the flip off busts the
   DefStatics slot — a clamped chain cannot be caught up incrementally
   (Test_PropagateClamp). */
let flip_enabled: ref(bool) = ref(false);
let set_flip = (b: bool): unit =>
  if (b != flip_enabled^) {
    flip_enabled := b;
    Haz3lcore.DefStatics.clamp := b;
    Haz3lcore.DefStatics.slot := None; /* fresh baseline either way */
    print_endline("[w2-shadow] flip " ++ (b ? "ON" : "OFF"));
  };

let () =
  Js_of_ocaml.Js.Unsafe.set(
    Js_of_ocaml.Js.Unsafe.global,
    "__w2flip",
    Js_of_ocaml.Js.wrap_callback(b => set_flip(Js_of_ocaml.Js.to_bool(b))),
  );

/* console access: window.__w2shadowToggle(bool) flips shadow mode */
let () =
  Js_of_ocaml.Js.Unsafe.set(
    Js_of_ocaml.Js.Unsafe.global,
    "__w2shadowToggle",
    Js_of_ocaml.Js.wrap_callback(b => enabled := Js_of_ocaml.Js.to_bool(b)),
  );

/* console access: window.__w2shadow() -> "oks/mismatches/resyncs" */
let () =
  Js_of_ocaml.Js.Unsafe.set(
    Js_of_ocaml.Js.Unsafe.global,
    "__w2shadow",
    Js_of_ocaml.Js.wrap_callback(() =>
      Js_of_ocaml.Js.string(
        Printf.sprintf(
          "oks=%d mismatches=%d resyncs=%d full=%d items=%d",
          oks^,
          mismatches^,
          resyncs^,
          full_ships^,
          item_ships^,
        ),
      )
    ),
  );

let log = msg => print_endline("[w2-shadow] " ++ msg);

/* FLIP mode: the worker's per-item error/warning ids are the truth
   for cross-item display — graft them into the main slot's items (the
   next calculate pass rebuilds the view's error_ids from the slot).
   Superseded generations are dropped. Synthetic-node ids cannot cross
   (derivation-local); stale items keep their old synthetic ids. */
let graft_summary =
    (msg: WorkerServer.ServerMessage.summary_msg,
     theirs: Haz3lcore.ResidentProgram.Summary.t)
    : unit =>
  if (msg.generation == generation^) {
    switch (Haz3lcore.DefStatics.slot^) {
    | None => ()
    | Some(t) =>
      let items =
        t.items
        |> List.map((it: Haz3lcore.DefStatics.item) =>
             switch (
               List.find_opt(
                 (si: Haz3lcore.ResidentProgram.Summary.item_summary) =>
                   Id.equal(si.s_id, it.d_id),
                 theirs.s_items,
               )
             ) {
             | Some(si) => {
                 ...it,
                 d_error_ids: si.s_errors,
                 d_warning_ids: si.s_warnings,
               }
             | None => it
             }
           );
      Haz3lcore.DefStatics.slot := Some({...t, items});
    };
  };

let on_summary = (msg: WorkerServer.ServerMessage.summary_msg): unit =>
  switch (msg.verdict) {
  | NeedResync(reason) =>
    incr(resyncs);
    mirror := None; /* next statics update ships Full */
    log("worker demands resync: " ++ reason);
  | SyncOk(theirs) when flip_enabled^ =>
    incr(oks);
    graft_summary(msg, theirs);
  | SyncOk(theirs) =>
    switch (List.assoc_opt(msg.generation, pending^)) {
    | None => () /* superseded generation */
    | Some(ours) =>
      pending := List.remove_assoc(msg.generation, pending^);
      if (Haz3lcore.ResidentProgram.Summary.equal(ours, theirs)) {
        incr(oks);
        if (oks^ == 1) {
          log("first summary verified — shadow mode active");
        };
      } else {
        incr(mismatches);
        let ni = List.length(ours.s_items);
        let nj = List.length(theirs.s_items);
        log(
          Printf.sprintf(
            "SUMMARY MISMATCH gen %d (main %d items, worker %d items)",
            msg.generation,
            ni,
            nj,
          ),
        );
        if (ni == nj) {
          List.combine(ours.s_items, theirs.s_items)
          |> List.iteri(
               (
                 i,
                 (
                   o: Haz3lcore.ResidentProgram.Summary.item_summary,
                   t: Haz3lcore.ResidentProgram.Summary.item_summary,
                 ),
               ) =>
               if (o != t) {
                 log(
                   Printf.sprintf(
                     "  item %d: id %s vs %s | errs %d vs %d | warns %d vs %d",
                     i,
                     Id.to_string(o.s_id),
                     Id.to_string(t.s_id),
                     List.length(o.s_errors),
                     List.length(t.s_errors),
                     List.length(o.s_warnings),
                     List.length(t.s_warnings),
                   ),
                 );
               }
             );
        };
      };
    }
  };

let item_key = (slice: Haz3lcore.Segment.t): option(Id.t) =>
  Haz3lcore.ResidentProgram.item_id(slice);

/* diff current slices against the mirror by pointer identity; returns
   (changed, roster) or None when the shape changed (restructure class
   → full sync; plan §4.9) */
let diff_items = (m: mirror, slices: list(Haz3lcore.Segment.t)) =>
  if (List.length(slices) != List.length(m.m_slices)) {
    None;
  } else {
    let (changed, roster) =
      List.fold_left2(
        ((changed, roster), (old_slice, (old_id, old_print)), nu) =>
          if (Haz3lcore.Segment.ptr_eq(old_slice, nu)) {
            (changed, [(old_id, old_print), ...roster]);
          } else {
            switch (item_key(nu)) {
            | None => (changed, [(old_id, old_print), ...roster])
            | Some(new_id) =>
              let print = Haz3lcore.ResidentProgram.fingerprint(nu);
              (
                [(old_id, nu, print), ...changed],
                [(new_id, print), ...roster],
              );
            };
          },
        ([], []),
        List.combine(m.m_slices, m.m_roster),
        slices,
      );
    Some((List.rev(changed), List.rev(roster)));
  };

let full_roster = (slices: list(Haz3lcore.Segment.t)) =>
  slices
  |> List.filter_map(slice =>
       switch (item_key(slice)) {
       | None => None
       | Some(id) => Some((id, Haz3lcore.ResidentProgram.fingerprint(slice)))
       }
     );

let on_master_statics =
    (
      ~key: string,
      ~root: Haz3lcore.Sort.t,
      ~settings: Language.CoreSettings.t,
      seg: Haz3lcore.Segment.t,
      ds: Haz3lcore.DefStatics.t,
    )
    : unit =>
  if (enabled^) {
    switch (last_ds^) {
    | Some(prev) when prev === ds => ()
    | _ =>
      last_ds := Some(ds);
      let probes =
        Id.Map.bindings(ds.Haz3lcore.DefStatics.probe_ids) |> List.map(fst);
      let slices = Haz3lcore.MakeTerm.Incr.slices(seg);
      let ship = payload => {
        incr(generation);
        let g = generation^;
        pending :=
          [
            (
              g,
              Haz3lcore.ResidentProgram.Summary.of_def_statics(
                ~generation=g,
                ~piece_ids=Haz3lcore.ResidentProgram.piece_ids(seg),
                ds,
              ),
            ),
            ...ListUtil.take(7, pending^),
          ];
        WorkerClient.sync({
          version: WorkerServer.w2_protocol_version,
          key,
          generation: g,
          probe_ids: probes,
          payload,
        });
      };
      switch (mirror^) {
      | Some(m)
          when m.m_key == key && m.m_root == root && m.m_settings == settings =>
        switch (diff_items(m, slices)) {
        | Some(([], _)) when m.m_probes == probes =>
          /* statics identity churn without segment or probe change —
             nothing to sync; refresh slice identities */
          mirror :=
            Some({
              ...m,
              m_slices: slices,
            })
        | Some(([], roster)) =>
          /* probe-only change: empty delta, same roster, new probes */
          mirror :=
            Some({
              ...m,
              m_slices: slices,
              m_probes: probes,
            });
          incr(item_ships);
          ship(Items([], roster));
        | Some((changed, roster)) =>
          mirror :=
            Some({
              ...m,
              m_slices: slices,
              m_roster: roster,
              m_probes: probes,
            });
          incr(item_ships);
          ship(Items(changed, roster));
        | None =>
          let roster = full_roster(slices);
          mirror :=
            Some({
              m_key: key,
              m_root: root,
              m_settings: settings,
              m_slices: slices,
              m_roster: roster,
              m_probes: probes,
            });
          incr(full_ships);
          ship(Full(root, settings, seg));
        }
      | _ =>
        let roster = full_roster(slices);
        mirror :=
          Some({
            m_key: key,
            m_root: root,
            m_settings: settings,
            m_slices: slices,
            m_roster: roster,
            m_probes: probes,
          });
        incr(full_ships);
        ship(Full(root, settings, seg));
      };
    };
  };
