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

/* OFF by default: shadow mode duplicates statics derivation and adds
   worker messaging on every master-statics update — it's a
   development/benchmark diagnostic, not a passenger users should pay
   for. Enable via __w2shadowToggle(true); __w2flip(true) implies it
   (the flip consumes the residency the shadow sync maintains). */
let enabled: ref(bool) = ref(false);

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

/* the latest AUTHORITATIVE program, cached so recovery (worker
   restart, NeedResync) can re-ship Full immediately instead of
   waiting for the user's next semantic edit — until then the worker
   would be stale and any queued Resident eval unservable */
type authoritative = {
  a_key: string,
  a_root: Haz3lcore.Sort.t,
  a_settings: Language.CoreSettings.t,
  a_seg: Haz3lcore.Segment.t,
  a_probes: list(Id.t),
};
let last_auth: ref(option(authoritative)) = ref(None);
/* piece ids of the last shipped program: the graft uses them to tell
   piece-anchored ids (worker-refreshable) from synthetic ones (main-
   only, must survive the graft) */
let last_piece_ids: ref(Id.Set.t) = ref(Id.Set.empty);
/* one recovery in flight at a time: a worker that NeedResyncs the
   recovery itself (e.g. version skew) must not loop */
let recovering: ref(bool) = ref(false);

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
    if (b) {
      enabled := true; /* the flip consumes the residency sync */
    };
    Haz3lcore.DefStatics.clamp := b;
    /* fresh baseline either way — and EVERY cache calc_auto reads:
       clearing only the active slot left clamped chains in the
       per-document table, reusable after toggle-off */
    Haz3lcore.DefStatics.reset_caches();
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

let item_key = (slice: Haz3lcore.Segment.t): option(Id.t) =>
  Haz3lcore.ResidentProgram.item_id(slice);

let full_roster = (slices: list(Haz3lcore.Segment.t)) =>
  slices
  |> List.filter_map(slice =>
       switch (item_key(slice)) {
       | None => None
       | Some(id) => Some((id, Haz3lcore.ResidentProgram.fingerprint(slice)))
       }
     );

/* dispatched after a graft so a real recalculate pass runs and the
   view rebuilds its error/warning ids — a bare cache mutation is
   invisible until the user's next edit. Registered by Main. */
let schedule_recalc: ref(unit => unit) = ref(() => ());

/* FLIP mode: the worker's per-item error/warning ids are the truth
   for cross-item display. Grafting must be DURABLE and OBSERVABLE:
   both DefStatics caches update (the active slot AND the per-document
   keyed entry — else the next calc_auto rebuilds from the ungrafted
   entry and silently reverts), and a recalculate is scheduled so the
   ids actually reach the rendered view. Superseded generations are
   dropped. Piece-anchored ids refresh from the worker; synthetic ids
   (derivation-local, filtered out of summaries) survive from main's
   own analysis. Known limitation: summaries carry ids only, so
   in-editor markers for cross-item errors still need main statics. */
let graft_summary =
    (
      msg: WorkerServer.ServerMessage.summary_msg,
      theirs: Haz3lcore.ResidentProgram.Summary.t,
    )
    : unit =>
  if (msg.generation == generation^) {
    switch (Haz3lcore.DefStatics.slot^) {
    | None => ()
    | Some(t) =>
      let synthetic = ids =>
        List.filter(id => !Id.Set.mem(id, last_piece_ids^), ids);
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
                 d_error_ids: si.s_errors @ synthetic(it.d_error_ids),
                 d_warning_ids: si.s_warnings @ synthetic(it.d_warning_ids),
               }
             | None => it
             }
           );
      let t' = {
        ...t,
        Haz3lcore.DefStatics.items,
      };
      Haz3lcore.DefStatics.slot := Some(t');
      Haz3lcore.DefStatics.replace_slot_entry(t');
      schedule_recalc^();
    };
  };

/* recovery: re-ship the cached authoritative program as Full, under
   the CURRENT generation (same content, not a new program state) so
   an already-queued Resident eval for that generation resolves after
   the sync lands (postMessage FIFO). One recovery in flight at a
   time; SyncOk clears the latch. */
let resync_now = (reason: string): unit => {
  mirror := None;
  last_ds := None; /* an unchanged doc must still re-ship */
  switch (last_auth^) {
  | Some(a) when enabled^ && ! recovering^ =>
    recovering := true;
    incr(resyncs);
    incr(full_ships);
    log("recovery full sync (" ++ reason ++ ")");
    let slices = Haz3lcore.MakeTerm.Incr.slices(a.a_seg);
    mirror :=
      Some({
        m_key: a.a_key,
        m_root: a.a_root,
        m_settings: a.a_settings,
        m_slices: slices,
        m_roster: full_roster(slices),
        m_probes: a.a_probes,
      });
    WorkerClient.sync({
      version: WorkerServer.w2_protocol_version,
      key: a.a_key,
      generation: generation^,
      probe_ids: a.a_probes,
      payload: Full(a.a_root, a.a_settings, a.a_seg),
    });
  | _ => ()
  };
};

let on_summary = (msg: WorkerServer.ServerMessage.summary_msg): unit =>
  switch (msg.verdict) {
  | NeedResync(reason) =>
    log("worker demands resync: " ++ reason);
    /* recover NOW from the cached authoritative program — waiting for
       the next semantic edit leaves the worker stale indefinitely and
       any queued Resident eval unservable. The latch stops a resync
       loop when the recovery itself is rejected (version skew). */
    resync_now(reason);
  | SyncOk(theirs) when flip_enabled^ =>
    recovering := false;
    incr(oks);
    graft_summary(msg, theirs);
  | SyncOk(theirs) =>
    recovering := false;
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
    };
  };

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
      /* recovery inputs: every authoritative pass refreshes them,
         shipped or not */
      last_auth :=
        Some({
          a_key: key,
          a_root: root,
          a_settings: settings,
          a_seg: seg,
          a_probes: probes,
        });
      let ship = payload => {
        incr(generation);
        let g = generation^;
        last_piece_ids := Haz3lcore.ResidentProgram.piece_ids(seg);
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

/* a worker restart drops all residency while the mirror survives —
   without this hook an unchanged document never re-syncs and flipped
   evals fail with "no resident program" forever */
let () = WorkerClient.on_restart := (() => resync_now("worker-restart"));
