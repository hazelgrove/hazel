/* Pointer/drag front-end: enumerate the draggable candidates at
 * the caret with their live+target geometry, driving the gesture
 * system per direction. */
open Language;
open RefactorBase;
open RefactorInline;
open RefactorMove;
open RefactorReduce;
open RefactorRegistry;
open RefactorGesture;

let emerge_source =
    (~info_map, ~target, kind: Action.refactor, term): list(Id.t) =>
  switch (kind) {
  | FeedLet =>
    switch (feed_plan(~info_map, ~target, term)) {
    | Some(Feed(_, def, _)) => exp_subtree_ids(def)
    | _ => []
    }
  /* inline: every substituted copy emerges from the def (the first
     copy MOVES — same ids — the rest fly as fan-out clones) */
  | InlineLet =>
    switch (find_hit(~hit=hit_let(target), term)) {
    | Some(l) =>
      switch (IdTagged.term_of(l)) {
      | Let(_, def, _) => exp_subtree_ids(def)
      | _ => []
      }
    | None =>
      switch (binder_of_occurrence(~info_map, ~target, term)) {
      | Some(binder) =>
        switch (find_hit(~hit=hit_let(binder), term)) {
        | Some(l) =>
          switch (IdTagged.term_of(l)) {
          | Let(_, def, _) => exp_subtree_ids(def)
          | _ => []
          }
        | None => []
        }
      | None => []
      }
    }
  /* beta: the copies emerge from the ARGUMENT */
  | BetaReduce =>
    switch (find_hit(~hit=hit_beta(target), term)) {
    | Some(e) =>
      switch (beta_parts(e)) {
      | Some((_, arg, _)) => exp_subtree_ids(arg)
      | None => []
      }
    | None => []
    }
  | _ => []
  };

let gesture_merge_target =
    (~info_map, ~term, g: Action.Gesture.t, z: Zipper.t)
    : (list(Id.t), list(Id.t)) =>
  switch (Indicated.index(z), gesture(~info_map, ~term, g, z)) {
  | (Some(target), Some(kind)) =>
    merge_target(~info_map, ~target, kind, term)
  | _ => ([], [])
  };

let refactor_merge_target =
    (~info_map, ~term, kind: Action.refactor, z: Zipper.t)
    : (list(Id.t), list(Id.t)) =>
  switch (Indicated.index(z)) {
  | Some(target) => merge_target(~info_map, ~target, kind, term)
  | None => ([], [])
  };

let gesture_emerge_source =
    (~info_map, ~term, g: Action.Gesture.t, z: Zipper.t): list(Id.t) =>
  switch (Indicated.index(z), gesture(~info_map, ~term, g, z)) {
  | (Some(target), Some(kind)) =>
    emerge_source(~info_map, ~target, kind, term)
  | _ => []
  };

let refactor_emerge_source =
    (~info_map, ~term, kind: Action.refactor, z: Zipper.t): list(Id.t) =>
  switch (Indicated.index(z)) {
  | Some(target) => emerge_source(~info_map, ~target, kind, term)
  | None => []
  };

/* === Drag candidates (D2's reachable states) ===
 * The grabbed element's candidates are EVERY refactor it can drive
 * (drag_kinds), not one per arrow key: each is prepared and measured
 * — measures only, no statics/eval/view (dragology's isTracking, done
 * with Measured) — and defines a TRACK from the anchor's current
 * position to its position in that state. Which way you pull falls out
 * of where the states put the element. Anchors are (from, to) id
 * pairs: the grabbed construct for movement kinds; def -> fed
 * occurrence for feeds (the value travels, the binding doesn't). The
 * element must keep a place in the state (D2's model-driven rule):
 * a candidate whose anchor vanishes drops, except the kinds whose
 * track ends elsewhere (lands_elsewhere). Degenerate
 * tracks (anchor doesn't move) drop too. Coincident targets keep the
 * first in drag_kinds order. */

module DragCandidate = {
  /* how the candidate layout maps onto the screen during the drag
     (the space-duality rule): candidate rows >= shift_from move by
     shift_rows; scroll_rows bumps the scroller at commit.
     - remove-kinds (feed): +N below the vacated line — bystanders
       and the target hold their LIVE positions; the blank persists
       until release (two-stage).
     - add-kinds (extract): global -N + a commit scroll bump — the
       origin line stays pinned while space opens above it. */
  type frame = {
    shift_from: int,
    shift_rows: int,
    scroll_rows: int,
  };
  let no_frame = {
    shift_from: 0,
    shift_rows: 0,
    scroll_rows: 0,
  };
  let frame_point = (f: frame, p: Measured.Point.t): Measured.Point.t =>
    p.row >= f.shift_from
      ? {
        ...p,
        row: p.row + f.shift_rows,
      }
      : p;

  type t = {
    kind: Action.refactor,
    /* where the step is invoked: the grab, or the let of a grabbed
       value whose line moves (composites) */
    at: Id.t,
    label: string,
    current: Measured.Point.t, /* track start (live layout) */
    target: Measured.Point.t, /* gesture end; proof targets can ignore soft wraps */
    frame,
    /* entering-token ORIGINS (dragology's emergeFrom): a transform
       that DUPLICATES content (feed with surviving uses) maps each
       fresh copy id to the live id it emerges from — the ghost
       travels from the source instead of growing in place */
    emerge: list((Id.t, Id.t)),
    /* dissolving ids -> the survivor's (merge, extract onto an
       identical definition): D2's emerge reversed, the copy converges
       onto what it merges into */
    absorb: list((Id.t, Id.t)),
    term: Exp.t,
    focus: Id.t,
    segment: Segment.t,
    measured: Measured.t,
  };
};

let total_rows = (m: Measured.t): int =>
  switch (Measured.Rows.max_binding_opt(m.rows)) {
  | Some((r, _)) => r + 1
  | None => 0
  };

/* (from, to) anchor ids for a kind's track */
let drag_anchor =
    (
      ~feed_pref: bool=false,
      ~info_map,
      ~target: Id.t,
      kind: Action.refactor,
      term: Exp.t,
    )
    : option((Id.t, Id.t)) =>
  switch (kind) {
  | SwapArms(i) =>
    /* rule delimiters (| and =>) live in Match.ids, not the Measured
       maps — anchor at the MOVED arm's pattern so grabbing the bar
       works like grabbing the pattern */
    switch (find_hit(~hit=hit_arm(target), term)) {
    | Some(m) =>
      switch (IdTagged.term_of(m), arm_index_at(target, m)) {
      | (Match(_, rules), Some(j)) when j < List.length(rules) =>
        let (rp, _) = List.nth(rules, j);
        /* the grabbed arm is index j; it swaps with i/i+1 — anchor
           the arm the user grabbed either way */
        ignore(i);
        Some((Pat.rep_id(rp), Pat.rep_id(rp)));
      | _ => None
      }
    | None => None
    }
  /* a joined line's variable moves into the tuple, whichever of the
     line's handles was grabbed */
  | JoinLet =>
    switch (join_site(~target, term)) {
    | Some((_, lower)) =>
      switch (IdTagged.term_of(lower)) {
      | Let(lp, _, _) => Some((Pat.rep_id(lp), Pat.rep_id(lp)))
      | _ => None
      }
    | None => None
    }
  | FeedLet =>
    switch (feed_site(~prefer_def_host=feed_pref, ~info_map, ~target, term)) {
    /* grabbed AT the use: a def->use track would start at its end
       (the pointer begins at t~1 and release commits instantly) —
       no track; the default (target, target) pair degenerates and
       the candidate drops. Feeds drag from the binding side. */
    | Some((_, _, Some(_))) => Some((target, target))
    | Some((l, x, None)) =>
      switch (IdTagged.term_of(l)) {
      | Let(_, def, body) =>
        first_occurrence(x, body)
        |> Option.map(o => (Exp.rep_id(def), Exp.rep_id(o)))
      | _ => None
      }
    | None => None
    }
  | _ => Some((target, target))
  };

/* the name a let binds as a whole: a bare binder, through any
   annotation or parens, or a sugar function's name */
let rec head_name_ids = (p: Pat.t): list(Id.t) =>
  switch (IdTagged.term_of(p)) {
  | Var(_) => IdTagged.ids(p)
  | Asc(p', _)
  | Parens(p') => head_name_ids(p')
  | Ap(f, _) => head_name_ids(f)
  | _ => []
  };

/* moves that change where a value lives — inline in its use, or bound
   on some line: the composite family (Settings > Drag Composites).
   One drag may run through any of them. */
let place_kind = (k: Action.refactor): bool =>
  switch (k) {
  | ExtractLet
  | HoistLet
  | SinkLet
  | HoistCarry
  | SinkCarry
  | LiftFunction
  | UnliftFunction
  | MergeUp
  | MergeDown
  | FeedLet => true
  | _ => false
  };

/* up (-1) or down (1) a value's path of places */
let place_way = (k: Action.refactor): option(int) =>
  switch (k) {
  | ExtractLet
  | HoistLet
  | HoistCarry
  | LiftFunction
  | MergeUp => Some(-1)
  | SinkLet
  | SinkCarry
  | UnliftFunction
  | MergeDown
  | FeedLet => Some(1)
  | _ => None
  };

type held = {
  /* the tile to indicate */
  tile: Id.t,
  /* the piece's own delimiters (what its indication backs) */
  own: list(Id.t),
  /* every token of the piece */
  tokens: list(Id.t),
  /* a line, whose indication runs on into a body it leaves */
  line: bool,
};

/* what a grab holds. A line handle holds its line (delimiters, pattern,
   definition; not the body); any other token holds its own term. */
let held_piece = (~target: Id.t, term: Exp.t): held => {
  let own_term = () =>
    switch (find_hit(~hit=e => List.mem(target, IdTagged.ids(e)), term)) {
    | Some(e) => {
        tile: target,
        own: IdTagged.ids(e),
        tokens: exp_subtree_ids(e),
        line: false,
      }
    | None => {
        tile: target,
        own: [target],
        tokens: [target],
        line: false,
      }
    };
  switch (find_hit(~hit=hit_def_line(target), term)) {
  | Some(l) =>
    switch (IdTagged.term_of(l)) {
    | Let(p, def, _)
        when hit_node(target, l) || List.mem(target, head_name_ids(p)) => {
        tile: Exp.rep_id(l),
        own: IdTagged.ids(l),
        tokens: IdTagged.ids(l) @ pat_subtree_ids(p) @ exp_subtree_ids(def),
        line: true,
      }
    | TyAlias(tp, _, _) => {
        tile: Exp.rep_id(l),
        own: IdTagged.ids(l),
        tokens: IdTagged.ids(l) @ IdTagged.ids(tp),
        line: true,
      }
    | _ => {
        tile: target,
        own: [target],
        tokens: [target],
        line: false,
      }
    }
  | None => own_term()
  };
};

/* the refactors a grab can drive: every rung of every direction's
   gesture zone (the zone table is the drag spec's handle map: which
   tokens drive which transform), without the arrow keys' first-match
   preemption; plus the insist remedies (carry the dependencies / lift
   to a helper) when pushing through is on. Order = coincident-target
   precedence. A use never drives its own unfolding: occurrences mean
   themselves, and the unfolded use has no place in the result. */
let drag_kinds =
    (
      ~climb: bool,
      ~info_map: Statics.Map.t,
      ~term: Exp.t,
      ~push: bool,
      ~target: Id.t,
      ~shard: option(int),
    )
    : list((Action.refactor, Id.t)) => {
  /* one element per token: a line's handles (let, =, in, and a bare
     binder name) move the LINE; a piece of a structured pattern
     (component, parameter, annotation) drives only its own moves; the
     value moves only by its own definition (grab the def's root to
     feed) */
  let line_zone = find_hit(~hit=hit_def_line(target), term);
  let line_handle =
    switch (line_zone) {
    | Some(l) =>
      switch (IdTagged.term_of(l)) {
      | Let(p, _, _) =>
        hit_node(target, l) || List.mem(target, head_name_ids(p))
      | _ => true
      }
    | None => false
    };
  let drives = (k: Action.refactor) =>
    switch (line_zone) {
    | None => true
    | Some(_) => k != Action.FeedLet && (line_handle || !moves_line(k))
    };
  /* an if flips by its BRANCH (the thing that moves), not by nudging
     then/else (the keyboard keeps those) */
  let branch_root = if_of_branch(~target, term) != None;
  /* drag-first moves no arrow key reaches: a tuple component splits
     out onto its own line, a line joins the tuple above it */
  let extra =
    [Action.NegateIf, SplitComponent, JoinLet]
    |> List.filter(k =>
         (k != Action.NegateIf || branch_root)
         && applies(k, ~info_map, ~target, term)
       );
  let insist =
    push
      ? [Action.HoistCarry, SinkCarry, LiftFunction]
        |> List.filter(k => applies(k, ~info_map, ~target, term))
      : [];
  /* composites: a grabbed value's line moves with it — its place is
     bound-on-this-line until a feed puts it in a use */
  /* a value already moving (extracted earlier in this drag) climbs with
     its line; a fresh grab of a value only feeds, and downward a value
     only ever feeds */
  let via_line =
    switch (climb ? value_line(~target, term) : None) {
    | Some(l) =>
      [Action.MergeUp, HoistLet]
      @ (push ? [Action.HoistCarry, LiftFunction] : [])
      |> List.filter(k =>
           applies(k, ~info_map, ~target=Exp.rep_id(l), term)
         )
      |> List.map(k => (k, target))
    | None => []
    };
  via_line
  @ (
    [Action.Gesture.Up, Down, Left, Right]
    |> List.concat_map(gesture_options(~info_map, ~term, ~target, ~shard))
    |> List.filter(k =>
         k != Action.UnfoldCall && (k != Action.NegateIf || branch_root)
       )
    |> (ks => ks @ extra @ insist)
    |> List.filter(drives)
    |> List.map(k => (k, target))
  )
  |> List.fold_left(
       (acc, (k, at)) =>
         List.exists(((k', _)) => k' == k, acc) ? acc : acc @ [(k, at)],
       [],
     );
};

/* kinds whose track ends somewhere other than the grabbed element's
   own place in the state (the transform's focus): a fed definition
   spawns the copy the drag follows (D2's duplicate-and-follow), an
   extracted expression that reuses an identical definition dissolves
   into it,
   an expanded wildcard becomes the arms it stood for. (Merges drop
   onto their live twin: merge_drop.) */
let lands_elsewhere = (kind: Action.refactor): bool =>
  switch (kind) {
  | FeedLet
  | ExtractLet
  | ExpandWildcard => true
  | _ => false
  };

/* A proof editor can reuse target geometry with its own theorem context
   and matcher, without enabling source-edit refactors. */
type theorem_source = {
  rules: list(RefactorTheorem.rule),
  env: Environment.t(Exp.t),
  render: Exp.t => Segment.t,
  measure: Segment.t => Measured.t,
  /* Optional logical layout for gesture displacement. The ordinary segment
     still describes the preview/result; soft wrapping must not redirect the
     hand when a proof expression grows across a line boundary. */
  gesture_layout: option(Exp.t => Measured.t),
  accepts: (~before: Exp.t, ~after: Exp.t, Segment.t) => bool,
  prepare:
    (~target: Id.t, ~site: Id.t, Id.t, Util.Direction.t, Exp.t) =>
    option((Exp.t, Id.t, SyntaxCorrespondence.t)),
};

let drag_candidates =
    (
      ~settings: CoreSettings.t=CoreSettings.on,
      ~composites: bool=false,
      /* the drag is already moving: a held value may climb with its line */
      ~continuing: bool=false,
      ~info_map: Statics.Map.t,
      ~term: Exp.t,
      ~measured: Measured.t,
      /* the LIVE projector shapes: projector ids survive transforms,
         so candidate layouts must reserve the same rendered widths —
         measuring with an empty map squeezed sliders to their token
         text and every tween target sat in the wrong geometry */
      ~shape_map: Id.Map.t(ProjectorCore.Shape.t)=Id.Map.empty,
      ~push: bool,
      /* the grabbed token: its tile and SHARD. The shard anchors the
         track when known: a tile's delimiters don't move rigidly (case
         stays, end drops a row on add-arm), so tile-level lookup shows
         zero travel for real moves */
      ~target: Id.t,
      ~shard as grab_shard: option(int),
      ~theorems: option(theorem_source)=?,
      ~only_theorems=false,
      (),
    )
    : list(DragCandidate.t) => {
  let shard_meas = (id: Id.t, m: Measured.t) =>
    switch (grab_shard) {
    | Some(k) when id == target =>
      switch (Id.Map.find_opt(id, m.tiles)) {
      | Some(shards) =>
        switch (List.assoc_opt(k, shards)) {
        | Some(meas) => Some(meas)
        | None => Measured.find_by_id(id, m)
        }
      | None => Measured.find_by_id(id, m)
      }
    | _ => Measured.find_by_id(id, m)
    };
  let mk =
      (~feed_pref: bool=false, (kind: Action.refactor, at: Id.t))
      : option(DragCandidate.t) => {
    /* feeds drag from the BINDING side only (established rule):
       an at-use feed's clone lands where you grabbed — a
       meaningless sliver of a track (it used to drop via the
       zero-track guard; keep_ids shifted the geometry by a
       column and it survived). Killed explicitly; the Down
       retry then picks up the def-host reading when one exists. */
    let at_use_feed =
      kind == FeedLet
      && (
        switch (
          feed_site(~prefer_def_host=feed_pref, ~info_map, ~target, term)
        ) {
        | Some((_, _, Some(_))) => true
        | _ => false
        }
      );
    if (at_use_feed) {
      None;
    } else {
      switch (
        impl(kind).prepare(
          ~info_map,
          ~target=step_target(kind, at, term),
          term,
        )
      ) {
      | None => None
      | Some((term', focus)) =>
        let segment =
          ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term')
          |> SpaceNormalize.go;
        let cand_measured =
          Measured.of_segment(segment, shape_map, Id.Map.empty);
        let (from_id, to_id) =
          drag_anchor(~feed_pref, ~info_map, ~target, kind, term)
          |> Option.value(~default=(target, target));
        let to_pos = (id: Id.t) =>
          shard_meas(id, cand_measured)
          |> Option.map((m: Measured.measurement) => m.origin);
        /* a merge drops the grab onto its twin (D2's drop target): the
           track ends at the twin's LIVE place (merging down, the
           survivor rises into the vacated row — the result alone
           would show no travel) */
        let merge_drop = () =>
          switch (
            merge_target(
              ~info_map,
              ~target=step_target(kind, at, term),
              kind,
              term,
            )
          ) {
          | ([d, ..._], [v, ..._]) =>
            switch (
              Measured.find_by_id(d, measured),
              Measured.find_by_id(v, measured),
              shard_meas(from_id, measured),
            ) {
            | (Some(dm), Some(vm), Some(cm)) =>
              Some(
                {
                  row: vm.origin.row + cm.origin.row - dm.origin.row,
                  col: vm.origin.col + cm.origin.col - dm.origin.col,
                }: Measured.Point.t,
              )
            | _ => None
            }
          | _ => None
          };
        switch (
          shard_meas(from_id, measured),
          switch (kind) {
          | MergeUp
          | MergeDown => merge_drop()
          | _ =>
            switch (to_pos(to_id)) {
            | Some(p) => Some(p)
            | None => lands_elsewhere(kind) ? to_pos(focus) : None
            }
          },
        ) {
        | (Some(cur), Some(tgt)) =>
          let live_rows = total_rows(measured);
          let cand_rows = total_rows(cand_measured);
          let frame =
            switch (kind) {
            | FeedLet when live_rows > cand_rows =>
              /* two-stage: the vacated lines persist as blank
                 until release; everything at/below them holds its
                 live position */
              {
                DragCandidate.shift_from: cur.origin.row,
                shift_rows: live_rows - cand_rows,
                scroll_rows: 0,
              }
            | ExtractLet when cand_rows > live_rows =>
              /* two insertion geometries: a LINE-TAKEOVER extract
                 (the slot starts a line) opens space at-or-above
                 the origin — pin the origin, slide above-content
                 up, bump the scroll at commit. A SUB-SLOT extract
                 (inline fun/arm/chain body) lands the binding
                 WHERE THE DISPLACED BODY SITS — that content's
                 departure IS the target-space opening (duality
                 rule), so it moves WITH the pull: plain candidate
                 frame. (Pinning it overlapped the flyer with the
                 pinned body mid-drag — andrew.) */
              let takeover =
                switch (extract_path(~target, term)) {
                | Some(path) =>
                  let line = lowest_line(path);
                  same_node(line, term)
                  || has_newline(sep_like(Slot.of_exp(line).lead));
                | None => true
                };
              if (takeover) {
                {
                  DragCandidate.shift_from: 0,
                  shift_rows: live_rows - cand_rows,
                  scroll_rows: cand_rows - live_rows,
                };
              } else {
                DragCandidate.no_frame;
              };
            | _ => DragCandidate.no_frame
            };
          let tgt = DragCandidate.frame_point(frame, tgt);
          /* emerge map (dragology's emergeFrom): the spawned clone's
             ids are exactly the FRESH ids of the candidate; both
             walks share traversal order, so they zip against the
             def's ids positionally — no clone lookup needed */
          let emerge =
            kind == FeedLet
              ? feed_copies(~feed_pref, ~info_map, ~target, term, term') : [];
          /* a spawned clone's track ends at the CLONE (the
             occurrence's ids no longer exist in the candidate) */
          let tgt =
            switch (emerge |> List.find_opt(((_, d)) => d == from_id)) {
            | Some((clone_id, _)) =>
              switch (to_pos(clone_id)) {
              | Some(p) => DragCandidate.frame_point(frame, p)
              | None => tgt
              }
            | None => tgt
            };
          let absorb =
            switch (kind) {
            | MergeUp
            | MergeDown
            | ExtractLet =>
              let (d, v) =
                merge_target(
                  ~info_map,
                  ~target=step_target(kind, at, term),
                  kind,
                  term,
                );
              List.length(d) == List.length(v) ? List.combine(d, v) : [];
            | _ => []
            };
          cur.origin != tgt
            ? Some({
                DragCandidate.kind,
                at,
                label: impl(kind).label,
                current: cur.origin,
                target: tgt,
                frame,
                emerge,
                absorb,
                term: term',
                focus,
                segment,
                measured: cand_measured,
              })
            : None;
        | _ => None
        };
      };
    };
  };
  /* NO def-host retry (reverted): a candidate prepared with
     ~prefer_def_host previews a transform the commit (plain go at
     the grab) then contradicts (andrew hit it: preview moved y's
     def, release fed a's binding). INVARIANT: enumeration must stay
     within what the commit re-derives. */
  /* composites: one track per direction for the place family — the
     first that applies starts it (merge, then a plain move, then carry,
     then lift; a value sinks before it feeds) and the ladder runs on
     through the rest */
  let one_place_track = (cs: list(DragCandidate.t)) =>
    composites
      ? cs
        |> List.fold_left(
             (acc, c: DragCandidate.t) => {
               let way = (c: DragCandidate.t) =>
                 compare(c.target.row, c.current.row);
               place_kind(c.kind)
               && List.exists(
                    (c': DragCandidate.t) =>
                      place_kind(c'.kind) && way(c') == way(c),
                    acc,
                  )
                 ? acc : acc @ [c];
             },
             [],
           )
      : cs;
  let moves =
    (
      only_theorems
        ? []
        : drag_kinds(
            ~climb=composites && continuing,
            ~info_map,
            ~term,
            ~push,
            ~target,
            ~shard=grab_shard,
          )
    )
    |> List.filter_map(k => mk(k))
    |> one_place_track
    |> List.fold_left(
         (acc, c: DragCandidate.t) =>
           List.exists((c': DragCandidate.t) => c'.target == c.target, acc)
             ? acc : acc @ [c],
         [],
       );
  /* Infer grips at enclosing rewrite sites. A merger pulls toward its
     surviving occurrence's LIVE position; other movers follow their result
     positions. Explicit choices remain for direct in-place rewrites. */
  let sites =
    find_path(~hit=hit_node(target), term)
    |> Option.value(~default=[])
    |> List.rev;
  let occupied = ref(List.map((c: DragCandidate.t) => c.target, moves));
  let theorems =
    Option.value(
      ~default={
        rules: RefactorTheorem.rules_at(~info_map, ~target),
        env: Environment.empty,
        render: e =>
          ExpToSegment.exp_to_segment(~settings=roundtrip_settings, e)
          |> SpaceNormalize.go,
        measure: segment =>
          Measured.of_segment(segment, shape_map, Id.Map.empty),
        gesture_layout: None,
        accepts: accepts_result(~settings, ~info_map),
        prepare: RefactorTheorem.prepare_details(~info_map),
      },
      theorems,
    );
  let logical_grab =
    lazy(
      theorems.gesture_layout
      |> Option.bind(_, layout => shard_meas(target, layout(term)))
      |> Option.map((m: Measured.measurement) => m.origin)
    );
  let rewrites =
    switch (shard_meas(target, measured)) {
    | None => []
    | Some(anchor) =>
      theorems.rules
      |> List.concat_map((r: RefactorTheorem.rule) =>
           [Util.Direction.Right, Left]
           |> List.concat_map(direction =>
                sites
                /* Reject non-matches and passengers before constructing
                   replacement syntax or re-reading the entire program. */
                |> List.filter(site_exp =>
                     switch (
                       ProofRule.match_equality(
                         ~info_map,
                         ~env=theorems.env,
                         ~direction,
                         r.rule,
                         site_exp,
                       )
                     ) {
                     | None => false
                     | Some((sub, _)) =>
                       !RefactorTheorem.grip_is_passenger(~target, sub)
                     }
                   )
                |> List.filter_map(site_exp => {
                     let site = Exp.rep_id(site_exp);
                     let kind = Action.RewriteTheorem(r.id, direction, site);
                     switch (
                       theorems.prepare(~target, ~site, r.id, direction, term)
                     ) {
                     | None => None
                     | Some((term', focus, correspondence)) =>
                       let result = find_hit(~hit=hit_node(focus), term');
                       let label =
                         r.name
                         ++ (direction == Left ? " ← " : " → ")
                         ++ (
                           result
                           |> Option.map(e =>
                                ExpToSegment.exp_to_segment(
                                  ~settings=roundtrip_settings,
                                  e,
                                )
                                |> SpaceNormalize.go
                                |> Printer.of_segment(
                                     ~holes="?",
                                     ~refractors=[],
                                   )
                                |> String.split_on_char('\n')
                                |> String.concat(" ")
                              )
                           |> Option.value(~default="")
                         );
                       let segment = theorems.render(term');
                       let cand_measured = theorems.measure(segment);
                       let endpoint =
                         (
                           switch (correspondence.destination) {
                           | Some(Live(id)) =>
                             Measured.find_by_id(id, measured)
                             |> Option.map((m: Measured.measurement) =>
                                  m.origin
                                )
                           | Some(Result(id)) =>
                             let physical = () =>
                               Measured.find_by_id(id, cand_measured)
                               |> Option.map((m: Measured.measurement) =>
                                    m.origin
                                  );
                             switch (
                               theorems.gesture_layout,
                               Lazy.force(logical_grab),
                             ) {
                             | (Some(layout), Some(before)) =>
                               switch (shard_meas(id, layout(term'))) {
                               | Some(after) =>
                                 Some(
                                   Measured.Point.{
                                     row:
                                       anchor.origin.row
                                       + after.origin.row
                                       - before.row,
                                     col:
                                       anchor.origin.col
                                       + after.origin.col
                                       - before.col,
                                   },
                                 )
                               | None => physical()
                               }
                             | _ => physical()
                             };
                           | None => None
                           }
                         )
                         |> (
                           pos =>
                             Option.bind(pos, p =>
                               p == anchor.origin ? None : Some(p)
                             )
                         );
                       /* Ancestor rules need an actual moving grip. A direct
                          rewrite can still be chosen when it has no spatial
                          interpretation (e.g. replacing a constant by holes). */
                       if (!hit_node(target, site_exp)
                           && endpoint == None
                           || !
                                theorems.accepts(
                                  ~before=term,
                                  ~after=term',
                                  segment,
                                )) {
                         None;
                       } else {
                         Some((
                           kind,
                           label,
                           term',
                           focus,
                           segment,
                           cand_measured,
                           endpoint,
                           correspondence.copies,
                           correspondence.merges,
                         ));
                       };
                     };
                   })
              )
         )
      |> List.mapi(
           (
             i,
             (
               kind,
               label,
               term',
               focus,
               segment,
               cand_measured,
               endpoint,
               emerge,
               absorb,
             ),
           ) => {
           let rec fallback = col => {
             let p = {
               Measured.Point.row: max(0, anchor.origin.row - 2) + 2 * i,
               col,
             };
             List.mem(p, occupied^) ? fallback(col + 4) : p;
           };
           let endpoint =
             switch (endpoint) {
             | Some(p) when !List.mem(p, occupied^) => p
             | _ => fallback(anchor.origin.col + 16)
             };
           occupied := [endpoint, ...occupied^];
           {
             DragCandidate.kind,
             at: target,
             label,
             current: anchor.origin,
             target: endpoint,
             frame: DragCandidate.no_frame,
             emerge,
             absorb,
             term: term',
             focus,
             segment,
             measured: cand_measured,
           };
         })
    };

  moves @ rewrites;
};

/* drag chaining's "same kind": one constructor, swap indices aside;
   hoist and sink are one motion, and carry/lift its pushes through */
let same_kind = (a: Action.refactor, b: Action.refactor): bool => {
  let family = (k: Action.refactor): Action.refactor =>
    switch (k) {
    | HoistLet
    | SinkLet
    | HoistCarry
    | SinkCarry
    | LiftFunction
    | UnliftFunction => HoistLet
    | MergeUp
    | MergeDown => MergeUp
    | AddParameter
    | RemoveParameter => AddParameter
    | SwapArms(_) => SwapArms(0)
    | SwapParams(_) => SwapParams(0)
    | SwapTuplePat(_) => SwapTuplePat(0)
    | k => k
    };
  family(a) == family(b);
};

/* A drag LADDER's next rung — D2's list reorder: every position of
   one drag is a state of it, nothing committed until release. From
   the state the last rung left, the move of the ladder's family that
   keeps the held element going the way the ladder goes. The held
   element is the grab while it survives, else the last rung's focus
   (a consumed feed's value). Statics are recomputed for that state. */
let next_rung =
    (
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~root: Exp.t,
      ~shape_map: Id.Map.t(ProjectorCore.Shape.t)=Id.Map.empty,
      ~push: bool,
      ~family: Action.refactor,
      ~held: (Id.t, option(int)),
      prev: DragCandidate.t,
    )
    : option((DragCandidate.t, (Id.t, option(int)))) => {
  switch (family) {
  /* A theorem choice is one rewrite, never a composite ladder. */
  | RewriteTheorem(_) => None
  | _ =>
    let composites = settings.drag_composites;
    /* extract takes a value from inline to bound: only ever a drag's
       first step (after a lift the value sits inline in the helper's
       body, and extracting it again would loop) */
    let continues = (k: Action.refactor) =>
      k != ExtractLet
      && (
        same_kind(family, k)
        || composites
        && place_kind(family)
        && place_kind(k)
      );
    /* a merge dissolves what the ladder held: nothing left to move */
    let ended =
      switch (prev.kind) {
      | MergeUp
      | MergeDown => composites
      | _ => false
      };
    let held =
      Id.Map.mem(fst(held), prev.measured.tiles)
        ? held : (prev.focus, None);
    /* the way a ladder goes. A move of where a value lives goes up its
       path of places (extract, hoist, lift, merge up) or down it (sink,
       move into caller, merge down, feed), whatever the geometry: a run of
       feeds visits uses left and right of the definition, and must not
       turn into an extract. Other ladders go forward or back in reading
       order. */
    let way = (c: DragCandidate.t) =>
      switch (place_way(c.kind)) {
      | Some(w) => w
      | None =>
        compare(
          (c.target.row, c.target.col),
          (c.current.row, c.current.col),
        )
      };
    ended
      ? None
      : drag_candidates(
          ~settings,
          ~composites,
          ~continuing=true,
          ~info_map=statics_of(~settings, ~info_map, ~root, prev.term),
          ~term=prev.term,
          ~measured=prev.measured,
          ~shape_map,
          ~push,
          ~target=fst(held),
          ~shard=snd(held),
          (),
        )
        |> List.find_opt((c: DragCandidate.t) =>
             continues(c.kind) && way(c) == way(prev)
           )
        |> Option.map(c => (c, held));
  };
};

/* the INSIST tier: remedied moves a dead press escalates to on a
   second press (web tracks the pending state; menu lists these as
   their own entries per P10) */
