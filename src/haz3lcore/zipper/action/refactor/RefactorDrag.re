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

/* mergeInto targets (D2 emerge reversed): when this invocation will
   ABSORB (identical definitions merging) or GLOM (extract reusing an
   identical existing def), the dissolved window's ids + the
   surviving window's ids — staged for the convergence flight. */
let merge_target =
    (~info_map as _, ~target, kind: Action.refactor, term)
    : (list(Id.t), list(Id.t)) => {
  let line_ids = (e: Exp.t): list(Id.t) =>
    switch (IdTagged.term_of(e)) {
    | Let(p, d, _) =>
      IdTagged.ids(e) @ pat_subtree_ids(p) @ exp_subtree_ids(d)
    | _ => IdTagged.ids(e)
    };
  switch (kind) {
  | MergeUp =>
    switch (merge_site_up(~target, term)) {
    | Some((p, l)) => (line_ids(l), line_ids(p))
    | None => ([], [])
    }
  | MergeDown =>
    switch (merge_site_down(~target, term)) {
    | Some(l) =>
      switch (def_line_of(l)) {
      | Some((_, lbody)) => (line_ids(l), line_ids(lbody))
      | None => ([], [])
      }
    | None => ([], [])
    }
  | ExtractLet =>
    switch (extract_path(~target, term)) {
    | Some(path) =>
      let t = List.nth(path, List.length(path) - 1);
      let line = lowest_line(path);
      let blocked =
        crossed_rec_binders(line, path) |> List.exists(n => mentions(n, t));
      let rec host = (path: list(Exp.t)) =>
        switch (path) {
        | [parent, child, ..._] when same_node(child, line) => Some(parent)
        | [_, ...rest] => host(rest)
        | [] => None
        };
      switch (blocked ? None : host(path)) {
      | Some(h) =>
        switch (IdTagged.term_of(h)) {
        | Let(lp, ldef, lbody)
            when same_node(lbody, line) && eq_defs(ldef, t) =>
          switch (let_head_name(lp)) {
          | Some(nm) when !List.mem(nm, binders_over(Exp.rep_id(t), lbody)) => (
              exp_subtree_ids(t),
              exp_subtree_ids(ldef),
            )
          | _ => ([], [])
          }
        | _ => ([], [])
        }
      | None => ([], [])
      };
    | None => ([], [])
    }
  | _ => ([], [])
  };
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

/* === Drag candidates (pointer front-end to the gesture system) ===
 * For each direction, resolve the gesture at the caret, prepare it,
 * and measure the result — measures only, no statics/eval/view
 * (dragology's isTracking, done with Measured). Each candidate
 * defines a TRACK from the anchor's current position to its position
 * in the candidate. Anchors are (from, to) id pairs: the grabbed
 * construct for movement kinds; def -> fed occurrence for feeds (the
 * value travels, the binding doesn't). Degenerate tracks (anchor
 * doesn't move) are dropped — that transform stays arrows/menu-only.
 * Coincident targets keep the first in direction order (ambiguity
 * policy v1). */

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
    dir: Action.Gesture.t,
    kind: Action.refactor,
    label: string,
    current: Measured.Point.t, /* track start (live layout) */
    target: Measured.Point.t, /* track end (screen frame) */
    frame,
    /* entering-token ORIGINS (dragology's emergeFrom): a transform
       that DUPLICATES content (feed with surviving uses) maps each
       fresh copy id to the live id it emerges from — the ghost
       travels from the source instead of growing in place */
    emerge: list((Id.t, Id.t)),
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

let drag_candidates =
    (
      ~info_map: Statics.Map.t,
      ~term: Exp.t,
      ~measured: Measured.t,
      /* the LIVE projector shapes: projector ids survive transforms,
         so candidate layouts must reserve the same rendered widths —
         measuring with an empty map squeezed sliders to their token
         text and every tween target sat in the wrong geometry */
      ~shape_map: Id.Map.t(ProjectorCore.Shape.t)=Id.Map.empty,
      z: Zipper.t,
    )
    : list(DragCandidate.t) =>
  switch (Indicated.index(z)) {
  | None => []
  | Some(target) =>
    /* the grabbed SHARD anchors the track when known: a tile's
       delimiters don't move rigidly (case stays, end drops a row on
       add-arm), so tile-level lookup shows zero travel for real
       moves */
    let grab_shard = Indicated.shard_index(z);
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
        (~feed_pref: bool=false, dir: Action.Gesture.t)
        : option(DragCandidate.t) =>
      switch (gesture(~info_map, ~term, dir, z)) {
      | None => None
      | Some(kind) =>
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
          let prepare =
            switch (kind) {
            | FeedLet => feed_prepare(~prefer_def_host=feed_pref)
            | _ => impl(kind).prepare
            };
          switch (prepare(~info_map, ~target, term)) {
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
            switch (
              shard_meas(from_id, measured),
              /* the grabbed id can vanish in a candidate (rare); the
                 focus is the moved content's id — try it second */
              switch (to_pos(to_id)) {
              | Some(p) => Some(p)
              | None => to_pos(focus)
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
                switch (kind) {
                | FeedLet =>
                  switch (
                    feed_plan(
                      ~prefer_def_host=feed_pref,
                      ~info_map,
                      ~target,
                      term,
                    )
                  ) {
                  | Some(Feed(_, def, _)) =>
                    let live = exp_subtree_ids(term);
                    let fresh =
                      exp_subtree_ids(term')
                      |> List.filter(id => !List.mem(id, live));
                    let d = exp_subtree_ids(def);
                    /* combine raises on length mismatch — guard FIRST
                       (the eager-evaluation gotcha) */
                    let zip = ids =>
                      List.length(ids) == List.length(d)
                        ? List.combine(ids, d) : [];
                    switch (zip(fresh)) {
                    | [] =>
                      /* reparse demanded a paren wrapper: the parens
                         are genuinely NEW material (no source) — pair
                         the inner clone, found structurally */
                      let fresh_parens = (e: Exp.t) =>
                        switch (IdTagged.term_of(e)) {
                        | Parens(_) =>
                          IdTagged.ids(e)
                          |> List.exists(id => List.mem(id, fresh))
                        | _ => false
                        };
                      switch (find_hit(~hit=fresh_parens, term')) {
                      | Some(p) =>
                        switch (IdTagged.term_of(p)) {
                        | Parens(inner) => zip(exp_subtree_ids(inner))
                        | _ => []
                        }
                      | None => []
                      };
                    | pairs => pairs
                    };
                  | _ => []
                  }
                | _ => []
                };
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
              cur.origin != tgt
                ? Some({
                    DragCandidate.dir,
                    kind,
                    label: impl(kind).label,
                    current: cur.origin,
                    target: tgt,
                    frame,
                    emerge,
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
    /* NO def-host retry (reverted): the commit dispatches the plain
       RefactorGesture, which RE-RESOLVES the position with default
       preferences — a candidate prepared with ~prefer_def_host
       previews a transform the commit then contradicts (andrew hit
       it: preview moved y's def, release fed a's binding).
       INVARIANT: enumeration must stay within what the gesture
       dispatch re-derives. The occurrence-inside-def spot is dead
       for drag until the commit path can carry a resolution. */
    [Action.Gesture.Up, Down, Left, Right]
    |> List.filter_map(dir => mk(dir))
    |> List.fold_left(
         (acc, c: DragCandidate.t) =>
           List.exists((c': DragCandidate.t) => c'.target == c.target, acc)
             ? acc : acc @ [c],
         [],
       );
  };

/* the INSIST tier: remedied moves a dead press escalates to on a
   second press (web tracks the pending state; menu lists these as
   their own entries per P10) */
