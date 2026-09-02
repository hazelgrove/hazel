/* Completion-triggered local re-indentation (plans/local-reformat.md,
 * gated by CoreSettings.auto_reindent).
 *
 * When an edit completes a tile (a shard gloms onto its form, via typed
 * delimiter or put-down), the content it absorbed as children gets
 * re-indented, per child:
 * - deeply settled child (no incomplete tiles): canonical indentation
 *   is unambiguous — recompute every line (also repairs enter-indent's
 *   type-time ambiguity on continuation lines);
 * - unsettled child: its whitespace is load-bearing for canonical
 *   completion, so only translate uniformly (preserving the relative
 *   comparisons completion reads), and not at all if the shift would
 *   clamp a line at column 0 (uniformity would break). */

let incomplete_ids = (seg: Segment.t): Id.Map.t(unit) =>
  Segment.incomplete_tiles_deep(seg)
  |> List.fold_left(
       (acc, t: Tile.t) => Id.Map.add(t.id, (), acc),
       Id.Map.empty,
     );

let snapshot = (~enabled: bool, z: Zipper.t): option(Id.Map.t(unit)) =>
  enabled ? Some(incomplete_ids(Zipper.unselect_and_zip(z))) : None;

/* First linebreak (textual order, descending into children) and the
 * count of space pieces following it at its own level */
let rec first_linebreak = (seg: Segment.t): option((Id.t, int)) =>
  switch (seg) {
  | [] => None
  | [Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
    let (spaces, _) = Segment.split_space_run(rest);
    Some((w.id, List.length(spaces)));
  | [Tile(t), ...rest] =>
    switch (List.find_map(first_linebreak, t.children)) {
    | Some(r) => Some(r)
    | None => first_linebreak(rest)
    }
  | [_, ...rest] => first_linebreak(rest)
  };

/* Minimum current indent over all linebreaks in the segment (deep) */
let rec min_indent = (seg: Segment.t): option(int) => {
  let min_opt = (a, b) =>
    switch (a, b) {
    | (Some(a), Some(b)) => Some(min(a, b))
    | (Some(a), None)
    | (None, Some(a)) => Some(a)
    | (None, None) => None
    };
  switch (seg) {
  | [] => None
  | [Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
    let (spaces, _) = Segment.split_space_run(rest);
    min_opt(Some(List.length(spaces)), min_indent(rest));
  | [Tile(t), ...rest] =>
    List.fold_left(
      (acc, ch) => min_opt(acc, min_indent(ch)),
      min_indent(rest),
      t.children,
    )
  | [_, ...rest] => min_indent(rest)
  };
};

/* Uniformly shift the indentation of every line in the segment,
 * reusing existing space pieces where possible */
let shift = (delta: int, seg: Segment.t): Segment.t => {
  let rec level = (seg: Segment.t): Segment.t =>
    switch (seg) {
    | [] => []
    | [Piece.Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
      let (spaces, rest) = Segment.split_space_run(rest);
      let n = max(0, List.length(spaces) + delta);
      let spaces =
        n <= List.length(spaces)
          ? spaces |> List.filteri((i, _) => i < n)
          : spaces
            @ List.init(n - List.length(spaces), _ =>
                Piece.Secondary(Secondary.mk_space(Id.mk()))
              );
      [Piece.Secondary(w)] @ spaces @ level(rest);
    | [p, ...rest] => [p, ...level(rest)]
    };
  Segment.map_deep(level, seg);
};

/* Per newly-completed tile, a plan per child:
   - Fix: child contains no incomplete tiles (deeply settled), so its
     canonical indentation is unambiguous — recompute every line. This
     also repairs enter-indent's type-time ambiguity (continuation
     lines are indented at base while their successor is unknown).
   - Shift(d): child contains incomplete tiles, so its whitespace is
     load-bearing for completion — only translate uniformly, preserving
     the relative comparisons completion reads. Skipped when the shift
     would clamp a line at column 0 (uniformity would break). */
type child_plan =
  | Fix
  | Shift(int)
  | Leave;

let plan_tile = (full: Segment.t, t: Tile.t): list(child_plan) =>
  t.children
  |> List.map(child =>
       switch (first_linebreak(child)) {
       | None => Leave
       | Some((lb_id, current)) =>
         if (Id.Map.is_empty(incomplete_ids(child))) {
           Fix;
         } else {
           let canonical = Indentation.level_of(~target_id=lb_id, full);
           let delta = canonical - current;
           if (delta == 0) {
             Leave;
           } else {
             switch (min_indent(child)) {
             | Some(m) when m + delta < 0 => Leave /* would clamp */
             | _ => Shift(delta)
             };
           };
         }
       }
     );

let rec find_tiles = (ids: Id.Map.t(unit), seg: Segment.t): list(Tile.t) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        (Id.Map.mem(t.id, ids) ? [t] : [])
        @ List.concat_map(find_tiles(ids), t.children)
      | _ => []
      },
    seg,
  );

let apply_plans =
    (
      ~indent_map: Lazy.t(Id.Map.t(int)),
      plans: Id.Map.t(list(child_plan)),
      seg: Segment.t,
    ) =>
  seg
  |> List.map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         switch (Id.Map.find_opt(t.id, plans)) {
         | Some(child_plans) =>
           Piece.Tile({
             ...t,
             children:
               List.mapi(
                 (i, ch) =>
                   switch (List.nth_opt(child_plans, i)) {
                   | Some(Fix) =>
                     Indentation.fix_indentation_in_segment(
                       Lazy.force(indent_map),
                       ch,
                     )
                   | Some(Shift(d)) => shift(d, ch)
                   | _ => ch
                   },
                 t.children,
               ),
           })
         | None => p
         }
       | p => p
       }
     );

/* === Region re-indent (paste-like insertions) ===
   Trigger: linebreaks present after the action but absent before —
   the inserted material's own lines (copied pieces re-mint ids on
   paste; text paste mints fresh ids). Policy unchanged from the
   completion trigger: buffer settled -> exact canonical per new
   line; unsettled -> uniform clamp-guarded shift anchored at the
   first new line (whitespace stays load-bearing for completion).
   Caveat: a caret sitting inside a new line's indentation run splits
   it across zipper sub-segments; the remainder is left alone. */

let all_piece_ids = (seg: Segment.t): Id.Map.t(unit) =>
  Segment.ids(seg)
  |> List.fold_left((acc, id) => Id.Map.add(id, (), acc), Id.Map.empty);

let snapshot_pieces = (~enabled: bool, z: Zipper.t): option(Id.Map.t(unit)) =>
  enabled ? Some(all_piece_ids(Zipper.unselect_and_zip(z))) : None;

let rec collect_lb_indents = (seg: Segment.t): list((Id.t, int)) =>
  switch (seg) {
  | [] => []
  | [Piece.Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
    let (spaces, rest) = Segment.split_space_run(rest);
    [(w.id, List.length(spaces)), ...collect_lb_indents(rest)];
  | [Tile(t), ...rest] =>
    List.concat_map(collect_lb_indents, t.children)
    @ collect_lb_indents(rest)
  | [_, ...rest] => collect_lb_indents(rest)
  };

let set_lb_indents = (targets: Id.Map.t(int), seg: Segment.t): Segment.t => {
  let rec level = (seg: Segment.t): Segment.t =>
    switch (seg) {
    | [] => []
    | [Piece.Secondary(w) as p, ...rest] when Secondary.is_linebreak(w) =>
      let (spaces, rest) = Segment.split_space_run(rest);
      let spaces =
        switch (Id.Map.find_opt(w.id, targets)) {
        | None => spaces
        | Some(n) =>
          List.length(spaces) >= n
            ? spaces |> List.filteri((i, _) => i < n)
            : spaces
              @ List.init(n - List.length(spaces), _ =>
                  Piece.Secondary(Secondary.mk_space(Id.mk()))
                )
        };
      [p] @ spaces @ level(rest);
    | [p, ...rest] => [p, ...level(rest)]
    };
  Segment.map_deep(level, seg);
};

let go_region =
    (~before_pieces: option(Id.Map.t(unit)), z: Zipper.t): Zipper.t =>
  switch (before_pieces) {
  | None => z
  | Some(before) =>
    let full = Zipper.unselect_and_zip(z);
    let new_lbs =
      collect_lb_indents(full)
      |> List.filter(((id, _)) => !Id.Map.mem(id, before));
    switch (new_lbs) {
    | [] => z
    | [(first_id, first_cur), ..._] =>
      let indent_map = Indentation.level_map(full);
      let settled = Id.Map.is_empty(incomplete_ids(full));
      let targets =
        settled
          ? new_lbs
            |> List.filter_map(((id, _)) =>
                 Id.Map.find_opt(id, indent_map) |> Option.map(l => (id, l))
               )
          : {
            let canonical =
              Id.Map.find_opt(first_id, indent_map)
              |> Option.value(~default=first_cur);
            let delta = canonical - first_cur;
            delta == 0
            || List.exists(((_, cur)) => cur + delta < 0, new_lbs)
              ? [] : new_lbs |> List.map(((id, cur)) => (id, cur + delta));
          };
      targets == []
        ? z
        : ZipperBase.MapSegment.go(
            set_lb_indents(targets |> List.to_seq |> Id.Map.of_seq),
            z,
          );
    };
  };

let go = (~before: option(Id.Map.t(unit)), z: Zipper.t): Zipper.t =>
  switch (before) {
  | None => z
  | Some(before) =>
    let full = Zipper.unselect_and_zip(z);
    let after = incomplete_ids(full);
    let completed =
      Id.Map.filter((id, _) => !Id.Map.mem(id, after), before);
    if (Id.Map.is_empty(completed)) {
      z;
    } else {
      let plans =
        find_tiles(completed, full)
        |> List.filter_map((t: Tile.t) => {
             let child_plans = plan_tile(full, t);
             List.exists(p => p != Leave, child_plans)
               ? Some((t.id, child_plans)) : None;
           })
        |> List.to_seq
        |> Id.Map.of_seq;
      let indent_map = lazy(Indentation.level_map(full));
      Id.Map.is_empty(plans)
        ? z : ZipperBase.MapSegment.go(apply_plans(~indent_map, plans), z);
    };
  };

/* Bracket an edit with the completion trigger: snapshot incomplete
   tiles, run the edit, re-indent what it completed */
let around =
    (~enabled: bool, z: Zipper.t, f: Zipper.t => option(Zipper.t))
    : option(Zipper.t) => {
  let before = snapshot(~enabled, z);
  f(z) |> Option.map(go(~before));
};

let around_res =
    (~enabled: bool, z: Zipper.t, f: Zipper.t => result(Zipper.t, 'e))
    : result(Zipper.t, 'e) => {
  let before = snapshot(~enabled, z);
  f(z) |> Result.map(go(~before));
};
