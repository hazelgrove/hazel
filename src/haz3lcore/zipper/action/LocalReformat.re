/* Completion-triggered local re-indentation (plans/local-reformat.md,
 * gated by CoreSettings.auto_reindent).
 *
 * When an edit completes a tile (a shard gloms onto its form, via typed
 * delimiter or backpack drop), the content it absorbed as children gets
 * re-indented, per child:
 * - deeply settled child (no incomplete tiles): canonical indentation
 *   is unambiguous — recompute every line (also repairs enter-indent's
 *   type-time ambiguity on continuation lines);
 * - unsettled child: its whitespace is load-bearing for canonical
 *   completion, so only translate uniformly (preserving the relative
 *   comparisons completion reads), and not at all if the shift would
 *   clamp a line at column 0 (uniformity would break). */

let rec incomplete_ids = (acc: Id.Map.t(unit), seg: Segment.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        let acc = Tile.is_complete(t) ? acc : Id.Map.add(t.id, (), acc);
        List.fold_left(incomplete_ids, acc, t.children);
      | _ => acc
      },
    acc,
    seg,
  );

let snapshot = (~enabled: bool, z: Zipper.t): option(Id.Map.t(unit)) =>
  enabled
    ? Some(incomplete_ids(Id.Map.empty, Zipper.unselect_and_zip(z))) : None;

/* Split the leading run of space pieces off a segment */
let split_spaces = (seg: Segment.t): (list(Piece.t), Segment.t) => {
  let rec go = (acc, seg: Segment.t) =>
    switch (seg) {
    | [Piece.Secondary(s) as p, ...rest] when Secondary.is_space(s) =>
      go([p, ...acc], rest)
    | _ => (List.rev(acc), seg)
    };
  go([], seg);
};

/* First linebreak (textual order, descending into children) and the
 * count of space pieces following it at its own level */
let rec first_linebreak = (seg: Segment.t): option((Id.t, int)) =>
  switch (seg) {
  | [] => None
  | [Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
    let (spaces, _) = split_spaces(rest);
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
    let (spaces, _) = split_spaces(rest);
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
let rec shift = (delta: int, seg: Segment.t): Segment.t =>
  switch (seg) {
  | [] => []
  | [Piece.Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
    let (spaces, rest) = split_spaces(rest);
    let n = max(0, List.length(spaces) + delta);
    let spaces =
      n <= List.length(spaces)
        ? spaces |> List.filteri((i, _) => i < n)
        : spaces
          @ List.init(n - List.length(spaces), _ =>
              Piece.Secondary(Secondary.mk_space(Id.mk()))
            );
    [Piece.Secondary(w)] @ spaces @ shift(delta, rest);
  | [Piece.Tile(t), ...rest] => [
      Piece.Tile({
        ...t,
        children: List.map(shift(delta), t.children),
      }),
      ...shift(delta, rest),
    ]
  | [p, ...rest] => [p, ...shift(delta, rest)]
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
         if (Id.Map.is_empty(incomplete_ids(Id.Map.empty, child))) {
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

let go = (~before: option(Id.Map.t(unit)), z: Zipper.t): Zipper.t =>
  switch (before) {
  | None => z
  | Some(before) =>
    let full = Zipper.unselect_and_zip(z);
    let after = incomplete_ids(Id.Map.empty, full);
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
