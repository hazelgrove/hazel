/* Indentation Calculation
 * ========================
 *
 * This module computes indentation levels for linebreaks in a segment.
 * The main entry point is `level_map` which returns a map from linebreak
 * IDs to their indentation level (number of spaces).
 *
 * CONTINUATION LINE DESIGN DECISION:
 * ----------------------------------
 * When content starts on the same line as an indentation-creating construct
 * (e.g., `let z = 4` vs `let z =\n4`), and continues on subsequent lines,
 * we face an ambiguity at typing time: we don't know if what follows the
 * linebreak will be continuation content (`+ 4`) or a completing keyword (`in`).
 *
 * - KNOWN CASE: Linebreak immediately after `=` (prev=None in child context)
 *   We know subsequent content is in the child, so we indent immediately.
 *
 * - AMBIGUOUS CASE: Content before linebreak (prev=Some(_), next=None)
 *   At typing time, next is unknown. We use conservative behavior (no indent).
 *   After Format (Cmd+S), when next is known, we indent if next=Some(_).
 *
 * This is implemented via the rule:
 *   `(_, Some(_)) when not_top => base + 2`
 * which only fires when there IS content after the linebreak (known structure).
 *
 * See Test_Indentation.re for comprehensive examples of both behaviors.
 */

/* Remove non-contentful items (whitespace and concave grout) */
let trim_non_content: Segment.t => Segment.t =
  List.filter_map(
    fun
    | Piece.Grout({shape: Concave, _}) => None
    | Secondary(s) when Secondary.is_space(s) => None
    | p => Some(p),
  );

/* Compute context (effective_prev, next, effective_next) for each piece in one pass.
 * - effective_prev: skips linebreaks to find the last contentful piece.
 *   Convex grout COUNTS as content (an atom, like a literal): a hole
 *   filling a branch must anchor the next line's indentation exactly
 *   as a literal would, else the incrementor/child rules re-fire and
 *   every hole-bearing line drifts deeper (empty if/then branches).
 * - next: immediate next piece (raw)
 * - effective_next: skips linebreaks to find next contentful piece */
let compute_context =
    (seg: Segment.t)
    : list((option(Piece.t), option(Piece.t), option(Piece.t))) => {
  /* Find next non-linebreak piece by scanning ahead */
  let rec find_effective_next = (xs: list(Piece.t)): option(Piece.t) =>
    switch (xs) {
    | [] => None
    | [Secondary(s), ...rest] when Secondary.is_linebreak(s) =>
      find_effective_next(rest)
    | [x, ..._] => Some(x)
    };

  let rec go =
          (acc, xs: list(Piece.t), last_contentful: option(Piece.t))
          : list((option(Piece.t), option(Piece.t), option(Piece.t))) =>
    switch (xs) {
    | [] => List.rev(acc)
    | [x, ...rest] =>
      let effective_prev = last_contentful;
      let next =
        switch (rest) {
        | [] => None
        | [n, ..._] => Some(n)
        };
      let effective_next = find_effective_next(rest);
      let new_last_contentful =
        switch (x) {
        | Secondary(s) when Secondary.is_linebreak(s) => last_contentful /* Skip linebreaks */
        /* a DERIVED HOLE never anchors indent: it is a presumption,
           not content — the (incrementor, prev_is_lb) chain reads
           through it; hole-adjacent cases are governed by the rule
           ORDER below (end-of-context rules precede the child +2) */
        | Grout(_) => last_contentful
        | _ => Some(x) /* Update for contentful pieces */
        };
      go(
        [(effective_prev, next, effective_next), ...acc],
        rest,
        new_last_contentful,
      );
    };
  go([], seg, None);
};

/* Check if a tile is a case rule (label is ["|", "=>"]) */
let is_case_rule_tile = (t: Tile.t): bool => t.label == ["|", "=>"];

/* This does not strictly 'complete' a segment but rather does a
 * rough version of it that suffices for indentation calculation.
 * Tail-recursive in segment length (recursion depth is bounded by
 * the number of incomplete tiles, not the number of pieces).
 *
 * EXCEPTION: Case rules are NOT completed. Unlike let bindings where
 * swallowing subsequent content as body makes sense, case rules are
 * fundamentally sibling-oriented - they don't nest into each other.
 * Completing an incomplete `|` by swallowing everything after it as
 * body content produces wrong indentation for what should be siblings. */
let rec shallow_complete_segment = (seg: Segment.t): Segment.t => {
  let rec go = (acc, seg: Segment.t): Segment.t =>
    switch (seg) {
    | [] => List.rev(acc)
    | [Tile(t), ...rest] when !Tile.is_complete(t) && !is_case_rule_tile(t) =>
      List.rev([
        Piece.Tile({
          ...t,
          shards: List.init(List.length(t.label), i => i),
          children: t.children @ [shallow_complete_segment(rest)],
          /* Note: Potentially wrong number of children */
        }),
        ...acc,
      ])
    | [p, ...rest] => go([p, ...acc], rest)
    };
  go([], seg);
};

/* Find the shortest prefix of the segment containing all incomplete tiles
 * followed by two consecutive linebreaks (aka a blank line) */
let incomplete_subseg_before_blank_line =
    (seg: Segment.t): option((Segment.t, Segment.t)) => {
  let rec find_split_point =
          (seg: Segment.t, acc: Segment.t, incomplete_before: bool)
          : option((Segment.t, Segment.t)) => {
    switch (seg) {
    | [] => None
    | [Secondary(w1) as p, Secondary(w2), ...rest]
        when Secondary.is_linebreak(w1) && Secondary.is_linebreak(w2) =>
      let incomplete_before = incomplete_before || !Piece.is_complete(p);
      if (incomplete_before) {
        /* Note: Leaves one linebreak in and one out (empty line) */
        Some((
          List.rev([Piece.Secondary(w1), ...acc]),
          [Secondary(w2), ...rest],
        ));
      } else {
        find_split_point(
          rest,
          [Secondary(w2), Secondary(w1), ...acc],
          incomplete_before,
        );
      };
    | [p, ...rest] =>
      find_split_point(
        rest,
        [p, ...acc],
        incomplete_before || !Piece.is_complete(p),
      )
    };
  };
  find_split_point(seg, [], false);
};

/* When a segment is incomplete, we try to complete it before calculating
 * indentation. This is necessarily a heuristic process. One obvious way
 * would be to consider dropping the missing shards at the cursor, but making
 * this calcuation cursor-sensitive (and hence active on movement) is
 * potentially expensive and janky. Thus we use a different indication
 * of user intent: leaving a blank line. In effect, this attempts to
 * completes the segment in the specific case where all incomplete tiles
 * in the segment are found before a blank line (two consecutive linebreaks).
 * There are many cases where this won't apply, but it is sufficient to
 * ensure non-janky left-to-right entry of a new definition seperated
 * from the rest of the existing below bidelimited context by an empty line,
 * assuming that the below bidelimited context doesn't contain incomplete
 * tiles at the top level. */
let complete_segment = (seg: Segment.t): Segment.t => {
  switch (incomplete_subseg_before_blank_line(seg)) {
  | None => shallow_complete_segment(seg)
  | Some((before, after)) => shallow_complete_segment(before) @ after
  };
};

let is_comma = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => t.label == [","]
  | _ => false
  };

let is_case_rule = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["|", "=>"], _}) => true
  | _ => false
  };

/* An incomplete case rule is just the `|` without the `=>`.
 * This has shards [0] instead of [0, 1]. */
let is_incomplete_case_rule = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["|", "=>"], shards, _}) => shards == [0]
  | _ => false
  };

/* Check if a segment has any contentful pieces (not just whitespace/grout) */
let has_content = (seg: Segment.t): bool =>
  List.exists(
    fun
    | Piece.Secondary(s) =>
      !Secondary.is_space(s) && !Secondary.is_linebreak(s)
    | Piece.Grout(_) => false
    | _ => true,
    seg,
  );

/* A complete case rule with a non-empty body. After such a rule,
 * we expect the next rule at the same level, not more body content. */
let is_complete_case_rule_with_body = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["|", "=>"], shards, children, _}) =>
    /* Complete = has both shards [0, 1] */
    shards == [0, 1]
    /* Body is children[1], check if it has content */
    && List.length(children) >= 2
    && has_content(List.nth(children, 1))
  | _ => false
  };

/* Check if piece is convex grout (hole for missing expression/pattern) */
let is_convex_grout = (p: Piece.t): bool =>
  switch (p) {
  | Grout({shape: Convex, _}) => true
  | _ => false
  };

let ends_with_in = (t: Tile.t): bool =>
  switch (t.label |> List.rev) {
  | ["in", ..._] => true
  | _ => false
  };

/* Linebreaks following these tiles should increment the indent. Basically
 * any non-infix-operator tiles which are concave on the right, except
 * for definition forms */
let is_incrementor = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (Tile.shapes(t)) {
    | _ when ends_with_in(t) => false
    | (_, Concave(_)) when List.length(t.label) >= 2 => true
    | _ => false
    }
  | _ => false
  };

/* Exception for short-circuit lookup of single linebreak's indentation */
exception Found_indent(int);

let rec go =
        (~not_top, ~target_id: option(Id.t)=?, base: int, seg: Segment.t)
        : Id.Map.t(int) => {
  let complete_trimmed_seg = complete_segment(trim_non_content(seg));
  let context = compute_context(complete_trimmed_seg);
  /* Mark pieces whose immediate predecessor (in the trimmed segment)
     is a linebreak: a run of linebreaks must share one indent, not
     staircase (each one's EFFECTIVE prev is still the incrementor) */
  let prev_is_lb = {
    let is_lb = (pc: Piece.t) =>
      switch (pc) {
      | Secondary(w) => Secondary.is_linebreak(w)
      | _ => false
      };
    /* grout is transparent to the marking, as to the anchor chain */
    let rec mark = (acc, flag, xs) =>
      switch (xs) {
      | [] => List.rev(acc)
      | [Piece.Grout(_), ...rest] => mark([flag, ...acc], flag, rest)
      | [x, ...rest] => mark([flag, ...acc], is_lb(x), rest)
      };
    mark([], false, complete_trimmed_seg);
  };
  /* stack-safe zip (List.combine is not tail-recursive) */
  let context =
    List.rev(List.rev_map2((ctx, lb) => (ctx, lb), context, prev_is_lb));
  let (_, map) =
    List.fold_left2(
      ((level: int, map: Id.Map.t(int)), p: Piece.t, ctx) => {
        let ((prev, next, effective_next), prev_is_lb) = ctx;
        switch (p) {
        | Secondary(w) when Secondary.is_linebreak(w) =>
          let indent =
            switch (prev, next) {
            | (_, Some(next)) when is_comma(next) => base + 2
            | (Some(prev), _) when is_comma(prev) => base + 2
            /* Incomplete case rules (just `|`) shouldn't increment.
             * An incomplete `|` is Concave on right, so would match
             * is_incrementor without this check. */
            | (Some(prev), _) when is_incomplete_case_rule(prev) => base
            /* After a complete case rule WITH a body, we expect the next
             * rule at the same level. Don't indent for "next rule" position. */
            | (Some(prev), _) when is_complete_case_rule_with_body(prev) => base
            /* only the FIRST linebreak after an incrementor takes
               the +2; consecutive linebreaks inherit its level */
            /* end-of-context first (before the incrementor and child
               +2 rules): a child whose remaining material is only its
               derived hole (if/then's else line, the let's in line,
               Enter in empty parens) stays at its entry level — the
               increments are for content ahead, and a hole is not
               content */
            | (_, None) when not_top => base
            | _ when not_top && effective_next == None => base
            | (Some(prev), _) when is_incrementor(prev) =>
              prev_is_lb ? level : level + 2
            | (None, _) when not_top => base + 2
            /* Check effective_next (skipping linebreaks) for case rule */
            | _ when Option.map(is_case_rule, effective_next) == Some(true) => base
            | (_, Some(next)) when is_case_rule(next) => base
            | (_, None) => base
            /* If next is linebreak but eff_next is None, effectively at end */
            | _ when effective_next == None => base
            | (_, Some(p)) when Piece.is_infix_delimiter_op_prefix(p) =>
              /* Special case for kw prefixes */
              base
            /* Continuation lines in children: when in child context with
             * content before and after the linebreak, use child indentation.
             * Note: This only works after Format, not during auto-indent,
             * because at typing time next is unknown.
             * A DERIVED HOLE is not user content: a presumption owed
             * after the linebreak (e.g. an unclosed `let` above
             * absorbing later lines, its owed body hole trailing the
             * caret) must not indent the user's fresh line — the user
             * has written nothing there yet (andrew's Enter-indent
             * repro, 2026-07-22). For real content, an incrementor
             * earlier in the child (fun ->) may have RAISED the
             * running level; sibling lines inherit it — base+2 alone
             * flattened every let-chain line after the first. */
            | (_, Some(Piece.Grout(_))) when not_top => level
            | (_, Some(_)) when not_top => max(level, base + 2)
            | (_, Some(_)) => level
            };
          switch (target_id) {
          | Some(id) when Id.equal(w.id, id) => raise(Found_indent(indent))
          | Some(_) => (indent, map) /* target mode: skip map add */
          | None => (indent, Id.Map.add(w.id, indent, map))
          };
        | Secondary(_)
        | Grout(_)
        | Projector(_) => (level, map)
        | Tile(t) =>
          switch (target_id) {
          | Some(_) =>
            /* target mode: just recurse, don't accumulate */
            List.iter(
              child => ignore(go(~not_top=true, ~target_id?, level, child)),
              t.children,
            );
            (level, map);
          | None =>
            let map =
              List.fold_left(
                (acc, child) =>
                  Id.Map.union(
                    (_, a, _) => Some(a),
                    go(~not_top=true, level, child),
                    acc,
                  ),
                map,
                t.children,
              );
            (level, map);
          }
        };
      },
      (base, Id.Map.empty),
      complete_trimmed_seg,
      context,
    );
  map;
};

/* ONE PARTITIONER (2026-07-27, andrew): the walk consumes the
   CANONICAL COMPLETION'S PARTITIONER — the same layout-intent
   reading that decides what the surfaced completion absorbs — so
   indent suggestions agree with the completion about which lines
   belong to an unclosed construct. Lines WITH content partition by
   their actual layout: flush-written lines under an unclosed let are
   siblings (no staircase — each partition restarts at base), indented
   ones are absorbed (nested, the typed-through reading). A
   CONTENTLESS line is no evidence at all (~absorb_empty_lines): the
   fresh line Enter just made is the very thing whose meaning is
   being decided, so it stays inside the open construct — where
   typing will land (P10 fill position).

   Within a partition the walk keeps its own shallow absorb-reading
   rather than the completed GEOMETRY: the completion anchors owed
   closers at the end of typed content (before trailing linebreaks —
   the right display/Tab promise, P10), but a delimiter obligation's
   position is FLEXIBLE (the taxonomy), so for the indent question an
   owed closer is not a wall — the construct is still open, and the
   next line is its content. Consuming completed_seg directly was
   tried first (2026-07-27) and reproduced the outside level for
   parens, defs and rule bodies alike. */
let partitions = (seg: Segment.t): list(Segment.t) =>
  CanonicalCompletion.partition_segment(~absorb_empty_lines=true, seg)
  |> List.map(fst);

let level_map = (seg: Segment.t): Id.Map.t(int) =>
  /* indentation rules read content anchors after linebreaks; the
     edit state is grout-free, so derive the holes first — a hole is
     content for indentation exactly as it is for statics */
  GroutPlace.place(seg)
  |> partitions
  |> List.fold_left(
       (map, part) =>
         Id.Map.union(
           (_, a, _) => Some(a),
           go(~not_top=false, 0, part),
           map,
         ),
       Id.Map.empty,
     );

/* Move the derived hole of the blank run containing `lb` to sit
   right after `lb`: for indent queries the obligation anchors where
   typing continues (the caret's line), while display placement keeps
   it on the run's first blank line. Top-level and child runs alike. */
let rec anchor_hole_after = (~lb: Id.t, seg: Segment.t): Segment.t => {
  let rec reorder = (run_grout, acc, rest: Segment.t) =>
    switch (rest) {
    | [Piece.Secondary(w) as p, ...tl] when Secondary.is_linebreak(w) =>
      Id.equal(w.id, lb)
        ? List.rev_append(acc, [p] @ run_grout @ tl)
        : reorder(run_grout, [p, ...acc], tl)
    | [Piece.Secondary(_) as p, ...tl] =>
      reorder(run_grout, [p, ...acc], tl)
    | [Piece.Grout(_) as g, ...tl] => reorder(run_grout @ [g], acc, tl)
    | _ => List.rev_append(acc, run_grout @ rest)
    };
  let rec go_seg = (acc, rest: Segment.t) =>
    switch (rest) {
    | [] => List.rev(acc)
    | [Piece.Secondary(w), ..._] as run
        when Secondary.is_linebreak(w) || Secondary.is_space(w) =>
      /* run start: reorder handles the whole secondary/grout run */
      let rec split_run = (run_acc, tl: Segment.t) =>
        switch (tl) {
        | [(Piece.Secondary(_) | Piece.Grout(_)) as p, ...tl] =>
          split_run([p, ...run_acc], tl)
        | _ => (List.rev(run_acc), tl)
        };
      let (run, tl) = split_run([], run);
      let run' = reorder([], [], run);
      go_seg(List.rev_append(run', acc), tl);
    | [Piece.Grout(_) as g, ...tl] => go_seg([g, ...acc], tl)
    | [Piece.Tile(t), ...tl] =>
      go_seg(
        [
          Piece.Tile({
            ...t,
            children: List.map(anchor_hole_after(~lb), t.children),
          }),
          ...acc,
        ],
        tl,
      )
    | [p, ...tl] => go_seg([p, ...acc], tl)
    };
  go_seg([], seg);
};

/* Look up indentation for a single linebreak by ID.
 * Uses exception-based short-circuit for efficiency.
 * ~anchor_lb: relocate the derived hole of that linebreak's blank run
 * to just after it before computing (the Enter/auto-indent query). */
let level_of = (~anchor_lb=?, ~target_id: Id.t, seg: Segment.t): int =>
  try({
    let placed = GroutPlace.place(seg);
    let placed =
      switch (anchor_lb) {
      | Some(lb) => anchor_hole_after(~lb, placed)
      | None => placed
      };
    placed
    |> partitions
    |> List.iter(part => ignore(go(~not_top=false, ~target_id, 0, part)));
    0;
  }) {
  /* Not found, default to 0 */

  | Found_indent(level) => level
  };

/* === Helper functions for user-managed indentation === */

/* Drop leading space pieces from a segment */
let rec drop_leading_spaces = (seg: Segment.t): Segment.t =>
  switch (seg) {
  | [Piece.Secondary(s), ...rest] when Secondary.is_space(s) =>
    drop_leading_spaces(rest)
  | _ => seg
  };

/* Drop trailing space pieces from a segment (spaces at the end, before linebreak) */
let drop_trailing_spaces = (seg: Segment.t): Segment.t => {
  let rec drop_trailing = (rev_seg: Segment.t): Segment.t =>
    switch (rev_seg) {
    | [Piece.Secondary(s), ...rest] when Secondary.is_space(s) =>
      drop_trailing(rest)
    | _ => rev_seg
    };
  seg |> List.rev |> drop_trailing |> List.rev;
};

/* Strip trailing spaces before each linebreak in a segment.
   Also processes tile children recursively. */
let rec strip_trailing_whitespace = (seg: Segment.t): Segment.t => {
  let rec go = (acc: Segment.t, seg: Segment.t): Segment.t =>
    switch (seg) {
    | [] => List.rev(acc)
    | [Piece.Secondary(w) as p, ...rest] when Secondary.is_linebreak(w) =>
      /* Before emitting linebreak, strip trailing spaces from accumulated */
      let acc_stripped = drop_trailing_spaces(List.rev(acc));
      go([p, ...List.rev(acc_stripped)], rest);
    | [Piece.Tile(t), ...rest] =>
      /* Process children recursively */
      let children = List.map(strip_trailing_whitespace, t.children);
      go(
        [
          Piece.Tile({
            ...t,
            children,
          }),
          ...acc,
        ],
        rest,
      );
    | [p, ...rest] => go([p, ...acc], rest)
    };
  go([], seg);
};

/* Fix indentation in a segment using the provided indent map.
   For each linebreak, removes following spaces and inserts the
   correct number based on the indent map.
   Also strips trailing spaces before linebreaks. */
let rec fix_indentation_in_segment =
        (indent_map: Id.Map.t(int), seg: Segment.t): Segment.t => {
  /* First strip trailing whitespace, then fix leading indentation */
  let seg = strip_trailing_whitespace(seg);
  fix_leading_indentation(indent_map, seg);
}
and fix_leading_indentation =
    (indent_map: Id.Map.t(int), seg: Segment.t): Segment.t =>
  switch (seg) {
  | [] => []
  | [Piece.Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
    let indent =
      Id.Map.find_opt(w.id, indent_map) |> Option.value(~default=0);
    let rest_without_leading_spaces = drop_leading_spaces(rest);
    let spaces =
      List.init(indent, _ => Piece.Secondary(Secondary.mk_space(Id.mk())));
    [Piece.Secondary(w), ...spaces]
    @ fix_leading_indentation(indent_map, rest_without_leading_spaces);
  | [Piece.Tile(t), ...rest] =>
    let children =
      List.map(fix_indentation_in_segment(indent_map), t.children);
    [
      Piece.Tile({
        ...t,
        children,
      }),
      ...fix_leading_indentation(indent_map, rest),
    ];
  | [p, ...rest] => [p, ...fix_leading_indentation(indent_map, rest)]
  };

/* Create space pieces for a given indent level */
let make_indent_spaces = (indent_level: int): Segment.t =>
  List.init(indent_level, _ => Piece.Secondary(Secondary.mk_space(Id.mk())));
