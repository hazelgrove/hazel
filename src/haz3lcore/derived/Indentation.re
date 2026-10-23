/* Computes indentation levels for linebreaks in a segment. Main entry
 * point is `level_map`: a map from linebreak IDs to indent (spaces).
 *
 * Continuation lines (content both before and after a linebreak inside
 * a child) are ambiguous at typing time — the next line could continue
 * the child or complete the form — so indentation is conservative while
 * typing and corrected on Format, when following content is known. */

/* spaces and concave grout: the walk reads past them */
let is_non_content = (p: Piece.t): bool =>
  switch (p) {
  | Grout({shape: Concave, _}) => true
  | Secondary(s) => Secondary.is_space(s)
  | _ => false
  };

/* Remove non-contentful items (whitespace and concave grout) */
let trim_non_content: Segment.t => Segment.t =
  List.filter(p => !is_non_content(p));

/* Context (effective_prev, next, effective_next) per piece, one pass;
 * the effective_* fields skip linebreaks. Convex grout counts as
 * content: a hole filling a branch must anchor the next line's indent
 * exactly as a literal would, else the incrementor/child rules re-fire
 * and every hole-bearing line drifts deeper. */
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
        | _ => Some(x) /* Update for contentful pieces (incl. grout) */
        };
      go(
        [(effective_prev, next, effective_next), ...acc],
        rest,
        new_last_contentful,
      );
    };
  go([], seg, None);
};

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
    | [Tile(t), ...rest] when !Tile.is_complete(t) && !Tile.is_case_rule(t) =>
      List.rev([
        Piece.Tile({
          ...t,
          shards: List.init(Tile.arity(t), i => i),
          children: t.children @ [shallow_complete_segment(rest)],
          /* Note: Potentially wrong number of children */
        }),
        ...acc,
      ])
    | [p, ...rest] => go([p, ...acc], rest)
    };
  go([], seg);
};

/* [seg]'s first incomplete tile, a case rule aside, completed as
   shallow_complete_segment completes it, with the rest of [seg] as is for
   its last child: the walk completes each level's open tile as it reaches
   the level, so it knows which closers aren't typed */
let complete_first = (seg: Segment.t): (Segment.t, option(Id.t)) => {
  let rec go = (acc, seg: Segment.t) =>
    switch (seg) {
    | [] => (List.rev(acc), None)
    | [Tile(t), ...rest] when !Tile.is_complete(t) && !Tile.is_case_rule(t) => (
        List.rev([
          Piece.Tile({
            ...t,
            shards: List.init(Tile.arity(t), i => i),
            children: t.children @ [rest],
          }),
          ...acc,
        ]),
        Some(t.id),
      )
    | [p, ...rest] => go([p, ...acc], rest)
    };
  go([], seg);
};

/* [seg] split at its first blank line after an incomplete tile (a line of
   spaces and concave grout is blank), one linebreak on each side. A
   member list whose `}` isn't typed reads on through blank lines, which
   part its members, as the layout partition reads it */
let incomplete_subseg_before_blank_line =
    (seg: Segment.t): option((Segment.t, Segment.t)) => {
  let rec blank = (seg: Segment.t) =>
    switch (seg) {
    | [p, ...rest] when is_non_content(p) => blank(rest)
    | [p, ..._] => Piece.is_linebreak(p)
    | [] => false
    };
  let rec go = (first: option(Tile.t), acc, seg: Segment.t) =>
    switch (seg, first) {
    | ([], _) => None
    | ([p, ...rest], Some(t))
        when
          Piece.is_linebreak(p)
          && blank(rest)
          && !MemberLines.opens_members(t) =>
      Some((List.rev([p, ...acc]), rest))
    | ([Tile(t) as p, ...rest], None) when !Tile.is_complete(t) =>
      go(Some(t), [p, ...acc], rest)
    | ([p, ...rest], _) => go(first, [p, ...acc], rest)
    };
  go(None, [], seg);
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
 * tiles at the top level.
 * Returns the open tile completed at this level, if any, and whether a
 * blank line ends its body. */
let complete_segment = (seg: Segment.t): (Segment.t, option(Id.t), bool) =>
  if (List.for_all(Piece.is_complete, seg)) {
    (seg, None, false);
  } else {
    switch (incomplete_subseg_before_blank_line(seg)) {
    | None =>
      let (seg, open_tile) = complete_first(seg);
      (seg, open_tile, false);
    | Some((before, after)) =>
      let (before, open_tile) = complete_first(before);
      (before @ after, open_tile, true);
    };
  };

/* An incomplete case rule is just the `|` without the `=>`.
 * This has shards [0] instead of [0, 1]. */
let is_incomplete_case_rule = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) when Tile.is_case_rule(t) => t.shards == [0]
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
  | Tile({shards, children, _} as t) when Tile.is_case_rule(t) =>
    /* Complete = has both shards [0, 1] */
    shards == [0, 1]
    /* Body is children[1], check if it has content */
    && List.length(children) >= 2
    && has_content(List.nth(children, 1))
  | _ => false
  };

/* Module members: `;`-separated ModLets have no `in` to exempt them from
   the incrementor rule, so without a reset each member's body indent
   compounds diagonally. A linebreak after a module semicolon returns to
   member level (one step inside the braces). The Exp `;` (Seq) shares the
   label but not the sort, and keeps its level as before. */
let is_module_semi = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) when Tile.is_semi(t) => Tile.mold(t).out == Sort.Mod
  | _ => false
  };

/* a `{`'s member list: a module's members, or a signature's */
let is_member_body = (t: Tile.t): bool =>
  List.exists(s => s == Sort.Mod || s == Sort.Sig, Tile.mold(t).in_);

/* Check if piece is convex grout (hole for missing expression/pattern) */
let is_convex_grout = (p: Piece.t): bool =>
  switch (p) {
  | Grout({shape: Convex, _}) => true
  | _ => false
  };

/* Linebreaks following these tiles should increment the indent. Basically
 * any non-infix-operator tiles which are concave on the right, except
 * for definition forms */
let is_incrementor = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (Tile.shapes(t)) {
    | _ when Tile.ends_with_in(t) => false
    | (_, Concave(_)) when Tile.is_multidelimiter(t) => true
    | _ => false
    }
  | _ => false
  };

/* Exception for short-circuit lookup of single linebreak's indentation */
exception Found_indent(int);

/* The walk consumes the canonical completion's partitioner — the same
   layout-intent reading that decides what the surfaced completion
   absorbs — so indent suggestions agree with the completion about
   which lines belong to an unclosed construct. Lines with content
   partition by their actual layout; a contentless line is no evidence
   (~absorb_empty_lines): the fresh line Enter just made stays inside
   the open construct, where typing will land. Within a partition the
   walk keeps its shallow absorb-reading rather than the completed
   geometry: an owed closer's position is flexible, so it is not a
   wall for next-line typing. A member list partitions alike, as
   completion reads each member level: an unfinished member, or an
   unclosed module, ends at its member line. [line]: the level of
   [seg]'s first line, when it starts mid-line */
let partitions = (~line=None, seg: Segment.t): list(Segment.t) =>
  CanonicalCompletion.partition_segment(~absorb_empty_lines=true, ~line, seg)
  |> List.map(fst);

/* [closer]: a typed closer ends [seg] (its tile's next shard), so its last
   line goes back to the tile's level; else that line is a fresh one, at
   the end of a partition or of an open tile's body */
let rec go =
        (
          ~not_top,
          ~members=false,
          ~closer: bool,
          ~line: option(int)=None,
          ~target_id: option(Id.t)=?,
          base: int,
          seg: Segment.t,
        )
        : Id.Map.t(int) =>
  switch (
    members && !List.for_all(Piece.is_complete, seg)
      ? partitions(~line, seg) : [seg]
  ) {
  | [part] =>
    walk(~not_top, ~members, ~closer, ~line, ~target_id?, base, part)
  | parts =>
    /* only the last part ends where the list does */
    let last = List.length(parts) - 1;
    parts
    |> List.mapi((i, part) =>
         walk(
           ~not_top,
           ~members,
           ~closer=closer && i == last,
           ~line=i == 0 ? line : None,
           ~target_id?,
           base,
           part,
         )
       )
    |> List.fold_left(
         (acc, map) => Id.Map.union((_, a, _) => Some(a), map, acc),
         Id.Map.empty,
       );
  }
/* one partition's lines */
and walk =
    (
      ~not_top,
      ~members,
      ~closer,
      ~line,
      ~target_id: option(Id.t)=?,
      base: int,
      seg: Segment.t,
    )
    : Id.Map.t(int) => {
  let (completed, open_tile, blank_end) = complete_segment(seg);
  let complete_trimmed_seg = trim_non_content(completed);
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
    let rec mark = (acc, flag, xs) =>
      switch (xs) {
      | [] => List.rev(acc)
      | [x, ...rest] => mark([flag, ...acc], is_lb(x), rest)
      };
    mark([], false, complete_trimmed_seg);
  };
  /* stack-safe zip (List.combine is not tail-recursive) */
  let context =
    List.rev(List.rev_map2((ctx, lb) => (ctx, lb), context, prev_is_lb));
  let step =
      ((level: int, map: Id.Map.t(int)), p: Piece.t, ctx, line_level: int) => {
    let ((prev, next, effective_next), prev_is_lb) = ctx;
    switch (p) {
    | Secondary(w) when Secondary.is_linebreak(w) =>
      let indent =
        switch (prev, next) {
        | (_, Some(next)) when Piece.is_comma(next) => base + 2
        | (Some(prev), _) when Piece.is_comma(prev) => base + 2
        /* module members: reset to member level, one step inside
           the braces (their `;` has no `in` to exempt it); a
           module-rooted editor's top level has no braces */
        | (Some(prev), _) when is_module_semi(prev) =>
          not_top ? MemberLines.member_level(base) : base
        /* Incomplete case rules (just `|`) shouldn't increment.
         * An incomplete `|` is Concave on right, so would match
         * is_incrementor without this check. */
        | (Some(prev), _) when is_incomplete_case_rule(prev) => base
        /* After a complete case rule WITH a body, we expect the next
         * rule at the same level. Don't indent for "next rule" position. */
        | (Some(prev), _) when is_complete_case_rule_with_body(prev) => base
        /* only the FIRST linebreak after an incrementor takes
           the +2; consecutive linebreaks inherit its level */
        | (Some(prev), _) when is_incrementor(prev) =>
          prev_is_lb ? level : level + 2
        | (None, _) when not_top => base + 2
        /* Check effective_next (skipping linebreaks) for case rule */
        | _ when Option.map(Piece.is_case_rule, effective_next) == Some(true) => base
        | (_, Some(next)) when Piece.is_case_rule(next) => base
        /* a fresh line past a member list's last member is the next
           member's; only the `}` line goes back to the braces, and a
           list whose `}` isn't typed has none */
        | (_, None) =>
          members && !closer ? MemberLines.member_level(base) : base
        | _ when members && effective_next == None =>
          MemberLines.member_level(base)
        /* If next is linebreak but eff_next is None, effectively at end */
        | _ when effective_next == None => base
        | (_, Some(p)) when Piece.is_infix_delimiter_op_prefix(p) =>
          /* Special case for kw prefixes */
          base
        /* Continuation lines in children: with content before and
         * after the linebreak, use child indentation. Only fires
         * after Format (at typing time next is unknown). max, not
         * base + 2: an incrementor earlier in the child (fun ->)
         * may have raised the running level, which sibling lines
         * must inherit. */
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
      let members = is_member_body(t);
      let line =
        Some(members ? MemberLines.member_level(line_level) : line_level);
      /* a typed shard closes each child but an open tile's last, which
         ends where this level does, or at a blank line */
      let open_ =
        switch (open_tile) {
        | Some(id) => Id.equal(id, t.id)
        | None => false
        };
      let last = List.length(t.children) - 1;
      let child = (j, c) =>
        go(
          ~not_top=true,
          ~members,
          ~closer=!open_ || j < last || closer && !blank_end,
          ~line,
          ~target_id?,
          level,
          c,
        );
      switch (target_id) {
      | Some(_) =>
        /* target mode: just recurse, don't accumulate */
        List.iteri((j, c) => ignore(child(j, c)), t.children);
        (level, map);
      | None =>
        let (_, map) =
          List.fold_left(
            ((j, acc), c) =>
              (
                j + 1,
                Id.Map.union((_, a, _) => Some(a), child(j, c), acc),
              ),
            (0, map),
            t.children,
          );
        (level, map);
      };
    };
  };
  /* the pieces read, each with its context; the spaces between give each
     line's level, as completion reads levels: its indentation, or [line]
     on a first line begun mid-line */
  let rec read = (acc, line_level, run, ctxs, ps: Segment.t) =>
    switch (ps, ctxs) {
    | ([p, ...ps], [ctx, ...ctxs]) when !is_non_content(p) =>
      let lb = Piece.is_linebreak(p);
      read(step(acc, p, ctx, line_level), lb ? 0 : line_level, lb, ctxs, ps);
    | ([p, ...ps], _) =>
      let run = run && Piece.is_space(p);
      read(acc, run ? line_level + 1 : line_level, run, ctxs, ps);
    | ([], _) => acc
    };
  let (_, map) =
    read(
      /* a member started on the `{` line sits at member level, as one
         on a line of its own does */
      (members ? MemberLines.member_level(base) : base, Id.Map.empty),
      Option.value(line, ~default=0),
      line == None,
      context,
      completed,
    );
  map;
};

let level_map = (seg: Segment.t): Id.Map.t(int) =>
  seg
  |> partitions
  |> List.fold_left(
       (map, part) =>
         Id.Map.union(
           (_, a, _) => Some(a),
           go(~not_top=false, ~closer=false, 0, part),
           map,
         ),
       Id.Map.empty,
     );

/* Look up indentation for a single linebreak by ID.
 * Uses exception-based short-circuit for efficiency. */
let level_of = (~target_id: Id.t, seg: Segment.t): int =>
  try(
    {
      seg
      |> partitions
      |> List.iter(part =>
           ignore(go(~not_top=false, ~closer=false, ~target_id, 0, part))
         );
      0;
    }
  ) {
  /* Not found, default to 0 */

  | Found_indent(level) => level
  };

/* === Helper functions for user-managed indentation === */

/* Drop leading space pieces from a segment */
let drop_leading_spaces = (seg: Segment.t): Segment.t =>
  snd(Segment.split_space_run(seg));

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
let strip_trailing_whitespace = (seg: Segment.t): Segment.t => {
  let level = (seg: Segment.t): Segment.t => {
    let rec go = (acc: Segment.t, seg: Segment.t): Segment.t =>
      switch (seg) {
      | [] => List.rev(acc)
      | [Piece.Secondary(w) as p, ...rest] when Secondary.is_linebreak(w) =>
        /* Before emitting linebreak, strip trailing spaces from accumulated */
        let acc_stripped = drop_trailing_spaces(List.rev(acc));
        go([p, ...List.rev(acc_stripped)], rest);
      | [p, ...rest] => go([p, ...acc], rest)
      };
    go([], seg);
  };
  Segment.map_deep(level, seg);
};

/* For each linebreak (recursively), remove following spaces and insert
   the count the indent map assigns. */
let fix_leading_indentation =
    (indent_map: Id.Map.t(int), seg: Segment.t): Segment.t => {
  let rec level = (seg: Segment.t): Segment.t =>
    switch (seg) {
    | [] => []
    | [Piece.Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
      let indent =
        Id.Map.find_opt(w.id, indent_map) |> Option.value(~default=0);
      let rest_without_leading_spaces = drop_leading_spaces(rest);
      let spaces =
        List.init(indent, _ => Piece.Secondary(Secondary.mk_space(Id.mk())));
      [Piece.Secondary(w), ...spaces] @ level(rest_without_leading_spaces);
    | [p, ...rest] => [p, ...level(rest)]
    };
  Segment.map_deep(level, seg);
};

/* Fix indentation in a segment using the provided indent map.
   Also strips trailing spaces before linebreaks. */
let fix_indentation_in_segment =
    (indent_map: Id.Map.t(int), seg: Segment.t): Segment.t =>
  seg |> strip_trailing_whitespace |> fix_leading_indentation(indent_map);

/* Create space pieces for a given indent level */
let make_indent_spaces = (indent_level: int): Segment.t =>
  List.init(indent_level, _ => Piece.Secondary(Secondary.mk_space(Id.mk())));

/* Whole-buffer re-indentation (the Format(Indent) action; also runs
 * before spacing normalization in Format(Spacing)). Rewrites only the
 * leading space runs after linebreaks — never linebreaks themselves.
 * TODO(andrew): once the pretty printer is fixed (it incorporates
 * level_map but has remaining issues), rip out the Format Action.t in
 * favor of an indentation-incorporating PrettyPrint as the single
 * reformat action; related cleanup: the Format/PrettyPrint menu +
 * keybinding split. */
let reindent_segment = (seg: Segment.t): Segment.t =>
  fix_indentation_in_segment(level_map(seg), seg);
