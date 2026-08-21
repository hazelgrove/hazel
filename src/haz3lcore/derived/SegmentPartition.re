/* Segment partitioning shared by canonical completion (deciding where an
 * incomplete tile's scope ends) and indentation (grouping lines under
 * their governing construct). Lives below both to avoid a dependency
 * cycle: probes-iv's drawer made ProbeProj depend on PrettySegment, so
 * Indentation (upstream of PrettySegment) can no longer reach into
 * CanonicalCompletion (downstream of Zipper/ProbeProj). Pure
 * Segment/Tile/Secondary machinery, moved verbatim. */
let count_leading_spaces = (seg: Segment.t): int => {
  let rec count = (seg, n) =>
    switch (seg) {
    | [Piece.Secondary(s), ...rest] when Secondary.is_space(s) =>
      count(rest, n + 1)
    | _ => n
    };
  count(seg, 0);
};

/* Single-pass partitioning based on indentation heuristics.
 * Returns list of (subsegment, incomplete_tiles_in_subsegment).
 *
 * Partition heuristics (when incomplete_before is true):
 * 1. BLANK LINE: Two consecutive linebreaks (always enabled)
 * 2. RELATIVE INDENT: After a linebreak, if the content's indentation is
 *    less than or equal to the incomplete tile's indentation, partition.
 *    (only when ~use_indent_heuristic=true)
 *
 * The relative indent heuristic interprets same-or-lesser indented content
 * after incomplete syntax as user intent to start something new.
 * This subsumes the old "zero indent" heuristic (incomplete at col 0,
 * content at col 0 means 0 <= 0 -> partition).
 *
 * This should be disabled for indentation calculation to avoid circular
 * dependency (indentation uses completion, completion uses indentation). */
/* Continuation lines: the indent heuristic reads same-indent as
   "not mine", but broken multiline forms put their own material at
   the head indent. Evidence-gated exceptions (neither can occur in
   healthy code): a line whose first content piece is (a) a NAKED
   rule tile (healthy rules live inside their case tile), or (b) a
   bare token proper-prefixing a delimiter some incomplete tile of
   this partition still expects (`en` under a case missing its end,
   `els` under an if missing its else) continues the partition. */
let continuation_line = (incomplete_acc: list(Tile.t), rest: Segment.t): bool => {
  let rec first_content = (sg: Segment.t) =>
    switch (sg) {
    | [Piece.Secondary(s), ...tl] when Secondary.is_space(s) =>
      first_content(tl)
    | [p, ..._] => Some(p)
    | [] => None
    };
  switch (first_content(rest)) {
  | Some(Tile(t)) when t.mold.out == Sort.Rul => true
  /* (c) a line opening with a concave-LEFT piece — an infix or
     postfix operator, a comma, a stranded closer shard — requires a
     left operand from the previous line, so it cannot start anything
     new (`+ 2` under an unclosed paren is a continuation, whatever
     its indent) */
  | Some(Tile(t))
      when
        switch (Tile.shapes(t)) {
        | (Concave(_), _) => true
        | _ => false
        } =>
    true
  | Some(Tile({label: [tok], children: [], _})) =>
    incomplete_acc
    |> List.exists((it: Tile.t) => {
         let missing =
           List.init(List.length(it.label), i => i)
           |> List.filter(i => !List.mem(i, it.shards))
           |> List.map(List.nth(it.label));
         missing
         |> List.exists(dt =>
              Token.length(tok) < Token.length(dt)
              && String.sub(dt, 0, Token.length(tok)) == tok
            );
       })
  | _ => false
  };
};

/* Does this line (the pieces up to its linebreak) carry any user
   content? Spaces and grout are not content; comments are. Used by
   ~absorb_empty_lines: a contentless line is no evidence of intent,
   so the relative-indent heuristic has nothing to read there. */
let line_has_content = (rest: Segment.t): bool => {
  let rec scan = (sg: Segment.t) =>
    switch (sg) {
    | [] => false
    | [Piece.Secondary(w), ..._] when Secondary.is_linebreak(w) => false
    | [Piece.Secondary(w), ...tl] when Secondary.is_space(w) => scan(tl)
    | [Piece.Grout(_), ...tl] => scan(tl)
    | [_, ..._] => true
    };
  scan(rest);
};

let partition_segment =
    (~use_indent_heuristic=true, ~absorb_empty_lines=false, seg: Segment.t)
    : list((Segment.t, list(Tile.t))) => {
  let rec go =
          (
            seg: Segment.t,
            acc: Segment.t,
            incomplete_acc: list(Tile.t),
            incomplete_before: bool,
            line_indent: int, /* spaces since last linebreak */
            past_indent: bool, /* have we seen non-space on this line? */
            incomplete_indent: option(int),
          ) /* indent of first incomplete tile */
          : list((Segment.t, list(Tile.t))) => {
    switch (seg) {
    | [] =>
      /* End of segment - return accumulated subsegment with its incomplete tiles */
      [(List.rev(acc), List.rev(incomplete_acc))]

    /* Heuristic 1: Blank line (two consecutive linebreaks) */
    | [Secondary(w1), Secondary(w2), ...rest]
        when Secondary.is_linebreak(w1) && Secondary.is_linebreak(w2) =>
      if (incomplete_before) {
        /* Split here: finish current subsegment, start new one */
        let current = List.rev([Piece.Secondary(w1), ...acc]);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining =
          go(rest, [Secondary(w2)], [], false, 0, false, None);
        [(current, current_incomplete), ...remaining];
      } else {
        /* No split - continue accumulating */
        go(
          rest,
          [Secondary(w2), Secondary(w1), ...acc],
          incomplete_acc,
          false,
          0,
          false,
          incomplete_indent,
        );
      }

    /* Heuristic 2: Relative indent comparison */
    | [Secondary(w), ...rest]
        when use_indent_heuristic && Secondary.is_linebreak(w) =>
      let spaces_after = count_leading_spaces(rest);
      switch (incomplete_indent) {
      | Some(inc_ind)
          when
            incomplete_before
            && spaces_after <= inc_ind
            && (!absorb_empty_lines || line_has_content(rest))
            && !continuation_line(incomplete_acc, rest) =>
        /* Partition: content at same/lesser indent than incomplete tile */
        let current = List.rev(acc);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining = go(rest, [Secondary(w)], [], false, 0, false, None);
        [(current, current_incomplete), ...remaining];
      | _ =>
        /* No partition - continue accumulating */
        go(
          rest,
          [Secondary(w), ...acc],
          incomplete_acc,
          incomplete_before,
          0,
          false,
          incomplete_indent,
        )
      };

    /* Space at start of line - increment indent */
    | [Secondary(s) as p, ...rest] when Secondary.is_space(s) && !past_indent =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent + 1,
        false,
        incomplete_indent,
      )

    /* Space after content - doesn't affect indent tracking */
    | [Secondary(_) as p, ...rest] =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent,
        past_indent,
        incomplete_indent,
      )

    /* Incomplete tile - record its indent level */
    | [Piece.Tile(t) as p, ...rest] when !Tile.is_complete(t) =>
      let new_incomplete_indent =
        switch (incomplete_indent) {
        | None => Some(line_indent)
        | some => some
        };
      go(
        rest,
        [p, ...acc],
        [t, ...incomplete_acc],
        true,
        line_indent,
        true,
        new_incomplete_indent,
      );

    /* Other pieces (complete tiles, grout, projectors) */
    | [p, ...rest] =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent,
        true,
        incomplete_indent,
      )
    };
  };
  go(seg, [], [], false, 0, false, None);
};
