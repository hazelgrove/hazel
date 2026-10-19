/* Where completion's layout reading ends an unfinished member: the
   partition by layout (blank lines, indentation) completion reads, and the
   member-line rule over it. At a member level an unfinished member ends
   before its MEMBER LINE, the next line at-or-left of its first that
   doesn't continue it. Completion cuts its units there (and synthesizes
   the member's `;`); ItemSpans' Read levels cut the parse and the layout
   there too, so an open form stays in its member. Lines are read by
   level, as Indentation reads them: a line's indentation, and on a
   module's `{` line the module's member level, so a member started there
   reads as one on a line of its own. A segment starting mid-line is given
   its line's level as [~line]. Pure piece structure. */

open Util;

let at_members = (sort: Sort.t) => sort == Sort.Mod || sort == Sort.Sig;

/* a form only a member level holds (every mold of its label is Mod or
   Sig sorted) */
let is_member_label = (label: Label.t): bool =>
  switch (Form.base_molds(label)) {
  | [] => false
  | molds =>
    List.for_all(
      (m: Mold.t) => m.out == Sort.Mod || m.out == Sort.Sig,
      molds,
    )
  };

let is_member_head = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => is_member_label(Tile.label(t))
  | _ => false
  };

/* ---- line levels ---- */

/* the member level of a module whose `{` is on a line at [level]: where
   Enter indents the module's own-line members */
let member_level = (level: int): int => level + 2;

/* the spaces from [arr]'s piece [j] on */
let spaces_from = (arr: array(Piece.t), j: int): int => {
  let rec go = (k, n) =>
    k < Array.length(arr) && Piece.is_space(arr[k]) ? go(k + 1, n + 1) : n;
  go(j, 0);
};

/* the level of the line holding [arr]'s piece [i] ([Array.length(arr)]:
   its end), of a segment that is [arr] from [from] on: the indentation
   after its last linebreak before [i], else its first line's, [line] when
   it starts mid-line, else its leading spaces. Only the segment's own
   linebreaks start its lines: one inside a tile is the tile's, as
   Indentation reads levels */
let level_at =
    (~line: option(int), ~from=0, arr: array(Piece.t), i: int): int => {
  let rec back = j =>
    j < from ? None : Piece.is_linebreak(arr[j]) ? Some(j) : back(j - 1);
  switch (back(i - 1), line) {
  | (Some(j), _) => spaces_from(arr, j + 1)
  | (None, Some(l)) => l
  | (None, None) => spaces_from(arr, from)
  };
};

/* the level of the line [seg] ends on, where what follows it starts:
   [seg] is cut after a separator or before a linebreak, so what follows
   starts past that line's indentation */
let line_after = (~line: option(int), seg: Segment.t): option(int) => {
  /* the spaces after the last linebreak so far, while they run */
  let (last, _) =
    List.fold_left(
      ((last, run), p) =>
        Piece.is_linebreak(p)
          ? (Some(0), true)
          : run && Piece.is_space(p)
              ? (Option.map(n => n + 1, last), true) : (last, false),
      (None, false),
      seg,
    );
  switch (last, line) {
  | (Some(_), _) => last
  | (None, Some(_)) => line
  | (None, None) => Some(List.length(fst(Segment.split_space_run(seg))))
  };
};

let count_leading_spaces = (seg: Segment.t): int =>
  List.length(fst(Segment.split_space_run(seg)));

/* Single-pass partitioning; returns (subsegment, its incomplete tiles)
 * pairs. Splits (only after an incomplete tile) on: (1) a blank line
 * (two consecutive linebreaks); (2) with ~use_indent_heuristic, a
 * linebreak followed by content indented <= the first incomplete
 * tile's indent — read as intent to start something new. Indentation
 * calculation must disable (2): indentation uses completion,
 * completion uses indentation. */
/* Continuation lines: the indent heuristic reads same-indent as
   "not mine", but broken multiline forms put their own material at
   the head indent. Evidence-gated exceptions (neither can occur in
   healthy code): a line whose first content piece is (a) a NAKED
   rule tile (healthy rules live inside their case tile), or (b) a
   bare token proper-prefixing a delimiter some incomplete tile of
   this partition still expects (`en` under a case missing its end,
   `els` under an if missing its else) continues the partition. */
let continuation_line = (incomplete_acc: list(Tile.t), rest: Segment.t): bool => {
  /* skips spaces only: a linebreak IS the line's first content here */
  let first_content = (sg: Segment.t) =>
    Segment.next_content(~skip=Segment.skip_space, sg, 0) |> Option.map(snd);
  switch (first_content(rest)) {
  | Some(Tile(t)) when Tile.mold(t).out == Sort.Rul => true
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
  | Some(Tile({children: [], _} as t)) when Tile.arity(t) == 1 =>
    let tok = Tile.token(t, 0);
    incomplete_acc
    |> List.exists((it: Tile.t) => {
         let missing =
           Tile.missing_shard_indices(it) |> List.map(Tile.token(it));
         missing
         |> List.exists(dt =>
              Token.length(tok) < Token.length(dt)
              && String.sub(dt, 0, Token.length(tok)) == tok
            );
       });
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

/* an unclosed tile whose open interior is a member list (a module's `{`) */
let opens_members = (t: Tile.t): bool =>
  !Tile.has_end(Right, t)
  && at_members(snd(Mold.nibs(~index=Tile.r_shard(t), Tile.mold(t))).sort);

/* [line]: the level of [seg]'s first line, when it starts mid-line */
let partition_segment =
    (
      ~use_indent_heuristic=true,
      ~absorb_empty_lines=false,
      ~line: option(int)=None,
      seg: Segment.t,
    )
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
    | [] => [(List.rev(acc), List.rev(incomplete_acc))]

    /* Heuristic 1: blank line (two consecutive linebreaks), except in a
       member list (an unclosed `{`), whose members blank lines part; the
       indent rule still ends it */
    | [Secondary(w1), Secondary(w2), ...rest]
        when
          Secondary.is_linebreak(w1)
          && Secondary.is_linebreak(w2)
          && incomplete_before
          && opens_members(ListUtil.last(incomplete_acc)) =>
      go(
        [Secondary(w2), ...rest],
        [Secondary(w1), ...acc],
        incomplete_acc,
        incomplete_before,
        0,
        false,
        incomplete_indent,
      )
    | [Secondary(w1), Secondary(w2), ...rest]
        when Secondary.is_linebreak(w1) && Secondary.is_linebreak(w2) =>
      if (incomplete_before) {
        let current = List.rev([Piece.Secondary(w1), ...acc]);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining =
          go(rest, [Secondary(w2)], [], false, 0, false, None);
        [(current, current_incomplete), ...remaining];
      } else {
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

    /* Heuristic 2: relative indent */
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
        let current = List.rev(acc);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining = go(rest, [Secondary(w)], [], false, 0, false, None);
        [(current, current_incomplete), ...remaining];
      | _ =>
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
  switch (line) {
  | Some(l) => go(seg, [], [], false, l, true, None)
  | None => go(seg, [], [], false, 0, false, None)
  };
};

/* what follows a part of a level: its first content, and the
   indentation of that content's line */
type follows = {
  indent: int,
  head: Piece.t,
};

let same_follows = (a: option(follows), b: option(follows)): bool =>
  switch (a, b) {
  | (None, None) => true
  | (Some(a), Some(b)) => a.indent == b.indent && a.head === b.head
  | _ => false
  };

/* a partition's first content after its boundary, and its line's
   indentation. Concave grout there is boundary debris (sanitation drops
   it) */
let lead = (seg: Segment.t): option(follows) => {
  let rec go = (indent, past, ps: Segment.t) =>
    switch (ps) {
    | [] => None
    | [p, ...ps] when Piece.is_linebreak(p) => go(0, false, ps)
    | [p, ...ps] when Piece.is_space(p) =>
      go(past ? indent : indent + 1, past, ps)
    | [Piece.Secondary(_) | Grout({shape: Concave, _}), ...ps] =>
      go(indent, true, ps)
    | [head, ..._] =>
      Some({
        indent,
        head,
      })
    };
  go(0, false, seg);
};

/* a line opening with [head] continues the line above */
let continues = (head: Piece.t): bool =>
  switch (head) {
  | Tile(t) =>
    Tile.mold(t).out == Sort.Rul
    || (
      switch (Tile.shapes(t)) {
      | (Concave(_), _) => true
      | _ => false
      }
    )
  | _ => false
  };

/* an open `in`: a body slot, which the line below fills */
let is_body_slot = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => Tile.ends_with_in(t) && Tile.has_end(Right, t)
  | _ => false
  };

/* a member level's separators: a `;`, or a concave grout (a missing `;`) */
let is_member_sep = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => Tile.is_semi(t)
  | Grout({shape: Concave, _}) => true
  | _ => false
  };

/* the member holding piece [i] of a member level, which starts at or
   after [floor]: its first content's index, and the level of the line
   that content is on */
let member_at =
    (~line: option(int)=None, ~floor=0, arr: array(Piece.t), i: int)
    : (int, int) => {
  let rec start = j =>
    j <= floor || is_member_sep(arr[j - 1]) ? j : start(j - 1);
  let rec content = j =>
    j < i && Piece.is_secondary(arr[j]) ? content(j + 1) : j;
  let c = content(start(i));
  (c, level_at(~line, ~from=floor, arr, c));
};

/* per partition of [seg]: what follows it (the next partition with
   content's lead, [follows] after the last), and, where it ends a member
   (its next line a member line), the id of the member's first content.
   A member starts no earlier than the boundary that ended the one before
   it, so a part cut there reads it alike */
let member_ends =
    (
      ~line: option(int)=None,
      ~sort: Sort.t,
      ~follows: option(follows),
      seg: Segment.t,
      parts: list((Segment.t, list(Tile.t))),
    )
    : list((option(follows), option(Id.t))) => {
  let arr = Array.of_list(seg);
  let rec index = (id, j, ps: Segment.t) =>
    switch (ps) {
    | [] => None
    | [p, ...ps] => Piece.id(p) == id ? Some(j) : index(id, j + 1, ps)
    };
  let rec go = (floor, offset, parts) =>
    switch (parts) {
    | [] => []
    | [(sub, inc), ...rest] =>
      let next =
        switch (List.find_map(((nxt, _)) => lead(nxt), rest)) {
        | Some(_) as f => f
        | None => follows
        };
      let ends =
        switch (at_members(sort), next, inc) {
        | (true, Some(f), [first, ..._]) =>
          let first: Tile.t = first;
          Option.bind(
            index(first.id, offset, sub),
            i => {
              let (c, indent) = member_at(~line, ~floor, arr, i);
              f.indent <= indent && !continues(f.head)
                ? Some(Piece.id(arr[c])) : None;
            },
          );
        | _ => None
        };
      let offset' = offset + List.length(sub);
      [(next, ends), ...go(ends == None ? floor : offset', offset', rest)];
    };
  go(0, 0, parts);
};

/* a level's unit: an item, or a part of one cut where a member ends,
   with what follows it */
type unit_ = (Segment.t, option(follows));

/* a member-level item cut where completion ends a member: the guess at a
   member's end, from layout, so an unfinished member's unit stops at the
   next member line instead of running on through every `;` its open form
   reaches. Each part but the last carries what follows it; complete_slices
   verifies each against its completion. [line]: the level of [item]'s
   first line, when it starts mid-line */
let member_parts =
    (~line: option(int)=None, ~sort: Sort.t, item: Segment.t): list(unit_) =>
  switch (Segment.incomplete_tiles(item)) {
  | [] => [(item, None)]
  | _ =>
    let parts = partition_segment(~line, item);
    let ends = member_ends(~line, ~sort, ~follows=None, item, parts);
    let rec go = (acc, parts, ends) =>
      switch (parts, ends) {
      | ([(sub, _), ...parts], [(next, Some(_)), ...ends])
          when parts != [] => [
          (acc @ sub, next),
          ...go([], parts, ends),
        ]
      | ([(sub, _), ...parts], [_, ...ends]) when parts != [] =>
        go(acc @ sub, parts, ends)
      | (parts, _) => [(acc @ List.concat_map(fst, parts), None)]
      };
    go([], parts, ends);
  };

/* ---- typing at a member line ---- */

/* a member list's closer whose `{` is gone */
let closes_members = (t: Tile.t): bool =>
  !Tile.has_end(Left, t)
  && at_members(fst(Mold.nibs(~index=Tile.l_shard(t), Tile.mold(t))).sort);

/* a piece whose right operand is still to come: `=`, `in`, `+`, `(` */
let open_right = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (Tile.shapes(t)) {
    | (_, Concave(_)) => true
    | _ => false
    }
  | _ => false
  };

/* The member level whose member line the caret after [pre] starts, if it
   does: [pre] is the caret's level's pieces before it, that level at
   [sort] starting at [line]. The caret has only indentation before it on
   its line, at-or-left of the first line of the member above, which isn't
   open on the right (an open slot or a let chain's `in` takes the line).
   The member level is [pre]'s own, or an unclosed `{`'s members in it. A
   member head typed there starts a member, as after the member's `;`, so
   its sort is the member level's */
let member_line_sort =
    (~sort: Sort.t, ~line: unit => option(int), pre: Segment.t)
    : option(Sort.t) => {
  let arr = Array.of_list(pre);
  /* the caret's line before it: indentation, and the grout a missing `;`
     leaves */
  let rec indent_from = (j, n) =>
    j > 0 && (Piece.is_space(arr[j - 1]) || Piece.is_grout(arr[j - 1]))
      ? indent_from(j - 1, Piece.is_space(arr[j - 1]) ? n + 1 : n) : (j, n);
  let (b, indent) = indent_from(Array.length(arr), 0);
  let rec last = j =>
    j < 0
      ? None
      : Piece.is_secondary(arr[j]) || Piece.is_grout(arr[j])
          ? last(j - 1) : Some(j);
  switch (b > 0 && Piece.is_linebreak(arr[b - 1]) ? last(b - 2) : None) {
  | None => None
  | Some(j) when is_member_sep(arr[j]) || open_right(arr[j]) => None
  | Some(j) =>
    let rec opener = k =>
      k < 0
        ? None
        : (
          switch (arr[k]) {
          | Tile(t) when opens_members(t) => Some((k, t))
          | Tile(t) when closes_members(t) => None
          | _ => opener(k - 1)
          }
        );
    let level =
      switch (opener(j)) {
      | Some((k, t)) =>
        Some((
          snd(Mold.nibs(~index=Tile.r_shard(t), Tile.mold(t))).sort,
          k + 1,
          Some(member_level(level_at(~line=line(), arr, k))),
        ))
      | None => at_members(sort) ? Some((sort, 0, line())) : None
      };
    switch (level) {
    | Some((sort, from, line)) =>
      let (_, first) = member_at(~line, ~floor=from, arr, j);
      indent <= first ? Some(sort) : None;
    | None => None
    };
  };
};
