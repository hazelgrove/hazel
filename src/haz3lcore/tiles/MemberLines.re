/* Where completion's layout reading ends an unfinished member: the
   partition by layout (blank lines, indentation) completion reads, and the
   member-line rule over it. At a member level an unfinished member ends
   before its MEMBER LINE, the next line at-or-left of its first that
   doesn't continue it. Completion cuts its units there (and synthesizes
   the member's `;`); ItemSpans' Read levels cut the parse and the layout
   there too, so an open form stays in its member. A member that starts
   mid-line (on its module's `{` line) takes its start column as its first
   line's indentation, given as [~col], the column the segment starts at.
   Pure piece structure. */

open Util;

let at_members = (sort: Sort.t) => sort == Sort.Mod || sort == Sort.Sig;

/* a form only a member level holds (every mold of its label is Mod or
   Sig sorted) */
let is_member_head = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (Form.base_molds(Tile.label(t))) {
    | [] => false
    | molds =>
      List.for_all(
        (m: Mold.t) => m.out == Sort.Mod || m.out == Sort.Sig,
        molds,
      )
    }
  | _ => false
  };

/* a piece's last line, as the editor lays it out (a hole is one column):
   whether it breaks a line at any depth, and its width after the last
   break (all of it, if none). Read right to left, so a long multi-line
   piece costs its last line */
let rec piece_tail = (p: Piece.t): (bool, int) =>
  switch (p) {
  | Secondary(w) when Secondary.is_linebreak(w) => (true, 0)
  | Secondary(w) => (false, Secondary.length(w))
  | Grout(_) => (false, 1)
  | Projector(pr) => piece_tail(pr.syntax)
  | Tile(t) =>
    /* its shards and the children between them, right to left */
    let rec go = (shards, children, w) =>
      switch (shards) {
      | [] => (false, w)
      | [s, ...shards] =>
        let w = w + Token.length(Tile.token(t, s));
        switch (children, shards) {
        | ([child, ...children], [_, ..._]) =>
          let (broken, cw) = tail(child);
          broken ? (true, w + cw) : go(shards, children, w + cw);
        | _ => (false, w)
        };
      };
    go(List.rev(t.shards), List.rev(t.children), 0);
  }
and tail = (seg: Segment.t): (bool, int) => {
  let rec go = (rev, w) =>
    switch (rev) {
    | [] => (false, w)
    | [p, ...rev] =>
      let (broken, pw) = piece_tail(p);
      broken ? (true, w + pw) : go(rev, w + pw);
    };
  go(List.rev(seg), 0);
};

/* the column after [seg] / [p] laid out from [col] */
let col_after = (col: int, seg: Segment.t): int =>
  switch (tail(seg)) {
  | (true, w) => w
  | (false, w) => col + w
  };
let col_after_piece = (col: int, p: Piece.t): int =>
  switch (piece_tail(p)) {
  | (true, w) => w
  | (false, w) => col + w
  };

/* tile [t] laid out from [col]: where each child starts, and the column
   after it */
let tile_layout = (col: int, t: Tile.t): (list(int), int) => {
  let rec go = (col, shards, children, starts) =>
    switch (shards) {
    | [] => (List.rev(starts), col)
    | [s, ...shards] =>
      let col = col + Token.length(Tile.token(t, s));
      switch (children, shards) {
      | ([child, ...children], [_, ..._]) =>
        go(col_after(col, child), shards, children, [col, ...starts])
      | _ => (List.rev(starts), col)
      };
    };
  go(col, t.shards, t.children, []);
};

/* the column [arr]'s piece [i] starts at, [arr] starting at column [col]:
   laid out from the line it is on */
let col_at = (~col: int, arr: array(Piece.t), i: int): int => {
  let rec bol = j =>
    j > 0 && !Piece.is_linebreak(arr[j - 1]) ? bol(j - 1) : j;
  let rec fwd = (j, c) =>
    j >= i ? c : fwd(j + 1, col_after_piece(c, arr[j]));
  let b = bol(i);
  fwd(b, b == 0 ? col : 0);
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

let partition_segment =
    (
      ~use_indent_heuristic=true,
      ~absorb_empty_lines=false,
      ~col=0,
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
  go(seg, [], [], false, col, false, None);
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

/* the indentation of the line holding [arr]'s piece [i], which starts at
   or after [floor]; on [arr]'s first line, which starts at column [col],
   the column its content starts at */
let indent_at = (~col=0, ~floor=0, arr: array(Piece.t), i: int): int => {
  let rec bol = j =>
    j <= floor || Piece.is_linebreak(arr[j - 1]) ? j : bol(j - 1);
  let rec spaces = (j, n) =>
    j < i && Piece.is_space(arr[j]) ? spaces(j + 1, n + 1) : n;
  let b = bol(i);
  (b == 0 ? col : 0) + spaces(b, 0);
};

/* the member holding piece [i] of a member level, which starts at or
   after [floor]: its first content's index, and the indentation of the
   line that content is on */
let member_at = (~col=0, ~floor=0, arr: array(Piece.t), i: int): (int, int) => {
  let rec start = j =>
    j <= floor || is_member_sep(arr[j - 1]) ? j : start(j - 1);
  let rec content = j =>
    j < i && Piece.is_secondary(arr[j]) ? content(j + 1) : j;
  let c = content(start(i));
  (c, indent_at(~col, ~floor, arr, c));
};

/* per partition of [seg]: what follows it (the next partition with
   content's lead, [follows] after the last), and, where it ends a member
   (its next line a member line), the id of the member's first content.
   A member starts no earlier than the boundary that ended the one before
   it, so a part cut there reads it alike */
let member_ends =
    (
      ~col=0,
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
              let (c, indent) = member_at(~col, ~floor, arr, i);
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
   verifies each against its completion. [col]: the column [item] starts
   at */
let member_parts = (~col=0, ~sort: Sort.t, item: Segment.t): list(unit_) =>
  switch (Segment.incomplete_tiles(item)) {
  | [] => [(item, None)]
  | _ =>
    let parts = partition_segment(~col, item);
    let ends = member_ends(~col, ~sort, ~follows=None, item, parts);
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
