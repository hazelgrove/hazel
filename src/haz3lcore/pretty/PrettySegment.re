/* Pretty printer for Hazel segments.

   Uses a Wadler/Lindig-style document IR with a greedy layout algorithm.
   Converts Segment.t → Doc → Segment.t, inserting linebreaks to keep
   code within a target line width. Indentation of linebreaks is handled
   downstream by Indentation.re / Measured.re.

   == Segment vs Term Nesting ==

   Segments represent delimiter-delimited nesting only — a tile's children
   are the segments between its matched delimiters. This does NOT include
   operator precedence parsing (done by Skel / MakeTerm). For prefix forms
   like `fun x -> body`, the segment doesn't know where `body` ends — the
   rest of the segment after `->` may include operators that bind looser
   than `fun`, which are NOT part of the body in the parsed term.

   The pretty printer works at the segment level, not the term level. The
   split_at_comma workaround and precedence-aware infix chains are sufficient
   for formatting. Full Skel/MakeTerm integration would add complexity for
   marginal benefit.

   == On-Demand Tile Decomposition ==

   When segment_to_doc encounters a tile with children, it decomposes the
   tile on-demand using Tile.contained_children and Tile.shard_of, building
   a doc that interleaves keyword doc nodes with child docs. This allows
   the layout algorithm to make coordinated break decisions across tile
   boundaries (e.g., keeping `(` on the same line as `let x =`).

   After layout, Segment.reassemble reconstructs full tiles from the
   single-shard pieces in the output segment. */

/* === Settings === */

type settings = {
  width: int,
  break_fun_params: bool, /* break function params onto separate lines */
  hanging_delimiters: bool, /* keep ( and [ on the = line in bindings */
  soft_semis: bool /* semis break softly (set inside braces); top-level
                      statement semis always hard-break */
};

let default_settings: settings = {
  width: 80,
  break_fun_params: false,
  hanging_delimiters: true,
  soft_semis: false,
};

/* === Document IR === */

type doc =
  | Empty
  | Piece(Piece.t, int) /* piece with pre-computed flat width */
  | Space /* always emits a space */
  | Break /* space if flat, newline if broken */
  | SoftBreak /* nothing if flat, newline if broken */
  | HardBreak /* always a newline */
  | Cat(doc, doc) /* concatenation */
  | Nest(int, doc) /* increase indent by N for nested breaks */
  | Group(doc); /* try flat first; if doesn't fit, use breaks */

/* Indent unit (chars per nest level). Matches canonical-completion's
 * editor convention (Indentation.re uses +2 per level). */
let indent_unit: int = 2;

/* Right-associative concatenation of a doc list */
let rec cats = (docs: list(doc)): doc =>
  switch (docs) {
  | [] => Empty
  | [d] => d
  | [d, ...rest] => Cat(d, cats(rest))
  };

/* === Width computation === */

let rec piece_width = (p: Piece.t): int =>
  switch (p) {
  | Tile(t) =>
    /* Use effective_label to only count tokens for present shards.
       For complete tiles this is the full label; for single-shard
       pieces from decomposition it's just the shard's token. */
    let label_w =
      List.fold_left(
        (acc, s) => acc + Token.length(s),
        0,
        Tile.effective_label(t),
      );
    let children_w =
      List.fold_left(
        (acc, child) => acc + segment_flat_width(child),
        0,
        t.children,
      );
    label_w + children_w;
  | Grout(_) => 1
  | Secondary(s) => Secondary.length(s)
  | Projector(_) => 10
  }
and segment_flat_width = (seg: Segment.t): int =>
  List.fold_left((acc, p) => acc + piece_width(p), 0, seg);

/* === Greedy layout algorithm (Lindig-style for strict evaluation) === */

type mode =
  | Flat
  | Breaking;

/* Check if the remaining doc fits on this line (first-line check only).
 * Indent is irrelevant to first-line fitting, so cmds carry only (mode, doc). */
let rec fits = (remaining: int, cmds: list((mode, doc))): bool =>
  if (remaining < 0) {
    false;
  } else {
    switch (cmds) {
    | [] => true
    | [(_, Empty), ...rest] => fits(remaining, rest)
    | [(_, Piece(_, w)), ...rest] => fits(remaining - w, rest)
    | [(_, Space), ...rest] => fits(remaining - 1, rest)
    | [(m, Cat(x, y)), ...rest] =>
      fits(remaining, [(m, x), (m, y), ...rest])
    | [(Flat, Break), ...rest] => fits(remaining - 1, rest)
    | [(Breaking, Break), ..._] => true
    | [(Flat, SoftBreak), ...rest] => fits(remaining, rest) /* 0 cost */
    | [(Breaking, SoftBreak), ..._] => true
    | [(Flat, HardBreak), ..._] => false /* Group can't go flat with HardBreak */
    | [(Breaking, HardBreak), ..._] => true
    | [(m, Nest(_, x)), ...rest] => fits(remaining, [(m, x), ...rest])
    | [(_, Group(x)), ...rest] => fits(remaining, [(Flat, x), ...rest])
    };
  };

/* Layout output commands */
type output =
  | OPiece(Piece.t)
  | OSpace
  | ONewline;

/* Push (onto the reversed accumulator) ONewline followed by N OSpaces
   of indent. */
let push_break_with_indent = (indent: int, acc: list(output)): list(output) =>
  List.init(indent, _ => OSpace) @ [ONewline, ...acc];

/* Greedy layout: process doc, deciding group modes based on fit.
 * Cmds carry (indent, mode, doc); indent is the current nesting level
 * in characters, used to indent newlines emitted inside Breaking groups.
 * Tail-recursive (reversed accumulator) so output length doesn't
 * grow the call stack under js_of_ocaml. */
let layout =
    (width: int, col: int, cmds: list((int, mode, doc))): list(output) => {
  let rec go =
          (acc: list(output), col: int, cmds: list((int, mode, doc)))
          : list(output) =>
    switch (cmds) {
    | [] => List.rev(acc)
    | [(_, _, Empty), ...rest] => go(acc, col, rest)
    | [(_, _, Piece(p, w)), ...rest] =>
      go([OPiece(p), ...acc], col + w, rest)
    | [(_, _, Space), ...rest] => go([OSpace, ...acc], col + 1, rest)
    | [(i, m, Cat(x, y)), ...rest] =>
      go(acc, col, [(i, m, x), (i, m, y), ...rest])
    | [(_, Flat, Break), ...rest] => go([OSpace, ...acc], col + 1, rest)
    | [(i, Breaking, Break), ...rest] =>
      /* col reset to 0 (not i) so fit-checks downstream use the full width.
       * Indent is rendered visually but doesn't reduce fit-check budget,
       * matching the "trailing keyword may overhang" convention. */
      go(push_break_with_indent(i, acc), 0, rest)
    | [(_, Flat, SoftBreak), ...rest] => go(acc, col, rest) /* emit nothing */
    | [(i, Breaking, SoftBreak), ...rest] =>
      go(push_break_with_indent(i, acc), 0, rest)
    | [(i, _, HardBreak), ...rest] =>
      go(push_break_with_indent(i, acc), 0, rest)
    | [(i, m, Nest(n, x)), ...rest] =>
      go(acc, col, [(i + n, m, x), ...rest])
    | [(i, _, Group(x)), ...rest] =>
      let fit_cmds =
        List.map(((_, m, d)) => (m, d), [(i, Flat, x), ...rest]);
      if (fits(width - col, fit_cmds)) {
        go(acc, col, [(i, Flat, x), ...rest]);
      } else {
        go(acc, col, [(i, Breaking, x), ...rest]);
      };
    };
  go([], col, cmds);
};

/* Convert layout output to segment (rev_map/rev to stay stack-safe) */
let output_to_segment = (outputs: list(output)): Segment.t =>
  List.rev(
    List.rev_map(
      fun
      | OPiece(p) => p
      | OSpace => Piece.secondary(Secondary.mk_space(Id.mk()))
      | ONewline => Piece.secondary(Secondary.mk_newline(Id.mk())),
      outputs,
    ),
  );

/* === Segment analysis helpers === */

let is_whitespace = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_space(s) || Secondary.is_linebreak(s)
  | _ => false
  };

let is_linebreak = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_linebreak(s)
  | _ => false
  };

let strip_whitespace = (seg: Segment.t): list(Piece.t) =>
  List.filter(p => !is_whitespace(p), seg);

/* Detect blank lines in original segment.
   Returns a set of piece IDs that had a blank line (2+ newlines) before them.
   Uses IDs rather than sequential position so that tile decomposition
   (which splits one tile into multiple shard pieces sharing the same ID)
   doesn't cause misalignment. */
let classify_blank_lines = (seg: Segment.t): Id.Map.t(unit) => {
  let (_, set) =
    List.fold_left(
      ((newline_count, set), p) =>
        if (is_whitespace(p)) {
          (is_linebreak(p) ? newline_count + 1 : newline_count, set);
        } else {
          (0, newline_count >= 2 ? Id.Map.add(Piece.id(p), (), set) : set);
        },
      (0, Id.Map.empty),
      seg,
    );
  set;
};

/* Re-insert blank lines into formatted segment based on piece IDs.
   Inserts a newline before the first occurrence of each flagged ID. */
let reinsert_blank_lines =
    (blank_ids: Id.Map.t(unit), formatted: Segment.t): Segment.t => {
  let nl = () => Piece.secondary(Secondary.mk_newline(Id.mk()));
  let rec go = (acc, remaining_ids, formatted) =>
    switch (formatted) {
    | [] => List.rev(acc)
    | [p, ...rest] =>
      if (is_whitespace(p)) {
        go([p, ...acc], remaining_ids, rest);
      } else {
        let pid = Piece.id(p);
        if (Id.Map.mem(pid, remaining_ids)) {
          /* Insert blank line and remove ID so we don't double-insert
             for other shards of the same decomposed tile */
          go(
            [p, nl(), ...acc],
            Id.Map.remove(pid, remaining_ids),
            rest,
          );
        } else {
          go([p, ...acc], remaining_ids, rest);
        };
      }
    };
  go([], blank_ids, formatted);
};

let is_comment = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_comment(s)
  | _ => false
  };

/* Comments that were on the SAME LINE as preceding content in the
   original segment (and only those) may be absorbed onto the previous
   piece's line. Standalone comments/comment blocks keep their own
   lines (and hence their preceding blank lines). Set per format run. */
let absorbable_comments: ref(Id.Map.t(unit)) = ref(Id.Map.empty);

let classify_trailing_comments = (seg: Segment.t): Id.Map.t(unit) => {
  let rec go = (acc, same_line, seg: Segment.t) =>
    switch (seg) {
    | [] => acc
    | [Piece.Secondary(w), ...rest] when Secondary.is_linebreak(w) =>
      go(acc, false, rest)
    | [Piece.Secondary(w), ...rest] when Secondary.is_space(w) =>
      go(acc, same_line, rest)
    | [Piece.Secondary(w) as pc, ...rest] when Secondary.is_comment(w) =>
      let acc = same_line ? Id.Map.add(Piece.id(pc), (), acc) : acc;
      go(acc, true, rest);
    | [Piece.Tile(t), ...rest] =>
      let acc =
        List.fold_left((acc, ch) => go(acc, true, ch), acc, t.children);
      go(acc, true, rest);
    | [_, ...rest] => go(acc, true, rest)
    };
  go(Id.Map.empty, false, seg);
};

/* Absorb leading comment pieces from a piece list.
   Returns (comments, remaining) where comments should stay
   on the same line as the preceding code piece. */
let absorb_comments =
    (pieces: list(Piece.t)): (list(Piece.t), list(Piece.t)) => {
  let rec go = (acc, pieces) =>
    switch (pieces) {
    | [p, ...rest]
        when is_comment(p) && Id.Map.mem(Piece.id(p), absorbable_comments^) =>
      go([p, ...acc], rest)
    | _ => (List.rev(acc), pieces)
    };
  go([], pieces);
};

let is_semi = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: [";"], _}) => true
  | _ => false
  };

let is_comma = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: [","], _}) => true
  | _ => false
  };

let is_infix = (p: Piece.t): bool =>
  switch (p) {
  | Tile({mold, label: [_], _}) =>
    Mold.is_infix_op(mold) && !is_comma(p) && !is_semi(p)
  | _ => false
  };

let is_dot = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["."], _}) => true
  | _ => false
  };

/* Label binding = in records/labeled tuples (not ==) */
let is_label_eq = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["="], _}) => true
  | _ => false
  };

/* Get precedence from an infix piece's left nib */
let infix_precedence = (p: Piece.t): option(int) =>
  switch (p) {
  | Tile(t) =>
    let (l, _) = Tile.nibs(t);
    switch (l.shape) {
    | Concave(prec) => Some(prec)
    | Convex => None
    };
  | _ => None
  };

/* Get effective precedence: infix ops + commas (for chain splitting) */
let piece_precedence = (p: Piece.t): option(int) =>
  if (is_infix(p)) {
    infix_precedence(p);
  } else if (is_comma(p)) {
    Some(Precedence.comma);
  } else {
    None;
  };

/* Find the loosest (highest int) precedence among operators in pieces */
let find_loosest_prec = (pieces: list(Piece.t)): option(int) =>
  List.fold_left(
    (best, p) =>
      switch (piece_precedence(p), best) {
      | (Some(prec), None) => Some(prec)
      | (Some(prec), Some(best_prec)) when prec > best_prec => Some(prec)
      | _ => best
      },
    None,
    pieces,
  );

/* Split pieces into operands at operators matching the given precedence.
   Returns (operands, operators) where len(operands) == len(operators) + 1. */
let split_infix_chain =
    (prec: int, pieces: list(Piece.t))
    : (list(list(Piece.t)), list(Piece.t)) => {
  let rec go = (operands_rev, ops_rev, current_rev, pieces) =>
    switch (pieces) {
    | [] => (
        List.rev([List.rev(current_rev), ...operands_rev]),
        List.rev(ops_rev),
      )
    | [p, ...rest] =>
      switch (piece_precedence(p)) {
      | Some(p_prec) when p_prec == prec =>
        go(
          [List.rev(current_rev), ...operands_rev],
          [p, ...ops_rev],
          [],
          rest,
        )
      | _ => go(operands_rev, ops_rev, [p, ...current_rev], rest)
      }
    };
  go([], [], [], pieces);
};

/* Single-token prefix operator (e.g., leading + in sum types) */
let is_single_prefix = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => List.length(t.label) == 1 && Mold.is_prefix_op(t.mold)
  | _ => false
  };

let is_compound_prefix = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    List.length(t.label) >= 2
    && Tile.is_complete(t)
    && (
      switch (Tile.shapes(t)) {
      | (Convex, Concave(_)) => true
      | _ => false
      }
    )
  | _ => false
  };

let is_case_rule_tile = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["|", "=>"], _}) => true
  | _ => false
  };

/* A "trailing hole" is either an implicit empty hole (Convex Grout) or
   an explicit hole (Tile with label `?`). When one of these appears as
   the sole body of a block-like form (e.g., `let x = 1 in ?`), we emit
   a HardBreak before it so it lands on its own line. */
let is_trailing_hole = (p: Piece.t): bool =>
  switch (p) {
  | Grout({shape: Convex, _}) => true
  | Tile({label: ["?"], _}) => true
  | _ => false
  };

/* A "block form" is a compound expression whose layout naturally spans
   multiple lines: compound prefixes (let/fun/if/...) and compound operand
   forms ending in "end" (case/end, test/end, ...). We use
   this to decide when to force a HardBreak before an expression so that
   block-like expressions always land on their own line. */
let is_block_form = (p: Piece.t): bool =>
  is_compound_prefix(p)
  || (
    switch (p) {
    | Tile(t) when Tile.is_complete(t) =>
      switch (t.label) {
      | ["case", "end"]
      | ["test", "end"]
      | ["hint", "test", "end"] => true
      | [_, "end"] => true
      | _ => false
      }
    | _ => false
    }
  );

/* Does this segment start with a block-form piece (possibly after whitespace)?
   Used to decide whether to force a HardBreak before the segment. */
let segment_starts_with_block = (seg: Segment.t): bool =>
  switch (strip_whitespace(seg)) {
  | [p, ..._] => is_block_form(p)
  | [] => false
  };

/* Match opening parens/brackets/type-application for tighten_applications.
   After decomposition, closing shards (e.g., ")") should not match —
   we only want to tighten f (x) → f(x), not remove breaks before ")". */
let is_paren_or_bracket = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["(", ")"], shards, children, _})
  | Tile({label: ["[", "]"], shards, children, _})
  | Tile({label: ["@<", ">"], shards, children, _}) =>
    /* Complete tile (has children) or opening shard (index 0) */
    List.length(children) > 0 || shards == [0]
  /* nullary application: f() */
  | Tile({label: ["()"], _}) => true
  | _ => false
  };

/* Does the segment start with a prefix-arrow form (fun/typfun/...)? */
let starts_with_arrow_prefix = (seg: list(Piece.t)): bool =>
  switch (seg) {
  | [Tile({label: [_, "->"], _}), ..._] => true
  | _ => false
  };

let is_right_convex = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (Tile.shapes(t)) {
    | (_, Convex) => true
    | _ => false
    }
  | Grout({shape: Convex, _}) => true
  | Projector(_) => true
  | _ => false
  };

/* Split pieces at the next case rule tile */
let split_at_next_rule =
    (pieces: list(Piece.t)): (list(Piece.t), list(Piece.t)) => {
  let rec go = (body_rev, pieces) =>
    switch (pieces) {
    | [] => (List.rev(body_rev), [])
    | [p, ..._] when is_case_rule_tile(p) => (List.rev(body_rev), pieces)
    | [p, ...rest] => go([p, ...body_rev], rest)
    };
  go([], pieces);
};

/* Split pieces at the first comma. Returns None if no comma found.
   Commas bind more loosely than compound prefixes (fun, if, let),
   so we scan ahead to ensure commas split the segment before
   compound prefixes absorb past them. */
let split_at_comma =
    (pieces: list(Piece.t))
    : option((list(Piece.t), Piece.t, list(Piece.t))) => {
  let rec go = (before_rev, pieces) =>
    switch (pieces) {
    | [] => None
    | [p, ...rest] when is_comma(p) =>
      Some((List.rev(before_rev), p, rest))
    | [p, ...rest] => go([p, ...before_rev], rest)
    };
  go([], pieces);
};

/* === Doc construction from segment content === */

let piece_doc = (p: Piece.t): doc => Piece(p, piece_width(p));

/* Build doc for a piece followed by trailing comments (Space-separated) */
let piece_with_comments = (p: Piece.t, comments: list(Piece.t)): doc =>
  List.fold_left(
    (acc, c) => Cat(acc, Cat(Space, piece_doc(c))),
    piece_doc(p),
    comments,
  );

/* Helper: build doc for a child segment (recursive) */
let rec child_doc = (s: settings, child: Segment.t): doc => {
  let content = strip_whitespace(child);
  switch (content) {
  | [] => Empty
  | _ => segment_to_doc(s, content)
  };
}

/* On-demand tile decomposition: when segment_to_doc encounters a tile
   with children, decompose it into shards + child segments and build
   a doc that interleaves keyword doc nodes with child docs. This lets
   the layout algorithm make coordinated decisions across tile boundaries.

   After layout, Segment.reassemble reconstructs the full tiles from
   the single-shard pieces in the output segment. */
and build_tile_doc = (s: settings, t: Tile.t, rest: list(Piece.t)): doc => {
  let triples = Tile.contained_children(t);
  let last_shard_idx = List.length(t.label) - 1;
  /* Shard doc: extract a single shard from this tile as a doc node */
  let shard = i => piece_doc(Tile.to_piece(Tile.shard_of(t, i)));
  /* Fallback for unexpected tile structure: emit whole tile + rest.
     Must be a thunk to avoid eagerly processing the entire rest segment. */
  let fallback = () =>
    cats([piece_doc(Tile.to_piece(t)), segment_to_doc(s, rest)]);

  /* Build the body doc (content after the tile in the segment).
     For binding forms (let/type/theorem/in, filter/use-in) we always
     HardBreak before the body so the expression after a trailing `in`
     lands on its own line. For non-binding prefixes, commas take priority
     since they bind more loosely in MakeTerm. Trailing holes get the same
     HardBreak treatment even in non-binding positions. */
  let body_doc = (is_binding): doc =>
    switch (rest) {
    | [] => Empty
    /* Single trailing hole body: always break onto its own line */
    | [p] when is_trailing_hole(p) => cats([HardBreak, piece_doc(p)])
    | _ when is_binding =>
      /* keep trailing comments on the `in` line: `... in # 1 #` */
      let (comments, rest_after) = absorb_comments(rest);
      let comment_suffix =
        List.fold_left(
          (acc, c) => Cat(acc, cats([Space, piece_doc(c)])),
          Empty,
          comments,
        );
      switch (rest_after) {
      | [] => comment_suffix
      | _ =>
        cats([
          comment_suffix,
          HardBreak,
          Group(segment_to_doc(s, rest_after)),
        ])
      };
    | _ =>
      switch (split_at_comma(rest)) {
      | Some((before, comma, after)) =>
        let bd = Group(cats([Break, Group(segment_to_doc(s, before))]));
        cats([
          bd,
          piece_doc(comma),
          switch (after) {
          | [] => Empty
          | _ => cats([Break, segment_to_doc(s, after)])
          },
        ]);
      | None => Group(cats([Break, Group(segment_to_doc(s, rest))]))
      }
    };

  /* Try hanging delimiter: when content is a single paren/bracket,
     keep the opener on the current line with SoftBreaks for content.
     Returns None if hanging doesn't apply. */
  let try_hanging_delim =
      (content: list(Piece.t), ~suffix: doc=Empty, ()): option(doc) =>
    switch (content) {
    | [Tile(dt)]
        when
          s.hanging_delimiters
          && (
            dt.label == ["(", ")"]
            || dt.label == ["[", "]"]
            || dt.label == ["{", "}"]
          )
          && List.length(dt.children) > 0 =>
      let open_s = Tile.to_piece(Tile.shard_of(dt, 0));
      let close_s =
        Tile.to_piece(Tile.shard_of(dt, List.length(dt.label) - 1));
      switch (Tile.contained_children(dt)) {
      | [(_, inner_child, _)] =>
        /* soft_semis so short brace bodies can inline; Group so the
           hanging content goes flat when it fits */
        let inner =
          child_doc(
            {
              ...s,
              soft_semis: true,
            },
            inner_child,
          );
        Some(
          Group(
            cats([
              piece_doc(open_s),
              Nest(indent_unit, cats([SoftBreak, inner])),
              SoftBreak,
              piece_doc(close_s),
              suffix,
            ]),
          ),
        );
      | _ => None
      };
    | _ => None
    };

  /* Attach semi and rest after a tile doc. Used by case/end, test/end,
     hint/test/end, and other operand forms ending in "end". */
  let tile_with_rest = (tile_doc: doc): doc =>
    switch (rest) {
    | [semi, ...rest2] when is_semi(semi) =>
      cats([
        tile_doc,
        piece_doc(semi),
        switch (rest2) {
        | [] => Empty
        | _ =>
          cats([s.soft_semis ? Break : HardBreak, semi_tail_doc(s, rest2)])
        },
      ])
    | [] => tile_doc
    | _ => cats([tile_doc, Break, Group(segment_to_doc(s, rest))])
    };

  switch (t.label) {
  /* Binding forms: let/=/in, type/=/in, theorem/=/in */
  | [_, "=", "in"] =>
    switch (triples) {
    | [(_, pat_child, _), (_, binding_child, _)] =>
      let prefix =
        cats([
          shard(0),
          Space,
          Group(child_doc(s, pat_child)),
          Space,
          shard(1),
        ]);
      /* Hanging style: let x = (\n...\n) in
         Otherwise:      let x =\n  (...) in */
      let binding_content = strip_whitespace(binding_child);
      let in_suffix = cats([Space, shard(last_shard_idx)]);
      /* Hanging lambda: keep `= fun ... ->` on the binding line when
         prefix + fun header fit the width budget (static decision —
         the greedy fits lookahead cannot express this preference
         without collapsing the params group). The body then breaks
         after the arrow; a short whole binding still inlines via the
         enclosing group. */
      let hang_header =
        switch (binding_content) {
        | [Tile({label: [_, "->"], _} as ft), ...fun_body]
            when
              List.length(ft.children) == 1
              && !s.break_fun_params
              && Tile.is_complete(ft) =>
          let prefix_w =
            2
            + Token.length(List.nth(t.label, 0))
            + segment_flat_width(pat_child)
            + Token.length(List.nth(t.label, 1))
            + 2;
          let header_w = piece_width(Tile.to_piece(ft));
          prefix_w + header_w <= s.width - 12 ? Some((ft, fun_body)) : None;
        | _ => None
        };
      let binding_doc =
        switch (try_hanging_delim(binding_content, ~suffix=in_suffix, ())) {
        | Some(hanging) => cats([Space, hanging])
        | None =>
          switch (hang_header) {
          | Some((ft, fun_body)) =>
            let header =
              cats([
                piece_doc(Tile.to_piece(Tile.shard_of(ft, 0))),
                Space,
                Group(child_doc(s, List.hd(ft.children))),
                Space,
                piece_doc(
                  Tile.to_piece(
                    Tile.shard_of(ft, List.length(ft.label) - 1),
                  ),
                ),
              ]);
            cats([
              Space,
              header,
              Nest(
                indent_unit,
                cats([
                  Break,
                  Group(segment_to_doc(s, strip_whitespace(fun_body))),
                ]),
              ),
              in_suffix,
            ]);
          | None =>
            cats([
              Nest(
                indent_unit,
                cats([Break, Group(child_doc(s, binding_child))]),
              ),
              in_suffix,
            ])
          }
        };
      let let_in_doc = Group(cats([prefix, binding_doc]));
      Group(cats([let_in_doc, body_doc(true)]));
    | _ => fallback()
    }

  /* if/then/else */
  | ["if", "then", "else"] =>
    switch (triples) {
    | [(_, cond_child, _), (_, conseq_child, _)] =>
      /* If conseq is a block-like expression (let/case/fun/if/...), force
         a HardBreak after `then` so the block lands on its own line, and
         indent the body. Otherwise keep `then <conseq>` on one line. */
      let conseq_starts_block = segment_starts_with_block(conseq_child);
      let then_sep = conseq_starts_block ? HardBreak : Space;
      let conseq_inner =
        cats([then_sep, Group(child_doc(s, conseq_child))]);
      let conseq_doc =
        conseq_starts_block ? Nest(indent_unit, conseq_inner) : conseq_inner;
      /* Alt: indent if block form (else <block>). Included inside the
         tile_doc Group so its HardBreak (when block) forces the whole
         if/then/else to break, not just the alt. */
      let alt_doc =
        switch (rest) {
        | [] => Empty
        | _ =>
          let alt_starts_block = segment_starts_with_block(rest);
          let alt_sep = alt_starts_block ? HardBreak : Space;
          let alt_inner = cats([alt_sep, Group(segment_to_doc(s, rest))]);
          alt_starts_block ? Nest(indent_unit, alt_inner) : alt_inner;
        };
      let tile_doc =
        Group(
          cats([
            shard(0),
            Space,
            Group(child_doc(s, cond_child)),
            Break,
            shard(1),
            conseq_doc,
            Break,
            shard(last_shard_idx),
            alt_doc,
          ]),
        );
      tile_doc;
    | _ => fallback()
    }

  /* Prefix arrow forms: fun/->, fix/->, typfun/->, poly/->, forall/->, rec/-> */
  | [_, "->"] =>
    switch (triples) {
    | [(_, param_child, _)] =>
      let param_doc = child_doc(s, param_child);
      let header =
        cats([shard(0), Space, param_doc, Space, shard(last_shard_idx)]);
      if (s.break_fun_params) {
        cats([header, body_doc(false)]);
      } else {
        switch (rest) {
        | [] => Group(header)
        | [p] when is_trailing_hole(p) =>
          cats([
            Group(header),
            Nest(indent_unit, cats([HardBreak, piece_doc(p)])),
          ])
        | _ =>
          switch (split_at_comma(rest)) {
          | Some(_) => cats([Group(header), body_doc(false)])
          | None =>
            switch (try_hanging_delim(rest, ())) {
            | Some(hanging) => Group(cats([Group(header), Space, hanging]))
            | None =>
              Group(
                cats([
                  Group(header),
                  Nest(
                    indent_unit,
                    cats([Break, Group(segment_to_doc(s, rest))]),
                  ),
                ]),
              )
            }
          }
        };
      };
    | _ => fallback()
    }

  /* Delimiter pairs: (...), [...], {...} */
  | ["(", ")"]
  | ["[", "]"]
  | ["{", "}"] =>
    switch (triples) {
    | [(_, content_child, _)] =>
      let inner =
        child_doc(
          {
            ...s,
            soft_semis: true,
          },
          content_child,
        );
      let delim_doc =
        switch (inner) {
        | Empty => cats([shard(0), shard(last_shard_idx)])
        | _ =>
          Group(
            cats([
              shard(0),
              Nest(indent_unit, cats([SoftBreak, inner])),
              SoftBreak,
              shard(last_shard_idx),
            ]),
          )
        };
      switch (rest) {
      | [] => delim_doc
      | [next, ..._] when is_paren_or_bracket(next) =>
        /* curried application: (f)(x) stays attached */
        cats([delim_doc, segment_to_doc(s, rest)])
      | _ => cats([delim_doc, Break, Group(segment_to_doc(s, rest))])
      };
    | _ => fallback()
    }

  /* Type application: @<...> — tight delimiters, no internal spacing */
  | ["@<", ">"] =>
    switch (triples) {
    | [(_, content_child, _)] =>
      let inner = child_doc(s, content_child);
      let delim_doc = cats([shard(0), inner, shard(last_shard_idx)]);
      switch (rest) {
      | [] => delim_doc
      | [next, ..._] when is_paren_or_bracket(next) =>
        /* application args stay attached: f@<T>(x), f@<T>@<U> */
        cats([delim_doc, segment_to_doc(s, rest)])
      | _ => cats([delim_doc, Break, Group(segment_to_doc(s, rest))])
      };
    | _ => fallback()
    }

  /* case/end */
  | ["case", "end"] =>
    switch (triples) {
    | [(_, body_child, _)] =>
      let inner = child_doc(s, body_child);
      let tile_doc =
        Group(
          cats([shard(0), Space, inner, Break, shard(last_shard_idx)]),
        );
      tile_with_rest(tile_doc);
    | _ => fallback()
    }

  /* test/end: flat when it fits; otherwise break after "test" with
     "end" trailing the last body line */
  | ["test", "end"] =>
    switch (triples) {
    | [(_, body_child, _)] =>
      let inner = child_doc(s, body_child);
      let tile_doc =
        Group(
          cats([
            shard(0),
            Nest(
              indent_unit,
              cats([
                Break,
                Group(cats([inner, Space, shard(last_shard_idx)])),
              ]),
            ),
          ]),
        );
      tile_with_rest(tile_doc);
    | _ => fallback()
    }

  /* hint/test/end: hint message on first line, test on own line, end trails body */
  | ["hint", "test", "end"] =>
    switch (triples) {
    | [(_, msg_child, _), (_, body_child, _)] =>
      let msg = child_doc(s, msg_child);
      let inner = child_doc(s, body_child);
      let tile_doc =
        cats([
          shard(0),
          Space,
          msg,
          HardBreak,
          shard(1),
          Nest(
            indent_unit,
            cats([
              HardBreak,
              Group(cats([inner, Space, shard(last_shard_idx)])),
            ]),
          ),
        ]);
      tile_with_rest(tile_doc);
    | _ => fallback()
    }

  /* induction/end: the scrutinee stays on the keyword line; each case
     rule sits on its own line with its body on the following line(s),
     indented; `end` closes on its own line:
       induction x
       | p =>
         body
       end
     A bare `induction scrut end` (no cases yet) gets the generic
     operand-form layout instead. */
  | ["induction", "end"] =>
    switch (triples) {
    | [(_, body_child, _)] =>
      switch (split_at_next_rule(strip_whitespace(body_child))) {
      | (_, []) =>
        let inner = child_doc(s, body_child);
        let tile_doc =
          Group(
            cats([shard(0), Space, inner, Break, shard(last_shard_idx)]),
          );
        tile_with_rest(tile_doc);
      | (scrut, rules) =>
        let scrut_doc =
          switch (scrut) {
          | [] => Empty
          | _ => cats([Space, Group(segment_to_doc(s, scrut))])
          };
        let rec rules_doc = (pieces: list(Piece.t)): doc =>
          switch (pieces) {
          | [] => Empty
          | [r, ...rest'] =>
            let (body, remaining) = split_at_next_rule(rest');
            let body_doc =
              switch (body) {
              | [] => Empty
              | _ =>
                Nest(
                  indent_unit,
                  cats([HardBreak, Group(segment_to_doc(s, body))]),
                )
              };
            cats([
              piece_doc(r),
              body_doc,
              switch (remaining) {
              | [] => Empty
              | _ => cats([HardBreak, rules_doc(remaining)])
              },
            ]);
          };
        let tile_doc =
          cats([
            shard(0),
            scrut_doc,
            HardBreak,
            rules_doc(rules),
            HardBreak,
            shard(last_shard_idx),
          ]);
        tile_with_rest(tile_doc);
      }
    | _ => fallback()
    }

  /* Other operand forms ending in "end".
     Same treatment as case/end (Space after keyword, Break before end). */
  | [_, "end"] =>
    switch (triples) {
    | [(_, body_child, _)] =>
      let inner = child_doc(s, body_child);
      let tile_doc =
        Group(
          cats([shard(0), Space, inner, Break, shard(last_shard_idx)]),
        );
      tile_with_rest(tile_doc);
    | _ => fallback()
    }

  /* Rule |/=> (case rule tiles with children) */
  | ["|", "=>"] =>
    switch (triples) {
    | [(_, pat_child, _)] =>
      cats([
        shard(0),
        Space,
        child_doc(s, pat_child),
        Space,
        shard(last_shard_idx),
      ])
    | _ => piece_doc(Tile.to_piece(t))
    }

  /* Filter/use forms: hide/eval/pause/debug/use expr in body.
     Simple binding-like prefix: chains with HardBreak like let-chains. */
  | [_, "in"] =>
    switch (triples) {
    | [(_, expr_child, _)] =>
      let tile_doc =
        Group(
          cats([
            shard(0),
            Space,
            Group(child_doc(s, expr_child)),
            Space,
            shard(last_shard_idx),
          ]),
        );
      Group(cats([tile_doc, body_doc(true)]));
    | _ => fallback()
    }

  /* Prefix binding forms with no trailing delimiter (module items
     let/= and type/=): header stays together, body nests after = */
  | [_, "="] =>
    switch (triples) {
    | [(_, pat_child, _)] =>
      let prefix =
        cats([
          shard(0),
          Space,
          Group(child_doc(s, pat_child)),
          Space,
          shard(1),
        ]);
      switch (rest) {
      | [] => Group(prefix)
      | _ =>
        switch (try_hanging_delim(rest, ())) {
        | Some(hanging) => Group(cats([prefix, Space, hanging]))
        | None =>
          Group(
            cats([
              prefix,
              Nest(
                indent_unit,
                cats([Break, Group(segment_to_doc(s, rest))]),
              ),
            ]),
          )
        }
      };
    | _ => fallback()
    }

  /* Generic multi-keyword tile: interleave shards and children with Break.
     Children get Nest so multi-line children indent relative to keywords.
     The tile is wrapped in a Group so it stays flat when it fits (e.g.
     `eval 1 at 0 end`) even when the surrounding context is breaking — for
     instance a proof step before a `;`. Tiles whose final token is `end`
     are self-contained operand forms (the proof steps eval/axiom/rewrite),
     so we attach a trailing `;` and hard-break after it like case/test. */
  | _ =>
    let rec build_rest =
            (idx, triples: list((Tile.t, Segment.t, Tile.t))): doc =>
      switch (triples) {
      | [] => Empty
      | [(_, child, _), ...rest_triples] =>
        cats([
          Space,
          Nest(indent_unit, child_doc(s, child)),
          Break,
          shard(idx),
          build_rest(idx + 1, rest_triples),
        ])
      };
    let tile_doc = Group(cats([shard(0), build_rest(1, triples)]));
    let ends_in_end = List.nth_opt(t.label, last_shard_idx) == Some("end");
    if (ends_in_end) {
      tile_with_rest(tile_doc);
    } else {
      switch (rest) {
      | [] => tile_doc
      | _ => cats([tile_doc, body_doc(false)])
      };
    };
  };
}

/* Group a semi tail only when it is the final (semi-less) item: the
   trailing item needs its own group (else its infix chains inherit the
   broken mode), but grouping a tail that still contains semis would
   let multiple items re-pack onto one line mid-block */
and semi_tail_doc = (s: settings, after: list(Piece.t)): doc => {
  let has_semi = List.exists(is_semi, after);
  has_semi ? segment_to_doc(s, after) : Group(segment_to_doc(s, after));
}

/* Split at the first top-level semicolon, but only when the item
   before it has 2+ pieces (single-piece items keep their specialized
   handling below) */
and split_at_semi_multi =
    (ps: list(Piece.t)): option((list(Piece.t), Piece.t, list(Piece.t))) => {
  let rec go = (acc, ps: list(Piece.t)) =>
    switch (ps) {
    | [] => None
    | [p, ...rest] when is_semi(p) =>
      List.length(acc) >= 2 ? Some((List.rev(acc), p, rest)) : None
    | [p, ...rest] => go([p, ...acc], rest)
    };
  go([], ps);
}

/* Build a doc from a list of content pieces (whitespace already stripped).
   Uses all-or-nothing for infix/comma chains, waterfall for compound forms.
   Tiles with children are decomposed on-demand via build_tile_doc.

   Implemented as a tail-recursive loop (seg_loop): branches whose doc is
   <prefix> ⧺ <doc of rest> push the prefix onto a reversed accumulator
   and tail-call, so segment length (e.g. the long comma chains of large
   list/tuple values) doesn't grow the call stack under js_of_ocaml.
   seg_finish folds the accumulator back into the right-nested Cat spine
   the naive recursion would have built. Branches that wrap the remainder
   in Group (waterfall layout) still recurse, but their depth is bounded
   by term nesting rather than segment length. */
and segment_to_doc = (s: settings, pieces: list(Piece.t)): doc =>
  seg_loop(s, [], pieces)

/* Wrap accumulated prefix docs (reversed) around the terminal doc */
and seg_finish = (acc_rev: list(doc), last: doc): doc =>
  List.fold_left((acc, d) => Cat(d, acc), last, acc_rev)

and seg_loop = (s: settings, acc_rev: list(doc), pieces: list(Piece.t)): doc =>
  switch (pieces) {
  | [] => seg_finish(acc_rev, Empty)
  | [p] =>
    switch (p) {
    /* Single tile with children: decompose */
    | Tile(t) when List.length(t.children) > 0 =>
      seg_finish(acc_rev, build_tile_doc(s, t, []))
    | _ => seg_finish(acc_rev, piece_doc(p))
    }

  /* Tile with children followed by semicolon: decompose the tile.
     build_tile_doc handlers (test/end, case/end, etc.) handle the semi
     in rest, so the tile gets proper Group wrapping for layout. */
  | [Tile(t), semi, ...rest]
      when List.length(t.children) > 0 && is_semi(semi) =>
    seg_finish(acc_rev, build_tile_doc(s, t, [semi, ...rest]))

  /* Semicolon sequence with a multi-piece item: the item is an
     independent layout group, so hard breaks after later semis can't
     force it to explode. (Without this, the first tile's body-doc
     swallows the whole remaining item list.) */
  | _ when Option.is_some(split_at_semi_multi(pieces)) =>
    let (item, semi, after) = Option.get(split_at_semi_multi(pieces));
    seg_finish(
      acc_rev,
      cats([
        Group(segment_to_doc(s, item)),
        piece_doc(semi),
        switch (after) {
        | [] => Empty
        | _ =>
          cats([s.soft_semis ? Break : HardBreak, semi_tail_doc(s, after)])
        },
      ]),
    );

  /* Piece followed by semicolon: keep semi with left operand, hard break */
  | [p, semi, ...rest] when is_semi(semi) =>
    let left = cats([piece_doc(p), piece_doc(semi)]);
    switch (rest) {
    | [] => seg_finish(acc_rev, left)
    | _ =>
      let br = s.soft_semis ? Break : HardBreak;
      /* semi_tail_doc semantics, kept tail-recursive when the tail has
         more semis (segment_to_doc) and Group-wrapped when it doesn't */
      if (List.exists(is_semi, rest)) {
        seg_loop(s, [Cat(left, br), ...acc_rev], rest);
      } else {
        seg_finish(
          acc_rev,
          cats([left, br, Group(segment_to_doc(s, rest))]),
        );
      };
    };

  /* Semicolon at start: hard break after */
  | [p, ...rest] when is_semi(p) =>
    seg_finish(
      acc_rev,
      cats([piece_doc(p), HardBreak, Group(segment_to_doc(s, rest))]),
    )

  /* Piece followed by comma: keep comma with left operand, break after.
     Trailing comments after comma stay on the same line.
     All-or-nothing: no Group wrapper on rest. */
  | [p, comma, ...rest] when is_comma(comma) =>
    let (comments, rest_after) = absorb_comments(rest);
    let left =
      List.fold_left(
        (acc, c) => Cat(acc, cats([Space, piece_doc(c)])),
        cats([piece_doc(p), piece_doc(comma)]),
        comments,
      );
    switch (rest_after) {
    | [] => seg_finish(acc_rev, left)
    | _ => seg_loop(s, [Cat(left, Break), ...acc_rev], rest_after)
    };

  /* Case rule (|...=>): group rule with its body, HardBreak between rules
     so case rules always appear on separate lines */
  | [p, ...rest] when is_case_rule_tile(p) =>
    let (body, remaining) = split_at_next_rule(rest);
    let body_doc =
      switch (body) {
      | [] => Empty
      | _ =>
        Nest(indent_unit, cats([Break, Group(segment_to_doc(s, body))]))
      };
    let rule_doc = Group(cats([piece_doc(p), body_doc]));
    switch (remaining) {
    | [] => seg_finish(acc_rev, rule_doc)
    | _ => seg_loop(s, [Cat(rule_doc, HardBreak), ...acc_rev], remaining)
    };

  /* Dot accessor: keep tight, no space or break */
  | [p, op, ...rest] when is_infix(op) && is_dot(op) =>
    seg_loop(s, [Cat(piece_doc(p), piece_doc(op)), ...acc_rev], rest)

  /* Infix operator: precedence-aware chain splitting.
     Find the loosest operator, split the whole expression there,
     and wrap each operand in a Group so tight sub-expressions stay flat. */
  | [p, op, ...rest] when is_infix(op) =>
    let all = [p, op, ...rest];
    let chain_doc =
      switch (find_loosest_prec(all)) {
      | Some(prec) =>
        let (operands, operators) = split_infix_chain(prec, all);
        build_infix_chain_doc(s, operands, operators);
      | None =>
        cats([
          piece_doc(p),
          Break,
          piece_doc(op),
          switch (rest) {
          | [] => Empty
          | _ => cats([Space, segment_to_doc(s, rest)])
          },
        ])
      };
    seg_finish(acc_rev, chain_doc);

  /* Tile with children: decompose on-demand */
  | [Tile(t), ...rest] when List.length(t.children) > 0 =>
    seg_finish(acc_rev, build_tile_doc(s, t, rest))

  /* Single-token prefix (leading +): keep attached via Space */
  | [p, ...rest] when is_single_prefix(p) =>
    seg_loop(s, [Cat(piece_doc(p), Space), ...acc_rev], rest)

  /* Infix operator at start of piece list (e.g., + between sum type
     constructors after preceding piece was processed): keep attached
     to the following piece with Space, since there's nothing to its left. */
  | [p, ...rest] when is_infix(p) =>
    seg_loop(s, [Cat(piece_doc(p), Space), ...acc_rev], rest)

  /* Default: space between pieces, Group for independent breaking.
     Trailing comments stay attached to the preceding piece.
     HardBreak before case rules so they always start on a new line.
     Application tightening: when a right-convex piece is followed by
     a paren/bracket, concatenate directly (no Break) so layout sees
     the correct combined width (e.g., RecordHarvest(Harvest) as a unit). */
  | [p, ...rest] =>
    let (comments, rest_after) =
      if (is_comment(p)) {
        (
          /* Don't absorb: standalone comment shouldn't grab following comments */
          [],
          rest,
        );
      } else {
        absorb_comments(rest);
      };
    let p_doc = piece_with_comments(p, comments);
    switch (rest_after) {
    | [] => seg_finish(acc_rev, p_doc)
    | _ when is_comment(p) =>
      /* a standalone comment owns its line */
      seg_finish(
        acc_rev,
        cats([p_doc, HardBreak, Group(segment_to_doc(s, rest_after))]),
      )
    | [next, ..._] when is_case_rule_tile(next) =>
      seg_loop(s, [Cat(p_doc, HardBreak), ...acc_rev], rest_after)
    | [next, ..._] when is_right_convex(p) && is_paren_or_bracket(next) =>
      seg_loop(s, [p_doc, ...acc_rev], rest_after)
    | _ =>
      seg_finish(
        acc_rev,
        cats([p_doc, Break, Group(segment_to_doc(s, rest_after))]),
      )
    };
  }

/* Build doc for an infix chain: operands joined by Break+op+Space.
   Comma operators stay with the left operand (trailing comma style).
   Label = operators stay with the left operand (break before value).
   Leading comments in each operand are moved to the preceding line
   so that trailing comments stay with their code (e.g., after commas). */
and build_infix_chain_doc =
    (s: settings, operands: list(list(Piece.t)), operators: list(Piece.t))
    : doc =>
  switch (operands) {
  | [] => Empty
  | [first, ...rest_operands] =>
    let first_doc = Group(segment_to_doc(s, first));
    /* Fill mode for long non-comma chains (e.g. wide sum types): the
       all-or-nothing layout below would put one operand per line, so
       instead pack greedily, hard-breaking between packed lines. Width
       budget leaves room for indentation (re-derived downstream). */
    /* Fill only operator chains where breaking mid-chain reads well
       (sums, arithmetic, arrows); never split label = value, x : T,
       or comma sequences */
    let no_fill = (op: Piece.t) =>
      is_comma(op)
      || is_label_eq(op)
      || (
        switch (op) {
        | Tile({label: ["="], _})
        | Tile({label: [":"], _}) => true
        | _ => false
        }
      );
    let is_comma_chain = List.exists(no_fill, operators);
    let chain_w =
      List.fold_left(
        (acc, seg) => acc + segment_flat_width(seg) + 1,
        List.fold_left(
          (acc, op) => acc + piece_width(op) + 1,
          0,
          operators,
        ),
        operands,
      );
    let budget = s.width - 8;
    if (!is_comma_chain && chain_w > budget) {
      let steps = List.combine(operators, rest_operands);
      let (doc, _) =
        List.fold_left(
          ((acc, line_w), (op, operand)) => {
            let step_w = piece_width(op) + segment_flat_width(operand) + 2;
            let operand_doc = Group(segment_to_doc(s, operand));
            line_w + step_w > budget
              ? (
                cats([acc, HardBreak, piece_doc(op), Space, operand_doc]),
                step_w,
              )
              : (
                cats([acc, Space, piece_doc(op), Space, operand_doc]),
                line_w + step_w,
              );
          },
          (first_doc, segment_flat_width(first)),
          steps,
        );
      doc;
    } else {
      let rec join = (acc, ops, operands) =>
        switch (ops, operands) {
        | ([], _)
        | (_, []) => acc
        | ([op, ...rest_ops], [operand, ...rest_operands]) =>
          /* Absorb leading comments from the next operand and keep them
             on the previous line (after the operator/comma). This preserves
             trailing comment style: `x, # comment #\ny` not `x,\n# comment # y` */
          let (leading_comments, actual_operand) = absorb_comments(operand);
          let operand_doc = Group(segment_to_doc(s, actual_operand));
          let comment_suffix =
            List.fold_left(
              (acc, c) => Cat(acc, cats([Space, piece_doc(c)])),
              Empty,
              leading_comments,
            );
          /* An operand that is a single delimited tile hangs: keep
             the operator on the current line and let the delimiters
             break internally (`module M : {` ... `} = {` ... `}`,
             `... == {` ... `} end`) */
          let hangable =
            switch (actual_operand) {
            | [Tile({label: ["{", "}"], children, _})]
                when List.length(children) > 0 =>
              true
            | _ => false
            };
          let next =
            if (is_comma(op)) {
              cats([acc, piece_doc(op), comment_suffix, Break, operand_doc]);
            } else if (is_label_eq(op)) {
              cats([
                acc,
                comment_suffix,
                Space,
                piece_doc(op),
                Break,
                operand_doc,
              ]);
            } else if (hangable) {
              cats([
                acc,
                comment_suffix,
                Space,
                piece_doc(op),
                Space,
                operand_doc,
              ]);
            } else {
              cats([
                acc,
                comment_suffix,
                Break,
                piece_doc(op),
                Space,
                operand_doc,
              ]);
            };
          join(next, rest_ops, rest_operands);
        };
      join(first_doc, operators, rest_operands);
    };
  };

/* === Post-processing: tight function application === */

/* Remove spaces/newlines between convex-right pieces and following parens/brackets.
   e.g., f (5) → f(5), f\n(5) → f(5), but let x = 5 in (x) stays unchanged.
   Also remove spaces around dot accessor: g . width → g.width
   Tail-recursive (reversed accumulator) so output length doesn't
   grow the call stack. */
let tighten_applications = (outputs: list(output)): list(output) => {
  let rec go = (acc, outputs) =>
    switch (outputs) {
    | [OPiece(prev), OSpace, OPiece(next), ...rest]
        when is_right_convex(prev) && is_paren_or_bracket(next) =>
      go([OPiece(prev), ...acc], [OPiece(next), ...rest])
    | [OPiece(prev), ONewline, OPiece(next), ...rest]
        when is_right_convex(prev) && is_paren_or_bracket(next) =>
      go([OPiece(prev), ...acc], [OPiece(next), ...rest])
    /* Remove space before dot */
    | [OPiece(prev), OSpace, OPiece(dot), ...rest] when is_dot(dot) =>
      go([OPiece(prev), ...acc], [OPiece(dot), ...rest])
    /* Remove space after dot */
    | [OPiece(dot), OSpace, OPiece(next), ...rest] when is_dot(dot) =>
      go([OPiece(dot), ...acc], [OPiece(next), ...rest])
    | [out, ...rest] => go([out, ...acc], rest)
    | [] => List.rev(acc)
    };
  go([], outputs);
};

/* === Main formatting === */

let format_segment = (~settings: settings, seg: Segment.t): Segment.t => {
  /* Step 1: Detect blank lines in original segment before stripping */
  let blank_lines = classify_blank_lines(seg);
  absorbable_comments := classify_trailing_comments(seg);

  /* Step 2: Strip whitespace, build doc */
  let content = strip_whitespace(seg);
  switch (content) {
  | [] => []
  | _ =>
    let doc = Group(segment_to_doc(settings, content));
    /* Step 3: Layout (start at indent 0, Breaking mode) */
    let outputs = layout(settings.width, 0, [(0, Breaking, doc)]);
    /* Step 4: Post-process (tight application) */
    let outputs = tighten_applications(outputs);
    /* Step 5: Convert to segment */
    let seg = output_to_segment(outputs);
    /* Step 6: Re-insert blank lines from original */
    let seg = reinsert_blank_lines(blank_lines, seg);
    /* Step 7: Reassemble tiles from single-shard pieces */
    Segment.reassemble(seg);
  };
};

/* Main entry point: pretty-print a segment to fit within width columns */
let prettify =
    (
      ~width: int=default_settings.width,
      ~settings=default_settings,
      seg: Segment.t,
    )
    : Segment.t => {
  let out =
    format_segment(
      ~settings={
        ...settings,
        width,
      },
      seg,
    );
  /* Canonicalize the indentation of the emitted linebreaks via the
     editor's single indentation authority, so pretty output is a
     fixpoint of Format (Indentation.level_map) for every caller */
  let indent_map = Indentation.level_map(out);
  Indentation.fix_indentation_in_segment(indent_map, out);
};

/* === Legacy API (used by ExpToSegment.re) === */

type pretty = Segment.t;

let p_concat = (pretty2: pretty, pretty1: pretty) => pretty1 @ pretty2;
let p_or = (_pretty2: pretty, pretty1: pretty) => pretty1;
let p_orif = (cond, pretty2: pretty, pretty1: pretty) =>
  if (cond) {pretty1} else {pretty2};
let p_just = (segment: Segment.t): pretty => segment;

let p_concat = (pretties: list(pretty)) =>
  List.fold_left(p_concat, [], pretties);

let (let+) = (pretty, f) => f(pretty);
let (and+) = (pretty1, pretty2) => (pretty1, pretty2);

let ( let* ) = (pretty, f) => f(pretty);
let ( and* ) = (pretty1, pretty2) => (pretty1, pretty2);

let all = x => x;

let select: pretty => Segment.t = x => x;
