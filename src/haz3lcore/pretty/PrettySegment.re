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
};

let default_settings: settings = {
  width: 60,
  break_fun_params: false,
  hanging_delimiters: true,
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
  | Group(doc); /* try flat first; if doesn't fit, use breaks */

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

/* Check if the remaining doc fits on this line (first-line check only) */
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
    | [(_, Group(x)), ...rest] => fits(remaining, [(Flat, x), ...rest])
    };
  };

/* Layout output commands */
type output =
  | OPiece(Piece.t)
  | OSpace
  | ONewline;

/* Greedy layout: process doc, deciding group modes based on fit */
let rec layout =
        (width: int, col: int, cmds: list((mode, doc))): list(output) =>
  switch (cmds) {
  | [] => []
  | [(_, Empty), ...rest] => layout(width, col, rest)
  | [(_, Piece(p, w)), ...rest] => [
      OPiece(p),
      ...layout(width, col + w, rest),
    ]
  | [(_, Space), ...rest] => [OSpace, ...layout(width, col + 1, rest)]
  | [(m, Cat(x, y)), ...rest] =>
    layout(width, col, [(m, x), (m, y), ...rest])
  | [(Flat, Break), ...rest] => [OSpace, ...layout(width, col + 1, rest)]
  | [(Breaking, Break), ...rest] => [ONewline, ...layout(width, 0, rest)]
  | [(Flat, SoftBreak), ...rest] => layout(width, col, rest) /* emit nothing */
  | [(Breaking, SoftBreak), ...rest] => [
      ONewline,
      ...layout(width, 0, rest),
    ]
  | [(_, HardBreak), ...rest] => [ONewline, ...layout(width, 0, rest)]
  | [(_, Group(x)), ...rest] =>
    if (fits(width - col, [(Flat, x), ...rest])) {
      layout(width, col, [(Flat, x), ...rest]);
    } else {
      layout(width, col, [(Breaking, x), ...rest]);
    }
  };

/* Convert layout output to segment */
let output_to_segment = (outputs: list(output)): Segment.t =>
  List.map(
    fun
    | OPiece(p) => p
    | OSpace => Piece.secondary(Secondary.mk_space(Id.mk()))
    | ONewline => Piece.secondary(Secondary.mk_newline(Id.mk())),
    outputs,
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
  let rec go = (newline_count, seg) =>
    switch (seg) {
    | [] => Id.Map.empty
    | [p, ...rest] =>
      if (is_whitespace(p)) {
        go(is_linebreak(p) ? newline_count + 1 : newline_count, rest);
      } else {
        let set = go(0, rest);
        newline_count >= 2 ? Id.Map.add(Piece.id(p), (), set) : set;
      }
    };
  go(0, seg);
};

/* Re-insert blank lines into formatted segment based on piece IDs.
   Inserts a newline before the first occurrence of each flagged ID. */
let reinsert_blank_lines =
    (blank_ids: Id.Map.t(unit), formatted: Segment.t): Segment.t => {
  let nl = () => Piece.secondary(Secondary.mk_newline(Id.mk()));
  let rec go = (remaining_ids, formatted) =>
    switch (formatted) {
    | [] => []
    | [p, ...rest] =>
      if (is_whitespace(p)) {
        [p, ...go(remaining_ids, rest)];
      } else {
        let pid = Piece.id(p);
        if (Id.Map.mem(pid, remaining_ids)) {
          /* Insert blank line and remove ID so we don't double-insert
             for other shards of the same decomposed tile */
          [nl(), p, ...go(Id.Map.remove(pid, remaining_ids), rest)];
        } else {
          [p, ...go(remaining_ids, rest)];
        };
      }
    };
  go(blank_ids, formatted);
};

let is_comment = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_comment(s)
  | _ => false
  };

/* Absorb leading comment pieces from a piece list.
   Returns (comments, remaining) where comments should stay
   on the same line as the preceding code piece. */
let rec absorb_comments =
        (pieces: list(Piece.t)): (list(Piece.t), list(Piece.t)) =>
  switch (pieces) {
  | [p, ...rest] when is_comment(p) =>
    let (more, remaining) = absorb_comments(rest);
    ([p, ...more], remaining);
  | _ => ([], pieces)
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
  let rec go = (current_rev, pieces) =>
    switch (pieces) {
    | [] => ([List.rev(current_rev)], [])
    | [p, ...rest] =>
      switch (piece_precedence(p)) {
      | Some(p_prec) when p_prec == prec =>
        let (more_operands, ops) = go([], rest);
        ([List.rev(current_rev), ...more_operands], [p, ...ops]);
      | _ => go([p, ...current_rev], rest)
      }
    };
  go([], pieces);
};

/* A shard is a single-shard piece from a multi-keyword tile (e.g., the "if"
   shard from an if/then/else tile that was decomposed via shard_of) */
let is_shard = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    List.length(t.label) >= 2
    && List.length(t.shards) == 1
    && List.length(t.children) == 0
  | _ => false
  };

/* Get the token text of a shard piece */
let shard_token = (p: Piece.t): option(string) =>
  switch (p) {
  | Tile(t) when is_shard(p) =>
    switch (t.shards) {
    | [i] => Some(List.nth(t.label, i))
    | _ => None
    }
  | _ => None
  };

/* Single-token prefix operator (e.g., leading + in sum types) */
let is_single_prefix = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => List.length(t.label) == 1 && Mold.is_prefix_op(t.mold)
  | _ => false
  };

/* Last shard of a prefix form — the keyword after which the body follows
   (e.g., "in", "->", "else"). Right nib is Concave. */
let is_body_shard = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) when is_shard(p) =>
    switch (t.shards) {
    | [i] when i == List.length(t.label) - 1 =>
      switch (Tile.shapes(t)) {
      | (_, Concave(_)) => true
      | _ => false
      }
    | _ => false
    }
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

/* A binding form is a compound prefix whose label ends with "in"
   (let, type, hint). Used to restrict HardBreak chaining to
   sequential binding chains, not fun+if or fun+fun. */
let is_binding_form = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (List.rev(t.label)) {
    | ["in", ..._] => true
    | _ => false
    }
  | _ => false
  };

/* Match opening parens/brackets for tighten_applications.
   After decomposition, closing shards (e.g., ")") should not match —
   we only want to tighten f (x) → f(x), not remove breaks before ")". */
let is_paren_or_bracket = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["(", ")"], shards, children, _})
  | Tile({label: ["[", "]"], shards, children, _}) =>
    /* Complete tile (has children) or opening shard (index 0) */
    List.length(children) > 0 || shards == [0]
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
  | _ => false
  };

/* Split pieces at the next case rule tile */
let rec split_at_next_rule =
        (pieces: list(Piece.t)): (list(Piece.t), list(Piece.t)) =>
  switch (pieces) {
  | [] => ([], [])
  | [p, ...rest] when is_case_rule_tile(p) => ([], [p, ...rest])
  | [p, ...rest] =>
    let (body, remaining) = split_at_next_rule(rest);
    ([p, ...body], remaining);
  };

/* Split pieces at the first comma. Returns None if no comma found.
   Commas bind more loosely than compound prefixes (fun, if, let),
   so we scan ahead to ensure commas split the segment before
   compound prefixes absorb past them. */
let rec split_at_comma =
        (pieces: list(Piece.t))
        : option((list(Piece.t), Piece.t, list(Piece.t))) =>
  switch (pieces) {
  | [] => None
  | [p, ...rest] when is_comma(p) => Some(([], p, rest))
  | [p, ...rest] =>
    switch (split_at_comma(rest)) {
    | Some((before, comma, after)) => Some(([p, ...before], comma, after))
    | None => None
    }
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
  let last_shard = Tile.to_piece(Tile.shard_of(t, last_shard_idx));
  let last_shard_doc = piece_doc(last_shard);

  /* Build the body doc (content after the tile in the segment).
     For binding forms, use HardBreak when next piece is also a compound
     prefix (let-chain formatting). For non-binding prefixes, commas take
     priority since they bind more loosely in MakeTerm. */
  let body_doc = (is_binding): doc =>
    switch (rest) {
    | [] => Empty
    | [next, ..._] when is_binding && is_compound_prefix(next) =>
      Cat(HardBreak, segment_to_doc(s, rest))
    | [next, ..._] when is_binding && is_case_rule_tile(next) =>
      Cat(HardBreak, segment_to_doc(s, rest))
    | _ when is_binding => Cat(Break, Group(segment_to_doc(s, rest)))
    | _ =>
      switch (split_at_comma(rest)) {
      | Some((before, comma, after)) =>
        let bd = Group(Cat(Break, Group(segment_to_doc(s, before))));
        Cat(
          Cat(bd, piece_doc(comma)),
          switch (after) {
          | [] => Empty
          | _ => Cat(Break, segment_to_doc(s, after))
          },
        );
      | None => Group(Cat(Break, Group(segment_to_doc(s, rest))))
      }
    };

  switch (t.label) {
  /* Binding forms: let/=/in, type/=/in, hint/=/in */
  | ["let", "=", "in"]
  | ["type", "=", "in"]
  | ["hint", "=", "in"] =>
    let first_shard = Tile.to_piece(Tile.shard_of(t, 0));
    let eq_shard = Tile.to_piece(Tile.shard_of(t, 1));
    switch (triples) {
    | [(_, pat_child, _), (_, binding_child, _)] =>
      let prefix =
        Cat(
          piece_doc(first_shard),
          Cat(
            Space,
            Cat(
              Group(child_doc(s, pat_child)),
              Cat(Space, piece_doc(eq_shard)),
            ),
          ),
        );
      /* Check if binding starts with a delimiter for hanging style:
         let x = (     vs     let x =
           ...                    (...)
         ) in                 in
         The opener stays on the = line; SoftBreaks handle content. */
      let binding_content = strip_whitespace(binding_child);
      let let_in_doc =
        switch (binding_content) {
        | [Tile(dt)]
            when
              s.hanging_delimiters
              && (dt.label == ["(", ")"] || dt.label == ["[", "]"])
              && List.length(dt.children) > 0 =>
          let open_shard = Tile.to_piece(Tile.shard_of(dt, 0));
          let close_shard =
            Tile.to_piece(
              Tile.shard_of(dt, List.length(dt.label) - 1),
            );
          let inner_triples = Tile.contained_children(dt);
          switch (inner_triples) {
          | [(_, inner_child, _)] =>
            let inner = child_doc(s, inner_child);
            Group(
              Cat(
                prefix,
                Cat(
                  Space,
                  Cat(
                    piece_doc(open_shard),
                    Cat(
                      SoftBreak,
                      Cat(
                        inner,
                        Cat(
                          SoftBreak,
                          Cat(piece_doc(close_shard), Cat(Space, last_shard_doc)),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            )
          | _ =>
            Group(
              Cat(
                prefix,
                Cat(Break, Cat(Group(child_doc(s, binding_child)), Cat(Space, last_shard_doc))),
              ),
            )
          };
        | _ =>
          Group(
            Cat(
              prefix,
              Cat(Break, Cat(Group(child_doc(s, binding_child)), Cat(Space, last_shard_doc))),
            ),
          )
        };
      /* Wrap entire let+body in Group so short bodies stay on the in line.
         HardBreak in body_doc for let-chains prevents the flat check. */
      Group(Cat(let_in_doc, body_doc(true)));
    | _ =>
      /* Fallback for unexpected structure */
      Cat(piece_doc(Tile.to_piece(t)), segment_to_doc(s, rest))
    };

  /* if/then/else */
  | ["if", "then", "else"] =>
    let if_shard = Tile.to_piece(Tile.shard_of(t, 0));
    let then_shard = Tile.to_piece(Tile.shard_of(t, 1));
    switch (triples) {
    | [(_, cond_child, _), (_, conseq_child, _)] =>
      /* Group(if Space Group(cond) Break then Space Group(conseq) Break else)
         Space Group(body) */
      let tile_doc =
        Group(
          Cat(
            piece_doc(if_shard),
            Cat(
              Space,
              Cat(
                Group(child_doc(s, cond_child)),
                Cat(
                  Break,
                  Cat(
                    piece_doc(then_shard),
                    Cat(
                      Space,
                      Cat(
                        Group(child_doc(s, conseq_child)),
                        Cat(Break, last_shard_doc),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        );
      Cat(tile_doc, body_doc(false));
    | _ => Cat(piece_doc(Tile.to_piece(t)), segment_to_doc(s, rest))
    };

  /* fun/-> */
  | ["fun", "->"] =>
    let fun_shard = Tile.to_piece(Tile.shard_of(t, 0));
    switch (triples) {
    | [(_, param_child, _)] =>
      /* When break_fun_params is false (default), wrap the entire
         fun...-> header in a Group so params stay on one line even
         when the body is too long. Break separates header from body
         so fits() returns true at the Break boundary. */
      let param_doc = child_doc(s, param_child);
      let header =
        Cat(
          piece_doc(fun_shard),
          Cat(Space, Cat(param_doc, Cat(Space, last_shard_doc))),
        );
      if (s.break_fun_params) {
        Cat(header, body_doc(false));
      } else {
        /* Comma case: body_doc handles comma splitting (tuples/lists).
           No-comma case: bare Break so fits() sees (Breaking, Break)
           → true, letting Group(header) go flat independently. */
        switch (rest) {
        | [] => Group(header)
        | _ =>
          switch (split_at_comma(rest)) {
          | Some(_) => Cat(Group(header), body_doc(false))
          | None =>
            Group(
              Cat(
                Group(header),
                Cat(Break, Group(segment_to_doc(s, rest))),
              ),
            )
          }
        };
      }
    | _ => Cat(piece_doc(Tile.to_piece(t)), segment_to_doc(s, rest))
    };

  /* Delimiter pairs: (...), [...] */
  | ["(", ")"]
  | ["[", "]"] =>
    let open_shard = Tile.to_piece(Tile.shard_of(t, 0));
    switch (triples) {
    | [(_, content_child, _)] =>
      let inner = child_doc(s, content_child);
      let delim_doc =
        switch (inner) {
        | Empty =>
          /* Empty delimiters: () or [] */
          Cat(piece_doc(open_shard), last_shard_doc)
        | _ =>
          Group(
            Cat(
              piece_doc(open_shard),
              Cat(SoftBreak, Cat(inner, Cat(SoftBreak, last_shard_doc))),
            ),
          )
        };
      switch (rest) {
      | [] => delim_doc
      | _ => Cat(delim_doc, Cat(Break, Group(segment_to_doc(s, rest))))
      };
    | _ => Cat(piece_doc(Tile.to_piece(t)), segment_to_doc(s, rest))
    };

  /* case/end */
  | ["case", "end"] =>
    let open_shard = Tile.to_piece(Tile.shard_of(t, 0));
    switch (triples) {
    | [(_, body_child, _)] =>
      let inner = child_doc(s, body_child);
      let tile_doc =
        Group(
          Cat(
            piece_doc(open_shard),
            Cat(Space, Cat(inner, Cat(Break, last_shard_doc))),
          ),
        );
      switch (rest) {
      | [semi, ...rest2] when is_semi(semi) =>
        Cat(
          Cat(tile_doc, piece_doc(semi)),
          switch (rest2) {
          | [] => Empty
          | _ => Cat(HardBreak, segment_to_doc(s, rest2))
          },
        )
      | [] => tile_doc
      | _ => Cat(tile_doc, Cat(Break, Group(segment_to_doc(s, rest))))
      };
    | _ => Cat(piece_doc(Tile.to_piece(t)), segment_to_doc(s, rest))
    };

  /* test/end: always break after "test", "end" trails the last body line */
  | ["test", "end"] =>
    let open_shard = Tile.to_piece(Tile.shard_of(t, 0));
    switch (triples) {
    | [(_, body_child, _)] =>
      let inner = child_doc(s, body_child);
      let tile_doc =
        Cat(
          piece_doc(open_shard),
          Cat(HardBreak, Group(Cat(inner, Cat(Space, last_shard_doc)))),
        );
      switch (rest) {
      | [semi, ...rest2] when is_semi(semi) =>
        Cat(
          Cat(tile_doc, piece_doc(semi)),
          switch (rest2) {
          | [] => Empty
          | _ => Cat(HardBreak, segment_to_doc(s, rest2))
          },
        )
      | [] => tile_doc
      | _ => Cat(tile_doc, Cat(Break, Group(segment_to_doc(s, rest))))
      };
    | _ => Cat(piece_doc(Tile.to_piece(t)), segment_to_doc(s, rest))
    };

  /* hint/test/end: hint message on first line, test on own line, end trails body */
  | ["hint", "test", "end"] =>
    let hint_shard = Tile.to_piece(Tile.shard_of(t, 0));
    let test_shard = Tile.to_piece(Tile.shard_of(t, 1));
    switch (triples) {
    | [(_, msg_child, _), (_, body_child, _)] =>
      let msg = child_doc(s, msg_child);
      let inner = child_doc(s, body_child);
      let tile_doc =
        Cat(
          Cat(piece_doc(hint_shard), Cat(Space, msg)),
          Cat(
            HardBreak,
            Cat(
              piece_doc(test_shard),
              Cat(HardBreak, Group(Cat(inner, Cat(Space, last_shard_doc)))),
            ),
          ),
        );
      switch (rest) {
      | [semi, ...rest2] when is_semi(semi) =>
        Cat(
          Cat(tile_doc, piece_doc(semi)),
          switch (rest2) {
          | [] => Empty
          | _ => Cat(HardBreak, segment_to_doc(s, rest2))
          },
        )
      | [] => tile_doc
      | _ => Cat(tile_doc, Cat(Break, Group(segment_to_doc(s, rest))))
      };
    | _ => Cat(piece_doc(Tile.to_piece(t)), segment_to_doc(s, rest))
    };

  /* Rule |/=> (case rule tiles with children) */
  | ["|", "=>"] =>
    let bar_shard = Tile.to_piece(Tile.shard_of(t, 0));
    switch (triples) {
    | [(_, pat_child, _)] =>
      /* | pat => */
      Cat(
        piece_doc(bar_shard),
        Cat(Space, Cat(child_doc(s, pat_child), Cat(Space, last_shard_doc))),
      )
    | _ => piece_doc(Tile.to_piece(t))
    };

  /* Generic multi-keyword tile: interleave shards and children with Break */
  | _ =>
    let first_shard = Tile.to_piece(Tile.shard_of(t, 0));
    let rec build_rest = (triples: list((Tile.t, Segment.t, Tile.t))): doc =>
      switch (triples) {
      | [] => Empty
      | [(_, child, r_shard), ...rest_triples] =>
        Cat(
          Space,
          Cat(
            child_doc(s, child),
            Cat(
              Break,
              Cat(
                piece_doc(Tile.to_piece(r_shard)),
                build_rest(rest_triples),
              ),
            ),
          ),
        )
      };
    let tile_doc = Cat(piece_doc(first_shard), build_rest(triples));
    switch (rest) {
    | [] => tile_doc
    | _ => Cat(tile_doc, body_doc(false))
    };
  };
}

/* Build a doc from a list of content pieces (whitespace already stripped).
   Uses all-or-nothing for infix/comma chains, waterfall for compound forms.
   Tiles with children are decomposed on-demand via build_tile_doc. */
and segment_to_doc = (s: settings, pieces: list(Piece.t)): doc =>
  switch (pieces) {
  | [] => Empty
  | [p] =>
    switch (p) {
    /* Single tile with children: decompose */
    | Tile(t) when List.length(t.children) > 0 => build_tile_doc(s, t, [])
    | _ => piece_doc(p)
    }

  /* Tile with children followed by semicolon: decompose the tile.
     build_tile_doc handlers (test/end, case/end, etc.) handle the semi
     in rest, so the tile gets proper Group wrapping for layout. */
  | [Tile(t), semi, ...rest] when List.length(t.children) > 0 && is_semi(semi) =>
    build_tile_doc(s, t, [semi, ...rest])

  /* Piece followed by semicolon: keep semi with left operand, hard break */
  | [p, semi, ...rest] when is_semi(semi) =>
    Cat(
      Cat(piece_doc(p), piece_doc(semi)),
      switch (rest) {
      | [] => Empty
      | _ => Cat(HardBreak, segment_to_doc(s, rest))
      },
    )

  /* Semicolon at start: hard break after */
  | [p, ...rest] when is_semi(p) =>
    Cat(piece_doc(p), Cat(HardBreak, segment_to_doc(s, rest)))

  /* Piece followed by comma: keep comma with left operand, break after.
     Trailing comments after comma stay on the same line.
     All-or-nothing: no Group wrapper on rest. */
  | [p, comma, ...rest] when is_comma(comma) =>
    let (comments, rest_after) = absorb_comments(rest);
    let left =
      List.fold_left(
        (acc, c) => Cat(acc, Cat(Space, piece_doc(c))),
        Cat(piece_doc(p), piece_doc(comma)),
        comments,
      );
    Cat(
      left,
      switch (rest_after) {
      | [] => Empty
      | _ => Cat(Break, segment_to_doc(s, rest_after))
      },
    )

  /* Case rule (|...=>): group rule with its body, HardBreak between rules
     so case rules always appear on separate lines */
  | [p, ...rest] when is_case_rule_tile(p) =>
    let (body, remaining) = split_at_next_rule(rest);
    let body_doc =
      switch (body) {
      | [] => Empty
      | _ => Cat(Break, Group(segment_to_doc(s, body)))
      };
    let rule_doc = Group(Cat(piece_doc(p), body_doc));
    switch (remaining) {
    | [] => rule_doc
    | _ => Cat(rule_doc, Cat(HardBreak, segment_to_doc(s, remaining)))
    };

  /* Dot accessor: keep tight, no space or break */
  | [p, op, ...rest] when is_infix(op) && is_dot(op) =>
    Cat(piece_doc(p), Cat(piece_doc(op), segment_to_doc(s, rest)))

  /* Infix operator: precedence-aware chain splitting.
     Find the loosest operator, split the whole expression there,
     and wrap each operand in a Group so tight sub-expressions stay flat. */
  | [p, op, ...rest] when is_infix(op) =>
    let all = [p, op, ...rest];
    switch (find_loosest_prec(all)) {
    | Some(prec) =>
      let (operands, operators) = split_infix_chain(prec, all);
      build_infix_chain_doc(s, operands, operators);
    | None =>
      Cat(
        piece_doc(p),
        Cat(
          Break,
          Cat(
            piece_doc(op),
            switch (rest) {
            | [] => Empty
            | _ => Cat(Space, segment_to_doc(s, rest))
            },
          ),
        ),
      )
    };

  /* Tile with children: decompose on-demand */
  | [Tile(t), ...rest] when List.length(t.children) > 0 =>
    build_tile_doc(s, t, rest)

  /* Single-token prefix (leading +): keep attached via Space */
  | [p, ...rest] when is_single_prefix(p) =>
    Cat(piece_doc(p), Cat(Space, segment_to_doc(s, rest)))

  /* Infix operator at start of piece list (e.g., + between sum type
     constructors after preceding piece was processed): keep attached
     to the following piece with Space, since there's nothing to its left. */
  | [p, ...rest] when is_infix(p) =>
    Cat(piece_doc(p), Cat(Space, segment_to_doc(s, rest)))

  /* Default: space between pieces, Group for independent breaking.
     Trailing comments stay attached to the preceding piece.
     HardBreak before case rules so they always start on a new line.
     Application tightening: when a right-convex piece is followed by
     a paren/bracket, concatenate directly (no Break) so layout sees
     the correct combined width (e.g., RecordHarvest(Harvest) as a unit). */
  | [p, ...rest] =>
    let (comments, rest_after) =
      if (is_comment(p)) {
        /* Don't absorb: standalone comment shouldn't grab following comments */
        ([], rest);
      } else {
        absorb_comments(rest);
      };
    let p_doc = piece_with_comments(p, comments);
    Cat(
      p_doc,
      switch (rest_after) {
      | [] => Empty
      | [next, ..._] when is_case_rule_tile(next) =>
        Cat(HardBreak, segment_to_doc(s, rest_after))
      | [next, ..._] when is_right_convex(p) && is_paren_or_bracket(next) =>
        segment_to_doc(s, rest_after)
      | _ => Cat(Break, Group(segment_to_doc(s, rest_after)))
      },
    )
  }

/* Build doc for an infix chain: operands joined by Break+op+Space.
   Comma operators stay with the left operand (trailing comma style).
   Label = operators stay with the left operand (break before value).
   Leading comments in each operand are moved to the preceding line
   so that trailing comments stay with their code (e.g., after commas). */
and build_infix_chain_doc =
    (s: settings, operands: list(list(Piece.t)), operators: list(Piece.t)): doc =>
  switch (operands) {
  | [] => Empty
  | [first, ...rest_operands] =>
    let first_doc = Group(segment_to_doc(s, first));
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
            (acc, c) => Cat(acc, Cat(Space, piece_doc(c))),
            Empty,
            leading_comments,
          );
        let next =
          if (is_comma(op)) {
            /* Comma stays with left operand, comments after comma */
            Cat(
              Cat(Cat(acc, piece_doc(op)), comment_suffix),
              Cat(Break, operand_doc),
            );
          } else if (is_label_eq(op)) {
            /* label =: stays with left, break before value */
            Cat(
              Cat(acc, comment_suffix),
              Cat(Space, Cat(piece_doc(op), Cat(Break, operand_doc))),
            );
          } else {
            /* Regular infix: break before operator */
            Cat(
              Cat(acc, comment_suffix),
              Cat(Break, Cat(piece_doc(op), Cat(Space, operand_doc))),
            );
          };
        join(next, rest_ops, rest_operands);
      };
    join(first_doc, operators, rest_operands);
  };

/* === Post-processing: tight function application === */

/* Remove spaces/newlines between convex-right pieces and following parens/brackets.
   e.g., f (5) → f(5), f\n(5) → f(5), but let x = 5 in (x) stays unchanged.
   Also remove spaces around dot accessor: g . width → g.width */
let rec tighten_applications = (outputs: list(output)): list(output) =>
  switch (outputs) {
  | [OPiece(prev), OSpace, OPiece(next), ...rest]
      when is_right_convex(prev) && is_paren_or_bracket(next) => [
      OPiece(prev),
      ...tighten_applications([OPiece(next), ...rest]),
    ]
  | [OPiece(prev), ONewline, OPiece(next), ...rest]
      when is_right_convex(prev) && is_paren_or_bracket(next) => [
      OPiece(prev),
      ...tighten_applications([OPiece(next), ...rest]),
    ]
  /* Remove space before dot */
  | [OPiece(prev), OSpace, OPiece(dot), ...rest] when is_dot(dot) => [
      OPiece(prev),
      ...tighten_applications([OPiece(dot), ...rest]),
    ]
  /* Remove space after dot */
  | [OPiece(dot), OSpace, OPiece(next), ...rest] when is_dot(dot) => [
      OPiece(dot),
      ...tighten_applications([OPiece(next), ...rest]),
    ]
  | [out, ...rest] => [out, ...tighten_applications(rest)]
  | [] => []
  };

/* === Main formatting === */

let format_segment = (~settings: settings, seg: Segment.t): Segment.t => {
  /* Step 1: Detect blank lines in original segment before stripping */
  let blank_lines = classify_blank_lines(seg);

  /* Step 2: Strip whitespace, build doc */
  let content = strip_whitespace(seg);
  switch (content) {
  | [] => []
  | _ =>
    let doc = Group(segment_to_doc(settings, content));
    /* Step 3: Layout */
    let outputs = layout(settings.width, 0, [(Breaking, doc)]);
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
    (~width: int=default_settings.width, ~settings=default_settings, seg: Segment.t)
    : Segment.t =>
  format_segment(~settings={...settings, width}, seg);

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
