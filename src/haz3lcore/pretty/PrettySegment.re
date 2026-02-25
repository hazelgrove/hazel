/* Pretty printer for Hazel segments.

   Uses a Wadler/Lindig-style document IR with a greedy layout algorithm.
   Converts Segment.t → Doc → Segment.t, inserting linebreaks to keep
   code within a target line width. Indentation of linebreaks is handled
   downstream by Indentation.re / Measured.re. */

/* === Document IR === */

type doc =
  | Empty
  | Piece(Piece.t, int) /* piece with pre-computed flat width */
  | Space /* always emits a space */
  | Break /* space if flat, newline if broken */
  | HardBreak /* always a newline */
  | Cat(doc, doc) /* concatenation */
  | Group(doc); /* try flat first; if doesn't fit, use breaks */

/* === Width computation === */

let rec piece_width = (p: Piece.t): int =>
  switch (p) {
  | Tile(t) =>
    let label_w =
      List.fold_left((acc, s) => acc + Token.length(s), 0, t.label);
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
   Returns a bool list (one per content piece) indicating whether
   each content piece had a blank line (2+ newlines) before it. */
let classify_blank_lines = (seg: Segment.t): list(bool) => {
  let rec go = (newline_count, seg) =>
    switch (seg) {
    | [] => []
    | [p, ...rest] =>
      if (is_whitespace(p)) {
        go(is_linebreak(p) ? newline_count + 1 : newline_count, rest);
      } else {
        [newline_count >= 2, ...go(0, rest)];
      }
    };
  go(0, seg);
};

/* Re-insert blank lines into formatted segment based on blank line flags.
   Walks formatted output, matching content pieces to flags in order. */
let reinsert_blank_lines =
    (flags: list(bool), formatted: Segment.t): Segment.t => {
  let nl = () => Piece.secondary(Secondary.mk_newline(Id.mk()));
  let rec go = (flags, formatted) =>
    switch (formatted) {
    | [] => []
    | [p, ...rest] =>
      if (is_whitespace(p)) {
        [p, ...go(flags, rest)];
      } else {
        switch (flags) {
        | [true, ...rest_flags] => [nl(), p, ...go(rest_flags, rest)]
        | [false, ...rest_flags] => [p, ...go(rest_flags, rest)]
        | [] => [p, ...go([], rest)]
        };
      }
    };
  go(flags, formatted);
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

let is_tuplabel_eq = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["="], mold, _}) =>
    Mold.is_infix_op(mold)
    && (
      switch (mold.nibs) {
      | ({shape: Concave(prec), _}, _) => prec == Precedence.lab
      | _ => false
      }
    )
  | _ => false
  };

let is_infix = (p: Piece.t): bool =>
  switch (p) {
  | Tile({mold, label: [_], _}) =>
    Mold.is_infix_op(mold) && !is_comma(p) && !is_semi(p)
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

let is_paren_or_bracket = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: ["(", ")"], _})
  | Tile({label: ["[", "]"], _}) => true
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

/* Build a doc from a list of content pieces (whitespace already stripped).
   Uses all-or-nothing for infix/comma chains, waterfall for compound forms. */
let rec segment_to_doc = (pieces: list(Piece.t)): doc =>
  switch (pieces) {
  | [] => Empty
  | [p] => piece_doc(p)

  /* Piece followed by semicolon: keep semi with left operand, hard break */
  | [p, semi, ...rest] when is_semi(semi) =>
    Cat(
      Cat(piece_doc(p), piece_doc(semi)),
      switch (rest) {
      | [] => Empty
      | _ => Cat(HardBreak, segment_to_doc(rest))
      },
    )

  /* Semicolon at start: hard break after */
  | [p, ...rest] when is_semi(p) =>
    Cat(piece_doc(p), Cat(HardBreak, segment_to_doc(rest)))

  /* Piece followed by comma: keep comma with left operand, break after.
     All-or-nothing: no Group wrapper on rest. */
  | [p, comma, ...rest] when is_comma(comma) =>
    Cat(
      Cat(piece_doc(p), piece_doc(comma)),
      switch (rest) {
      | [] => Empty
      | _ => Cat(Break, segment_to_doc(rest))
      },
    )

  /* Case rule (|...=>): group rule with its body, HardBreak between rules
     so case rules always appear on separate lines */
  | [p, ...rest] when is_case_rule_tile(p) =>
    let (body, remaining) = split_at_next_rule(rest);
    let body_doc =
      switch (body) {
      | [] => Empty
      | _ => Cat(Break, Group(segment_to_doc(body)))
      };
    let rule_doc = Group(Cat(piece_doc(p), body_doc));
    switch (remaining) {
    | [] => rule_doc
    | _ => Cat(rule_doc, Cat(HardBreak, segment_to_doc(remaining)))
    };

  /* TupLabel (label = value): keep label and = together */
  | [p, op, ...rest] when is_tuplabel_eq(op) =>
    Cat(
      Cat(piece_doc(p), Cat(Space, piece_doc(op))),
      switch (rest) {
      | [] => Empty
      | _ => Cat(Space, segment_to_doc(rest))
      },
    )

  /* Infix operator: break before operator, space between op and operand.
     All-or-nothing: no Group wrapper on rest. */
  | [p, op, ...rest] when is_infix(op) =>
    Cat(
      piece_doc(p),
      Cat(
        Break,
        Cat(
          piece_doc(op),
          switch (rest) {
          | [] => Empty
          | _ => Cat(Space, segment_to_doc(rest))
          },
        ),
      ),
    )

  /* Compound prefix form (let, fun, if, etc.): body in own Group.
     Binding forms (ending with "in": let, type, hint) use HardBreak
     when followed by another compound prefix, giving uniform
     let-chain formatting. Non-binding compound prefixes (fun, if)
     use width-based Group so e.g. fun n -> if ... stays flat.

     For non-binding prefixes, commas in the remaining pieces take
     priority: commas bind more loosely than fun/if in MakeTerm, so
     fun x -> a, b is (fun x -> a), b. Without this, the prefix
     absorbs past the comma, causing asymmetric tuple formatting. */
  | [p, ...rest] when is_compound_prefix(p) =>
    Cat(
      piece_doc(p),
      switch (rest) {
      | [] => Empty
      | [next, ..._] when is_binding_form(p) && is_compound_prefix(next) =>
        Cat(HardBreak, segment_to_doc(rest))
      | _ when !is_binding_form(p) =>
        switch (split_at_comma(rest)) {
        | Some((before, comma, after)) =>
          let body_doc = Group(Cat(Break, Group(segment_to_doc(before))));
          Cat(
            Cat(body_doc, piece_doc(comma)),
            switch (after) {
            | [] => Empty
            | _ => Cat(Break, segment_to_doc(after))
            },
          );
        | None => Group(Cat(Break, Group(segment_to_doc(rest))))
        }
      | _ => Group(Cat(Break, Group(segment_to_doc(rest))))
      },
    )

  /* Default: space between pieces, Group for independent breaking.
     HardBreak before case rules so they always start on a new line. */
  | [p, ...rest] =>
    Cat(
      piece_doc(p),
      switch (rest) {
      | [] => Empty
      | [next, ..._] when is_case_rule_tile(next) =>
        Cat(HardBreak, segment_to_doc(rest))
      | _ => Cat(Break, Group(segment_to_doc(rest)))
      },
    )
  };

/* === Post-processing: tight function application === */

/* Remove spaces/newlines between convex-right pieces and following parens/brackets.
   e.g., f (5) → f(5), f\n(5) → f(5), but let x = 5 in (x) stays unchanged. */
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
  | [out, ...rest] => [out, ...tighten_applications(rest)]
  | [] => []
  };

/* === Tile child formatting === */

/* Determine how children should be wrapped when the tile breaks.
   Returns (break_before, break_after) for each child index. */
let child_break_style = (label: Label.t, i: int): (bool, bool) =>
  switch (label) {
  | ["if", "then", "else"] =>
    /* Break after condition and consequent (before then/else keywords) */
    (false, true)
  | ["let", "=", "in"]
  | ["type", "=", "in"]
  | ["hint", "=", "in"] =>
    /* Break before binding (child 1 only) */
    i == 1 ? (true, false) : (false, false)
  | ["case", "end"] =>
    /* Keep scrutinee with case keyword, end on same line as last rule */
    (false, false)
  | ["|", "=>"] =>
    /* Pattern stays with |, no breaking */
    (false, false)
  | ["(", ")"]
  | ["[", "]"] =>
    /* Break before and after content */
    (true, true)
  | _ when List.length(label) >= 2 =>
    /* Generic: break before later children */
    i > 0 ? (true, false) : (false, false)
  | _ => (false, false)
  };

/* Whether a tile's children need boundary spaces in flat mode.
   Delimiter pairs like (...) and [...] render tight: f(x) not f( x ). */
let needs_boundary_spaces = (label: Label.t): bool =>
  switch (label) {
  | ["(", ")"]
  | ["[", "]"] => false
  | _ => true
  };

/* Add boundary whitespace to tile children.
   In flat mode: space before and after content (except for delimiters).
   In broken mode: break before/after based on child_break_style. */
let wrap_children =
    (~breaking: bool, label: Label.t, children: list(Segment.t))
    : list(Segment.t) => {
  let sp = () => Piece.secondary(Secondary.mk_space(Id.mk()));
  let nl = () => Piece.secondary(Secondary.mk_newline(Id.mk()));
  let is_delim = !needs_boundary_spaces(label);
  List.mapi(
    (i, child) => {
      let (break_before, break_after) = child_break_style(label, i);
      if (breaking && (break_before || break_after) && child != []) {
        let leader = break_before ? nl() : sp();
        let trailer = break_after ? nl() : sp();
        [leader, ...child] @ [trailer];
      } else if (is_delim) {
        child;
      } else {
        [sp(), ...child] @ [sp()];
      };
    },
    children,
  );
};

/* === Main formatting === */

let rec format_segment = (~width: int, seg: Segment.t): Segment.t => {
  /* Step 1: Recursively format tile children (bottom-up) */
  let seg = List.map(format_piece(~width), seg);

  /* Step 2: Detect blank lines in original segment before stripping */
  let blank_lines = classify_blank_lines(seg);

  /* Step 3: Strip whitespace, build doc */
  let content = strip_whitespace(seg);
  switch (content) {
  | [] => []
  | _ =>
    let doc = Group(segment_to_doc(content));
    /* Step 4: Layout */
    let outputs = layout(width, 0, [(Breaking, doc)]);
    /* Step 5: Post-process (tight application) */
    let outputs = tighten_applications(outputs);
    /* Step 6: Convert to segment */
    let seg = output_to_segment(outputs);
    /* Step 7: Re-insert blank lines from original */
    reinsert_blank_lines(blank_lines, seg);
  };
}
and format_piece = (~width: int, p: Piece.t): Piece.t =>
  switch (p) {
  | Tile(t) when List.length(t.children) > 0 =>
    /* Recursively format children (reduced width to account for indentation) */
    let child_width = max(width - 4, 4);
    let raw_children =
      List.map(format_segment(~width=child_width), t.children);
    /* If any child has newlines (from structural breaks), skip flat attempt */
    let has_multiline_child =
      List.exists(seg => List.exists(is_linebreak, seg), raw_children);
    /* Try flat layout with boundary spaces */
    let flat_children = wrap_children(~breaking=false, t.label, raw_children);
    let flat_tile = {
      ...t,
      children: flat_children,
    };
    let w = piece_width(Tile(flat_tile));
    if (!has_multiline_child && w <= width) {
      Tile(flat_tile);
    } else {
      /* Break: use newlines at child boundaries */
      let broken_children =
        wrap_children(~breaking=true, t.label, raw_children);
      Tile({
        ...t,
        children: broken_children,
      });
    };
  | _ => p
  };

/* Main entry point: pretty-print a segment to fit within width columns */
let prettify = (~width: int=60, seg: Segment.t): Segment.t =>
  format_segment(~width, seg);

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
