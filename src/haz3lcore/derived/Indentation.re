/* Remove non-contentful items (whitespace and concave grout) */
let trim_non_content: Segment.t => Segment.t =
  List.filter_map(
    fun
    | Piece.Grout({shape: Concave, _}) => None
    | Secondary(s) when Secondary.is_space(s) => None
    | p => Some(p),
  );

let prev_pieces = (seg: Segment.t): list(option(Piece.t)) => {
  let rec go =
          (xs: list(Piece.t), prev: option(Piece.t))
          : list(option(Piece.t)) =>
    switch (xs) {
    | [] => []
    | [x, ...xs] => [prev, ...go(xs, Some(x))]
    };
  go(seg, None);
};

let next_pieces = (seg: Segment.t): list(option(Piece.t)) => {
  let rec go = (xs: list(Piece.t)): list(option(Piece.t)) =>
    switch (xs) {
    | [] => []
    | [_] => [None]
    | [_, next, ...rest] => [Some(next), ...go([next, ...rest])]
    };
  go(seg);
};

/* Memoize for perf */
let indent_hash = Hashtbl.create(10000);

let union_all =
  List.fold_left(
    (map, new_map) => Id.Map.union((_, a, _) => Some(a), new_map, map),
    Id.Map.empty,
  );

/* This does not strictly 'complete' a segment but rather does a
 * rough version of it that suffices for indentation calculation */
let rec shallow_complete_segment = (seg: Segment.t): Segment.t =>
  switch (seg) {
  | [] => []
  | [Tile(t), ...rest] when !Tile.is_complete(t) => [
      Tile({
        ...t,
        shards: List.init(List.length(t.label), i => i),
        children: t.children @ [shallow_complete_segment(rest)],
        /* Note: Potentially wrong number of children */
      }),
    ]
  | [p, ...rest] => [p, ...shallow_complete_segment(rest)]
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
 * would be to consider dropping the backpack at the cursor, but making
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

let rec go' = ((not_top, base: int, seg: Segment.t)) => {
  let complete_trimmed_seg = complete_segment(trim_non_content(seg));
  let (_, map) =
    List.fold_left2(
      ((level: int, map: Id.Map.t(int)), p: Piece.t, prev_next) => {
        switch (p) {
        | Secondary(w) when Secondary.is_linebreak(w) =>
          let level =
            switch (prev_next) {
            | (_, Some(next)) when is_comma(next) => base + 2
            | (Some(prev), _) when is_comma(prev) => base + 2
            | (Some(prev), _) when is_incrementor(prev) => level + 2
            | (None, _) when not_top => level + 2
            | (_, Some(next)) when is_case_rule(next) => base
            | (_, None) => base
            | (_, Some(p)) when Piece.is_infix_delimiter_op_prefix(p) =>
              /* Special case for kw prefixes */
              base
            | (_, Some(_)) => level
            };
          (level, Id.Map.add(w.id, level, map));
        | Secondary(_)
        | Grout(_)
        | Projector(_) => (level, map)
        | Tile(t) =>
          let map =
            union_all([
              map,
              ...List.map(go(~not_top=true, level), t.children),
            ]);
          (level, map);
        }
      },
      (base, Id.Map.empty),
      complete_trimmed_seg,
      List.combine(
        prev_pieces(complete_trimmed_seg),
        next_pieces(complete_trimmed_seg),
      ),
    );
  map;
}
and go = (~not_top, base: int, seg: Segment.t) => {
  let arg = (not_top, base, seg);
  try(Hashtbl.find(indent_hash, arg)) {
  | _ =>
    let res = go'(arg);
    Hashtbl.add(indent_hash, arg, res);
    res;
  };
};

let level_map = (seg: Segment.t): Id.Map.t(int) =>
  go(~not_top=false, 0, seg);

/* === Helper functions for user-managed indentation === */

/* Drop leading space pieces from a segment */
let rec drop_leading_spaces = (seg: Segment.t): Segment.t =>
  switch (seg) {
  | [Piece.Secondary(s), ...rest] when Secondary.is_space(s) =>
    drop_leading_spaces(rest)
  | _ => seg
  };

/* Fix indentation in a segment using the provided indent map.
   For each linebreak, removes following spaces and inserts the
   correct number based on the indent map. */
let rec fix_indentation_in_segment =
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
    @ fix_indentation_in_segment(indent_map, rest_without_leading_spaces);
  | [Piece.Tile(t), ...rest] =>
    let children =
      List.map(fix_indentation_in_segment(indent_map), t.children);
    [
      Piece.Tile({
        ...t,
        children,
      }),
      ...fix_indentation_in_segment(indent_map, rest),
    ];
  | [p, ...rest] => [p, ...fix_indentation_in_segment(indent_map, rest)]
  };

/* Create space pieces for a given indent level */
let make_indent_spaces = (indent_level: int): Segment.t =>
  List.init(indent_level, _ => Piece.Secondary(Secondary.mk_space(Id.mk())));
