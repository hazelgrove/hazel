let is_comma = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => t.label == [","]
  | _ => false
  };

let is_case_rule = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (t.label) {
    //| _ when t.label == ["i"] => true /* hack to reduce let-in jank; comes at a cost */
    | ["|"] => true /* hack */
    | ["|", "=>"] => true
    | _ => false
    }
  | _ => false
  };

let ends_with_in = (t: Tile.t): bool =>
  switch (t.label |> List.rev) {
  | ["in", ..._] => true
  | _ => false
  };

/* Linebreaks following these tiles should increment the indent */
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

/* Remove non-contentful items (whitespace and concave grout) */
let trim_non_content: Segment.t => Segment.t =
  List.filter_map(
    fun
    | Piece.Grout({shape: Concave, _}) => None
    | Secondary(s) when Secondary.is_space(s) => None
    | p => Some(p),
  );

let prev_pieces = (seg: Segment.t): list(option(Piece.t)) =>
  seg
  |> List.map(Option.some)
  |> List.cons(None)
  |> List.rev
  |> List.tl
  |> List.rev;

let next_pieces = (seg: Segment.t): list(option(Piece.t)) =>
  seg
  |> List.map(Option.some)
  |> List.rev
  |> List.cons(None)
  |> List.rev
  |> List.tl;

/* Memoize for perf */
let indent_hash = Hashtbl.create(10000);

/* While a traversal would in isolation be move efficient
 * than unioning, we adopt the approach that avoids taking
 * the map as an argument to make memo hits more likely. */
let union_all =
  List.fold_left(
    (map, new_map) => Id.Map.union((_, a, _) => Some(a), new_map, map),
    Id.Map.empty,
  );

let rec shallow_complete_segment = (seg: Segment.t): Segment.t =>
  switch (seg) {
  | [] => []
  | [Tile(t), ...rest] when !Tile.is_complete(t) => [
      Tile({
        ...t,
        shards: List.init(List.length(t.label), i => i),
        children: t.children @ [shallow_complete_segment(rest)],
        // note wrong number of children
      }),
    ]
  | [p, ...rest] => [p, ...shallow_complete_segment(rest)]
  };

/* Split segment at first run of two consecutive linebreaks where all incomplete tiles occur before those linebreaks */
//TODO(andrew): should maybe(??) split along all double linebreaks and complete subsegs? pros and cons...
let split_at_consecutive_linebreaks =
    (seg: Segment.t): option((Segment.t, Segment.t)) => {
  let rec find_split_point =
          (seg: Segment.t, acc: Segment.t, incomplete_before: bool)
          : option((Segment.t, Segment.t)) => {
    switch (seg) {
    | [] => None
    | [p, ...rest] =>
      let incomplete_here =
        switch (p) {
        | Tile(t) => !Tile.is_complete(t)
        | _ => false
        };

      let incomplete_before' = incomplete_before || incomplete_here;

      switch (p) {
      | Secondary(w) when Secondary.is_linebreak(w) =>
        /* Check if next piece is also a linebreak */
        switch (rest) {
        | [Secondary(w2), ...rest2] when Secondary.is_linebreak(w2) =>
          /* Found two consecutive linebreaks */
          if (incomplete_before') {
            /* All incomplete tiles are before this point, so we can split here */
            Some((
              List.rev([p, ...acc]),
              /* note to live one linebreak in and one out (empty line) */
              [Secondary(w2), ...rest2],
            ));
          } else {
            /* Continue searching */
            find_split_point(
              rest2,
              [Secondary(w2), p, ...acc],
              incomplete_before',
            );
          }
        | _ =>
          /* Single linebreak, continue */
          find_split_point(rest, [p, ...acc], incomplete_before')
        }
      | _ =>
        /* Not a linebreak, continue */
        find_split_point(rest, [p, ...acc], incomplete_before')
      };
    };
  };

  find_split_point(seg, [], false);
};

let rec go' = ((not_top, base: int, seg: Segment.t)) => {
  let trimmed_seg = trim_non_content(seg);
  let trimmed_seg =
    switch (split_at_consecutive_linebreaks(trimmed_seg)) {
    | None => shallow_complete_segment(trimmed_seg)
    | Some((before, after)) => shallow_complete_segment(before) @ after
    };
  let (_, map) =
    List.fold_left2(
      ((level: int, map: Id.Map.t(int)), p: Piece.t, prev_next) => {
        switch (p) {
        | Secondary(w) when Secondary.is_linebreak(w) =>
          let (prev, next) = prev_next;

          let level =
            // switch (prev_next) {
            // | (_, None) => base
            // | (_, Some(next)) when is_comma(next) => base + 2
            // | (None, _) when not_top => level + 2
            // | (Some(prev), _) when is_incrementor(prev) => level + 2
            // | (Some(prev), _) when is_comma(prev) => base + 2
            // | (_, Some(next)) when is_case_rule(next) => base
            // | _ => level
            // };
            if (next |> Option.map(is_comma) |> Option.value(~default=false)) {
              base + 2;
            } else if (prev
                       |> Option.map(is_comma)
                       |> Option.value(~default=false)) {
              base + 2;
            } else if (prev
                       |> Option.map(is_incrementor)
                       |> Option.value(~default=not_top)) {
              level + 2;
            } else if (next
                       |> Option.map(is_case_rule)
                       |> Option.value(~default=true)) {
              base;
            } else {
              level;
            };
          (level, Id.Map.add(w.id, level, map));
        | Secondary(_)
        | Grout(_)
        | Projector(_) => (level, map) //TODO(andrew)
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
      trimmed_seg,
      List.combine(prev_pieces(trimmed_seg), next_pieces(trimmed_seg)),
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
