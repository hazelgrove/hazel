let ends_with_in = (t: Tile.t): bool =>
  switch (t.label |> List.rev) {
  | ["in", ..._] => true
  | _ => false
  };

/* Linebreaks following these tiles should increment the indent */
let is_incrementor = (p: Piece.t): bool =>
  switch (p) {
  //| Tile(t) when Tile.effective_label(t) == ["case"] => true
  | Tile(t) when ends_with_in(t) => false
  | Tile(t) =>
    switch (Tile.shapes(t)) {
    // convexity at beginning would exclude `| =>`
    | (_, Concave(_)) when List.length(t.label) >= 2 => true
    | _ => false
    }
  | _ => false
  };

/* Linebreaks following these tiles should reset indent
 * to its level at the beginning of the bidelimited ctx */
let is_resetter = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (Tile.shapes(t)) {
    | _ when t.label == ["i"] => true /* hack to reduce let-in jank */
    | (Concave(_), _) when t.label == ["|"] => true
    | (Concave(_), _) when t.label == ["|", "=>"] => true
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
  | [Tile(t), ...rest]
      when
        !Tile.is_complete(t) && List.length(t.label) == 2 && t.shards == [0] => [
      Tile({
        ...t,
        shards: [0, 1],
        children: t.children @ [shallow_complete_segment(rest)],
      }),
    ]
  | [Tile(t), ...rest]
      when
        !Tile.is_complete(t)
        && List.length(t.label) == 3
        && t.shards == [0, 1] => [
      Tile({
        ...t,
        shards: [0, 1, 2],
        children: t.children @ [shallow_complete_segment(rest)],
      }),
    ]
  | [p, ...rest] => [p, ...shallow_complete_segment(rest)]
  };

/* Split segment at first run of two consecutive linebreaks where all incomplete tiles occur before those linebreaks */
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
              List.rev(acc),
              rest2,
            ));
          } else {
            /* Continue searching */
            find_split_point(
              rest2,
              [p, ...acc],
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

let rec go' = ((base: int, seg: Segment.t)) => {
  let trimmed_seg =
    switch (split_at_consecutive_linebreaks(seg)) {
    | None => shallow_complete_segment(trim_non_content(seg))
    | Some((before, after)) => shallow_complete_segment(before) @ after
    };
  let (_, map) =
    List.fold_left2(
      ((level: int, map: Id.Map.t(int)), p: Piece.t, prev_next) => {
        switch (p) {
        | Secondary(w) when Secondary.is_linebreak(w) =>
          let (prev, next) = prev_next;
          let level =
            if (prev
                |> Option.map(is_incrementor)
                |> Option.value(~default=true)) {
              level + 2;
            } else if (next
                       |> Option.map(is_resetter)
                       |> Option.value(~default=true)) {
              base;
            } else {
              level;
            };
          (level, Id.Map.add(w.id, level, map));
        | Secondary(_)
        | Grout(_) => (level, map)
        | Projector(_) => (level, map) //TODO(andrew)
        | Tile(t) =>
          let map = union_all([map, ...List.map(go(level), t.children)]);
          (level, map);
        }
      },
      (base, Id.Map.empty),
      trimmed_seg,
      List.combine(prev_pieces(trimmed_seg), next_pieces(trimmed_seg)),
    );
  map;
}
and go = (base: int, seg: Segment.t) => {
  let arg = (base, seg);
  try(Hashtbl.find(indent_hash, arg)) {
  | _ =>
    let res = go'(arg);
    Hashtbl.add(indent_hash, arg, res);
    res;
  };
};

let level_map = (seg: Segment.t): Id.Map.t(int) => go(0, seg);
