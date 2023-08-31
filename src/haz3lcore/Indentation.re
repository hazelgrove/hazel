/* Linebreaks following these tiles should increment the indent */
let is_incrementor = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label, _} as t) when Tile.is_complete(t) =>
    switch (label) {
    | ["fun", "->"] => true
    | ["if", "then", "else"] => true
    | ["|", "=>"] => true
    | _ => false
    }

  | _ => false
  };

/* Linebreaks following these tiles should reset indent
 * to its level at the beginning of the bidelimited ctx */
let is_resetter = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label, _} as t) when Tile.is_complete(t) =>
    switch (label) {
    | ["|", "=>"] => true
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

/* Cons true as an initial linebreak should always increment */
let prevs = indents =>
  indents
  |> List.map(is_incrementor)
  |> List.cons(true)
  |> List.rev
  |> List.tl
  |> List.rev;

/* Snoc true as a final linebreak should always reset */
let nexts = indents =>
  indents
  |> List.map(is_resetter)
  |> List.rev
  |> List.cons(true)
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

let rec go' = ((base: int, seg: Segment.t)) => {
  let trimmed_seg = trim_non_content(seg);
  List.fold_left2(
    ((level: int, map: Id.Map.t(int)), p: Piece.t, (prev, next)) => {
      switch (p) {
      | Secondary(w) when Secondary.is_linebreak(w) =>
        let level =
          if (prev) {
            level + 2;
          } else if (next) {
            base;
          } else {
            level;
          };
        (level, Id.Map.add(w.id, level, map));
      | Secondary(_)
      | Grout(_) => (level, map)
      | Tile(t) =>
        let map = union_all([map, ...List.map(go(level), t.children)]);
        (level, map);
      }
    },
    (base, Id.Map.empty),
    trimmed_seg,
    List.combine(prevs(trimmed_seg), nexts(trimmed_seg)),
  )
  |> snd;
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
