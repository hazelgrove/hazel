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

/* Trim pieces that don't feel contentful, which we take
 * here to mean empty whitespace and concave grout */
let trim_non_things = (seg: Segment.t) => {
  let rec trim: list(Base.piece) => list(Base.piece) =
    xs =>
      switch (xs) {
      | [] => []
      | [Grout({shape: Concave, _}), ...xs] => trim(xs)
      | [Secondary(s), ...xs] when Secondary.is_space(s) => trim(xs)
      | [_, ..._] => xs
      };
  Segment.trim_f(trim, Left, seg);
};

/* Get the last contentful piece in seg before idx */
let last_thing_before = (idx: int, seg: Segment.t): option(Piece.t) =>
  switch (Util.ListUtil.split_nth_opt(idx, seg)) {
  | Some((pre, _, _)) =>
    switch (pre |> List.rev |> trim_non_things) {
    | [] => None
    | [p, ..._] => Some(p)
    }
  | _ => None
  };

/* Get the next contentful piece in seg after idx */
let next_thing_after = (idx: int, seg: Segment.t): option(Piece.t) =>
  switch (Util.ListUtil.split_nth_opt(idx, seg)) {
  | Some((_, _, suf)) =>
    switch (suf |> trim_non_things) {
    | [] => None
    | [p, ..._] => Some(p)
    }
  | _ => None
  };

/* Is the previous contentful piece before idx
 *  an indentation incrementor? */
let prev_thing_is_incrementor = (idx: int, seg: Segment.t): bool => {
  switch (last_thing_before(idx, seg)) {
  | Some(p) => is_incrementor(p)
  | None => false
  };
};

/* Is the next contentful piece after idx
 * an indetation resetter? */
let next_thing_is_resetter = (idx: int, seg: Segment.t) =>
  switch (next_thing_after(idx, seg)) {
  | Some(p) => is_resetter(p)
  | None => false
  };

let is_first_thing = (idx: int, seg: Segment.t) =>
  switch (Util.ListUtil.split_nth_opt(idx, seg)) {
  | Some((pre, _, _)) =>
    switch (pre |> trim_non_things) {
    | [] => true
    | _ => false
    }
  | None => false
  };

let is_last_thing = (idx: int, seg: Segment.t) =>
  switch (Util.ListUtil.split_nth_opt(idx, seg)) {
  | Some((_, _, suf)) =>
    switch (suf |> trim_non_things) {
    | [] => true
    | _ => false
    }
  | None => false
  };

let should_increment = (idx: int, seg: Segment.t) =>
  is_first_thing(idx, seg) || prev_thing_is_incrementor(idx, seg);

let should_reset = (idx: int, seg: Segment.t) =>
  is_last_thing(idx, seg) || next_thing_is_resetter(idx, seg);

let update_indent = (idx, seg, level: int, base: int): int =>
  if (should_increment(idx, seg)) {
    level + 2;
  } else if (should_reset(idx, seg)) {
    base;
  } else {
    level;
  };

let rec go = (base: int, map: Id.Map.t(int), seg: Segment.t) =>
  List.fold_left2(
    ((level: int, map: Id.Map.t(int)), p: Piece.t, idx: int) => {
      switch (p) {
      | Secondary(w) when Secondary.is_linebreak(w) =>
        let level = update_indent(idx, seg, level, base);
        (level, Id.Map.add(w.id, level, map));
      | Secondary(_)
      | Grout(_) => (level, map)
      | Tile(t) => (level, List.fold_left(go(level), map, t.children))
      }
    },
    (base, map),
    seg,
    List.init(List.length(seg), Fun.id),
  )
  |> snd;

let level_map = (seg: Segment.t): Id.Map.t(int) => go(0, Id.Map.empty, seg);
