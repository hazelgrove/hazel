open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type paint =
  | Type(Typ.head)
  | Scope(int);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  id: Id.t,
  level: int,
  paint,
};

let scope_colors = 6;

let scope_color_name = (i: int) => "depth-" ++ string_of_int(i);

let scope_color_names = List.init(scope_colors, scope_color_name);

let paint_name: paint => string =
  fun
  | Type(head) => "type-block-" ++ Typ.head_name(head)
  | Scope(depth) =>
    "scope-block-" ++ scope_color_name(depth mod scope_colors);

/* The nearest ancestor that is itself painted. A pattern's ancestors begin
   with the pattern itself (Statics.upat_to_info_map defaults to the
   inclusive list, unlike expressions), so the term's own id is skipped. */
let parent =
    (
      painted: Id.Map.t((paint, Info.ancestors)),
      id: Id.t,
      ancestors: Info.ancestors,
    )
    : option((Id.t, (paint, Info.ancestors))) =>
  List.find_map(
    a =>
      Id.equal(a, id)
        ? None : Option.map(p => (a, p), Id.Map.find_opt(a, painted)),
    ancestors,
  );

/* Whether a painted term starts a block, and the level of the block it is
   in; memoized in `levels` since every term's answer depends on its
   parent's. */
let rec level_of =
        (
          painted: Id.Map.t((paint, Info.ancestors)),
          levels: Id.Map.t((bool, int)),
          id: Id.t,
          (paint, ancestors): (paint, Info.ancestors),
        )
        : (Id.Map.t((bool, int)), (bool, int)) =>
  switch (Id.Map.find_opt(id, levels)) {
  | Some(r) => (levels, r)
  | None =>
    let (levels, r) =
      switch (parent(painted, id, ancestors)) {
      | None => (levels, (true, 0))
      | Some((parent_id, (parent_paint, _) as p)) =>
        let (levels, (_, parent_level)) =
          level_of(painted, levels, parent_id, p);
        equal_paint(paint, parent_paint)
          ? (levels, (false, parent_level))
          : (levels, (true, parent_level + 1));
      };
    (Id.Map.add(id, r, levels), r);
  };

let nest = (painted: Id.Map.t((paint, Info.ancestors))): list(t) => {
  let (_, blocks) =
    Id.Map.fold(
      (id, p, (levels, blocks)) => {
        let (levels, (is_block, level)) = level_of(painted, levels, id, p);
        (
          levels,
          is_block
            ? [
              {
                id,
                level,
                paint: fst(p),
              },
              ...blocks,
            ]
            : blocks,
        );
      },
      painted,
      (Id.Map.empty, []),
    );
  List.stable_sort(
    (a: t, b: t) => Int.compare(a.level, b.level),
    List.rev(blocks),
  );
};
