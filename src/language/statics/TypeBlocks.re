open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type block = {
  id: Id.t,
  head: Typ.head,
  level: int,
};

type typed = {
  head: Typ.head,
  ancestors: Info.ancestors,
};

/* Representative typed terms whose type has a head. */
let typed_terms = (m: StaticsBase.Map.t): Id.Map.t(typed) =>
  Id.Map.fold(
    (id, info, acc) =>
      switch (Info.typed_of(info)) {
      | Some((ty, ctx, ancestors)) when Id.equal(id, Info.id_of(info)) =>
        switch (Typ.head_of(ctx, ty)) {
        | Some(head) =>
          Id.Map.add(
            id,
            {
              head,
              ancestors,
            },
            acc,
          )
        | None => acc
        }
      | _ => acc
      },
    m,
    Id.Map.empty,
  );

/* The nearest ancestor that is itself a typed term. A pattern's ancestors
   begin with the pattern itself (Statics.upat_to_info_map defaults to the
   inclusive list, unlike expressions), so the term's own id is skipped. */
let parent =
    (typed: Id.Map.t(typed), id: Id.t, ancestors: Info.ancestors)
    : option((Id.t, typed)) =>
  List.find_map(
    a =>
      Id.equal(a, id)
        ? None : Option.map(t => (a, t), Id.Map.find_opt(a, typed)),
    ancestors,
  );

/* Whether a typed term starts a block, and the level of the block it is in;
   memoized in `levels` since every term's answer depends on its parent's. */
let rec level_of =
        (
          typed: Id.Map.t(typed),
          levels: Id.Map.t((bool, int)),
          id: Id.t,
          t: typed,
        )
        : (Id.Map.t((bool, int)), (bool, int)) =>
  switch (Id.Map.find_opt(id, levels)) {
  | Some(r) => (levels, r)
  | None =>
    let (levels, r) =
      switch (parent(typed, id, t.ancestors)) {
      | None => (levels, (true, 0))
      | Some((parent_id, p)) =>
        let (levels, (_, parent_level)) =
          level_of(typed, levels, parent_id, p);
        Typ.equal_head(t.head, p.head)
          ? (levels, (false, parent_level))
          : (levels, (true, parent_level + 1));
      };
    (Id.Map.add(id, r, levels), r);
  };

let of_map = (m: StaticsBase.Map.t): list(block) => {
  let typed = typed_terms(m);
  let (_, blocks) =
    Id.Map.fold(
      (id, t, (levels, blocks)) => {
        let (levels, (is_block, level)) = level_of(typed, levels, id, t);
        (
          levels,
          is_block
            ? [
              {
                id,
                head: t.head,
                level,
              },
              ...blocks,
            ]
            : blocks,
        );
      },
      typed,
      (Id.Map.empty, []),
    );
  List.stable_sort(
    (a, b) => Int.compare(a.level, b.level),
    List.rev(blocks),
  );
};
