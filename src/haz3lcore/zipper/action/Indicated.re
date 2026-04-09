open Util;
open OptUtil.Syntax;

[@deriving show]
type relation =
  | Parent
  | Sibling;

type piece = (Piece.t, Direction.t, relation);

let piece' =
    (~no_ws: bool, ~ign: Piece.t => bool, z: ZipperBase.t): option(piece) => {
  /* Returns the piece currently indicated (if any) and which side of
     that piece the caret is on. We favor indicating the piece to the
     (R)ight, but may end up indicating the (P)arent or the (L)eft.
     We don't indicate secondary tiles. This function ignores whether
     or not there is a selection so this can be used to get the caret
     direction, but the caller shouldn't indicate if there's a selection */
  switch (
    Siblings.neighbors(ZipperBase.sibs_with_sel(z)),
    ZipperBase.parent(z),
  ) {
  /* Empty syntax => no indication */
  | ((None, None), None) => None
  /* L not secondary, R is secondary => indicate L */
  | ((Some(l), Some(r)), _) when !ign(l) && ign(r) =>
    Some((l, Left, Sibling))
  /* L and R are secondarys => no indication */
  | ((Some(l), Some(r)), _) when ign(l) && ign(r) =>
    no_ws ? None : Some((l, Left, Sibling))
  /* At right end of syntax and L is secondary => no indication */
  | ((Some(l), None), None) when ign(l) =>
    no_ws ? None : Some((l, Left, Sibling))
  /* At left end of syntax and R is secondary => no indication */
  | ((None, Some(r)), None) when ign(r) =>
    no_ws ? None : Some((r, Right, Sibling))
  /* No L and R is a secondary and there is a P => indicate P */
  | ((None, Some(r)), Some(parent)) when ign(r) =>
    Some((parent, Left, Parent))
  /* L is not secondary and caret is outer => indicate L */
  | ((Some(l), _), _) when !ign(l) && z.caret == Outer =>
    Some((l, Left, Sibling))
  /* No L, some P, and caret is outer => indicate R */
  | ((None, _), Some(parent)) when z.caret == Outer =>
    Some((parent, Left, Parent))
  /* R is not secondary, either no L or L is secondary or caret is inner => indicate R */
  | ((_, Some(r)), _) => Some((r, Right, Sibling))
  /* No R and there is a P => indicate P */
  | ((_, None), Some(parent)) => Some((parent, Right, Parent))
  /* There is an L but no R and no P => indicate L */
  //WEIRD: Right below seems wrong but behaves right
  | ((Some(l), None), None) => Some((l, Right, Sibling))
  };
};

let piece =
  piece'(~no_ws=true, ~ign=p => Piece.(is_secondary(p) || is_grout(p)));

let shard_index = (z: ZipperBase.t): option(int) =>
  switch (piece(z)) {
  | None => None
  | Some((p, side, relation)) =>
    switch (relation) {
    | Parent =>
      switch (Ancestors.parent(z.relatives.ancestors)) {
      | None => failwith("indicated_shard_index impossible")
      | Some({children: (before, _), _}) =>
        let before = List.length(before);
        switch (Siblings.neighbors(z.relatives.siblings)) {
        | (_, None) => Some(before + 1)
        | _ => Some(before)
        };
      }
    | Sibling =>
      switch (p) {
      | Secondary(_)
      | Grout(_)
      | Projector(_) => Some(0)
      | Tile(t) =>
        switch (side) {
        | Left => Some(List.length(t.children))
        | Right => Some(0)
        }
      }
    }
  };

let for_index = piece'(~no_ws=false, ~ign=Piece.is_secondary);

let direction = (z: ZipperBase.t): option(Direction.t) =>
  switch (piece'(~no_ws=false, ~ign=Piece.is_secondary, z)) {
  | None => None
  | Some((_, d, _)) => Some(d)
  };

let index = (z: ZipperBase.t): option(Id.t) =>
  switch (for_index(z)) {
  | None => None
  | Some((p, _, _)) => Some(Piece.id(p))
  };

let piece'' = piece'(~no_ws=true, ~ign=Piece.is_secondary);

let ci_of =
    (z: ZipperBase.t, info_map: Language.Statics.Map.t)
    : option(Language.Statics.Info.t) => {
  /* This version takes into accounts Secondary, while accounting for the
   * fact that Secondary is not currently added to the info_map. First we
   * try the basic indication function, specifying that we do not want
   * Secondary. But if this doesn't succeed, then we create a 'virtual'
   * info map entry representing the Secondary notation, which takes on
   * some of the semantic context of a nearby 'proxy' term */
  let nearest_term_ci = () => {
    let (left_sibs, right_sibs) = ZipperBase.sibs_with_sel(z);
    let left_term = List.find_opt(Piece.is_term, List.rev(left_sibs));
    let right_term = List.find_opt(Piece.is_term, right_sibs);
    let ancestor_ci =
      List.find_map(
        ((ancestor, _)) =>
          Id.Map.find_opt(
            Piece.id(Piece.Tile(Ancestor.zip([], ancestor))),
            info_map,
          ),
        z.relatives.ancestors,
      );
    switch (right_term, left_term) {
    | (Some(p), _)
    | (_, Some(p)) => Id.Map.find_opt(Piece.id(p), info_map)
    | _ => ancestor_ci
    };
  };
  switch (piece''(z)) {
  | Some((p, _, _)) =>
    switch (Id.Map.find_opt(Piece.id(p), info_map)) {
    | Some(ci) => Some(ci)
    | None =>
      switch (piece(z)) {
      | Some((proxy, _, _)) =>
        switch (Id.Map.find_opt(Piece.id(proxy), info_map)) {
        | Some(ci) => Some(ci)
        | None =>
          let by_parent =
            switch (ZipperBase.parent(z)) {
            | Some(parent) => Id.Map.find_opt(Piece.id(parent), info_map)
            | None => None
            };
          switch (by_parent) {
          | Some(ci) => Some(ci)
          | None =>
            switch (nearest_term_ci()) {
            | Some(ci) => Some(ci)
            | None =>
              let sibs = ZipperBase.sibs_with_sel(z);
              let proxy_id =
                switch (Siblings.neighbors(Siblings.trim_secondary(sibs))) {
                | (_, Some(p))
                | (Some(p), _) => Some(Piece.id(p))
                | _ => None
                };
              Option.bind(proxy_id, id => Id.Map.find_opt(id, info_map));
            }
          };
        }
      | None =>
        let by_parent =
          switch (ZipperBase.parent(z)) {
          | Some(parent) => Id.Map.find_opt(Piece.id(parent), info_map)
          | None => None
          };
        switch (by_parent) {
        | Some(ci) => Some(ci)
        | None =>
          switch (nearest_term_ci()) {
          | Some(ci) => Some(ci)
          | None =>
            let sibs = ZipperBase.sibs_with_sel(z);
            let proxy_id =
              switch (Siblings.neighbors(Siblings.trim_secondary(sibs))) {
              | (_, Some(p))
              | (Some(p), _) => Some(Piece.id(p))
              | _ => None
              };
            Option.bind(proxy_id, id => Id.Map.find_opt(id, info_map));
          }
        };
      }
    }
  | None =>
    switch (nearest_term_ci()) {
    | Some(ci) => Some(ci)
    | None =>
      let sibs = ZipperBase.sibs_with_sel(z);
      let* cls =
        switch (Siblings.neighbors(sibs)) {
        /* If on side of comment, say we're on comment */
        | (Some(Secondary(sl)), Some(Secondary(_)))
            when Secondary.is_comment(sl) =>
          Some(Language.Secondary.cls_of(sl))
        | (Some(Secondary(_)), Some(Secondary(sr)))
            when Secondary.is_comment(sr) =>
          Some(Language.Secondary.cls_of(sr))
        | (_, Some(Secondary(s)))
        | (Some(Secondary(s)), _) => Some(Language.Secondary.cls_of(s))
        | _ => None
        };
      let* proxy_id =
        switch (Siblings.neighbors(Siblings.trim_secondary(sibs))) {
        | (_, Some(p))
        | (Some(p), _) => Some(Piece.id(p))
        | _ => None
        };
      let+ ci = Id.Map.find_opt(proxy_id, info_map);
      Language.Statics.Info.Secondary({
        id: proxy_id,
        cls: Secondary(cls),
        sort: Language.Statics.Info.sort_of(ci),
        ctx: Language.Statics.Info.ctx_of(ci),
      });
    }
  };
};
