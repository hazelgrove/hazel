open Util;
open OptUtil.Syntax;
open Zipper;

let primary = (d: Direction.t, z: t): option(t) =>
  if (z.caret == Outer) {
    Zipper.select(d, z);
  } else if (d == Left) {
    z
    |> Zipper.Caret.set(Outer)
    |> Zipper.move(Right)
    |> OptUtil.and_then(Zipper.select(d));
  } else {
    z |> Zipper.Caret.set(Outer) |> Zipper.select(d);
  };

let grow_right_until_case_or_rule =
  Zipper.do_until(primary(Right), neighbors =>
    switch (neighbors) {
    | (_, Some(piece)) => Piece.is_case_or_rule(piece)
    | _ => false
    }
  );

let shrink_left_until_not_case_or_rule_or_space =
  Zipper.do_until(primary(Left), neighbors =>
    switch (neighbors) {
    | (_, Some(piece)) => Piece.is_not_case_or_rule_or_space(piece)
    | _ => false
    }
  );

let grow_right_until_not_comment_or_space =
  Zipper.do_until(primary(Right), neighbors =>
    switch (neighbors) {
    | (_, Some(piece)) => Piece.not_comment_or_space(piece)
    | (_, None) => true
    }
  );

let not_comment_or_space_to_left = neighbors =>
  switch (neighbors) {
  | (Some(piece), _) => Piece.not_comment_or_space(piece)
  | (None, _) => true
  };

let move_left_until_not_comment_or_space = z =>
  not_comment_or_space_to_left(Zipper.generalized_neighbors(z))
    ? Some(z)
    : Zipper.do_until(
        Move.local(ByToken, Left),
        not_comment_or_space_to_left,
        z,
      );

let containing_secondary_run = (z: t): option(t) => {
  let z =
    switch (move_left_until_not_comment_or_space(z)) {
    | None => z
    | Some(z) => z
    };
  grow_right_until_not_comment_or_space(z);
};

let current_term_id = (z: t): option(Id.t) => {
  let* (p, _, rel) = Indicated.piece''(z);
  switch (p) {
  | Secondary(_) => None
  | Grout(_)
  | Projector(_) => Some(Piece.id(p))
  | Tile(t) =>
    /* Basic term selection uses termranges, which is out of data
     * with the parsing logic which makes list listerals. We also
     * treat tuples as including the parens (if any), though this
     * is a free choice. We also handle case rules, whose parent
     * term in tylr is considered to be the combination of the
     * rules and the scrutinee, but we want to consider it to be
     * the whole case expression. */
    switch (t.label, Zipper.parent(z)) {
    | ([","], Some(Tile({label: ["[", "]"] | ["(", ")"], id, _}))) =>
      Some(id)
    | (["|", "=>"], Some(Tile({label: ["case", "end"], id, _})))
        when rel == Sibling =>
      Some(id)
    | (["|", "=>"], Some(Tile({label: ["|", "=>"], _})))
        when rel == Parent =>
      switch (z.relatives.ancestors) {
      | [_, (gp, _), ..._] => Some(gp.id)
      | _ => None
      }
    | _ => Some(Piece.id(p))
    }
  };
};

let indicated_token = (z: t) =>
  switch (Indicated.piece'(~no_ws=false, ~ign=Piece.is_secondary, z)) {
  | Some((Secondary(_), _, _)) =>
    /* If there is secondary on both sides, select the
     * largest contiguous run of non-linebreak secondary */
    containing_secondary_run(z)
  | Some((_, Left, _)) when z.caret == Outer =>
    /* If we're on the far right side of a non-secondary piece, we
     * still prefer to select it over secondary to the right */
    let* z = Move.local(ByToken, Left, z);
    primary(Right, z);
  | Some(_) => primary(Right, z)
  | _ => None
  };

let move_left_until_case_or_rule =
  Zipper.do_until(Move.local(ByToken, Left), neighbors =>
    switch (neighbors) {
    | (Some(piece), _) => Piece.is_case_or_rule(piece)
    | _ => false
    }
  );

let is_inside_rule = (z: t) => {
  let* z = move_left_until_case_or_rule(z);
  let* (p, _, _) = Indicated.piece''(z);
  switch (p) {
  | Tile({label: ["|", "=>"], id, _}) => Some(id)
  | _ => None
  };
};

let parent_cls = (z: t, info_map) => {
  let* id = Indicated.index(z);
  let* statics = Language.Statics.Map.lookup(id, info_map);
  let* parent_id =
    statics |> Language.Statics.Info.ancestors_of |> ListUtil.hd_opt;
  let+ parent_statics = Language.Statics.Map.lookup(parent_id, info_map);
  Language.Statics.Info.cls_of(parent_statics);
};

let parent_is_rule = (z: t, info_map): option(Id.t) => {
  switch (is_inside_rule(z)) {
  | Some(id) when parent_cls(z, info_map) == Some(Exp(Match)) => Some(id)
  | _ => None
  };
};

/* If the indicated term is the body of a definition
 * (let or type), return the id of the body, otherwise None */
let def_body_indicated =
    (z: t, info_map: Language.Statics.Map.t): option(Id.t) => {
  let* id = Indicated.index(z);
  let* statics = Language.Statics.Map.lookup(id, info_map);
  let* parent_id =
    statics |> Language.Statics.Info.ancestors_of |> ListUtil.hd_opt;
  let* ci_parent = Language.Statics.Map.lookup(parent_id, info_map);
  switch (ci_parent) {
  | InfoExp({term: {term: Let(_, _, body) | TyAlias(_, _, body), _}, _}) =>
    let body_id = Language.IdTagged.rep_id(body);
    id == body_id ? Some(body_id) : None;
  | _ => None
  };
};

let parent_id = (z: t, info_map) => {
  let* base_id = Indicated.index(z);
  /* Rules aren't counted as terms in the base syntax,
   * but we do want to treat them as possible parents */
  switch (parent_is_rule(z, info_map)) {
  | Some(id) => Some(id)
  | _ =>
    let* statics = Id.Map.find_opt(base_id, info_map);
    statics |> Language.Info.ancestors_of |> ListUtil.hd_opt;
  };
};

let shard_range = (l: Piece.t, r: Piece.t, z: t): option(t) => {
  let pl = neighbors =>
    switch (neighbors) {
    | (_, Some(piece)) => piece == l
    | _ => false
    };
  let pr = neighbors =>
    switch (neighbors) {
    | (Some(piece), _) => piece == r
    | _ => false
    };
  let* z =
    pl(Zipper.generalized_neighbors(z))
      ? Some(z) : Zipper.do_until(Move.local(ByToken, Left), pl, z);
  Zipper.do_until(primary(Right), pr, z);
};

let tile = (id: Id.t, z: t): option(t) => {
  let* z = Move.jump_to_side_of_id(Left, z, id);
  switch (z.relatives.siblings) {
  | (_, []) => None
  | (l, [r, ...rs]) =>
    let z = Zipper.update_siblings(_ => (l, rs), z);
    let z = Zipper.replace_selection(Right, [r], z);
    Some(z);
  };
};

let current_tile = (z: t): option(t) => {
  let* id = Indicated.index(z);
  tile(id, z);
};

let containing_rule = (z: t): option(t) => {
  let* z = current_tile(z);
  let* z = grow_right_until_case_or_rule(z);
  //TODO(andrew): this busted
  // shrink_left_until_not_case_or_rule_or_space(z);
  Some(z);
};

/* Select the currently indicated term. Optionally, we can consider
 * definitions to not include their bodies, and we can consider case
 * rules as separate pseudo-terms. */
let current_term =
    (
      term_data: TermData.t,
      ~defs_exclude_bodies: bool,
      ~case_rules: bool,
      z: t,
    ) => {
  let* (p, _, _) = Indicated.piece''(z);
  switch (p) {
  | Tile({label: ["let" | "type", ..._], _}) when defs_exclude_bodies =>
    current_tile(z)
  | Tile({label: ["|", "=>"], _}) when case_rules => containing_rule(z)
  | _ =>
    let* id = current_term_id(z);
    let* (l, r) = TermData.extremes_shards(id, term_data);
    shard_range(l, r, z);
  };
};

let term =
    (
      ~defs_exclude_bodies: bool,
      ~case_rules: bool,
      term_data: TermData.t,
      id: Id.t,
      z: t,
    )
    : option(t) => {
  let* z = Move.jump_to_id_indicated(z, id);
  current_term(term_data, ~defs_exclude_bodies, ~case_rules, z);
};

let parent_of_indicated = (z: t, term_data, info_map) => {
  let* id = parent_id(z, info_map);
  let* z' = Move.jump_to_id_indicated(z, id);
  /* Annoying special case here: In general when selecting the parent term
   * we can just use the current term logic, for which we're using the option
   * that definitions count as 'pseudo-terms', meaning their bodies won't be
   * selected. But if the indicated term is the body of a definition, this
   * would result in the parent selection excluding that body, which feels
   * very weird. Take care in refactoring this, as it's very easy to miss
   * this case, or to overgeneralize this case (note in particular that
   * the name and def terms of a def should not exhibit this behavior,
   * only the body. */
  switch (def_body_indicated(z, info_map)) {
  | Some(_) =>
    current_term(term_data, ~defs_exclude_bodies=false, ~case_rules=true, z')
  | None =>
    current_term(term_data, ~defs_exclude_bodies=true, ~case_rules=true, z')
  };
};

let smart = (term_data, info_map, n, z: t): option(t) => {
  switch (n) {
  | 2 => indicated_token(z)
  | 3 =>
    open OptUtil.Syntax;
    /* For things where triple-clicking would otherwise have
     * no additional effect, select the parent term instead */
    let* (p, _, _) = Indicated.piece''(z);
    Piece.is_term(p)
      ? parent_of_indicated(z, term_data, info_map)
      : current_term(
          term_data,
          ~defs_exclude_bodies=true,
          ~case_rules=true,
          z,
        );
  | _ => None
  };
};

let vertical =
    (d: Action.vertical, ~col_target: int, ~measured: Measured.t, z: t)
    : option(t) => {
  let goal =
    Point.{
      col: col_target,
      row: Zipper.Caret.point(measured, z).row + (d == Down ? 1 : (-1)),
    };
  Zipper.do_towards_point(~measured, ~force_progress=true, primary, goal, z);
};

let to_point = (~measured: Measured.t, ~goal: Point.t, z: t): option(t) => {
  let anchor = z |> toggle_focus |> Zipper.Caret.point(measured);
  switch (Zipper.do_towards_point(~measured, ~anchor, primary, goal, z)) {
  | None => Some(z)
  | Some(z) => Some(z)
  };
};

let to_start: t => t = Zipper.do_to_extreme(primary(Left));

let to_end: t => t = Zipper.do_to_extreme(primary(Right));

let all = (z: t): t => z |> Move.to_start |> to_end;

let to_linebreak = (d: Direction.t, z: t): option(t) =>
  Zipper.do_until_linebreak(primary(d), d, z);
