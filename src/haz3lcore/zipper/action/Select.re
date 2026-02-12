open Util;
open OptUtil.Syntax;
open Zipper;

let local = (d: Direction.t, z: t): option(t) =>
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

/* Basic term selection uses term data, which is out of date
 * with the parsing logic which makes list listerals. We also
 * treat tuples as including the parens (if any), though this
 * is a free choice. We also handle case rules, whose parent
 * term in tylr is considered to be the combination of the
 * rules and the scrutinee, but we want to consider it to be
 * the whole case expression. */
let current_term_id = (z: t): option(Id.t) => {
  let* (p, _, rel) = Indicated.piece''(z);
  switch (p) {
  | Secondary(_) => None
  | Grout(_)
  | Projector(_) => Some(Piece.id(p))
  | Tile(t) =>
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

/* Select the containing case rule, if any */
let containing_rule = (z: t): option(t) => {
  let grow_left_by_piece = (z: t): option(t) => {
    switch (z.relatives.siblings) {
    | (_, []) => None
    | (l, [r, ...rs]) =>
      let z = Zipper.Caret.set(Outer, z);
      let z = Zipper.update_siblings(_ => (l, rs), z);
      Some({
        ...z,
        selection: Selection.push(r, z.selection),
      });
    };
  };
  let shrink_right_by_piece = (z: t): option(t) => {
    switch (z.selection.content) {
    | [] => None
    | [_, ..._] as s =>
      let z = Zipper.Caret.set(Outer, z);
      let (s, p) = ListUtil.split_last(s);
      let z = Zipper.update_siblings(((l, r)) => (l, [p, ...r]), z);
      Some({
        ...z,
        selection: {
          ...z.selection,
          content: s,
        },
      });
    };
  };
  let rule_or_end_of_seg_to_right =
    fun
    | (_, None | Some(Piece.Tile({label: ["|", "=>"], _}))) => true
    | _ => false;
  let grow_right_until_case_or_rule = z =>
    Zipper.do_until_piece(grow_left_by_piece, rule_or_end_of_seg_to_right, z);
  let secondary_to_left =
    fun
    | (Some(Piece.Secondary(_)), _) => true
    | _ => false;
  let shrink_past_secondary = z =>
    !secondary_to_left(Siblings.neighbors(z.relatives.siblings))
      ? Some(z)
      : Zipper.do_until_piece(shrink_right_by_piece, secondary_to_left, z);
  let* z = current_tile(z);
  let* z = grow_right_until_case_or_rule(z);
  let* z = shrink_past_secondary(z);
  Some(z);
};

/* Select the (inclusive) range between two shards */
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
  Zipper.do_until(local(Right), pr, z);
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
  | Tile({label: ["let" | "type", "=", "in"], _}) when defs_exclude_bodies =>
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

/* Select the containing run of secondary if any */
let containing_secondary_run = (z: t): option(t) => {
  let not_comment_or_space = (p: Piece.t) =>
    switch (p) {
    | Secondary(s) => Secondary.is_linebreak(s)
    | _ => true
    };
  let not_comment_or_space_to_right =
    fun
    | (_, Some(piece)) => not_comment_or_space(piece)
    | (_, None) => true;
  let not_comment_or_space_to_left =
    fun
    | (Some(piece), _) => not_comment_or_space(piece)
    | (None, _) => true;
  let grow_right_until_not_comment_or_space =
    Zipper.do_until(local(Right), not_comment_or_space_to_right);
  let move_left_until_not_comment_or_space = z =>
    not_comment_or_space_to_left(Zipper.generalized_neighbors(z))
      ? Some(z)
      : Zipper.do_until(
          Move.local(ByToken, Left),
          not_comment_or_space_to_left,
          z,
        );
  let z =
    switch (move_left_until_not_comment_or_space(z)) {
    | None => z
    | Some(z) => z
    };
  grow_right_until_not_comment_or_space(z);
};

/* Select the indicated token. For the purposes of this function,
 * contiguous spans of secondary are considered a single token,
 * although technically this is not the case */
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
    local(Right, z);
  | Some(_) => local(Right, z)
  | _ => None
  };

/* See `parent_of_indicated` */
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

/* Rules aren't treated as terms in the syntax model, but we
 * do want to treat them as possible parents for selection */
let parent_is_rule = (z: t, info_map): option(Id.t) => {
  let is_case_or_rule = (p: Piece.t) =>
    switch (p) {
    | Tile({label: ["case", "end"], _}) => true
    | Tile({label: ["|", "=>"], _}) => true
    | _ => false
    };
  let move_left_until_case_or_rule =
    Zipper.do_until(Move.local(ByToken, Left), neighbors =>
      switch (neighbors) {
      | (Some(piece), _) => is_case_or_rule(piece)
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
  switch (is_inside_rule(z)) {
  | Some(id) when parent_cls(z, info_map) == Some(Exp(Match)) => Some(id)
  | _ => None
  };
};

let parent_term_id = (z: t, info_map) => {
  let* base_id = Indicated.index(z);
  switch (parent_is_rule(z, info_map)) {
  | Some(id) => Some(id)
  | None =>
    let* statics = Id.Map.find_opt(base_id, info_map);
    statics |> Language.Info.ancestors_of |> ListUtil.hd_opt;
  };
};

/* If the indicated term is the body of a definition
 * (`let` or `type`), return the id of the body, otherwise None */
let parent_of_indicated = (z: t, term_data, info_map) => {
  let* id = parent_term_id(z, info_map);
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
    let* (p, _, _) = Indicated.piece''(z);
    /* For things where triple-clicking would otherwise have
     * no additional effect, select the parent term instead */
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
  Zipper.do_towards_point(~measured, ~force_progress=true, local, goal, z);
};

let to_point = (~measured: Measured.t, ~goal: Point.t, z: t): option(t) => {
  let anchor = z |> toggle_focus |> Zipper.Caret.point(measured);
  switch (Zipper.do_towards_point(~measured, ~anchor, local, goal, z)) {
  | None => Some(z)
  | Some(z) => Some(z)
  };
};

let to_start: t => t = Zipper.do_to_extreme(local(Left));

let to_end: t => t = Zipper.do_to_extreme(local(Right));

let all = (z: t): t => z |> Move.to_start |> to_end;

let to_linebreak = (d: Direction.t, z: t): option(t) =>
  Zipper.do_until_linebreak(local(d), d, z);
