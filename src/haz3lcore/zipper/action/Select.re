open Util;
open OptUtil.Syntax;

module Make = (M: Move.S) => {
  module Move = Move.Make(M);

  let primary = (d: Direction.t, z: Zipper.t('p)): option(Zipper.t('p)) =>
    if (z.caret == Outer) {
      Zipper.select(d, z);
    } else if (d == Left) {
      z
      |> Zipper.set_caret(Outer)
      |> Zipper.move(Right)
      |> OptUtil.and_then(Zipper.select(d));
    } else {
      z |> Zipper.set_caret(Outer) |> Zipper.select(d);
    };

  let vertical = (d: Direction.t, ed: Zipper.t('p)): option(Zipper.t('p)) =>
    Move.do_vertical(primary, d, ed);

  let go = (d: Action.move, z: Zipper.t('p)): option(Zipper.t('p)) =>
    switch (d) {
    | Goal(Piece(_)) => failwith("Select.go not implemented for Piece Goal")
    | Goal(Point(goal)) =>
      let anchor = z |> Zipper.toggle_focus |> Zipper.caret_point(M.measured);
      Move.do_towards(~anchor, primary, goal, z);
    | Extreme(d) => Move.do_extreme(primary, d, z)
    | Local(d) =>
      /* Note: Don't update target on vertical selection */
      switch (d) {
      | Left(_) => primary(Left, z)
      | Right(_) => primary(Right, z)
      | Up => vertical(Left, z)
      | Down => vertical(Right, z)
      }
    };

  let range = (l: Id.t, r: Id.t, z: Zipper.t('p)): option(Zipper.t('p)) => {
    let* z = Move.jump_to_id(z, l);
    let* Measured.{last, _} = Measured.find_by_id(r, M.measured);
    Move.do_towards(Zipper.select, last, z);
  };

  let tile = (id: Id.t, z: Zipper.t('p)): option(Zipper.t('p)) => {
    let* z = Move.jump_to_id(z, id);
    let* Measured.{last, _} = Measured.find_by_id(id, M.measured);
    Move.do_towards(primary, last, z);
  };

  let current_tile = z => {
    let* id = Indicated.index(z);
    tile(id, z);
  };

  let term = (id: Id.t, z: Zipper.t('p)): option(Zipper.t('p)) => {
    let* (l, r) = TermRanges.find_opt(id, M.term_ranges);
    range(Piece.id(l), Piece.id(r), z);
  };

  let current_term_id = (z: Zipper.t('p)): option(Id.t) => {
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

  let grow_right_until_case_or_rule =
    Move.do_until(go(Local(Right(ByToken))), Piece.is_case_or_rule, _);

  let shrink_left_until_not_case_or_rule_or_space =
    Move.do_until(
      go(Local(Left(ByToken))),
      Piece.is_not_case_or_rule_or_space,
      _,
    );

  let containing_rule = z => {
    let* z = current_tile(z);
    let* z = grow_right_until_case_or_rule(z);
    shrink_left_until_not_case_or_rule_or_space(z);
  };

  /* Select the currently indicated term. Optionally, we can consider
   * definitions to not include their bodies, and we can consider case
   * rules as separate pseudo-terms. */
  let current_term =
      (~defs_exclude_bodies: bool, ~case_rules: bool, z: Zipper.t('p)) => {
    let* (p, _, _) = Indicated.piece''(z);
    switch (p) {
    | Tile({label: ["let" | "type", ..._], _}) when defs_exclude_bodies =>
      current_tile(z)
    | Tile({label: ["|", "=>"], _}) when case_rules => containing_rule(z)
    | _ =>
      let* id = current_term_id(z);
      term(id, z);
    };
  };

  let grow_right_until_not_comment_or_space =
    Move.do_until(
      go(Local(Right(ByToken))),
      Piece.not_comment_or_space,
      _,
    );

  let containing_secondary_run = z => {
    let z =
      switch (Move.left_until_not_comment_or_space(~move_first=false, z)) {
      | None =>
        /* Due to implementation details of Move.do_until (specifically its
         * use of Indicated), this behaves poorly if we're one token away
         * from the beginning of the syntax. We handle that case here */
        let z = Zipper.set_caret(Outer, z);
        switch (Zipper.move(Left, z)) {
        | Some(z) => z
        | None => z
        };
      | Some(z) => z
      };
    let* z = grow_right_until_not_comment_or_space(z);
    go(Local(Left(ByToken)), z); /* above overshoots */
  };

  let indicated_token = (z: Zipper.t('p)) =>
    switch (Indicated.piece'(~no_ws=false, ~ign=Piece.is_secondary, z)) {
    | Some((Secondary(_), _, _)) =>
      /* If there is secondary on both sides, select the
       * largest contiguous run of non-linebreak secondary */
      containing_secondary_run(z)
    | Some((_, Left, _)) when z.caret == Outer =>
      /* If we're on the far right side of a non-secondary piece, we
       * still prefer to select it over secondary to the right */
      let* z = Move.go(Local(Left(ByToken)), z);
      go(Local(Right(ByToken)), z);
    | Some(_) => go(Local(Right(ByToken)), z)
    | _ => None
    };

  let is_inside_rule = (z: Zipper.t('p)) => {
    let* z = Move.left_until_case_or_rule(z);
    let* (p, _, _) = Indicated.piece''(z);
    switch (p) {
    | Tile({label: ["|", "=>"], id, _}) => Some(id)
    | _ => None
    };
  };

  let parent_cls = (z: Zipper.t('p), info_map: Language.Statics.Map.t) => {
    let* id = Indicated.index(z);
    let* statics = Language.Statics.Map.lookup(id, info_map);
    let* parent_id =
      statics |> Language.Statics.Info.ancestors_of |> ListUtil.hd_opt;
    let+ parent_statics = Language.Statics.Map.lookup(parent_id, info_map);
    Language.Statics.Info.cls_of(parent_statics);
  };

  let parent_is_rule =
      (z: Zipper.t('p), info_map: Language.Statics.Map.t): option(Id.t) => {
    switch (is_inside_rule(z)) {
    | Some(id) when parent_cls(z, info_map) == Some(Exp(Match)) => Some(id)
    | _ => None
    };
  };

  /* If the indicated term is the body of a definition
   * (let or type), return the id of the body, otherwise None */
  let def_body_indicated =
      (z: Zipper.t('p), info_map: Language.Statics.Map.t): option(Id.t) => {
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

  let parent_id = (z: Zipper.t('p), info_map: Language.Statics.Map.t) => {
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

  let parent_of_indicated =
      (z: Zipper.t('p), info_map: Language.Statics.Map.t) => {
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
      current_term(~defs_exclude_bodies=false, ~case_rules=true, z')
    | None => current_term(~defs_exclude_bodies=true, ~case_rules=true, z')
    };
  };
};
