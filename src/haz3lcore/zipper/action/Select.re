open Util;
open OptUtil.Syntax;

module Make = (M: Move.S) => {
  module Move = Move.Make(M);

  let primary = (d: Direction.t, z: Zipper.t): option(Zipper.t) =>
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

  let vertical = (d: Direction.t, ed: Zipper.t): option(Zipper.t) =>
    Move.do_vertical(primary, d, ed);

  let go = (d: Action.move, z: Zipper.t) =>
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

  let range = (l: Id.t, r: Id.t, z: Zipper.t): option(Zipper.t) => {
    let* z = Move.jump_to_id(z, l);
    let* Measured.{last, _} = Measured.find_by_id(r, M.measured);
    Move.do_towards(Zipper.select, last, z);
  };

  let term = (id: Id.t, z: Zipper.t): option(Zipper.t) => {
    //TODO: check if selection is already a term: no-op in this case
    let* (l, r) = TermRanges.find_opt(id, M.term_ranges);
    range(Piece.id(l), Piece.id(r), z);
  };

  let tile = (id: Id.t, z: Zipper.t): option(Zipper.t) => {
    let* z = Move.jump_to_id(z, id);
    let* Measured.{last, _} = Measured.find_by_id(id, M.measured);
    Move.do_towards(primary, last, z);
  };

  let current_term = z => {
    let* id = Indicated.index(z);
    term(id, z);
  };

  let current_tile = z => {
    let* id = Indicated.index(z);
    tile(id, z);
  };

  let grow_right_until_case_or_rule =
    Move.do_until(go(Local(Right(ByToken))), Piece.is_case_or_rule);

  let shrink_left_until_not_case_or_rule_or_space =
    Move.do_until(
      go(Local(Left(ByToken))),
      Piece.is_not_case_or_rule_or_space,
    );

  let containing_rule = z => {
    let* z = current_tile(z);
    let* z = grow_right_until_case_or_rule(z);
    shrink_left_until_not_case_or_rule_or_space(z);
  };

  let nice_term = (z: Zipper.t) =>
    switch (Indicated.piece''(z)) {
    | Some((p, _, _)) =>
      switch (p) {
      | Secondary(_) => failwith("Select.nice_term unimplemented")
      | Grout(_)
      | Projector(_)
      | Tile({
          label: [_],
          mold: {nibs: ({shape: Convex, _}, {shape: Convex, _}), _},
          _,
        }) =>
        current_term(z)
      | Tile({label: ["let" | "type", ..._], _}) => current_tile(z)
      | Tile({label: ["|", "=>"], _}) => containing_rule(z)
      | Tile(t) =>
        switch (t.label, Zipper.parent(z)) {
        | ([","], Some(Tile({label: ["[", "]"] | ["(", ")"], id, _}))) =>
          term(id, z)
        | _ => current_term(z)
        }
      }
    | _ => None
    };

  let grow_right_until_not_comment_or_space =
    Move.do_until(go(Local(Right(ByToken))), Piece.not_comment_or_space);

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

  let indicated_token = (z: Zipper.t) =>
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

  let parent_of_indicated = (z: Zipper.t, info_map) => {
    let statics_of = Id.Map.find_opt(_, info_map);
    let* base_id = Indicated.index(z);
    let* ci = statics_of(base_id);
    let* id =
      switch (Info.ancestors_of(ci)) {
      | [] => None
      | [parent, ..._] => Some(parent)
      };
    let* ci_parent = statics_of(id);
    switch (Info.cls_of(ci_parent)) {
    | Exp(Let | TyAlias) =>
      /* For definition-type forms, don't select the body,
       * unless the body is precisely what we're clicking on */
      switch (ci_parent) {
      | InfoExp({term: t, _}) =>
        switch (IdTagged.term_of(t)) {
        | Let(_, _, body)
        | TyAlias(_, _, body) =>
          let body_id = IdTagged.rep_id(body);
          base_id == body_id ? term(id, z) : tile(id, z);
        | _ => tile(id, z)
        }
      | _ => tile(id, z)
      }
    | Exp(Match) =>
      /* Case rules aren't terms in the syntax model,
       * but here we pretend they are */
      let* z = Move.left_until_case_or_rule(z);
      switch (Indicated.piece''(z)) {
      | Some((p, _, _)) =>
        switch (p) {
        | Tile({label: ["|", "=>"], _}) => containing_rule(z)
        | Tile({label: ["case", "end"], _}) => term(id, z)
        | _ => None
        }
      | _ => None
      };
    | _ =>
      switch (Info.ancestors_of(ci_parent)) {
      | [] => term(id, z)
      | [gp, ..._] =>
        let* ci_gp = statics_of(gp);
        switch (Info.cls_of(ci_parent), Info.cls_of(ci_gp)) {
        | (
            Exp(Tuple) | Pat(Tuple) | Typ(Prod),
            Exp(Parens) | Pat(Parens) | Typ(Parens),
          ) =>
          /* If parent is tuple, check if it's in parens,
           * and if so, select the parens as well */
          term(gp, z)
        | _ => term(id, z)
        };
      }
    };
  };
};
