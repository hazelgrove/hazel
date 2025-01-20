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

  let tile = (id: Id.t, z: Zipper.t): option(Zipper.t) => {
    let* z = Move.jump_to_id(z, id);
    let* Measured.{last, _} = Measured.find_by_id(id, M.measured);
    Move.do_towards(primary, last, z);
  };

  let current_tile = z => {
    let* id = Indicated.index(z);
    tile(id, z);
  };

  let term = (id: Id.t, z: Zipper.t): option(Zipper.t) => {
    let* (l, r) = TermRanges.find_opt(id, M.term_ranges);
    range(Piece.id(l), Piece.id(r), z);
  };

  let current_term_id = (z: Zipper.t): option(Id.t) => {
    let* (p, _, _) = Indicated.piece''(z);
    switch (p) {
    | Secondary(_) => None
    | Grout(_)
    | Projector(_) => Some(Piece.id(p))
    | Tile(t) =>
      /* Basic term selection uses termranges, which is out of data
       * with the parsing logic which makes list listerals. We also
       * treat tuples as including the parens (if any), though this
       * is a free choice */
      switch (t.label, Zipper.parent(z)) {
      | ([","], Some(Tile({label: ["[", "]"] | ["(", ")"], id, _}))) =>
        Some(id)
      | _ => Some(Piece.id(p))
      }
    };
  };

  let current_term = (z: Zipper.t): option(Zipper.t) => {
    let* id = current_term_id(z);
    term(id, z);
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

  let current_term_fancy = (z: Zipper.t) => {
    let* (p, _, _) = Indicated.piece''(z);
    switch (p) {
    | Tile({label: ["let" | "type", ..._], _}) => current_tile(z)
    | Tile({label: ["|", "=>"], _}) => containing_rule(z)
    | _ => current_term(z)
    };
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

  let is_inside_rule = (z: Zipper.t) => {
    let* z = Move.left_until_case_or_rule(z);
    let* (p, _, _) = Indicated.piece''(z);
    switch (p) {
    | Tile({label: ["|", "=>"], id, _}) => Some(id)
    | _ => None
    };
  };

  let parent_cls = (z: Zipper.t, info_map) => {
    let* id = Indicated.index(z);
    let* statics = Statics.Map.lookup(id, info_map);
    let* parent_id = statics |> Info.ancestors_of |> ListUtil.hd_opt;
    let+ parent_statics = Statics.Map.lookup(parent_id, info_map);
    Info.cls_of(parent_statics);
  };

  let parent_is_rule = (z: Zipper.t, info_map): option(Id.t) => {
    switch (is_inside_rule(z)) {
    | Some(id) when parent_cls(z, info_map) == Some(Exp(Match)) => Some(id)
    | _ => None
    };
  };

  let parent_id = (z: Zipper.t, info_map) => {
    let* base_id = Indicated.index(z);
    /* Rules aren't counted as terms in the base syntax,
     * but we do want to treat them as possible parents */
    switch (parent_is_rule(z, info_map)) {
    | Some(id) => Some(id)
    | _ =>
      let* statics = Id.Map.find_opt(base_id, info_map);
      statics |> Info.ancestors_of |> ListUtil.hd_opt;
    };
  };

  let parent_of_indicated = (z: Zipper.t, info_map) => {
    let* id = parent_id(z, info_map);
    let* z = Move.jump_to_id_indicated(z, id);
    current_term_fancy(z);
  };
};
