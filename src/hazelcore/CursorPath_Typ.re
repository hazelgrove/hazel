open OptUtil.Syntax;

let rec of_z = (zty: ZTyp.t): CursorPath.t => of_zopseq(zty)
and of_zopseq = zopseq => CursorPath_common.of_zopseq_(~of_zoperand, zopseq)
and of_zoperand =
  fun
  | CursorT(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody) => CursorPath_common.cons'(0, of_z(zbody))
  | SumZ(zsumty) => CursorPath_common.cons'(0, of_zsumtyp(zsumty))
  | ListZ(zbody) => CursorPath_common.cons'(0, of_z(zbody))
and of_zoperator =
  fun
  | (cursor, _) => ([], cursor)
and of_zsumtyp = zsumty =>
  CursorPath_common.of_zopseq_(~of_zoperand=of_zsumtyp_operand, zsumty)
and of_zsumtyp_operand =
  fun
  | CursorTS(cursor, _) => ([], cursor)
  | ConstTagZ(ztag)
  | ArgTagZT(ztag, _) => of_ztag(ztag)
  | ArgTagZA(_, zty) => of_z(zty)
and of_zsumtyp_operator = ((cursor, _)) => ([], cursor)
and of_ztag = (CursorTag(cursor, _)) => ([], cursor);

let rec follow = (path: CursorPath.t, uty: UHTyp.t): option(ZTyp.t) =>
  follow_opseq(path, uty)
and follow_opseq =
    (path: CursorPath.t, opseq: UHTyp.opseq): option(ZTyp.zopseq) =>
  CursorPath_common.follow_opseq_(
    ~follow_operand,
    ~follow_operator,
    path,
    opseq,
  )
and follow_operand =
    ((steps, cursor): CursorPath.t, operand: UHTyp.operand)
    : option(ZTyp.zoperand) =>
  switch (steps) {
  | [] => operand |> ZTyp.place_cursor_operand(cursor)
  | [x, ...xs] =>
    switch (operand) {
    | Hole
    | Unit
    | Int
    | Float
    | Bool => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZTyp.ParenthesizedZ(zbody))
      | _ => None
      }
    | Sum(sumty) =>
      switch (x) {
      | 0 =>
        sumty
        |> follow_sumtyp((xs, cursor))
        |> Option.map(zsumty => ZTyp.SumZ(zsumty))
      | _ => None
      }
    | List(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZTyp.ListZ(zbody))
      | _ => None
      }
    }
  }
and follow_operator =
    ((steps, cursor): CursorPath.t, operator: UHTyp.operator)
    : option(ZTyp.zoperator) =>
  switch (steps) {
  | [] => operator |> ZTyp.place_cursor_operator(cursor)
  | [_, ..._] => None
  }
and follow_sumtyp =
    (path: CursorPath.t, sumty: UHTyp.sumtyp): option(ZTyp.zsumtyp) =>
  CursorPath_common.follow_opseq_(
    ~follow_operand=follow_sumtyp_operand,
    ~follow_operator=follow_sumtyp_operator,
    path,
    sumty,
  )
and follow_sumtyp_operand =
    ((steps, cursor): CursorPath.t, sumty_operand: UHTyp.sumtyp_operand)
    : option(ZTyp.zsumtyp_operand) =>
  switch (steps) {
  | [] => sumty_operand |> ZTyp.place_cursor_sumtyp_operand(cursor)
  | [x, ...xs] =>
    switch (sumty_operand) {
    | ConstTag(_) => None
    | ArgTag(tag, ty) =>
      switch (x) {
      | 0 =>
        let+ zty = follow((xs, cursor), ty);
        ZTyp.ArgTagZA(tag, zty);
      | _ => None
      }
    }
  }
and follow_sumtyp_operator =
    ((steps, cursor): CursorPath.t, sumty_operator: UHTyp.sumtyp_operator)
    : option(ZTyp.zsumtyp_operator) =>
  switch (steps) {
  | [] => sumty_operator |> ZTyp.place_cursor_sumtyp_operator(cursor)
  | [_, ..._] => None
  };

let rec of_steps =
        (steps: CursorPath.steps, ~side: Side.t=Before, uty: UHTyp.t)
        : option(CursorPath.t) =>
  of_steps_opseq(steps, ~side, uty)
and of_steps_opseq =
    (steps: CursorPath.steps, ~side: Side.t, opseq: UHTyp.opseq)
    : option(CursorPath.t) =>
  CursorPath_common.of_steps_opseq_(
    ~of_steps_operand,
    ~of_steps_operator,
    steps,
    ~side,
    opseq,
  )
and of_steps_operand =
    (steps: CursorPath.steps, ~side: Side.t, operand: UHTyp.operand)
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_before_operand
      | After => ZTyp.place_after_operand
      };
    Some(of_zoperand(place_cursor(operand)));
  | [x, ...xs] =>
    switch (operand) {
    | Hole
    | Unit
    | Int
    | Float
    | Bool => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        of_steps(xs, ~side, body)
        |> Option.map(path => CursorPath_common.cons'(0, path))
      | _ => None
      }
    | Sum(sumty) =>
      switch (x) {
      | 0 =>
        let+ path = of_steps_sumtyp(xs, ~side, sumty);
        CursorPath_common.cons'(0, path);
      | _ => None
      }
    | List(body) =>
      switch (x) {
      | 0 =>
        body
        |> of_steps(xs, ~side)
        |> Option.map(path => CursorPath_common.cons'(0, path))
      | _ => None
      }
    }
  }
and of_steps_operator =
    (steps: CursorPath.steps, ~side: Side.t, operator: UHTyp.operator)
    : option(CursorPath.t) =>
  switch (steps) {
  | [_, ..._] => None
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_before_operator
      | After => ZTyp.place_after_operator
      };
    switch (place_cursor(operator)) {
    | Some(zty) => Some(of_zoperator(zty))
    | _ => None
    };
  }
and of_steps_sumtyp =
    (steps: CursorPath.steps, ~side: Side.t, sumty: UHTyp.sumtyp)
    : option(CursorPath.t) =>
  CursorPath_common.of_steps_opseq_(
    ~of_steps_operand=of_steps_sumtyp_operand,
    ~of_steps_operator=of_steps_sumtyp_operator,
    steps,
    ~side,
    sumty,
  )
and of_steps_sumtyp_operand =
    (
      steps: CursorPath.steps,
      ~side: Side.t,
      sumty_operand: UHTyp.sumtyp_operand,
    )
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_before_sumtyp_operand
      | After => ZTyp.place_after_sumtyp_operand
      };
    Some(of_zsumtyp_operand(place_cursor(sumty_operand)));
  | [x, ...xs] =>
    switch (sumty_operand) {
    | ConstTag(_) => None
    | ArgTag(_, ty) =>
      switch (x) {
      | 1 =>
        let+ path = of_steps(xs, ~side, ty);
        CursorPath_common.cons'(1, path);
      | _ => None
      }
    }
  }
and of_steps_sumtyp_operator =
    (
      steps: CursorPath.steps,
      ~side: Side.t,
      sumty_operator: UHTyp.sumtyp_operator,
    )
    : option(CursorPath.t) =>
  switch (steps) {
  | [_, ..._] => None
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_before_sumtyp_operator
      | After => ZTyp.place_after_sumtyp_operator
      };
    switch (place_cursor(sumty_operator)) {
    | Some(zsumty_operator) => Some(of_zsumtyp_operator(zsumty_operator))
    | _ => None
    };
  };

let hole_sort = _ => CursorPath.TypHole;
let is_space = _ => false;

let hole_tag =
    (tag: Tag.t, rev_steps: CursorPath.rev_steps)
    : option(CursorPath.hole_info) =>
  switch (tag) {
  | Tag(_) => None
  | TagHole(u) => Some({sort: TagHole(u), steps: List.rev(rev_steps)})
  };
let holes_tag =
    (tag: Tag.t, rev_steps: CursorPath.rev_steps, hs: CursorPath.hole_list)
    : CursorPath.hole_list =>
  switch (hole_tag(tag, rev_steps)) {
  | None => hs
  | Some(h) => [h, ...hs]
  };

let rec holes =
        (
          uty: UHTyp.t,
          rev_steps: CursorPath.rev_steps,
          hs: CursorPath.hole_list,
        )
        : CursorPath.hole_list =>
  hs
  |> CursorPath_common.holes_opseq(
       ~holes_operand,
       ~hole_sort,
       ~is_space,
       ~rev_steps,
       uty,
     )
and holes_operand =
    (
      operand: UHTyp.operand,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hole_list,
    )
    : CursorPath.hole_list =>
  switch (operand) {
  | Hole => [{sort: TypHole, steps: List.rev(rev_steps)}, ...hs]
  | Unit
  | Int
  | Float
  | Bool => hs
  | Sum(sumty) => hs |> holes_sumtyp(sumty, [0, ...rev_steps])
  | Parenthesized(body)
  | List(body) => hs |> holes(body, [0, ...rev_steps])
  }
and holes_sumtyp =
    (
      sumty: UHTyp.sumtyp,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hole_list,
    )
    : CursorPath.hole_list =>
  hs
  |> CursorPath_common.holes_opseq(
       ~holes_operand=holes_sumtyp_operand,
       ~hole_sort,
       ~is_space,
       ~rev_steps,
       sumty,
     )
and holes_sumtyp_operand =
    (
      sumty_operand: UHTyp.sumtyp_operand,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hole_list,
    )
    : CursorPath.hole_list =>
  switch (sumty_operand) {
  | ConstTag(tag) => holes_tag(tag, [0, ...rev_steps], hs)
  | ArgTag(tag, ty) =>
    let hs = holes_tag(tag, [0, ...rev_steps], hs);
    holes(ty, [1, ...rev_steps], hs);
  };

let rec holes_z =
        (zty: ZTyp.t, rev_steps: CursorPath.rev_steps): CursorPath.zhole_list =>
  holes_zopseq(zty, rev_steps)
and holes_zopseq =
    (zopseq: ZTyp.zopseq, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  CursorPath_common.holes_zopseq_(
    ~holes_operand,
    ~holes_zoperand,
    ~hole_sort,
    ~is_space,
    ~rev_steps,
    ~erase_zopseq=ZTyp.erase_zopseq,
    zopseq,
  )
and holes_zoperand =
    (zoperand: ZTyp.zoperand, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  switch (zoperand) {
  | CursorT(_, Hole) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=Some({sort: TypHole, steps: List.rev(rev_steps)}),
      (),
    )
  | CursorT(_, Unit | Int | Float | Bool) => CursorPath_common.no_holes
  | CursorT(OnDelim(k, _), Parenthesized(body) | List(body)) =>
    let holes = holes(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_before=holes, ())
    | 1 => CursorPath_common.mk_zholes(~holes_after=holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorT(OnDelim(k, _), Sum(sumty)) =>
    let holes = holes_sumtyp(sumty, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_before=holes, ())
    | 1 => CursorPath_common.mk_zholes(~holes_after=holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorT(OnOp(_) | OnText(_), Parenthesized(_) | List(_) | Sum(_)) =>
    /* invalid cursor position */
    CursorPath_common.no_holes
  | SumZ(zsumty) => holes_zsumtyp(zsumty, [0, ...rev_steps])
  | ParenthesizedZ(zbody)
  | ListZ(zbody) => holes_z(zbody, [0, ...rev_steps])
  }
and holes_zsumtyp =
    (zsumty: ZTyp.zsumtyp, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  CursorPath_common.holes_zopseq_(
    ~holes_operand=holes_sumtyp_operand,
    ~holes_zoperand=holes_zsumtyp_operand,
    ~hole_sort,
    ~is_space,
    ~rev_steps,
    ~erase_zopseq=ZTyp.erase_zsumtyp,
    zsumty,
  )
and holes_zsumtyp_operand =
    (zsumty_operand: ZTyp.zsumtyp_operand, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  switch (zsumty_operand) {
  | CursorTS(OnDelim(k, _), ConstTag(tag)) =>
    let holes = holes_tag(tag, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_after=holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorTS(OnDelim(k, _), ArgTag(tag, ty)) =>
    let tag_holes = holes_tag(tag, [0, ...rev_steps], []);
    let ty_holes = holes(ty, [1, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_after=tag_holes @ ty_holes, ())
    | 1 =>
      CursorPath_common.mk_zholes(
        ~holes_before=tag_holes,
        ~holes_after=ty_holes,
        (),
      )
    | _ => CursorPath_common.no_holes
    };
  | CursorTS(OnOp(_) | OnText(_), _) =>
    /* invalid cursor position */
    CursorPath_common.no_holes
  | ConstTagZ(ztag) => holes_ztag(ztag, [0, ...rev_steps])
  | ArgTagZT(ztag, ty) =>
    let ty_holes = holes(ty, [1, ...rev_steps], []);
    let holes = holes_ztag(ztag, [0, ...rev_steps]);
    {...holes, holes_after: ty_holes @ holes.holes_after};
  | ArgTagZA(tag, zty) =>
    let tag_holes = holes_tag(tag, [0, ...rev_steps], []);
    let holes = holes_z(zty, [1, ...rev_steps]);
    {...holes, holes_before: tag_holes @ holes.holes_before};
  }
and holes_ztag =
    (ztag: ZTag.t, rev_steps: CursorPath.rev_steps): CursorPath.zhole_list =>
  switch (ztag) {
  | CursorTag(OnDelim(0, _), tag) =>
    switch (hole_tag(tag, [0, ...rev_steps])) {
    | None => CursorPath_common.no_holes
    | Some(_) as hole => CursorPath_common.mk_zholes(~hole_selected=hole, ())
    }
  | CursorTag(OnDelim(_, _) | OnOp(_) | OnText(_), _) =>
    /* invalid cursor position */
    CursorPath_common.no_holes
  };
