open OptUtil.Syntax;

let mk_hole_sort = CursorPath.mk_hole_sort;

let rec of_z = (zty: ZTyp.t): CursorPath.t => of_zopseq(zty)

and of_zopseq = zopseq => CursorPath_common.of_zopseq_(~of_zoperand, zopseq)

and of_zoperand =
  fun
  | CursorT(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody) => CursorPath_common.cons'(0, of_z(zbody))
  | ListZ(zbody) => CursorPath_common.cons'(0, of_z(zbody))
  | FiniteSumZ(zsum_body) =>
    CursorPath_common.cons'(0, of_zsum_body(zsum_body))
  | ElidedSumZ(zoperand) =>
    CursorPath_common.cons'(0, of_zsum_body_operand(zoperand))

and of_zoperator = ((cursor, _)) => ([], cursor)

and of_zsum_body = zsum_body =>
  CursorPath_common.of_zopseq_(~of_zoperand=of_zsum_body_operand, zsum_body)

and of_zsum_body_operator = ((cursor, _)) => ([], cursor)

and of_zsum_body_operand =
  fun
  | CursorArgTag(cursor, _, _) => ([], cursor)
  | ConstTagZ(ztag) => CursorPath_Tag.of_z(ztag)
  | ArgTagZT(ztag, _) =>
    CursorPath_common.cons'(0, CursorPath_Tag.of_z(ztag))
  | ArgTagZA(_, zty) => CursorPath_common.cons'(1, of_z(zty));

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
    | List(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZTyp.ListZ(zbody))
      | _ => None
      }
    | FiniteSum(None) =>
      switch (steps) {
      | [] => ZTyp.place_cursor_operand(cursor, operand)
      | _ => None
      }
    | FiniteSum(Some(sum_body)) =>
      switch (x) {
      | 0 =>
        let+ zsum_body = follow_sum_body((xs, cursor), sum_body);
        ZTyp.FiniteSumZ(zsum_body);
      | _ => None
      }
    | ElidedSum(operand) =>
      switch (x) {
      | 0 =>
        let+ zoperand = follow_sum_body_operand((xs, cursor), operand);
        ZTyp.ElidedSumZ(zoperand);
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

and follow_sum_body =
    (path: CursorPath.t, sum_body: UHTyp.sum_body): option(ZTyp.zsum_body) =>
  CursorPath_common.follow_opseq_(
    ~follow_operand=follow_sum_body_operand,
    ~follow_operator=follow_sum_body_operator,
    path,
    sum_body,
  )

and follow_sum_body_operand =
    ((steps, cursor): CursorPath.t, operand: UHTyp.sum_body_operand)
    : option(ZTyp.zsum_body_operand) =>
  switch (operand) {
  | ConstTag(tag) =>
    let+ ztag = CursorPath_Tag.follow((steps, cursor), tag);
    ZTyp.ConstTagZ(ztag);
  | ArgTag(tag, ty) =>
    switch (steps) {
    | [] =>
      switch (cursor) {
      | OnDelim((-1), _) =>
        let+ ztag = CursorPath_Tag.follow((steps, cursor), tag);
        ZTyp.ArgTagZT(ztag, ty);
      | _ => ZTyp.place_cursor_sum_body_operand(cursor, operand)
      }
    | [0, ...xs] =>
      let+ ztag = CursorPath_Tag.follow((xs, cursor), tag);
      ZTyp.ArgTagZT(ztag, ty);
    | [1, ...xs] =>
      let+ zty = follow((xs, cursor), ty);
      ZTyp.ArgTagZA(tag, zty);
    | _ => None
    }
  }

and follow_sum_body_operator =
    ((steps, cursor): CursorPath.t, operator: UHTyp.sum_body_operator)
    : option(ZTyp.zsum_body_operator) =>
  switch (steps) {
  | [] => ZTyp.place_cursor_sum_body_operator(cursor, operator)
  | _ => None
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
    : option(CursorPath.t) => {
  let place_cursor =
    switch (side) {
    | Before => ZTyp.place_before_operand
    | After => ZTyp.place_after_operand
    };
  switch (steps) {
  | [] => Some(of_zoperand(place_cursor(operand)))
  | [x, ...xs] =>
    switch (operand) {
    | Hole
    | Unit
    | Int
    | Float
    | Bool
    | FiniteSum(None) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        of_steps(xs, ~side, body)
        |> Option.map(path => CursorPath_common.cons'(0, path))
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
    | FiniteSum(Some(sum_body)) =>
      switch (x) {
      | 0 =>
        let+ path = of_steps_sum_body(xs, ~side, sum_body);
        CursorPath_common.cons'(0, path);
      | _ => None
      }
    | ElidedSum(operand) =>
      switch (x) {
      | 0 =>
        let+ path = of_steps_sum_body_operand(xs, ~side, operand);
        CursorPath_common.cons'(0, path);
      | _ => None
      }
    }
  };
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

and of_steps_sum_body =
    (steps: CursorPath.steps, ~side: Side.t=Before, sum_body: UHTyp.sum_body)
    : option(CursorPath.t) =>
  CursorPath_common.of_steps_opseq_(
    ~of_steps_operand=of_steps_sum_body_operand,
    ~of_steps_operator=of_steps_sum_body_operator,
    steps,
    ~side,
    sum_body,
  )

and of_steps_sum_body_operand =
    (steps: CursorPath.steps, ~side: Side.t, operand: UHTyp.sum_body_operand)
    : option(CursorPath.t) =>
  switch (operand) {
  | ConstTag(tag) => CursorPath_Tag.of_steps(steps, ~side, tag)
  | ArgTag(tag, ty) =>
    switch (steps) {
    | [] => Some((steps, OnDelim(0, side)))
    | [0, ...xs] =>
      let+ path = CursorPath_Tag.of_steps(xs, ~side, tag);
      CursorPath_common.cons'(0, path);
    | [1, ...xs] =>
      let+ path = of_steps(xs, ~side, ty);
      CursorPath_common.cons'(1, path);
    | _ => None
    }
  }

and of_steps_sum_body_operator =
    (
      steps: CursorPath.steps,
      ~side: Side.t,
      operator: UHTyp.sum_body_operator,
    )
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_before_sum_body_operator
      | After => ZTyp.place_after_sum_body_operator
      };
    switch (place_cursor(operator)) {
    | Some(zoperator) => Some(of_zsum_body_operator(zoperator))
    | _ => None
    };
  | _ => None
  };

let hole_sort = _ => CursorPath.TypHole;
let sum_body_hole_sort = (shape: CursorPath.tag_hole_shape, u: MetaVar.t) =>
  CursorPath.TagHole(u, shape);

let is_space = _ => false;

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
  | Hole => [mk_hole_sort(TypHole, List.rev(rev_steps)), ...hs]
  | Unit
  | Int
  | Float
  | Bool
  | FiniteSum(None) => hs
  | FiniteSum(Some(sum_body)) =>
    hs |> holes_sum_body(sum_body, [0, ...rev_steps])
  | ElidedSum(operand) =>
    hs |> holes_sum_body_operand(operand, [0, ...rev_steps])
  | Parenthesized(body)
  | List(body) => hs |> holes(body, [0, ...rev_steps])
  }
and holes_sum_body =
    (
      sum_body: UHTyp.sum_body,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hole_list,
    )
    : CursorPath.hole_list =>
  hs
  |> CursorPath_common.holes_opseq(
       ~holes_operand=holes_sum_body_operand,
       ~hole_sort=sum_body_hole_sort(TypeErr),
       ~is_space,
       ~rev_steps,
       sum_body,
     )
and holes_sum_body_operand =
    (
      sum_body_operand: UHTyp.sum_body_operand,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hole_list,
    )
    : CursorPath.hole_list =>
  switch (sum_body_operand) {
  | ConstTag(tag) => CursorPath_Tag.holes(tag, rev_steps, hs)
  | ArgTag(tag, ty) =>
    hs
    |> holes(ty, [1, ...rev_steps])
    |> CursorPath_Tag.holes(tag, [0, ...rev_steps])
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
      ~hole_selected=Some(mk_hole_sort(TypHole, List.rev(rev_steps))),
      (),
    )
  | CursorT(_, Unit | Int | Float | Bool) => CursorPath_common.no_holes
  | CursorT(OnDelim(k, _), Parenthesized(body) | List(body)) =>
    let holes = holes(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_after=holes, ())
    | 1 => CursorPath_common.mk_zholes(~holes_before=holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorT(OnDelim(_, _), FiniteSum(None)) => CursorPath_common.no_holes
  | CursorT(OnDelim(k, _), FiniteSum(Some(sum_body))) =>
    let holes = holes_sum_body(sum_body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_after=holes, ())
    | 1 => CursorPath_common.mk_zholes(~holes_before=holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorT(OnDelim(k, _), ElidedSum(operand)) =>
    let holes = holes_sum_body_operand(operand, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_after=holes, ())
    | 1 => CursorPath_common.mk_zholes(~holes_before=holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorT(
      OnOp(_) | OnText(_),
      Parenthesized(_) | List(_) | FiniteSum(_) | ElidedSum(_),
    ) =>
    /* invalid cursor position */
    CursorPath_common.no_holes
  | ParenthesizedZ(zbody)
  | ListZ(zbody) => holes_z(zbody, [0, ...rev_steps])
  | FiniteSumZ(zsum_body) => holes_zsum_body(zsum_body, [0, ...rev_steps])
  | ElidedSumZ(zoperand) =>
    holes_zsum_body_operand(zoperand, [0, ...rev_steps])
  }
and holes_zsum_body =
    (zsum_body: ZTyp.zsum_body, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  CursorPath_common.holes_zopseq_(
    ~holes_operand=holes_sum_body_operand,
    ~holes_zoperand=holes_zsum_body_operand,
    ~hole_sort=sum_body_hole_sort(TypeErr),
    ~is_space,
    ~rev_steps,
    ~erase_zopseq=ZTyp.erase_zsum_body,
    zsum_body,
  )
and holes_zsum_body_operand =
    (
      zsum_body_operand: ZTyp.zsum_body_operand,
      rev_steps: CursorPath.rev_steps,
    )
    : CursorPath.zhole_list =>
  switch (zsum_body_operand) {
  | CursorArgTag(OnText(_) | OnOp(_), _, _) => CursorPath_common.no_holes
  | CursorArgTag(cursor, tag, ty) =>
    switch (cursor) {
    | OnText(_)
    | OnOp(_) => CursorPath_common.no_holes
    | OnDelim(k, _) =>
      let tag_holes = [] |> CursorPath_Tag.holes(tag, [0, ...rev_steps]);
      let ty_holes = [] |> holes(ty, [1, ...rev_steps]);
      switch (k) {
      | 0 =>
        CursorPath_common.mk_zholes(
          ~holes_before=tag_holes,
          ~holes_after=ty_holes,
          (),
        )
      | 1 =>
        CursorPath_common.mk_zholes(~holes_before=tag_holes @ ty_holes, ())
      | _ => CursorPath_common.no_holes
      };
    }
  | ConstTagZ(ztag) => CursorPath_Tag.holes_z(ztag, rev_steps)
  | ArgTagZT(ztag, ty) =>
    let CursorPath.{holes_before, hole_selected, holes_after} =
      CursorPath_Tag.holes_z(ztag, [0, ...rev_steps]);
    let ty_holes = [] |> holes(ty, [1, ...rev_steps]);
    CursorPath_common.mk_zholes(
      ~holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ ty_holes,
      (),
    );
  | ArgTagZA(tag, zty) =>
    let tag_holes = [] |> CursorPath_Tag.holes(tag, [0, ...rev_steps]);
    let CursorPath.{holes_before, hole_selected, holes_after} =
      holes_z(zty, [1, ...rev_steps]);
    CursorPath_common.mk_zholes(
      ~holes_before=tag_holes @ holes_before,
      ~hole_selected,
      ~holes_after,
      (),
    );
  };
