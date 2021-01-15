let rec of_z = (zp: ZPat.t): CursorPath_common.t => of_zopseq(zp)
and of_zopseq = (zopseq: ZPat.zopseq): CursorPath_common.t =>
  CursorPath_common.of_zopseq_(~of_zoperand, zopseq)
and of_zoperand =
  fun
  | CursorP(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody)
  | ListLitZ(_, zbody)
  | InjZ(_, _, zbody) => CursorPath_common.cons'(0, of_z(zbody))
and of_zoperator =
  fun
  | (cursor, _) => ([], cursor);

let rec follow = (path: CursorPath_common.t, p: UHPat.t): option(ZPat.t) =>
  follow_opseq(path, p)
and follow_opseq =
    (path: CursorPath_common.t, opseq: UHPat.opseq): option(ZPat.zopseq) =>
  CursorPath_common.follow_opseq_(
    ~follow_operand,
    ~follow_operator,
    path,
    opseq,
  )
and follow_operand =
    ((steps, cursor): CursorPath_common.t, operand: UHPat.operand)
    : option(ZPat.zoperand) =>
  switch (steps) {
  | [] => operand |> ZPat.place_cursor_operand(cursor)
  | [x, ...xs] =>
    switch (operand) {
    | EmptyHole(_)
    | Wild(_)
    | InvalidText(_)
    | Var(_)
    | IntLit(_)
    | FloatLit(_)
    | BoolLit(_)
    | ListLit(_, None) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZPat.ParenthesizedZ(zbody))
      | _ => None
      }
    | ListLit(err, Some(body)) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        // |> OptUtil.map2(zbody => ZPat.ListLitZ(err, zbody))
        |> Option.map(zbody => ZPat.ListLitZ(err, zbody))
      | _ => None
      }
    | Inj(err, side, body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZPat.InjZ(err, side, zbody))
      | _ => None
      }
    }
  }
and follow_operator =
    ((steps, cursor): CursorPath_common.t, operator: UHPat.operator)
    : option(ZPat.zoperator) =>
  switch (steps) {
  | [] => operator |> ZPat.place_cursor_operator(cursor)
  | [_, ..._] => None
  };

let rec of_steps =
        (steps: CursorPath_common.steps, ~side: Side.t=Before, p: UHPat.t)
        : option(CursorPath_common.t) =>
  of_steps_opseq(steps, ~side, p)
and of_steps_opseq =
    (steps: CursorPath_common.steps, ~side: Side.t, opseq: UHPat.opseq)
    : option(CursorPath_common.t) =>
  CursorPath_common.of_steps_opseq_(
    ~of_steps_operand,
    ~of_steps_operator,
    steps,
    ~side,
    opseq,
  )
and of_steps_operand =
    (steps: CursorPath_common.steps, ~side: Side.t, operand: UHPat.operand)
    : option(CursorPath_common.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZPat.place_before_operand
      | After => ZPat.place_after_operand
      };
    Some(of_zoperand(place_cursor(operand)));
  | [x, ...xs] =>
    switch (operand) {
    | EmptyHole(_)
    | Wild(_)
    | InvalidText(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | ListLit(_, None) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> of_steps(xs, ~side)
        |> Option.map(path => CursorPath_common.cons'(0, path))
      | _ => None
      }
    | ListLit(_, Some(body)) =>
      switch (x) {
      | 0 =>
        body
        |> of_steps(xs, ~side)
        |> Option.map(path => CursorPath_common.cons'(0, path))
      | _ => None
      }
    | Inj(_, _, body) =>
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
    (steps: CursorPath_common.steps, ~side: Side.t, operator: UHPat.operator)
    : option(CursorPath_common.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZPat.place_before_operator
      | After => ZPat.place_after_operator
      };
    switch (place_cursor(operator)) {
    | Some(zop) => Some(of_zoperator(zop))
    | _ => None
    };
  | [_, ..._] => None
  };

let hole_sort = (u: MetaVar.t): CursorPath_common.hole_sort => PatHole(u);

let rec holes =
        (
          p: UHPat.t,
          rev_steps: CursorPath_common.rev_steps,
          hs: CursorPath_common.hole_list,
        )
        : CursorPath_common.hole_list =>
  hs
  |> CursorPath_common.holes_opseq(
       ~holes_operand,
       ~hole_sort,
       ~is_space=Operators_Pat.is_Space,
       ~rev_steps,
       p,
     )
and holes_operand =
    (
      operand: UHPat.operand,
      rev_steps: CursorPath_common.rev_steps,
      hs: CursorPath_common.hole_list,
    )
    : CursorPath_common.hole_list =>
  switch (operand) {
  | EmptyHole(u) => [
      {sort: PatHole(u), steps: List.rev(rev_steps), is_empty: true},
      ...hs,
    ]
  | InvalidText(u, _)
  | Wild(InHole(_, u))
  | Var(InHole(_, u), _, _)
  | Var(_, InVarHole(_, u), _)
  | IntLit(InHole(_, u), _)
  | FloatLit(InHole(_, u), _)
  | BoolLit(InHole(_, u), _)
  | ListLit(StandardErrStatus(InHole(_, u)), None) => [
      {sort: PatHole(u), steps: List.rev(rev_steps), is_empty: false},
      ...hs,
    ]
  | Var(NotInHole, NotInVarHole, _)
  | Wild(NotInHole)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _)
  | ListLit(_, None) => hs
  | ListLit(_, Some(body))
  | Parenthesized(body) => hs |> holes(body, [0, ...rev_steps])
  | Inj(err, _, body) =>
    let body_holes = hs |> holes(body, [0, ...rev_steps]);
    switch (err) {
    | NotInHole => body_holes
    | InHole(_, u) => [
        {sort: PatHole(u), steps: List.rev(rev_steps), is_empty: false},
        ...body_holes,
      ]
    };
  };

let rec holes_z =
        (zp: ZPat.t, rev_steps: CursorPath_common.rev_steps)
        : CursorPath_common.zhole_list =>
  holes_zopseq(zp, rev_steps)
and holes_zopseq =
    (zopseq: ZPat.zopseq, rev_steps: CursorPath_common.rev_steps)
    : CursorPath_common.zhole_list =>
  CursorPath_common.holes_zopseq_(
    ~holes_operand,
    ~holes_zoperand,
    ~hole_sort,
    ~is_space=Operators_Pat.is_Space,
    ~rev_steps,
    ~erase_zopseq=ZPat.erase_zopseq,
    zopseq,
  )
and holes_zoperand =
    (zoperand: ZPat.zoperand, rev_steps: CursorPath_common.rev_steps)
    : CursorPath_common.zhole_list =>
  switch (zoperand) {
  | CursorP(OnOp(_), _) => CursorPath_common.no_holes
  | CursorP(_, EmptyHole(u)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some({
          sort: PatHole(u),
          steps: List.rev(rev_steps),
          is_empty: true,
        }),
      (),
    )
  | CursorP(_, InvalidText(u, _)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some({
          sort: PatHole(u),
          steps: List.rev(rev_steps),
          is_empty: false,
        }),
      (),
    )
  | CursorP(_, Var(err, verr, _)) =>
    switch (err, verr) {
    | (NotInHole, NotInVarHole) => CursorPath_common.no_holes
    | (InHole(_, u), _)
    | (_, InVarHole(_, u)) =>
      CursorPath_common.mk_zholes(
        ~hole_selected=
          Some({
            sort: PatHole(u),
            steps: List.rev(rev_steps),
            is_empty: false,
          }),
        (),
      )
    }
  | CursorP(_, Wild(err))
  | CursorP(_, IntLit(err, _))
  | CursorP(_, FloatLit(err, _))
  | CursorP(_, BoolLit(err, _))
  | CursorP(_, ListLit(StandardErrStatus(err), None)) =>
    switch (err) {
    | NotInHole => CursorPath_common.no_holes
    | InHole(_, u) =>
      CursorPath_common.mk_zholes(
        ~hole_selected=
          Some({
            sort: PatHole(u),
            steps: List.rev(rev_steps),
            is_empty: false,
          }),
        (),
      )
    }
  | CursorP(OnDelim(_, _), ListLit(InconsistentBranches(_, _), None)) => CursorPath_common.no_holes
  | CursorP(OnDelim(k, _), Parenthesized(body) | ListLit(_, Some(body))) =>
    let body_holes = holes(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_before=body_holes, ())
    | 1 => CursorPath_common.mk_zholes(~holes_after=body_holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorP(OnDelim(k, _), Inj(err, _, body)) =>
    let body_holes = holes(body, [0, ...rev_steps], []);
    let hole_selected: option(CursorPath_common.hole_info) =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) =>
        Some({
          sort: PatHole(u),
          steps: List.rev(rev_steps),
          is_empty: false,
        })
      };
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zholes(
        ~holes_before=body_holes,
        ~hole_selected,
        (),
      )
    | 1 =>
      CursorPath_common.mk_zholes(~hole_selected, ~holes_after=body_holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorP(OnText(_), Parenthesized(_) | Inj(_, _, _) | ListLit(_, _)) =>
    // invalid cursor position
    CursorPath_common.no_holes
  | ParenthesizedZ(zbody) => holes_z(zbody, [0, ...rev_steps])
  | ListLitZ(_, zbody) => holes_z(zbody, [0, ...rev_steps])
  | InjZ(err, _, zbody) =>
    let zbody_holes = holes_z(zbody, [0, ...rev_steps]);
    switch (err) {
    | NotInHole => zbody_holes
    | InHole(_, u) => {
        ...zbody_holes,
        holes_before: [
          {sort: PatHole(u), steps: List.rev(rev_steps), is_empty: true},
          ...zbody_holes.holes_before,
        ],
      }
    };
  };
