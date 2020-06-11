let rec of_z = (zp: ZPat.t): CursorPath.t => of_zopseq(zp)
and of_zopseq = (zopseq: ZPat.zopseq): CursorPath.t =>
  CursorPath.of_zopseq_(~of_zoperand, zopseq)
and of_zoperand =
  fun
  | CursorP(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody)
  | InjZ(_, _, zbody) => CursorPath.cons'(0, of_z(zbody))
and of_zoperator =
  fun
  | (cursor, _) => ([], cursor);

let rec follow = (path: CursorPath.t, p: UHPat.t): option(ZPat.t) =>
  follow_opseq(path, p)
and follow_opseq =
    (path: CursorPath.t, opseq: UHPat.opseq): option(ZPat.zopseq) =>
  CursorPath.follow_opseq_(~follow_operand, ~follow_operator, path, opseq)
and follow_operand =
    ((steps, cursor): CursorPath.t, operand: UHPat.operand)
    : option(ZPat.zoperand) =>
  switch (steps) {
  | [] => operand |> ZPat.place_cursor_operand(cursor)
  | [x, ...xs] =>
    switch (operand) {
    | EmptyHole(_)
    | Wild(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | ListNil(_) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> OptUtil.map(zbody => ZPat.ParenthesizedZ(zbody))
      | _ => None
      }
    | Inj(err, side, body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> OptUtil.map(zbody => ZPat.InjZ(err, side, zbody))
      | _ => None
      }
    }
  }
and follow_operator =
    ((steps, cursor): CursorPath.t, operator: UHPat.operator)
    : option(ZPat.zoperator) =>
  switch (steps) {
  | [] => operator |> ZPat.place_cursor_operator(cursor)
  | [_, ..._] => None
  };

let rec of_steps =
        (steps: CursorPath.steps, ~side: Side.t=Before, p: UHPat.t)
        : option(CursorPath.t) =>
  of_steps_opseq(steps, ~side, p)
and of_steps_opseq =
    (steps: CursorPath.steps, ~side: Side.t, opseq: UHPat.opseq)
    : option(CursorPath.t) =>
  CursorPath.of_steps_opseq_(
    ~of_steps_operand,
    ~of_steps_operator,
    steps,
    ~side,
    opseq,
  )
and of_steps_operand =
    (steps: CursorPath.steps, ~side: Side.t, operand: UHPat.operand)
    : option(CursorPath.t) =>
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
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | ListNil(_) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> of_steps(xs, ~side)
        |> OptUtil.map(path => CursorPath.cons'(0, path))
      | _ => None
      }
    | Inj(_, _, body) =>
      switch (x) {
      | 0 =>
        body
        |> of_steps(xs, ~side)
        |> OptUtil.map(path => CursorPath.cons'(0, path))
      | _ => None
      }
    }
  }
and of_steps_operator =
    (steps: CursorPath.steps, ~side: Side.t, operator: UHPat.operator)
    : option(CursorPath.t) =>
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

let hole_desc = (u: MetaVar.t): CursorPath.hole_desc => PatHole(u);

let rec holes =
        (
          p: UHPat.t,
          rev_steps: CursorPath.rev_steps,
          hs: CursorPath.hole_list,
        )
        : CursorPath.hole_list =>
  hs
  |> CursorPath.holes_opseq(
       ~holes_operand,
       ~hole_desc,
       ~is_space=Operators_Pat.is_Space,
       ~rev_steps,
       p,
     )
and holes_operand =
    (
      operand: UHPat.operand,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hole_list,
    )
    : CursorPath.hole_list =>
  switch (operand) {
  | EmptyHole(u)
  | Wild(InHole(_, u))
  | Var(InHole(_, u), _, _)
  | Var(_, InVarHole(_, u), _)
  | IntLit(InHole(_, u), _)
  | FloatLit(InHole(_, u), _)
  | BoolLit(InHole(_, u), _)
  | ListNil(InHole(_, u)) => [(PatHole(u), rev_steps |> List.rev), ...hs]
  | Var(NotInHole, NotInVarHole, _)
  | Wild(NotInHole)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _)
  | ListNil(NotInHole) => hs
  | Parenthesized(body) => hs |> holes(body, [0, ...rev_steps])
  | Inj(err, _, body) =>
    let body_holes = hs |> holes(body, [0, ...rev_steps]);
    switch (err) {
    | NotInHole => body_holes
    | InHole(_, u) => [(PatHole(u), rev_steps |> List.rev), ...body_holes]
    };
  };

let rec holes_z =
        (zp: ZPat.t, rev_steps: CursorPath.rev_steps): CursorPath.zhole_list =>
  holes_zopseq(zp, rev_steps)
and holes_zopseq =
    (zopseq: ZPat.zopseq, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  CursorPath.holes_zopseq_(
    ~holes_operand,
    ~holes_zoperand,
    ~hole_desc,
    ~is_space=Operators_Pat.is_Space,
    ~rev_steps,
    ~erase_zopseq=ZPat.erase_zopseq,
    zopseq,
  )
and holes_zoperand =
    (zoperand: ZPat.zoperand, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  switch (zoperand) {
  | CursorP(OnOp(_), _) => CursorPath.no_holes
  | CursorP(_, EmptyHole(u)) =>
    CursorPath.mk_zholes(
      ~hole_selected=Some((PatHole(u), rev_steps |> List.rev)),
      (),
    )
  | CursorP(_, Var(err, verr, _)) =>
    switch (err, verr) {
    | (NotInHole, NotInVarHole) => CursorPath.no_holes
    | (InHole(_, u), _)
    | (_, InVarHole(_, u)) =>
      CursorPath.mk_zholes(
        ~hole_selected=Some((PatHole(u), rev_steps |> List.rev)),
        (),
      )
    }
  | CursorP(_, Wild(err))
  | CursorP(_, IntLit(err, _))
  | CursorP(_, FloatLit(err, _))
  | CursorP(_, BoolLit(err, _))
  | CursorP(_, ListNil(err)) =>
    switch (err) {
    | NotInHole => CursorPath.no_holes
    | InHole(_, u) =>
      CursorPath.mk_zholes(
        ~hole_selected=Some((PatHole(u), rev_steps |> List.rev)),
        (),
      )
    }
  | CursorP(OnDelim(k, _), Parenthesized(body)) =>
    let body_holes = holes(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath.mk_zholes(~holes_before=body_holes, ())
    | 1 => CursorPath.mk_zholes(~holes_after=body_holes, ())
    | _ => CursorPath.no_holes
    };
  | CursorP(OnDelim(k, _), Inj(err, _, body)) =>
    let body_holes = holes(body, [0, ...rev_steps], []);
    let hole_selected =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) => Some((CursorPath.PatHole(u), rev_steps |> List.rev))
      };
    switch (k) {
    | 0 => CursorPath.mk_zholes(~holes_before=body_holes, ~hole_selected, ())
    | 1 => CursorPath.mk_zholes(~hole_selected, ~holes_after=body_holes, ())
    | _ => CursorPath.no_holes
    };
  | CursorP(OnText(_), Parenthesized(_) | Inj(_, _, _)) =>
    // invalid cursor position
    CursorPath.no_holes
  | ParenthesizedZ(zbody) => holes_z(zbody, [0, ...rev_steps])
  | InjZ(err, _, zbody) =>
    let zbody_holes = holes_z(zbody, [0, ...rev_steps]);
    switch (err) {
    | NotInHole => zbody_holes
    | InHole(_, u) => {
        ...zbody_holes,
        holes_before: [
          (PatHole(u), rev_steps |> List.rev),
          ...zbody_holes.holes_before,
        ],
      }
    };
  };
