open OptUtil.Syntax;

let mk_hole_sort = CursorPath.mk_hole_sort;

let rec of_z = (zp: ZPat.t): CursorPath.t => of_zopseq(zp)
and of_zopseq = (zopseq: ZPat.zopseq): CursorPath.t =>
  CursorPath_common.of_zopseq_(~of_zoperand, zopseq)
and of_zoperand =
  fun
  | CursorP(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody) => CursorPath_common.cons'(0, of_z(zbody))
  | InjZT(_, ztag, _) =>
    CursorPath_common.cons'(0, CursorPath_Tag.of_z(ztag))
  | InjZP(_, _, zbody) => CursorPath_common.cons'(1, of_z(zbody))
  | TypeAnnZP(_, zop, _) => CursorPath_common.cons'(0, of_zoperand(zop))
  | TypeAnnZA(_, _, zann) =>
    CursorPath_common.cons'(1, CursorPath_Typ.of_z(zann))
and of_zoperator =
  fun
  | (cursor, _) => ([], cursor);

let rec follow = (path: CursorPath.t, p: UHPat.t): option(ZPat.t) =>
  follow_opseq(path, p)
and follow_opseq =
    (path: CursorPath.t, opseq: UHPat.opseq): option(ZPat.zopseq) =>
  CursorPath_common.follow_opseq_(
    ~follow_operand,
    ~follow_operator,
    path,
    opseq,
  )
and follow_operand =
    ((steps, cursor): CursorPath.t, operand: UHPat.operand)
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
    | ListNil(_) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZPat.ParenthesizedZ(zbody))
      | _ => None
      }
    | Inj(err, tag, None) =>
      switch (x) {
      | 0 =>
        let+ ztag = tag |> CursorPath_Tag.follow((xs, cursor));
        ZPat.InjZT(err, ztag, None);
      | _ => None
      }
    | Inj(err, tag, Some(arg) as arg_opt) =>
      switch (x) {
      | 0 =>
        let+ ztag = tag |> CursorPath_Tag.follow((xs, cursor));
        ZPat.InjZT(err, ztag, arg_opt);
      | 1 =>
        let+ zarg = arg |> follow((xs, cursor));
        ZPat.InjZP(err, tag, zarg);
      | _ => None
      }
    | TypeAnn(err, op, ann) =>
      switch (x) {
      | 0 =>
        op
        |> follow_operand((xs, cursor))
        |> Option.map(zop => ZPat.TypeAnnZP(err, zop, ann))
      | 1 =>
        ann
        |> CursorPath_Typ.follow((xs, cursor))
        |> Option.map(zann => ZPat.TypeAnnZA(err, op, zann))
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
  CursorPath_common.of_steps_opseq_(
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
    | InvalidText(_)
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
        |> Option.map(path => CursorPath_common.cons'(0, path))
      | _ => None
      }
    | Inj(_, tag, None) =>
      switch (x) {
      | 0 =>
        let+ path = tag |> CursorPath_Tag.of_steps(xs, ~side);
        CursorPath_common.cons'(0, path);
      | _ => None
      }
    | Inj(_, tag, Some(body)) =>
      switch (x) {
      | 0 =>
        let+ path = tag |> CursorPath_Tag.of_steps(xs, ~side);
        CursorPath_common.cons'(0, path);
      | 1 =>
        let+ path = body |> of_steps(xs, ~side);
        CursorPath_common.cons'(1, path);
      | _ => None
      }
    | TypeAnn(_, op, ann) =>
      switch (x) {
      | 0 =>
        op
        |> of_steps_operand(xs, ~side)
        |> Option.map(path => CursorPath_common.cons'(0, path))
      | 1 =>
        ann
        |> CursorPath_Typ.of_steps(xs, ~side)
        |> Option.map(path => CursorPath_common.cons'(1, path))
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

let hole_sort = (shape, u: MetaVar.t): CursorPath.hole_sort =>
  PatHole(u, shape);
let holes_err = CursorPath_common.holes_err(~hole_sort=hole_sort(TypeErr));
let holes_verr = CursorPath_common.holes_verr(~hole_sort=hole_sort(VarErr));

let rec holes =
        (
          p: UHPat.t,
          rev_steps: CursorPath.rev_steps,
          hs: CursorPath.hole_list,
        )
        : CursorPath.hole_list =>
  hs
  |> CursorPath_common.holes_opseq(
       ~holes_operand,
       ~hole_sort=hole_sort(TypeErr),
       ~is_space=Operators_Pat.is_Space,
       ~rev_steps,
       p,
     )
and holes_opseq =
    (
      rev_steps: CursorPath.rev_steps,
      opseq: OpSeq.t('operand, 'operator),
      hs: CursorPath.hole_list,
    )
    : CursorPath.hole_list =>
  hs
  |> CursorPath_common.holes_opseq(
       ~holes_operand,
       ~hole_sort=hole_sort(TypeErr),
       ~is_space=Operators_Pat.is_Space,
       ~rev_steps,
       opseq,
     )
and holes_operand =
    (
      operand: UHPat.operand,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hole_list,
    )
    : CursorPath.hole_list =>
  switch (operand) {
  | EmptyHole(u) => [
      mk_hole_sort(PatHole(u, Empty), List.rev(rev_steps)),
      ...hs,
    ]
  | Var(err, verr, _) =>
    hs |> holes_verr(verr, rev_steps) |> holes_err(err, rev_steps)
  | Wild(err)
  | IntLit(err, _)
  | FloatLit(err, _)
  | BoolLit(err, _)
  | ListNil(err) => hs |> holes_err(err, rev_steps)
  | InvalidText(u, _) => [
      mk_hole_sort(ExpHole(u, VarErr), List.rev(rev_steps)),
    ]
  | Parenthesized(body) => hs |> holes(body, [0, ...rev_steps])
  | Inj(err, tag, None) =>
    hs
    |> CursorPath_Tag.holes(tag, [0, ...rev_steps])
    |> CursorPath_common.holes_inj_err(err, rev_steps)
  | Inj(err, tag, Some(body)) =>
    hs
    |> holes(body, [1, ...rev_steps])
    |> CursorPath_Tag.holes(tag, [0, ...rev_steps])
    |> CursorPath_common.holes_inj_err(err, rev_steps)
  | TypeAnn(err, op, ann) =>
    hs
    |> CursorPath_Typ.holes(ann, [1, ...rev_steps])
    |> holes_operand(op, [0, ...rev_steps])
    |> CursorPath_common.holes_err(
         ~hole_sort=u => PatHole(u, TypeErr),
         err,
         rev_steps,
       )
  };

let rec holes_z =
        (zp: ZPat.t, rev_steps: CursorPath.rev_steps): CursorPath.zhole_list =>
  holes_zopseq(zp, rev_steps)
and holes_zopseq =
    (zopseq: ZPat.zopseq, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  CursorPath_common.holes_zopseq_(
    ~holes_operand,
    ~holes_zoperand,
    ~hole_sort=hole_sort(TypeErr),
    ~is_space=Operators_Pat.is_Space,
    ~rev_steps,
    ~erase_zopseq=ZPat.erase_zopseq,
    zopseq,
  )
and holes_zoperand =
    (zoperand: ZPat.zoperand, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhole_list =>
  switch (zoperand) {
  | CursorP(OnOp(_), _) => CursorPath_common.no_holes
  | CursorP(_, EmptyHole(u)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some(mk_hole_sort(PatHole(u, Empty), List.rev(rev_steps))),
      (),
    )
  | CursorP(_, InvalidText(u, _)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some(mk_hole_sort(PatHole(u, VarErr), List.rev(rev_steps))),
      (),
    )
  | CursorP(_, Var(err, verr, _)) =>
    switch (err, verr) {
    | (NotInHole, NotInVarHole) => CursorPath_common.no_holes
    | (InHole(_, u), _) =>
      CursorPath_common.mk_zholes(
        ~hole_selected=
          Some(mk_hole_sort(PatHole(u, TypeErr), List.rev(rev_steps))),
        (),
      )
    | (_, InVarHole(_, u)) =>
      CursorPath_common.mk_zholes(
        ~hole_selected=
          Some(mk_hole_sort(PatHole(u, VarErr), List.rev(rev_steps))),
        (),
      )
    }
  | CursorP(_, Wild(err))
  | CursorP(_, IntLit(err, _))
  | CursorP(_, FloatLit(err, _))
  | CursorP(_, BoolLit(err, _))
  | CursorP(_, ListNil(err)) =>
    switch (err) {
    | NotInHole => CursorPath_common.no_holes
    | InHole(_, u) =>
      CursorPath_common.mk_zholes(
        ~hole_selected=
          Some(mk_hole_sort(PatHole(u, TypeErr), List.rev(rev_steps))),
        (),
      )
    }
  | CursorP(OnDelim(k, _), Parenthesized(body)) =>
    let body_holes = holes(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_before=body_holes, ())
    | 1 => CursorPath_common.mk_zholes(~holes_after=body_holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorP(OnDelim(k, _), TypeAnn(_, op, ann)) =>
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_operand(op, [0, ...rev_steps], []),
        ~holes_after=CursorPath_Typ.holes(ann, [1, ...rev_steps], []),
        (),
      )
    | _ => CursorPath_common.no_holes
    }
  | CursorP(OnDelim(k, _), Inj(err, tag, None)) =>
    let tag_holes = CursorPath_Tag.holes(tag, [0, ...rev_steps], []);
    let hole_selected: option(CursorPath.hole_info) =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) =>
        let steps = List.rev(rev_steps);
        Some({sort: PatHole(u, TypeErr), steps, ap_steps: steps});
      };
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zholes(~hole_selected, ~holes_after=tag_holes, ())
    | 1 =>
      CursorPath_common.mk_zholes(~holes_before=tag_holes, ~hole_selected, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorP(OnDelim(k, _), Inj(err, tag, Some(body))) =>
    let tag_holes = CursorPath_Tag.holes(tag, [0, ...rev_steps], []);
    let body_holes = holes(body, [1, ...rev_steps], []);
    let hole_selected: option(CursorPath.hole_info) =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) =>
        Some(mk_hole_sort(PatHole(u, TypeErr), List.rev(rev_steps)))
      };
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zholes(
        ~hole_selected,
        ~holes_after=tag_holes @ body_holes,
        (),
      )
    | 1 =>
      CursorPath_common.mk_zholes(
        ~holes_before=tag_holes,
        ~hole_selected,
        ~holes_after=body_holes,
        (),
      )
    | 2 =>
      CursorPath_common.mk_zholes(
        ~holes_before=tag_holes @ body_holes,
        ~hole_selected,
        (),
      )
    | _ => CursorPath_common.no_holes
    };
  | CursorP(OnText(_), Parenthesized(_) | Inj(_, _, _) | TypeAnn(_)) =>
    // invalid cursor position
    CursorPath_common.no_holes
  | ParenthesizedZ(zbody) => holes_z(zbody, [0, ...rev_steps])
  | InjZT(err, ztag, None) =>
    let tag_holes = CursorPath_Tag.holes_z(ztag, [0, ...rev_steps]);
    switch (err) {
    | NotInHole => tag_holes
    | InHole(_, u) =>
      let steps = List.rev(rev_steps);
      {
        ...tag_holes,
        holes_before: [
          {sort: ExpHole(u, TypeErr), steps, ap_steps: steps},
          ...tag_holes.holes_before,
        ],
      };
    };
  | InjZT(err, ztag, Some(body)) =>
    let tag_holes = CursorPath_Tag.holes_z(ztag, [0, ...rev_steps]);
    let body_holes = holes(body, [1, ...rev_steps], []);
    let all_holes = {
      ...tag_holes,
      holes_after: tag_holes.holes_after @ body_holes,
    };
    switch (err) {
    | NotInHole => all_holes
    | InHole(_, u) =>
      let steps = List.rev(rev_steps);
      {
        ...all_holes,
        holes_before: [
          {sort: ExpHole(u, TypeErr), steps, ap_steps: steps},
          ...all_holes.holes_before,
        ],
      };
    };
  | InjZP(err, tag, zbody) =>
    let tag_holes = CursorPath_Tag.holes(tag, [0, ...rev_steps], []);
    let zbody_holes = holes_z(zbody, [1, ...rev_steps]);
    switch (err) {
    | NotInHole => {
        ...zbody_holes,
        holes_before: tag_holes @ zbody_holes.holes_before,
      }
    | InHole(_, u) => {
        ...zbody_holes,
        holes_before: [
          mk_hole_sort(PatHole(u, TypeErr), List.rev(rev_steps)),
          ...zbody_holes.holes_before,
        ],
      }
    };
  | TypeAnnZP(err, zop, ann) =>
    let zop_holes = holes_zoperand(zop, [0, ...rev_steps]);
    let ann_holes = CursorPath_Typ.holes(ann, [1, ...rev_steps], []);
    let all_holes = {
      ...zop_holes,
      holes_after: ann_holes @ zop_holes.holes_after,
    };
    switch (err) {
    | NotInHole => all_holes
    | InHole(_, u) => {
        ...all_holes,
        holes_before: [
          mk_hole_sort(PatHole(u, TypeErr), List.rev(rev_steps)),
          ...all_holes.holes_before,
        ],
      }
    };
  | TypeAnnZA(err, op, zann) =>
    let op_holes = holes_operand(op, [0, ...rev_steps], []);
    let zann_holes = CursorPath_Typ.holes_z(zann, [1, ...rev_steps]);
    let all_holes = {
      ...zann_holes,
      holes_before: op_holes @ zann_holes.holes_before,
    };
    switch (err) {
    | NotInHole => all_holes
    | InHole(_, u) => {
        ...all_holes,
        holes_before: [
          mk_hole_sort(PatHole(u, TypeErr), List.rev(rev_steps)),
          ...all_holes.holes_before,
        ],
      }
    };
  };
