let mk_hook = CursorPath.mk_hook;

let rec of_z = (zp: ZPat.t): CursorPath.t => of_zopseq(zp)
and of_zopseq = (zopseq: ZPat.zopseq): CursorPath.t =>
  CursorPath_common.of_zopseq_(~of_zoperand, zopseq)
and of_zoperand =
  fun
  | CursorP(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody)
  | InjZ(_, _, zbody) => CursorPath_common.cons'(0, of_z(zbody))
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
    | Inj(err, side, body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZPat.InjZ(err, side, zbody))
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
    | Inj(_, _, body) =>
      switch (x) {
      | 0 =>
        body
        |> of_steps(xs, ~side)
        |> Option.map(path => CursorPath_common.cons'(0, path))
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

let hook = (shape, u: MetaVar.t): CursorPath.hook => PatHole(u, shape);
let hooks_err = CursorPath_common.hooks_err(~hook=hook(TypeErr));
let hooks_verr = CursorPath_common.hooks_verr(~hook=hook(VarErr));

let rec hooks =
        (
          p: UHPat.t,
          rev_steps: CursorPath.rev_steps,
          hs: CursorPath.hook_list,
        )
        : CursorPath.hook_list =>
  hs
  |> CursorPath_common.hooks_opseq(
       ~hooks_operand,
       ~hook=hook(TypeErr),
       ~is_space=Operators_Pat.is_Space,
       ~rev_steps,
       p,
     )
and hooks_opseq =
    (
      rev_steps: CursorPath.rev_steps,
      opseq: OpSeq.t('operand, 'operator),
      hs: CursorPath.hook_list,
    )
    : CursorPath.hook_list =>
  hs
  |> CursorPath_common.hooks_opseq(
       ~hooks_operand,
       ~hook=hook(TypeErr),
       ~is_space=Operators_Pat.is_Space,
       ~rev_steps,
       opseq,
     )
and hooks_operand =
    (
      operand: UHPat.operand,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hook_list,
    )
    : CursorPath.hook_list =>
  switch (operand) {
  | EmptyHole(u) => [
      mk_hook(PatHole(u, Empty), List.rev(rev_steps)),
      ...hs,
    ]
  | Var(err, verr, _) =>
    hs |> hooks_verr(verr, rev_steps) |> hooks_err(err, rev_steps)
  | Wild(err)
  | IntLit(err, _)
  | FloatLit(err, _)
  | BoolLit(err, _)
  | ListNil(err) => hs |> hooks_err(err, rev_steps)
  | InvalidText(u, _) => [
      mk_hook(ExpHole(u, VarErr), List.rev(rev_steps)),
    ]
  | Parenthesized(body) => hs |> hooks(body, [0, ...rev_steps])
  | Inj(err, _, body) =>
    hs |> hooks_err(err, rev_steps) |> hooks(body, [0, ...rev_steps])
  | TypeAnn(err, op, ann) =>
    hs
    |> CursorPath_Typ.hooks(ann, [1, ...rev_steps])
    |> hooks_operand(op, [0, ...rev_steps])
    |> CursorPath_common.hooks_err(
         ~hook=u => PatHole(u, TypeErr),
         err,
         rev_steps,
       )
  };

let rec hooks_z =
        (zp: ZPat.t, rev_steps: CursorPath.rev_steps): CursorPath.zhook_list =>
  hooks_zopseq(zp, rev_steps)
and hooks_zopseq =
    (zopseq: ZPat.zopseq, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhook_list =>
  CursorPath_common.hooks_zopseq_(
    ~hooks_operand,
    ~hooks_zoperand,
    ~hook=hook(TypeErr),
    ~is_space=Operators_Pat.is_Space,
    ~rev_steps,
    ~erase_zopseq=ZPat.erase_zopseq,
    zopseq,
  )
and hooks_zoperand =
    (zoperand: ZPat.zoperand, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhook_list =>
  switch (zoperand) {
  | CursorP(OnOp(_), _) => CursorPath_common.no_hooks
  | CursorP(_, EmptyHole(u)) =>
    CursorPath_common.mk_zhooks(
      ~hook_selected=Some(mk_hook(PatHole(u, Empty), List.rev(rev_steps))),
      (),
    )
  | CursorP(_, InvalidText(u, _)) =>
    CursorPath_common.mk_zhooks(
      ~hook_selected=
        Some(mk_hook(PatHole(u, VarErr), List.rev(rev_steps))),
      (),
    )
  | CursorP(_, Var(err, verr, _)) =>
    switch (err, verr) {
    | (NotInHole, NotInVarHole) => CursorPath_common.no_hooks
    | (InHole(_, u), _) =>
      CursorPath_common.mk_zhooks(
        ~hook_selected=
          Some(mk_hook(PatHole(u, TypeErr), List.rev(rev_steps))),
        (),
      )
    | (_, InVarHole(_, u)) =>
      CursorPath_common.mk_zhooks(
        ~hook_selected=
          Some(mk_hook(PatHole(u, VarErr), List.rev(rev_steps))),
        (),
      )
    }
  | CursorP(_, Wild(err))
  | CursorP(_, IntLit(err, _))
  | CursorP(_, FloatLit(err, _))
  | CursorP(_, BoolLit(err, _))
  | CursorP(_, ListNil(err)) =>
    switch (err) {
    | NotInHole => CursorPath_common.no_hooks
    | InHole(_, u) =>
      CursorPath_common.mk_zhooks(
        ~hook_selected=
          Some(mk_hook(PatHole(u, TypeErr), List.rev(rev_steps))),
        (),
      )
    }
  | CursorP(OnDelim(k, _), Parenthesized(body)) =>
    let body_hooks = hooks(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zhooks(~hooks_before=body_hooks, ())
    | 1 => CursorPath_common.mk_zhooks(~hooks_after=body_hooks, ())
    | _ => CursorPath_common.no_hooks
    };
  | CursorP(OnDelim(k, _), TypeAnn(_, op, ann)) =>
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zhooks(
        ~hooks_before=hooks_operand(op, [0, ...rev_steps], []),
        ~hooks_after=CursorPath_Typ.hooks(ann, [1, ...rev_steps], []),
        (),
      )
    | _ => CursorPath_common.no_hooks
    }
  | CursorP(OnDelim(k, _), Inj(err, _, body)) =>
    let body_hooks = hooks(body, [0, ...rev_steps], []);
    let hook_selected: option(CursorPath.hook_info) =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) =>
        Some(mk_hook(PatHole(u, TypeErr), List.rev(rev_steps)))
      };
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zhooks(
        ~hooks_before=body_hooks,
        ~hook_selected,
        (),
      )
    | 1 =>
      CursorPath_common.mk_zhooks(~hook_selected, ~hooks_after=body_hooks, ())
    | _ => CursorPath_common.no_hooks
    };
  | CursorP(OnText(_), Parenthesized(_) | Inj(_, _, _) | TypeAnn(_)) =>
    // invalid cursor position
    CursorPath_common.no_hooks
  | ParenthesizedZ(zbody) => hooks_z(zbody, [0, ...rev_steps])
  | InjZ(err, _, zbody) =>
    let zbody_hooks = hooks_z(zbody, [0, ...rev_steps]);
    switch (err) {
    | NotInHole => zbody_hooks
    | InHole(_, u) => {
        ...zbody_hooks,
        hooks_before: [
          mk_hook(PatHole(u, TypeErr), List.rev(rev_steps)),
          ...zbody_hooks.hooks_before,
        ],
      }
    };
  | TypeAnnZP(err, zop, ann) =>
    let zop_hooks = hooks_zoperand(zop, [0, ...rev_steps]);
    let ann_hooks = CursorPath_Typ.hooks(ann, [1, ...rev_steps], []);
    let all_hooks = {
      ...zop_hooks,
      hooks_after: ann_hooks @ zop_hooks.hooks_after,
    };
    switch (err) {
    | NotInHole => all_hooks
    | InHole(_, u) => {
        ...all_hooks,
        hooks_before: [
          mk_hook(PatHole(u, TypeErr), List.rev(rev_steps)),
          ...all_hooks.hooks_before,
        ],
      }
    };
  | TypeAnnZA(err, op, zann) =>
    let op_hooks = hooks_operand(op, [0, ...rev_steps], []);
    let zann_hooks = CursorPath_Typ.hooks_z(zann, [1, ...rev_steps]);
    let all_hooks = {
      ...zann_hooks,
      hooks_before: op_hooks @ zann_hooks.hooks_before,
    };
    switch (err) {
    | NotInHole => all_hooks
    | InHole(_, u) => {
        ...all_hooks,
        hooks_before: [
          mk_hook(PatHole(u, TypeErr), List.rev(rev_steps)),
          ...all_hooks.hooks_before,
        ],
      }
    };
  };
