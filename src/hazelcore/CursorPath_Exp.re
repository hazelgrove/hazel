open CursorPath;

let mk_hook = CursorPath.mk_hook;
let cons' = CursorPath_common.cons';
let rec of_z = (ze: ZExp.t): CursorPath.t => of_zblock(ze)
and of_zblock = (zblock: ZExp.zblock): CursorPath.t => {
  let prefix_len = ZList.prefix_length(zblock);
  let zline = ZList.prj_z(zblock);
  cons'(prefix_len, of_zline(zline));
}
and of_zline = (zline: ZExp.zline): CursorPath.t =>
  switch (zline) {
  | CursorL(cursor, _) => ([], cursor)
  | LetLineZP(zp, _) => cons'(0, CursorPath_Pat.of_z(zp))
  | LetLineZE(_, zdef) => cons'(1, of_z(zdef))
  | ExpLineZ(zopseq) => of_zopseq(zopseq)
  }
and of_zopseq = (zopseq: ZExp.zopseq): CursorPath.t =>
  CursorPath_common.of_zopseq_(~of_zoperand, zopseq)
and of_zoperand = (zoperand: ZExp.zoperand): CursorPath.t =>
  switch (zoperand) {
  | CursorE(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody) => cons'(0, of_z(zbody))
  | LamZP(_, zp, _) => cons'(0, CursorPath_Pat.of_z(zp))
  | LamZE(_, _, zdef) => cons'(1, of_z(zdef))
  | InjZ(_, _, zbody) => cons'(0, of_z(zbody))
  | CaseZE(_, zscrut, _) => cons'(0, of_z(zscrut))
  | CaseZR(_, _, zrules) =>
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons'(prefix_len + 1, of_zrule(zrule));
  | ApPaletteZ(_, _, _, zpsi) =>
    let zhook_map = zpsi.zsplice_map;
    let (n, (_, ze)) = ZIntMap.prj_z_kv(zhook_map);
    cons'(n, of_z(ze));
  }
and of_zoperator = (zoperator: ZExp.zoperator): CursorPath.t => {
  let (cursor, _) = zoperator;
  ([], cursor);
}
and of_zrules = (zrules: ZExp.zrules): CursorPath.t => {
  let prefix_len = List.length(ZList.prj_prefix(zrules));
  let zrule = ZList.prj_z(zrules);
  cons'(prefix_len, of_zrule(zrule));
}
and of_zrule = (zrule: ZExp.zrule): CursorPath.t =>
  switch (zrule) {
  | CursorR(cursor, _) => ([], cursor)
  | RuleZP(zp, _) => cons'(0, CursorPath_Pat.of_z(zp))
  | RuleZE(_, zclause) => cons'(1, of_z(zclause))
  };

let rec follow = (path: CursorPath.t, e: UHExp.t): option(ZExp.t) =>
  follow_block(path, e)
and follow_block =
    ((steps, cursor): CursorPath.t, block: UHExp.block): option(ZExp.zblock) =>
  switch (steps) {
  | [] => None // no block level cursor
  | [x, ...xs] =>
    switch (ZList.split_at(x, block)) {
    | None => None
    | Some(split_lines) =>
      split_lines |> ZList.optmap_z(follow_line((xs, cursor)))
    }
  }
and follow_line =
    ((steps, cursor) as path: CursorPath.t, line: UHExp.line)
    : option(ZExp.zline) =>
  switch (steps, line) {
  | (_, ExpLine(opseq)) =>
    follow_opseq(path, opseq) |> Option.map(zopseq => ZExp.ExpLineZ(zopseq))
  | ([], EmptyLine | LetLine(_) | CommentLine(_)) =>
    line |> ZExp.place_cursor_line(cursor)
  | ([_, ..._], EmptyLine | CommentLine(_)) => None
  | ([x, ...xs], LetLine(p, def)) =>
    switch (x) {
    | 0 =>
      p
      |> CursorPath_Pat.follow((xs, cursor))
      |> Option.map(zp => ZExp.LetLineZP(zp, def))
    | 1 =>
      def
      |> follow((xs, cursor))
      |> Option.map(zdef => ZExp.LetLineZE(p, zdef))
    | _ => None
    }
  }
and follow_opseq =
    (path: CursorPath.t, opseq: UHExp.opseq): option(ZExp.zopseq) =>
  CursorPath_common.follow_opseq_(
    ~follow_operand,
    ~follow_operator,
    path,
    opseq,
  )
and follow_operator =
    ((steps, cursor): CursorPath.t, operator: UHExp.operator)
    : option(ZExp.zoperator) =>
  switch (steps) {
  | [] => operator |> ZExp.place_cursor_operator(cursor)
  | [_, ..._] => None
  }
and follow_operand =
    ((steps, cursor): CursorPath.t, operand: UHExp.operand)
    : option(ZExp.zoperand) =>
  switch (steps) {
  | [] => operand |> ZExp.place_cursor_operand(cursor)
  | [x, ...xs] =>
    switch (operand) {
    | EmptyHole(_)
    | InvalidText(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | Keyword(_)
    | ListNil(_) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZExp.ParenthesizedZ(zbody))
      | _ => None
      }
    | Lam(err, p, body) =>
      switch (x) {
      | 0 =>
        p
        |> CursorPath_Pat.follow((xs, cursor))
        |> Option.map(zp => ZExp.LamZP(err, zp, body))
      | 1 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZExp.LamZE(err, p, zbody))
      | _ => None
      }
    | Inj(err, side, body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZExp.InjZ(err, side, zbody))
      | _ => None
      }
    | Case(err, scrut, rules) =>
      switch (x) {
      | 0 =>
        scrut
        |> follow((xs, cursor))
        |> Option.map(zscrut => ZExp.CaseZE(err, zscrut, rules))
      | _ =>
        switch (ZList.split_at(x - 1, rules)) {
        | None => None
        | Some(split_rules) =>
          split_rules
          |> ZList.optmap_z(follow_rule((xs, cursor)))
          |> Option.map(zrules => ZExp.CaseZR(err, scrut, zrules))
        }
      }
    | ApPalette(err, name, serialized_model, splice_info) =>
      switch (
        ZSpliceInfo.select_opt(splice_info, x, ((ty, e)) =>
          switch (follow((xs, cursor), e)) {
          | None => None
          | Some(ze) => Some((ty, ze))
          }
        )
      ) {
      | None => None
      | Some(zsplice_info) =>
        Some(ApPaletteZ(err, name, serialized_model, zsplice_info))
      }
    }
  }
and follow_rules =
    ((steps, cursor): CursorPath.t, rules: UHExp.rules): option(ZExp.zrules) =>
  switch (steps) {
  | [] => None
  | [x, ...xs] =>
    switch (ZList.split_at(x, rules)) {
    | None => None
    | Some(split_rules) =>
      split_rules |> ZList.optmap_z(follow_rule((xs, cursor)))
    }
  }
and follow_rule =
    ((steps, cursor): CursorPath.t, Rule(p, clause) as rule: UHExp.rule)
    : option(ZExp.zrule) =>
  switch (steps) {
  | [] => rule |> ZExp.place_cursor_rule(cursor)
  | [x, ...xs] =>
    switch (x) {
    | 0 =>
      p
      |> CursorPath_Pat.follow((xs, cursor))
      |> Option.map(zp => ZExp.RuleZP(zp, clause))
    | 1 =>
      clause
      |> follow((xs, cursor))
      |> Option.map(zclause => ZExp.RuleZE(p, zclause))
    | _ => None
    }
  };

let rec of_steps =
        (steps: CursorPath.steps, ~side: Side.t=Before, e: UHExp.t)
        : option(CursorPath.t) =>
  of_steps_block(steps, ~side, e)
and of_steps_block =
    (steps: CursorPath.steps, ~side: Side.t, block: UHExp.block)
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZExp.place_before_block
      | After => ZExp.place_after_block
      };
    Some(of_zblock(place_cursor(block)));
  | [x, ...xs] =>
    switch (ZList.split_at(x, block)) {
    | None => None
    | Some(split_lines) =>
      let (_, z, _) = split_lines;
      z |> of_steps_line(xs, ~side) |> Option.map(path => cons'(x, path));
    }
  }
and of_steps_line =
    (steps: CursorPath.steps, ~side: Side.t, line: UHExp.line)
    : option(CursorPath.t) =>
  switch (steps, line) {
  | (_, ExpLine(opseq)) => of_steps_opseq(steps, ~side, opseq)
  | ([], EmptyLine | LetLine(_) | CommentLine(_)) =>
    let place_cursor =
      switch (side) {
      | Before => ZExp.place_before_line
      | After => ZExp.place_after_line
      };
    Some(of_zline(place_cursor(line)));
  | ([_, ..._], EmptyLine | CommentLine(_)) => None
  | ([x, ...xs], LetLine(p, def)) =>
    switch (x) {
    | 0 =>
      p
      |> CursorPath_Pat.of_steps(xs, ~side)
      |> Option.map(path => cons'(0, path))
    | 1 => def |> of_steps(xs, ~side) |> Option.map(path => cons'(1, path))
    | _ => None
    }
  }
and of_steps_opseq =
    (steps: CursorPath.steps, ~side: Side.t, opseq: UHExp.opseq)
    : option(CursorPath.t) =>
  CursorPath_common.of_steps_opseq_(
    ~of_steps_operand,
    ~of_steps_operator,
    steps,
    ~side,
    opseq,
  )
and of_steps_operator =
    (steps: CursorPath.steps, ~side: Side.t, operator: UHExp.operator)
    : option(CursorPath.t) =>
  switch (steps) {
  | [_, ..._] => None
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZExp.place_before_operator
      | After => ZExp.place_after_operator
      };
    switch (place_cursor(operator)) {
    | Some(zop) => Some(of_zoperator(zop))
    | _ => None
    };
  }
and of_steps_operand =
    (steps: CursorPath.steps, ~side: Side.t, operand: UHExp.operand)
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZExp.place_before_operand
      | After => ZExp.place_after_operand
      };
    Some(of_zoperand(place_cursor(operand)));
  | [x, ...xs] =>
    switch (operand) {
    | EmptyHole(_)
    | InvalidText(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | Keyword(_)
    | ListNil(_) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body |> of_steps(xs, ~side) |> Option.map(path => cons'(0, path))
      | _ => None
      }
    | Lam(_, p, body) =>
      switch (x) {
      | 0 =>
        p
        |> CursorPath_Pat.of_steps(xs, ~side)
        |> Option.map(path => cons'(0, path))
      | 1 =>
        body |> of_steps(xs, ~side) |> Option.map(path => cons'(1, path))
      | _ => None
      }
    | Inj(_, _, body) =>
      switch (x) {
      | 0 =>
        body |> of_steps(xs, ~side) |> Option.map(path => cons'(2, path))
      | _ => None
      }
    | Case(_, scrut, rules) =>
      switch (x) {
      | 0 =>
        scrut |> of_steps(~side, xs) |> Option.map(path => cons'(0, path))
      | _ =>
        switch (ZList.split_at(x - 1, rules)) {
        | None => None
        | Some(split_rules) =>
          let (_, z, _) = split_rules;
          z |> of_steps_rule(xs, ~side) |> Option.map(path => cons'(x, path));
        }
      }
    | ApPalette(_, _, _, splice_info) =>
      let splice_map = splice_info.splice_map;
      switch (IntMap.find_opt(x, splice_map)) {
      | None => None
      | Some((_, e)) =>
        e |> of_steps(xs, ~side) |> Option.map(path => cons'(x, path))
      };
    }
  }
and of_steps_rule =
    (steps: CursorPath.steps, ~side: Side.t, rule: UHExp.rule)
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZExp.place_before_rule
      | After => ZExp.place_after_rule
      };
    Some(of_zrule(place_cursor(rule)));
  | [x, ...xs] =>
    let Rule(p, clause) = rule;
    switch (x) {
    | 0 =>
      p
      |> CursorPath_Pat.of_steps(~side, xs)
      |> Option.map(path => cons'(0, path))
    | 1 =>
      clause |> of_steps(~side, xs) |> Option.map(path => cons'(1, path))
    | _ => None
    };
  };

let hook = (shape, u: MetaVar.t): CursorPath.hook => ExpHole(u, shape);
let hooks_err = CursorPath_common.hooks_err(~hook=hook(TypeErr));
let hooks_case_err = CursorPath_common.hooks_case_err(~hook=hook(TypeErr));
let hooks_verr = CursorPath_common.hooks_verr(~hook=hook(VarErr));

let rec hooks =
        (
          e: UHExp.t,
          rev_steps: CursorPath.rev_steps,
          hs: CursorPath.hook_list,
        )
        : CursorPath.hook_list =>
  hs |> hooks_block(e, rev_steps)
and hooks_block =
    (
      block: UHExp.block,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hook_list,
    )
    : CursorPath.hook_list =>
  hs
  |> ListUtil.fold_right_i(
       ((i, line), hs) => hs |> hooks_line(line, [i, ...rev_steps]),
       block,
     )
and hooks_line =
    (
      line: UHExp.line,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hook_list,
    )
    : CursorPath.hook_list =>
  switch (line) {
  | EmptyLine
  | CommentLine(_) => hs
  | LetLine(p, def) =>
    hs
    |> hooks(def, [1, ...rev_steps])
    |> CursorPath_Pat.hooks(p, [0, ...rev_steps])
  | ExpLine(opseq) =>
    hs
    |> CursorPath_common.hooks_opseq(
         ~hooks_operand,
         ~hook=hook(TypeErr),
         ~is_space=Operators_Exp.is_Space,
         ~rev_steps,
         opseq,
       )
  }
and hooks_operand =
    (
      operand: UHExp.operand,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hook_list,
    )
    : CursorPath.hook_list =>
  switch (operand) {
  | EmptyHole(u) => [
      mk_hook(ExpHole(u, Empty), List.rev(rev_steps)),
      ...hs,
    ]
  | InvalidText(u, _) => [
      mk_hook(ExpHole(u, VarErr), List.rev(rev_steps)),
      ...hs,
    ]
  | Var(err, verr, _) =>
    hs |> hooks_verr(verr, rev_steps) |> hooks_err(err, rev_steps)
  | Keyword(Typed(_, InHole(TypeInconsistent, u), id)) => [
      CursorPath.mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps)),
      CursorPath.mk_hook(KeywordHook(id), List.rev(rev_steps)),
      ...hs,
    ]
  | Keyword(Typed(_, _, id)) => [
      CursorPath.mk_hook(KeywordHook(id), List.rev(rev_steps)),
      ...hs,
    ]
  | IntLit(err, _)
  | FloatLit(err, _)
  | BoolLit(err, _)
  | ListNil(err) => hs |> hooks_err(err, rev_steps)
  | Parenthesized(body) => hs |> hooks(body, [0, ...rev_steps])
  | Inj(err, _, body) =>
    hs |> hooks(body, [0, ...rev_steps]) |> hooks_err(err, rev_steps)
  | Lam(err, p, body) =>
    hs
    |> hooks(body, [1, ...rev_steps])
    |> CursorPath_Pat.hooks(p, [0, ...rev_steps])
    |> hooks_err(err, rev_steps)
  | Case(err, scrut, rules) =>
    hs
    |> ListUtil.fold_right_i(
         ((i, rule), hs) => hs |> hooks_rule(rule, [1 + i, ...rev_steps]),
         rules,
       )
    |> hooks(scrut, [0, ...rev_steps])
    |> hooks_case_err(err, rev_steps)
  | ApPalette(err, _, _, psi) =>
    let splice_map = psi.splice_map;
    let splice_order = psi.splice_order;
    List.fold_right(
      (i, hs) =>
        switch (IntMap.find_opt(i, splice_map)) {
        | None => hs
        | Some((_, e)) => hs |> hooks(e, [i, ...rev_steps])
        },
      splice_order,
      hs,
    )
    |> hooks_err(err, rev_steps);
  }
and hooks_rule =
    (
      Rule(p, clause): UHExp.rule,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hook_list,
    )
    : CursorPath.hook_list => {
  hs
  |> hooks(clause, [1, ...rev_steps])
  |> CursorPath_Pat.hooks(p, [0, ...rev_steps]);
};

let rec hooks_z =
        (ze: ZExp.t, rev_steps: CursorPath.rev_steps): CursorPath.zhook_list =>
  hooks_zblock(ze, rev_steps)
and hooks_zblock =
    ((prefix, zline, suffix): ZExp.zblock, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhook_list => {
  let hooks_prefix =
    ListUtil.fold_right_i(
      ((i, line), hs) => hs |> hooks_line(line, [i, ...rev_steps]),
      prefix,
      [],
    );
  let CursorPath.{hooks_before, hook_selected, hooks_after} =
    hooks_zline(zline, [List.length(prefix), ...rev_steps]);
  let hooks_suffix =
    ListUtil.fold_right_i(
      ((i, line), hs) =>
        hs |> hooks_line(line, [List.length(prefix) + 1 + i, ...rev_steps]),
      suffix,
      [],
    );
  CursorPath_common.mk_zhooks(
    ~hooks_before=hooks_prefix @ hooks_before,
    ~hook_selected,
    ~hooks_after=hooks_after @ hooks_suffix,
    (),
  );
}
and hooks_zline =
    (zline: ZExp.zline, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhook_list =>
  switch (zline) {
  | CursorL(OnOp(_), _) => CursorPath_common.no_hooks
  | CursorL(_, EmptyLine) => CursorPath_common.no_hooks
  | CursorL(_, CommentLine(_)) => CursorPath_common.no_hooks
  | CursorL(_, ExpLine(_)) => CursorPath_common.no_hooks /* invalid cursor position */
  | CursorL(cursor, LetLine(p, def)) =>
    let hooks_p = CursorPath_Pat.hooks(p, [0, ...rev_steps], []);
    let hooks_def = hooks(def, [1, ...rev_steps], []);
    switch (cursor) {
    | OnDelim(0, _) =>
      CursorPath_common.mk_zhooks(~hooks_after=hooks_p @ hooks_def, ())
    | OnDelim(1, _) =>
      CursorPath_common.mk_zhooks(
        ~hooks_before=hooks_p,
        ~hooks_after=hooks_def,
        (),
      )
    | OnDelim(2, _) =>
      CursorPath_common.mk_zhooks(~hooks_before=hooks_p @ hooks_def, ())
    | _ => CursorPath_common.no_hooks
    };
  | ExpLineZ(zopseq) => hooks_zopseq(zopseq, rev_steps)
  | LetLineZP(zp, body) =>
    let CursorPath.{hooks_before, hook_selected, hooks_after} =
      CursorPath_Pat.hooks_z(zp, [0, ...rev_steps]);
    let hooks_body = hooks(body, [1, ...rev_steps], []);
    CursorPath_common.mk_zhooks(
      ~hooks_before,
      ~hook_selected,
      ~hooks_after=hooks_after @ hooks_body,
      (),
    );
  | LetLineZE(p, zbody) =>
    let hooks_p = CursorPath_Pat.hooks(p, [0, ...rev_steps], []);
    let CursorPath.{hooks_before, hook_selected, hooks_after} =
      hooks_z(zbody, [1, ...rev_steps]);
    CursorPath_common.mk_zhooks(
      ~hooks_before=hooks_p @ hooks_before,
      ~hook_selected,
      ~hooks_after,
      (),
    );
  }
and hooks_zopseq =
    (zopseq: ZExp.zopseq, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhook_list =>
  CursorPath_common.hooks_zopseq_(
    ~hooks_operand,
    ~hooks_zoperand,
    ~hook=hook(TypeErr),
    ~is_space=Operators_Exp.is_Space,
    ~rev_steps,
    ~erase_zopseq=ZExp.erase_zopseq,
    zopseq,
  )
and hooks_zoperand =
    (zoperand: ZExp.zoperand, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhook_list =>
  switch (zoperand) {
  | CursorE(OnOp(_), _) => CursorPath_common.no_hooks
  | CursorE(_, EmptyHole(u)) =>
    CursorPath_common.mk_zhooks(
      ~hook_selected=Some(mk_hook(ExpHole(u, Empty), List.rev(rev_steps))),
      (),
    )
  | CursorE(_, InvalidText(u, _)) =>
    CursorPath_common.mk_zhooks(
      ~hook_selected=
        Some(mk_hook(ExpHole(u, VarErr), List.rev(rev_steps))),
      (),
    )
  | CursorE(_, Var(err, verr, _)) =>
    switch (err, verr) {
    | (NotInHole, NotInVarHole) => CursorPath_common.no_hooks
    | (InHole(_, u), _) =>
      CursorPath_common.mk_zhooks(
        ~hook_selected=
          Some(mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps))),
        (),
      )
    | (_, InVarHole(_, u)) =>
      CursorPath_common.mk_zhooks(
        ~hook_selected=
          Some(mk_hook(ExpHole(u, VarErr), List.rev(rev_steps))),
        (),
      )
    }
  | CursorE(_, Keyword(Typed(_, err, id))) =>
    switch (err) {
    | InHole(TypeInconsistent, u) =>
      CursorPath_common.mk_zhooks(
        ~hooks_before=[
          CursorPath.mk_hook(KeywordHook(id), List.rev(rev_steps)),
        ],
        ~hook_selected=
          Some(
            CursorPath.mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps)),
          ),
        (),
      )
    | _ =>
      CursorPath_common.mk_zhooks(
        ~hook_selected=
          Some(CursorPath.mk_hook(KeywordHook(id), List.rev(rev_steps))),
        (),
      )
    }
  | CursorE(_, IntLit(err, _))
  | CursorE(_, FloatLit(err, _))
  | CursorE(_, BoolLit(err, _))
  | CursorE(_, ListNil(err)) =>
    switch (err) {
    | NotInHole => CursorPath_common.no_hooks
    | InHole(_, u) =>
      CursorPath_common.mk_zhooks(
        ~hook_selected=
          Some(mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps))),
        (),
      )
    }
  | CursorE(OnDelim(k, _), Parenthesized(body)) =>
    let body_hooks = hooks(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zhooks(~hooks_after=body_hooks, ())
    | 1 => CursorPath_common.mk_zhooks(~hooks_before=body_hooks, ())
    | _ => CursorPath_common.no_hooks
    };
  | CursorE(OnDelim(k, _), Inj(err, _, body)) =>
    let hook_selected: option(CursorPath.hook_info) =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) =>
        Some(mk_hook(CursorPath.ExpHole(u, TypeErr), List.rev(rev_steps)))
      };
    let body_hooks = hooks(body, [0, ...rev_steps], []);
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
  | CursorE(OnDelim(k, _), Lam(err, p, body)) =>
    let hook_selected: option(CursorPath.hook_info) =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) =>
        Some(mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps)))
      };
    let hooks_p = CursorPath_Pat.hooks(p, [0, ...rev_steps], []);
    let hooks_body = hooks(body, [1, ...rev_steps], []);
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zhooks(
        ~hook_selected,
        ~hooks_after=hooks_p @ hooks_body,
        (),
      )
    | 1 =>
      CursorPath_common.mk_zhooks(
        ~hooks_before=hooks_p,
        ~hook_selected,
        ~hooks_after=hooks_body,
        (),
      )
    | _ => CursorPath_common.no_hooks
    };
  | CursorE(OnDelim(k, _), Case(err, scrut, rules)) =>
    let hook_selected: option(CursorPath.hook_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => None
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) =>
        Some(mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps)))
      };
    let hooks_scrut = hooks(scrut, [0, ...rev_steps], []);
    let hooks_rules =
      ListUtil.fold_right_i(
        ((i, rule), hs) => hs |> hooks_rule(rule, [1 + i, ...rev_steps]),
        rules,
        [],
      );
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zhooks(
        ~hooks_after=hooks_scrut @ hooks_rules,
        ~hook_selected,
        (),
      )
    | 1 =>
      CursorPath_common.mk_zhooks(
        ~hooks_before=hooks_scrut @ hooks_rules,
        ~hook_selected,
        ~hooks_after=[],
        (),
      )
    | _ => CursorPath_common.no_hooks
    };
  | CursorE(OnText(_), Inj(_) | Parenthesized(_) | Lam(_) | Case(_)) =>
    /* invalid cursor position */
    CursorPath_common.no_hooks
  | CursorE(_, ApPalette(_)) => CursorPath_common.no_hooks /* TODO[livelits] */
  | ParenthesizedZ(zbody) => hooks_z(zbody, [0, ...rev_steps])
  | LamZP(err, zp, body) =>
    let hooks_err: list(CursorPath.hook_info) =
      switch (err) {
      | NotInHole => []
      | InHole(_, u) => [
          mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps)),
        ]
      };
    let CursorPath.{hooks_before, hook_selected, hooks_after} =
      CursorPath_Pat.hooks_z(zp, [0, ...rev_steps]);
    let hooks_body = hooks(body, [1, ...rev_steps], []);
    CursorPath_common.mk_zhooks(
      ~hooks_before=hooks_err @ hooks_before,
      ~hook_selected,
      ~hooks_after=hooks_after @ hooks_body,
      (),
    );
  | LamZE(err, p, zbody) =>
    let hooks_err: list(CursorPath.hook_info) =
      switch (err) {
      | NotInHole => []
      | InHole(_, u) => [
          mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps)),
        ]
      };
    let hooks_p = CursorPath_Pat.hooks(p, [0, ...rev_steps], []);
    let CursorPath.{hooks_before, hook_selected, hooks_after} =
      hooks_z(zbody, [1, ...rev_steps]);
    CursorPath_common.mk_zhooks(
      ~hooks_before=hooks_err @ hooks_p @ hooks_before,
      ~hook_selected,
      ~hooks_after,
      (),
    );
  | InjZ(err, _, zbody) =>
    let hooks_err: list(CursorPath.hook_info) =
      switch (err) {
      | NotInHole => []
      | InHole(_, u) => [
          mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps)),
        ]
      };
    let CursorPath.{hooks_before, hook_selected, hooks_after} =
      hooks_z(zbody, [0, ...rev_steps]);
    CursorPath_common.mk_zhooks(
      ~hooks_before=hooks_err @ hooks_before,
      ~hook_selected,
      ~hooks_after,
      (),
    );
  | CaseZE(err, zscrut, rules) =>
    let hooks_err: list(CursorPath.hook_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => []
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) => [
          mk_hook(CursorPath.ExpHole(u, TypeErr), List.rev(rev_steps)),
        ]
      };
    let CursorPath.{hooks_before, hook_selected, hooks_after} =
      hooks_z(zscrut, [0, ...rev_steps]);
    let hooks_rules =
      ListUtil.fold_right_i(
        ((i, rule), hs) => hs |> hooks_rule(rule, [1 + i, ...rev_steps]),
        rules,
        [],
      );
    CursorPath_common.mk_zhooks(
      ~hooks_before=hooks_err @ hooks_before,
      ~hook_selected,
      ~hooks_after=hooks_after @ hooks_rules,
      (),
    );
  | CaseZR(err, scrut, (prefix, zrule, suffix)) =>
    let hooks_err: list(CursorPath.hook_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => []
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) => [
          mk_hook(ExpHole(u, TypeErr), List.rev(rev_steps)),
        ]
      };
    let hooks_scrut = hooks(scrut, [0, ...rev_steps], []);
    let hooks_prefix =
      ListUtil.fold_right_i(
        ((i, rule), hs) => hs |> hooks_rule(rule, [1 + i, ...rev_steps]),
        prefix,
        [],
      );
    let CursorPath.{hooks_before, hook_selected, hooks_after} =
      hooks_zrule(zrule, [1 + List.length(prefix), ...rev_steps]);
    let hooks_suffix =
      ListUtil.fold_right_i(
        ((i, rule), hs) =>
          hs
          |> hooks_rule(
               rule,
               [1 + List.length(prefix) + 1 + i, ...rev_steps],
             ),
        suffix,
        [],
      );
    {
      hooks_before: hooks_err @ hooks_scrut @ hooks_prefix @ hooks_before,
      hook_selected,
      hooks_after: hooks_after @ hooks_suffix,
    };
  | ApPaletteZ(_, _, _, zpsi) =>
    let zsplice_map = zpsi.zsplice_map;
    let (n, (_, ze)) = ZIntMap.prj_z_kv(zsplice_map);
    let CursorPath.{hooks_before, hook_selected, hooks_after} =
      hooks_z(ze, [n, ...rev_steps]);
    let splice_order = zpsi.splice_order;
    let splice_map = ZIntMap.prj_map(zsplice_map);
    let (splices_before, splices_after) = ListUtil.split_at(splice_order, n);
    let hooks_splices_before =
      List.fold_left(
        (hs, n) =>
          switch (IntMap.find_opt(n, splice_map)) {
          | None => hs
          | Some((_, e)) => hs @ hooks(e, [n, ...rev_steps], [])
          },
        [],
        splices_before,
      );
    let hooks_splices_after =
      List.fold_left(
        (hs, n) =>
          switch (IntMap.find_opt(n, splice_map)) {
          | None => hs
          | Some((_, e)) => hs @ hooks(e, [n, ...rev_steps], [])
          },
        [],
        splices_after,
      );
    {
      hooks_before: hooks_splices_before @ hooks_before,
      hook_selected,
      hooks_after: hooks_after @ hooks_splices_after,
    };
  }
and hooks_zrule = (zrule: ZExp.zrule, rev_steps: CursorPath.rev_steps) =>
  switch (zrule) {
  | CursorR(OnOp(_) | OnText(_), _) =>
    // invalid cursor position
    CursorPath_common.no_hooks
  | CursorR(OnDelim(k, _), Rule(p, clause)) =>
    let hooks_p = CursorPath_Pat.hooks(p, [0, ...rev_steps], []);
    let hooks_clause = hooks(clause, [1, ...rev_steps], []);
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zhooks(~hooks_after=hooks_p @ hooks_clause, ())
    | 1 =>
      CursorPath_common.mk_zhooks(
        ~hooks_before=hooks_p,
        ~hooks_after=hooks_clause,
        (),
      )
    | _ => CursorPath_common.no_hooks
    };
  | RuleZP(zp, clause) =>
    let zhooks_p = CursorPath_Pat.hooks_z(zp, [0, ...rev_steps]);
    let hooks_clause = hooks(clause, [1, ...rev_steps], []);
    {...zhooks_p, hooks_after: zhooks_p.hooks_after @ hooks_clause};
  | RuleZE(p, zclause) =>
    let hooks_p = CursorPath_Pat.hooks(p, [0, ...rev_steps], []);
    let zhooks_clause = hooks_z(zclause, [1, ...rev_steps]);
    {...zhooks_clause, hooks_before: hooks_p @ zhooks_clause.hooks_before};
  };

let prev_hole_steps_z = (ze: ZExp.t): option(CursorPath.steps) =>
  hooks_z(ze, []) |> filter_holes_z |> CursorPath_common.prev_hook_steps;

let prev_hole_steps_zline = (zline: ZExp.zline): option(CursorPath.steps) =>
  hooks_zline(zline, [])
  |> filter_holes_z
  |> CursorPath_common.prev_hook_steps;

let next_hole_steps_z = (ze: ZExp.t): option(CursorPath.steps) => {
  hooks_z(ze, []) |> filter_holes_z |> CursorPath_common.next_hook_steps;
};
let next_hole_steps_zline = (zline: ZExp.zline): option(CursorPath.steps) =>
  hooks_zline(zline, [])
  |> filter_holes_z
  |> CursorPath_common.next_hook_steps;

let holes =
    (e: UHExp.t, rev_steps: CursorPath.rev_steps, hs: CursorPath.hook_list)
    : CursorPath.hook_list =>
  hooks(e, rev_steps, hs) |> List.filter(CursorPath.is_hole);
