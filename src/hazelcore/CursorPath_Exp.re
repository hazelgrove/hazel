let cons' = CursorPath_common.cons';
let rec of_z = (ze: ZExp.t): CursorPath_common.t => of_zblock(ze)
and of_zblock = (zblock: ZExp.zblock): CursorPath_common.t => {
  let prefix_len = ZList.prefix_length(zblock);
  let zline = ZList.prj_z(zblock);
  cons'(prefix_len, of_zline(zline));
}
and of_zline = (zline: ZExp.zline): CursorPath_common.t =>
  switch (zline) {
  | CursorL(cursor, _) => ([], cursor)
  | LetLineZP(zp, _, _) => cons'(0, CursorPath_Pat.of_z(zp))
  | LetLineZA(_, zann, _) => cons'(1, CursorPath_Typ.of_z(zann))
  | LetLineZE(_, _, zdef) => cons'(2, of_z(zdef))
  | ExpLineZ(zopseq) => of_zopseq(zopseq)
  }
and of_zopseq = (zopseq: ZExp.zopseq): CursorPath_common.t =>
  CursorPath_common.of_zopseq_(~of_zoperand, zopseq)
and of_zoperand = (zoperand: ZExp.zoperand): CursorPath_common.t =>
  switch (zoperand) {
  | CursorE(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody) => cons'(0, of_z(zbody))
  | LamZP(_, zp, _, _) => cons'(0, CursorPath_Pat.of_z(zp))
  | LamZA(_, _, zann, _) => cons'(1, CursorPath_Typ.of_z(zann))
  | LamZE(_, _, _, zdef) => cons'(2, of_z(zdef))
  | InjZ(_, _, zbody) => cons'(0, of_z(zbody))
  | CaseZE(_, zscrut, _) => cons'(0, of_z(zscrut))
  | CaseZR(_, _, zrules) =>
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons'(prefix_len + 1, of_zrule(zrule));
  | IfZ1(_, t1, _, _) => cons'(0, of_z(t1))
  | IfZ2(_, _, t2, _) => cons'(1, of_z(t2))
  | IfZ3(_, _, _, t3) => cons'(2, of_z(t3))
  | ApPaletteZ(_, _, _, zpsi) =>
    let zhole_map = zpsi.zsplice_map;
    let (n, (_, ze)) = ZIntMap.prj_z_kv(zhole_map);
    cons'(n, of_z(ze));
  }
and of_zoperator = (zoperator: ZExp.zoperator): CursorPath_common.t => {
  let (cursor, _) = zoperator;
  ([], cursor);
}
and of_zrules = (zrules: ZExp.zrules): CursorPath_common.t => {
  let prefix_len = List.length(ZList.prj_prefix(zrules));
  let zrule = ZList.prj_z(zrules);
  cons'(prefix_len, of_zrule(zrule));
}
and of_zrule = (zrule: ZExp.zrule): CursorPath_common.t =>
  switch (zrule) {
  | CursorR(cursor, _) => ([], cursor)
  | RuleZP(zp, _) => cons'(0, CursorPath_Pat.of_z(zp))
  | RuleZE(_, zclause) => cons'(1, of_z(zclause))
  };

let rec follow = (path: CursorPath_common.t, e: UHExp.t): option(ZExp.t) =>
  follow_block(path, e)
and follow_block =
    ((steps, cursor): CursorPath_common.t, block: UHExp.block)
    : option(ZExp.zblock) =>
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
    ((steps, cursor) as path: CursorPath_common.t, line: UHExp.line)
    : option(ZExp.zline) =>
  switch (steps, line) {
  | (_, ExpLine(opseq)) =>
    follow_opseq(path, opseq) |> Option.map(zopseq => ZExp.ExpLineZ(zopseq))
  | ([], EmptyLine | LetLine(_, _, _) | CommentLine(_)) =>
    line |> ZExp.place_cursor_line(cursor)
  | ([_, ..._], EmptyLine | CommentLine(_)) => None
  | ([x, ...xs], LetLine(p, ann, def)) =>
    switch (x) {
    | 0 =>
      p
      |> CursorPath_Pat.follow((xs, cursor))
      |> Option.map(zp => ZExp.LetLineZP(zp, ann, def))
    | 1 =>
      switch (ann) {
      | None => None
      | Some(ann) =>
        ann
        |> CursorPath_Typ.follow((xs, cursor))
        |> Option.map(zann => ZExp.LetLineZA(p, zann, def))
      }
    | 2 =>
      def
      |> follow((xs, cursor))
      |> Option.map(zdef => ZExp.LetLineZE(p, ann, zdef))
    | _ => None
    }
  }
and follow_opseq =
    (path: CursorPath_common.t, opseq: UHExp.opseq): option(ZExp.zopseq) =>
  CursorPath_common.follow_opseq_(
    ~follow_operand,
    ~follow_operator,
    path,
    opseq,
  )
and follow_operator =
    ((steps, cursor): CursorPath_common.t, operator: UHExp.operator)
    : option(ZExp.zoperator) =>
  switch (steps) {
  | [] => operator |> ZExp.place_cursor_operator(cursor)
  | [_, ..._] => None
  }
and follow_operand =
    ((steps, cursor): CursorPath_common.t, operand: UHExp.operand)
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
    | ListNil(_) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZExp.ParenthesizedZ(zbody))
      | _ => None
      }
    | Lam(err, p, ann, body) =>
      switch (x) {
      | 0 =>
        p
        |> CursorPath_Pat.follow((xs, cursor))
        |> Option.map(zp => ZExp.LamZP(err, zp, ann, body))
      | 1 =>
        switch (ann) {
        | None => None
        | Some(ann) =>
          ann
          |> CursorPath_Typ.follow((xs, cursor))
          |> Option.map(zann => ZExp.LamZA(err, p, zann, body))
        }
      | 2 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZExp.LamZE(err, p, ann, zbody))
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
    | If(err, t1, t2, t3) =>
      switch (x) {
      | 0 =>
        t1
        |> follow((xs, cursor))
        |> Option.map(zt1 => ZExp.IfZ1(err, zt1, t2, t3))
      | 1 =>
        t2
        |> follow((xs, cursor))
        |> Option.map(zt2 => ZExp.IfZ2(err, t1, zt2, t3))
      | 2 =>
        t3
        |> follow((xs, cursor))
        |> Option.map(zt3 => ZExp.IfZ3(err, t1, t2, zt3))
      | _ => None
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
    ((steps, cursor): CursorPath_common.t, rules: UHExp.rules)
    : option(ZExp.zrules) =>
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
    (
      (steps, cursor): CursorPath_common.t,
      Rule(p, clause) as rule: UHExp.rule,
    )
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
        (steps: CursorPath_common.steps, ~side: Side.t=Before, e: UHExp.t)
        : option(CursorPath_common.t) =>
  of_steps_block(steps, ~side, e)
and of_steps_block =
    (steps: CursorPath_common.steps, ~side: Side.t, block: UHExp.block)
    : option(CursorPath_common.t) =>
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
    (steps: CursorPath_common.steps, ~side: Side.t, line: UHExp.line)
    : option(CursorPath_common.t) =>
  switch (steps, line) {
  | (_, ExpLine(opseq)) => of_steps_opseq(steps, ~side, opseq)
  | ([], EmptyLine | LetLine(_, _, _) | CommentLine(_)) =>
    let place_cursor =
      switch (side) {
      | Before => ZExp.place_before_line
      | After => ZExp.place_after_line
      };
    Some(of_zline(place_cursor(line)));
  | ([_, ..._], EmptyLine | CommentLine(_)) => None
  | ([x, ...xs], LetLine(p, ann, def)) =>
    switch (x) {
    | 0 =>
      p
      |> CursorPath_Pat.of_steps(xs, ~side)
      |> Option.map(path => cons'(0, path))
    | 1 =>
      switch (ann) {
      | None => None
      | Some(ann) =>
        ann
        |> CursorPath_Typ.of_steps(xs, ~side)
        |> Option.map(path => cons'(1, path))
      }
    | 2 => def |> of_steps(xs, ~side) |> Option.map(path => cons'(2, path))
    | _ => None
    }
  }
and of_steps_opseq =
    (steps: CursorPath_common.steps, ~side: Side.t, opseq: UHExp.opseq)
    : option(CursorPath_common.t) =>
  CursorPath_common.of_steps_opseq_(
    ~of_steps_operand,
    ~of_steps_operator,
    steps,
    ~side,
    opseq,
  )
and of_steps_operator =
    (steps: CursorPath_common.steps, ~side: Side.t, operator: UHExp.operator)
    : option(CursorPath_common.t) =>
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
    (steps: CursorPath_common.steps, ~side: Side.t, operand: UHExp.operand)
    : option(CursorPath_common.t) =>
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
    | ListNil(_) => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body |> of_steps(xs, ~side) |> Option.map(path => cons'(0, path))
      | _ => None
      }
    | Lam(_, p, ann, body) =>
      switch (x) {
      | 0 =>
        p
        |> CursorPath_Pat.of_steps(xs, ~side)
        |> Option.map(path => cons'(0, path))
      | 1 =>
        switch (ann) {
        | None => None
        | Some(ann) =>
          ann
          |> CursorPath_Typ.of_steps(xs, ~side)
          |> Option.map(path => cons'(1, path))
        }
      | 2 =>
        body |> of_steps(xs, ~side) |> Option.map(path => cons'(2, path))
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
    | If(_, t1, t2, t3) =>
      switch (x) {
      | 0 => t1 |> of_steps(~side, xs) |> Option.map(path => cons'(0, path))
      | 1 => t2 |> of_steps(~side, xs) |> Option.map(path => cons'(1, path))
      | 2 => t3 |> of_steps(~side, xs) |> Option.map(path => cons'(2, path))
      | _ => None
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
    (steps: CursorPath_common.steps, ~side: Side.t, rule: UHExp.rule)
    : option(CursorPath_common.t) =>
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

let hole_sort = (shape, u: MetaVar.t): CursorPath_common.hole_sort =>
  ExpHole(u, shape);
let holes_err = CursorPath_common.holes_err(~hole_sort=hole_sort(TypeErr));
let holes_case_err =
  CursorPath_common.holes_case_err(~hole_sort=hole_sort(TypeErr));
let holes_verr = CursorPath_common.holes_verr(~hole_sort=hole_sort(VarErr));

let rec holes =
        (
          e: UHExp.t,
          rev_steps: CursorPath_common.rev_steps,
          hs: CursorPath_common.hole_list,
        )
        : CursorPath_common.hole_list =>
  hs |> holes_block(e, rev_steps)
and holes_block =
    (
      block: UHExp.block,
      rev_steps: CursorPath_common.rev_steps,
      hs: CursorPath_common.hole_list,
    )
    : CursorPath_common.hole_list =>
  hs
  |> ListUtil.fold_right_i(
       ((i, line), hs) => hs |> holes_line(line, [i, ...rev_steps]),
       block,
     )
and holes_line =
    (
      line: UHExp.line,
      rev_steps: CursorPath_common.rev_steps,
      hs: CursorPath_common.hole_list,
    )
    : CursorPath_common.hole_list =>
  switch (line) {
  | EmptyLine
  | CommentLine(_) => hs
  | LetLine(p, ann, def) =>
    hs
    |> holes(def, [2, ...rev_steps])
    |> (
      switch (ann) {
      | None => (hs => hs)
      | Some(ann) => CursorPath_Typ.holes(ann, [1, ...rev_steps])
      }
    )
    |> CursorPath_Pat.holes(p, [0, ...rev_steps])
  | ExpLine(opseq) =>
    hs
    |> CursorPath_common.holes_opseq(
         ~holes_operand,
         ~hole_sort=hole_sort(TypeErr),
         ~is_space=Operators_Exp.is_Space,
         ~rev_steps,
         opseq,
       )
  }
and holes_operand =
    (
      operand: UHExp.operand,
      rev_steps: CursorPath_common.rev_steps,
      hs: CursorPath_common.hole_list,
    )
    : CursorPath_common.hole_list =>
  switch (operand) {
  | EmptyHole(u) => [
      {sort: ExpHole(u, Empty), steps: List.rev(rev_steps)},
      ...hs,
    ]
  | InvalidText(u, _) => [
      {sort: ExpHole(u, VarErr), steps: List.rev(rev_steps)},
      ...hs,
    ]
  | Var(err, verr, _) =>
    hs |> holes_verr(verr, rev_steps) |> holes_err(err, rev_steps)
  | IntLit(err, _)
  | FloatLit(err, _)
  | BoolLit(err, _)
  | ListNil(err) => hs |> holes_err(err, rev_steps)
  | Parenthesized(body) => hs |> holes(body, [0, ...rev_steps])
  | Inj(err, _, body) =>
    hs |> holes(body, [0, ...rev_steps]) |> holes_err(err, rev_steps)
  | Lam(err, p, ann, body) =>
    hs
    |> holes(body, [2, ...rev_steps])
    |> (
      switch (ann) {
      | None => (hs => hs)
      | Some(ann) => CursorPath_Typ.holes(ann, [1, ...rev_steps])
      }
    )
    |> CursorPath_Pat.holes(p, [0, ...rev_steps])
    |> holes_err(err, rev_steps)
  | Case(err, scrut, rules) =>
    hs
    |> ListUtil.fold_right_i(
         ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
         rules,
       )
    |> holes(scrut, [0, ...rev_steps])
    |> holes_case_err(err, rev_steps)
  | If(err, t1, t2, t3) =>
    hs
    |> holes(t3, [2, ...rev_steps])
    |> holes(t2, [1, ...rev_steps])
    |> holes(t1, [0, ...rev_steps])
    |> holes_case_err(err, rev_steps)
  | ApPalette(err, _, _, psi) =>
    let splice_map = psi.splice_map;
    let splice_order = psi.splice_order;
    List.fold_right(
      (i, hs) =>
        switch (IntMap.find_opt(i, splice_map)) {
        | None => hs
        | Some((_, e)) => hs |> holes(e, [i, ...rev_steps])
        },
      splice_order,
      hs,
    )
    |> holes_err(err, rev_steps);
  }
and holes_rule =
    (
      Rule(p, clause): UHExp.rule,
      rev_steps: CursorPath_common.rev_steps,
      hs: CursorPath_common.hole_list,
    )
    : CursorPath_common.hole_list => {
  hs
  |> holes(clause, [1, ...rev_steps])
  |> CursorPath_Pat.holes(p, [0, ...rev_steps]);
};

let rec holes_z =
        (ze: ZExp.t, rev_steps: CursorPath_common.rev_steps)
        : CursorPath_common.zhole_list =>
  holes_zblock(ze, rev_steps)
and holes_zblock =
    (
      (prefix, zline, suffix): ZExp.zblock,
      rev_steps: CursorPath_common.rev_steps,
    )
    : CursorPath_common.zhole_list => {
  let holes_prefix =
    ListUtil.fold_right_i(
      ((i, line), hs) => hs |> holes_line(line, [i, ...rev_steps]),
      prefix,
      [],
    );
  let CursorPath_common.{holes_before, hole_selected, holes_after} =
    holes_zline(zline, [List.length(prefix), ...rev_steps]);
  let holes_suffix =
    ListUtil.fold_right_i(
      ((i, line), hs) =>
        hs |> holes_line(line, [List.length(prefix) + 1 + i, ...rev_steps]),
      suffix,
      [],
    );
  CursorPath_common.mk_zholes(
    ~holes_before=holes_prefix @ holes_before,
    ~hole_selected,
    ~holes_after=holes_after @ holes_suffix,
    (),
  );
}
and holes_zline =
    (zline: ZExp.zline, rev_steps: CursorPath_common.rev_steps)
    : CursorPath_common.zhole_list =>
  switch (zline) {
  | CursorL(OnOp(_), _) => CursorPath_common.no_holes
  | CursorL(_, EmptyLine) => CursorPath_common.no_holes
  | CursorL(_, CommentLine(_)) => CursorPath_common.no_holes
  | CursorL(_, ExpLine(_)) => CursorPath_common.no_holes /* invalid cursor position */
  | CursorL(cursor, LetLine(p, ann, def)) =>
    let holes_p = CursorPath_Pat.holes(p, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) => CursorPath_Typ.holes(uty, [1, ...rev_steps], [])
      };
    let holes_def = holes(def, [2, ...rev_steps], []);
    switch (cursor) {
    | OnDelim(0, _) =>
      CursorPath_common.mk_zholes(
        ~holes_after=holes_p @ holes_ann @ holes_def,
        (),
      )
    | OnDelim(1, _) =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_p,
        ~holes_after=holes_ann @ holes_def,
        (),
      )
    | OnDelim(2, _) =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_p @ holes_ann,
        ~holes_after=holes_def,
        (),
      )
    | OnDelim(3, _) =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_p @ holes_ann @ holes_def,
        (),
      )
    | _ => CursorPath_common.no_holes
    };
  | ExpLineZ(zopseq) => holes_zopseq(zopseq, rev_steps)
  | LetLineZP(zp, ann, body) =>
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      CursorPath_Pat.holes_z(zp, [0, ...rev_steps]);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(ann) => CursorPath_Typ.holes(ann, [1, ...rev_steps], [])
      };
    let holes_body = holes(body, [2, ...rev_steps], []);
    CursorPath_common.mk_zholes(
      ~holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ holes_ann @ holes_body,
      (),
    );
  | LetLineZA(p, zann, body) =>
    let holes_p = CursorPath_Pat.holes(p, [0, ...rev_steps], []);
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      CursorPath_Typ.holes_z(zann, [1, ...rev_steps]);
    let holes_body = holes(body, [2, ...rev_steps], []);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_p @ holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ holes_body,
      (),
    );
  | LetLineZE(p, ann, zbody) =>
    let holes_p = CursorPath_Pat.holes(p, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(ann) => CursorPath_Typ.holes(ann, [1, ...rev_steps], [])
      };
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_z(zbody, [2, ...rev_steps]);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_p @ holes_ann @ holes_before,
      ~hole_selected,
      ~holes_after,
      (),
    );
  }
and holes_zopseq =
    (zopseq: ZExp.zopseq, rev_steps: CursorPath_common.rev_steps)
    : CursorPath_common.zhole_list =>
  CursorPath_common.holes_zopseq_(
    ~holes_operand,
    ~holes_zoperand,
    ~hole_sort=hole_sort(TypeErr),
    ~is_space=Operators_Exp.is_Space,
    ~rev_steps,
    ~erase_zopseq=ZExp.erase_zopseq,
    zopseq,
  )
and holes_zoperand =
    (zoperand: ZExp.zoperand, rev_steps: CursorPath_common.rev_steps)
    : CursorPath_common.zhole_list =>
  switch (zoperand) {
  | CursorE(OnOp(_), _) => CursorPath_common.no_holes
  | CursorE(_, EmptyHole(u)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some({sort: ExpHole(u, Empty), steps: List.rev(rev_steps)}),
      (),
    )
  | CursorE(_, InvalidText(u, _)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some({sort: ExpHole(u, VarErr), steps: List.rev(rev_steps)}),
      (),
    )
  | CursorE(_, Var(err, verr, _)) =>
    switch (err, verr) {
    | (NotInHole, NotInVarHole) => CursorPath_common.no_holes
    | (InHole(_, u), _) =>
      CursorPath_common.mk_zholes(
        ~hole_selected=
          Some({sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)}),
        (),
      )
    | (_, InVarHole(_, u)) =>
      CursorPath_common.mk_zholes(
        ~hole_selected=
          Some({sort: ExpHole(u, VarErr), steps: List.rev(rev_steps)}),
        (),
      )
    }
  | CursorE(_, IntLit(err, _))
  | CursorE(_, FloatLit(err, _))
  | CursorE(_, BoolLit(err, _))
  | CursorE(_, ListNil(err)) =>
    switch (err) {
    | NotInHole => CursorPath_common.no_holes
    | InHole(_, u) =>
      CursorPath_common.mk_zholes(
        ~hole_selected=
          Some({sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)}),
        (),
      )
    }
  | CursorE(OnDelim(k, _), Parenthesized(body)) =>
    let body_holes = holes(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zholes(~holes_before=body_holes, ())
    | 1 => CursorPath_common.mk_zholes(~holes_after=body_holes, ())
    | _ => CursorPath_common.no_holes
    };
  | CursorE(OnDelim(k, _), Inj(err, _, body)) =>
    let hole_selected: option(CursorPath_common.hole_info) =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) =>
        Some({
          sort: CursorPath_common.ExpHole(u, TypeErr),
          steps: List.rev(rev_steps),
        })
      };
    let body_holes = holes(body, [0, ...rev_steps], []);
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
  | CursorE(OnDelim(k, _), Lam(err, p, ann, body)) =>
    let hole_selected: option(CursorPath_common.hole_info) =
      switch (err) {
      | NotInHole => None
      | InHole(_, u) =>
        Some({sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)})
      };
    let holes_p = CursorPath_Pat.holes(p, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) => CursorPath_Typ.holes(uty, [1, ...rev_steps], [])
      };
    let holes_body = holes(body, [2, ...rev_steps], []);
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zholes(
        ~hole_selected,
        ~holes_after=holes_p @ holes_ann @ holes_body,
        (),
      )
    | 1 =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_p,
        ~hole_selected,
        ~holes_after=holes_ann @ holes_body,
        (),
      )
    | 2 =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_p @ holes_ann,
        ~hole_selected,
        ~holes_after=holes_body,
        (),
      )
    | _ => CursorPath_common.no_holes
    };
  | CursorE(OnDelim(k, _), Case(err, scrut, rules)) =>
    let hole_selected: option(CursorPath_common.hole_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => None
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) =>
        Some({sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)})
      };
    let holes_scrut = holes(scrut, [0, ...rev_steps], []);
    let holes_rules =
      ListUtil.fold_right_i(
        ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
        rules,
        [],
      );
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zholes(
        ~holes_after=holes_scrut @ holes_rules,
        ~hole_selected,
        (),
      )
    | 1 =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_scrut @ holes_rules,
        ~hole_selected,
        ~holes_after=[],
        (),
      )
    | _ => CursorPath_common.no_holes
    };
  | CursorE(OnDelim(k, _), If(err, t1, t2, t3)) =>
    let hole_selected: option(CursorPath_common.hole_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => None
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) =>
        Some({sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)})
      };
    let holes_t1 = holes(t1, [0, ...rev_steps], []);
    let holes_t2 = holes(t2, [1, ...rev_steps], []);
    let holes_t3 = holes(t3, [2, ...rev_steps], []);
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zholes(
        ~hole_selected,
        ~holes_after=holes_t1 @ holes_t2 @ holes_t3,
        (),
      )
    | 1 =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_t1,
        ~hole_selected,
        ~holes_after=holes_t2 @ holes_t3,
        (),
      )
    | 2 =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_t1 @ holes_t2,
        ~hole_selected,
        ~holes_after=holes_t3,
        (),
      )
    | _ => CursorPath_common.no_holes
    };
  | CursorE(OnText(_), Inj(_) | Parenthesized(_) | Lam(_) | Case(_) | If(_)) =>
    /* invalid cursor position */
    CursorPath_common.no_holes
  | CursorE(_, ApPalette(_)) => CursorPath_common.no_holes /* TODO[livelits] */
  | ParenthesizedZ(zbody) => holes_z(zbody, [0, ...rev_steps])
  | LamZP(err, zp, ann, body) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | NotInHole => []
      | InHole(_, u) => [
          {sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)},
        ]
      };
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      CursorPath_Pat.holes_z(zp, [0, ...rev_steps]);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(ann) => CursorPath_Typ.holes(ann, [1, ...rev_steps], [])
      };
    let holes_body = holes(body, [2, ...rev_steps], []);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_err @ holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ holes_ann @ holes_body,
      (),
    );
  | LamZA(err, p, zann, body) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | NotInHole => []
      | InHole(_, u) => [
          {sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)},
        ]
      };
    let holes_p = CursorPath_Pat.holes(p, [0, ...rev_steps], []);
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      CursorPath_Typ.holes_z(zann, [1, ...rev_steps]);
    let holes_body = holes(body, [2, ...rev_steps], []);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_err @ holes_p @ holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ holes_body,
      (),
    );
  | LamZE(err, p, ann, zbody) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | NotInHole => []
      | InHole(_, u) => [
          {sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)},
        ]
      };
    let holes_p = CursorPath_Pat.holes(p, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) => CursorPath_Typ.holes(uty, [1, ...rev_steps], [])
      };
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_z(zbody, [2, ...rev_steps]);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_err @ holes_p @ holes_ann @ holes_before,
      ~hole_selected,
      ~holes_after,
      (),
    );
  | InjZ(err, _, zbody) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | NotInHole => []
      | InHole(_, u) => [
          {sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)},
        ]
      };
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_z(zbody, [0, ...rev_steps]);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_err @ holes_before,
      ~hole_selected,
      ~holes_after,
      (),
    );
  | CaseZE(err, zscrut, rules) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => []
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) => [
          {
            sort: CursorPath_common.ExpHole(u, TypeErr),
            steps: List.rev(rev_steps),
          },
        ]
      };
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_z(zscrut, [0, ...rev_steps]);
    let holes_rules =
      ListUtil.fold_right_i(
        ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
        rules,
        [],
      );
    CursorPath_common.mk_zholes(
      ~holes_before=holes_err @ holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ holes_rules,
      (),
    );
  | CaseZR(err, scrut, (prefix, zrule, suffix)) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => []
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) => [
          {sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)},
        ]
      };
    let holes_scrut = holes(scrut, [0, ...rev_steps], []);
    let holes_prefix =
      ListUtil.fold_right_i(
        ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
        prefix,
        [],
      );
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_zrule(zrule, [1 + List.length(prefix), ...rev_steps]);
    let holes_suffix =
      ListUtil.fold_right_i(
        ((i, rule), hs) =>
          hs
          |> holes_rule(
               rule,
               [1 + List.length(prefix) + 1 + i, ...rev_steps],
             ),
        suffix,
        [],
      );
    {
      holes_before: holes_err @ holes_scrut @ holes_prefix @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_suffix,
    };
  | IfZ1(err, zt1, t2, t3) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => []
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) => [
          {sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)},
        ]
      };
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_z(zt1, [0, ...rev_steps]);
    let holes_t2 = holes(t2, [1, ...rev_steps], []);
    let holes_t3 = holes(t3, [2, ...rev_steps], []);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_err @ holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ holes_t2 @ holes_t3,
      (),
    );
  | IfZ2(err, t1, zt2, t3) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => []
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) => [
          {sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)},
        ]
      };
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_z(zt2, [1, ...rev_steps]);
    let holes_t1 = holes(t1, [0, ...rev_steps], []);
    let holes_t3 = holes(t3, [2, ...rev_steps], []);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_err @ holes_t1 @ holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ holes_t3,
      (),
    );
  | IfZ3(err, t1, t2, zt3) =>
    let holes_err: list(CursorPath_common.hole_info) =
      switch (err) {
      | StandardErrStatus(NotInHole) => []
      | StandardErrStatus(InHole(_, u))
      | InconsistentBranches(_, u) => [
          {sort: ExpHole(u, TypeErr), steps: List.rev(rev_steps)},
        ]
      };
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_z(zt3, [2, ...rev_steps]);
    let holes_t1 = holes(t1, [0, ...rev_steps], []);
    let holes_t2 = holes(t2, [1, ...rev_steps], []);
    CursorPath_common.mk_zholes(
      ~holes_before=holes_err @ holes_t1 @ holes_t2 @ holes_before,
      ~hole_selected,
      ~holes_after,
      (),
    );
  | ApPaletteZ(_, _, _, zpsi) =>
    let zsplice_map = zpsi.zsplice_map;
    let (n, (_, ze)) = ZIntMap.prj_z_kv(zsplice_map);
    let CursorPath_common.{holes_before, hole_selected, holes_after} =
      holes_z(ze, [n, ...rev_steps]);
    let splice_order = zpsi.splice_order;
    let splice_map = ZIntMap.prj_map(zsplice_map);
    let (splices_before, splices_after) = ListUtil.split_at(splice_order, n);
    let holes_splices_before =
      List.fold_left(
        (hs, n) =>
          switch (IntMap.find_opt(n, splice_map)) {
          | None => hs
          | Some((_, e)) => hs @ holes(e, [n, ...rev_steps], [])
          },
        [],
        splices_before,
      );
    let holes_splices_after =
      List.fold_left(
        (hs, n) =>
          switch (IntMap.find_opt(n, splice_map)) {
          | None => hs
          | Some((_, e)) => hs @ holes(e, [n, ...rev_steps], [])
          },
        [],
        splices_after,
      );
    {
      holes_before: holes_splices_before @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_splices_after,
    };
  }
and holes_zrule = (zrule: ZExp.zrule, rev_steps: CursorPath_common.rev_steps) =>
  switch (zrule) {
  | CursorR(OnOp(_) | OnText(_), _) =>
    // invalid cursor position
    CursorPath_common.no_holes
  | CursorR(OnDelim(k, _), Rule(p, clause)) =>
    let holes_p = CursorPath_Pat.holes(p, [0, ...rev_steps], []);
    let holes_clause = holes(clause, [1, ...rev_steps], []);
    switch (k) {
    | 0 =>
      CursorPath_common.mk_zholes(~holes_after=holes_p @ holes_clause, ())
    | 1 =>
      CursorPath_common.mk_zholes(
        ~holes_before=holes_p,
        ~holes_after=holes_clause,
        (),
      )
    | _ => CursorPath_common.no_holes
    };
  | RuleZP(zp, clause) =>
    let zholes_p = CursorPath_Pat.holes_z(zp, [0, ...rev_steps]);
    let holes_clause = holes(clause, [1, ...rev_steps], []);
    {...zholes_p, holes_after: zholes_p.holes_after @ holes_clause};
  | RuleZE(p, zclause) =>
    let holes_p = CursorPath_Pat.holes(p, [0, ...rev_steps], []);
    let zholes_clause = holes_z(zclause, [1, ...rev_steps]);
    {...zholes_clause, holes_before: holes_p @ zholes_clause.holes_before};
  };

let prev_hole_steps_z = (ze: ZExp.t): option(CursorPath_common.steps) => {
  let holes = holes_z(ze, []);
  CursorPath_common.prev_hole_steps(holes);
};
let prev_hole_steps_zline =
    (zline: ZExp.zline): option(CursorPath_common.steps) => {
  let holes = holes_zline(zline, []);
  CursorPath_common.prev_hole_steps(holes);
};

let next_hole_steps_z = (ze: ZExp.t): option(CursorPath_common.steps) => {
  let holes = holes_z(ze, []);
  CursorPath_common.next_hole_steps(holes);
};
let next_hole_steps_zline =
    (zline: ZExp.zline): option(CursorPath_common.steps) => {
  let holes = holes_zline(zline, []);
  CursorPath_common.next_hole_steps(holes);
};
