open Sexplib.Std;
open SemanticsCommon;
open GeneralUtil;

[@deriving (show({with_path: false}), sexp)]
type steps = list(child_index);
[@deriving (show({with_path: false}), sexp)]
type rev_steps = steps;

[@deriving (show({with_path: false}), sexp)]
type t = (steps, cursor_position);

let cons' = (step: int, (steps, cursor): t): t => {
  ([step, ...steps], cursor);
};

let rec of_ztyp = (zty: ZTyp.t): t =>
  switch (zty) {
  | CursorT(cursor, _) => ([], cursor)
  | ParenthesizedZ(zty1) => cons'(0, of_ztyp(zty1))
  | ListZ(zty1) => cons'(0, of_ztyp(zty1))
  | OpSeqZ(_, zty1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_ztyp(zty1));
  };

let rec of_zpat = (zp: ZPat.t): t =>
  switch (zp) {
  | CursorP(cursor, _) => ([], cursor)
  | ParenthesizedZ(zp1) => cons'(0, of_zpat(zp1))
  | OpSeqZ(_, zp1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zpat(zp1));
  | InjZ(_, _, zp1) => cons'(0, of_zpat(zp1))
  };

let rec of_zblock = (zblock: ZExp.zblock): t =>
  switch (zblock) {
  | BlockZL(zlines, _) => of_zlines(zlines)
  | BlockZE(lines, ze) => cons'(List.length(lines), of_zexp(ze))
  }
and of_zlines = (zlines: ZExp.zlines): t => {
  let prefix_len = ZList.prefix_length(zlines);
  let zline = ZList.prj_z(zlines);
  cons'(prefix_len, of_zline(zline));
}
and of_zline = (zline: ZExp.zline): t =>
  switch (zline) {
  | CursorL(cursor, _) => ([], cursor)
  | ExpLineZ(ze) => of_zexp(ze)
  | LetLineZP(zp, _, _) => cons'(0, of_zpat(zp))
  | LetLineZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | LetLineZE(_, _, zblock) => cons'(2, of_zblock(zblock))
  }
and of_zexp = (ze: ZExp.t): t =>
  switch (ze) {
  | CursorE(cursor, _) => ([], cursor)
  | ParenthesizedZ(zblock) => cons'(0, of_zblock(zblock))
  | OpSeqZ(_, ze, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zexp(ze));
  | LamZP(_, zp, _, _) => cons'(0, of_zpat(zp))
  | LamZA(_, _, zann, _) => cons'(1, of_ztyp(zann))
  | LamZE(_, _, _, zblock) => cons'(2, of_zblock(zblock))
  | InjZ(_, _, zblock) => cons'(0, of_zblock(zblock))
  | CaseZE(_, zblock, _, _) => cons'(0, of_zblock(zblock))
  | CaseZR(_, _, zrules, _) =>
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons'(prefix_len + 1, of_zrule(zrule));
  | CaseZA(_, _, rules, zann) =>
    cons'(List.length(rules) + 1, of_ztyp(zann))
  | ApPaletteZ(_, _, _, zpsi) =>
    let zhole_map = zpsi.zsplice_map;
    let (n, (_, zblock)) = ZNatMap.prj_z_kv(zhole_map);
    cons'(n, of_zblock(zblock));
  }
and of_zrule = (zrule: ZExp.zrule): t =>
  switch (zrule) {
  | CursorR(cursor, _) => ([], cursor)
  | RuleZP(zp, _) => cons'(0, of_zpat(zp))
  | RuleZE(_, zblock) => cons'(1, of_zblock(zblock))
  };

let of_OpSeqZ = (ze: ZExp.t, surround: ZExp.opseq_surround): t => {
  let n = OperatorSeq.surround_prefix_length(surround);
  cons'(n, of_zexp(ze));
};

let of_OpSeqZ_pat = (zp: ZPat.t, surround: ZPat.opseq_surround): t => {
  let n = OperatorSeq.surround_prefix_length(surround);
  cons'(n, of_zpat(zp));
};

let append = ((appendee_steps, appendee_cursor): t, steps): t => (
  steps @ appendee_steps,
  appendee_cursor,
);

let rec before_typ = (~steps=[], ty: UHTyp.t): t =>
  switch (ty) {
  | Hole
  | Unit
  | Num
  | Bool => (steps, OnDelim(0, Before))
  | Parenthesized(_)
  | List(_) => (steps, OnDelim(0, Before))
  | OpSeq(_, seq) =>
    let (first, _) = seq |> OperatorSeq.split0;
    before_typ(~steps=steps @ [0], first);
  };

let rec before_pat = (~steps=[], p: UHPat.t): t =>
  switch (p) {
  | EmptyHole(_)
  | Wild(_)
  | ListNil(_) => (steps, OnDelim(0, Before))
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _) => (steps, OnText(0))
  | Parenthesized(_)
  | Inj(_, _, _) => (steps, OnDelim(0, Before))
  | OpSeq(_, seq) =>
    let (first, _) = seq |> OperatorSeq.split0;
    before_pat(~steps=steps @ [0], first);
  };

let rec before_block =
        (~steps=[], Block(leading, conclusion): UHExp.block): t =>
  switch (leading) {
  | [] => before_exp(~steps=steps @ [0], conclusion)
  | [first, ..._] => before_line(~steps=steps @ [0], first)
  }
and before_line = (~steps=[], line: UHExp.line): t =>
  switch (line) {
  | EmptyLine => (steps, OnText(0))
  | ExpLine(e) => before_exp(~steps, e)
  | LetLine(_, _, _) => (steps, OnDelim(0, Before))
  }
and before_exp = (~steps=[], e: UHExp.t): t =>
  switch (e) {
  | EmptyHole(_)
  | ListNil(_) => (steps, OnDelim(0, Before))
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _) => (steps, OnText(0))
  | Lam(_, _, _, _)
  | Inj(_, _, _)
  | Case(_, _, _, _)
  | Parenthesized(_)
  | ApPalette(_, _, _, _) => (steps, OnDelim(0, Before))
  | OpSeq(_, seq) =>
    let (first, _) = seq |> OperatorSeq.split0;
    before_exp(~steps=steps @ [0], first);
  };

let rec follow_ty_and_place_cursor =
        (steps: steps, place_cursor: UHTyp.t => option(ZTyp.t), uty: UHTyp.t)
        : option(ZTyp.t) =>
  switch (steps) {
  | [] => place_cursor(uty)
  | [x, ...xs] =>
    switch (uty) {
    | TVar(_)
    | TVarHole(_)
    | Forall(_)
    | ForallHole(_)
    | Hole
    | Unit
    | Num
    | Bool => None
    | Parenthesized(uty1) =>
      switch (x) {
      | 0 =>
        switch (follow_ty_and_place_cursor(xs, place_cursor, uty1)) {
        | None => None
        | Some(zty) => Some(ParenthesizedZ(zty))
        }
      | _ => None
      }
    | List(uty1) =>
      switch (x) {
      | 0 =>
        switch (follow_ty_and_place_cursor(xs, place_cursor, uty1)) {
        | None => None
        | Some(zty) => Some(ListZ(zty))
        }
      | _ => None
      }
    | OpSeq(skel, seq) =>
      switch (OperatorSeq.split(x, seq)) {
      | None => None
      | Some((uty_n, surround)) =>
        switch (follow_ty_and_place_cursor(xs, place_cursor, uty_n)) {
        | None => None
        | Some(zty_n) => Some(OpSeqZ(skel, zty_n, surround))
        }
      }
    }
  };

let follow_ty_and_place_before = (steps: steps, uty: UHTyp.t): option(ZTyp.t) =>
  follow_ty_and_place_cursor(
    steps,
    uty => Some(ZTyp.place_before(uty)),
    uty,
  );

let follow_ty_and_place_after = (steps: steps, uty: UHTyp.t): option(ZTyp.t) =>
  follow_ty_and_place_cursor(
    steps,
    uty => Some(ZTyp.place_after(uty)),
    uty,
  );

let follow_ty = ((steps, cursor): t, uty: UHTyp.t): option(ZTyp.t) =>
  follow_ty_and_place_cursor(steps, ZTyp.place_cursor(cursor), uty);

exception UHTypeNodeNotFound(t, UHTyp.t);
let follow_ty_or_fail = (path: t, uty: UHTyp.t): ZTyp.t =>
  switch (follow_ty(path, uty)) {
  | None => raise(UHTypeNodeNotFound(path, uty))
  | Some(zty) => zty
  };

let rec follow_pat_and_place_cursor =
        (steps: steps, place_cursor: UHPat.t => option(ZPat.t), p: UHPat.t)
        : option(ZPat.t) =>
  switch (steps) {
  | [] => place_cursor(p)
  | [x, ...xs] =>
    switch (x, p) {
    /* outer nodes */
    | (_, EmptyHole(_))
    | (_, Wild(_))
    | (_, Var(_, _, _))
    | (_, NumLit(_, _))
    | (_, BoolLit(_, _))
    | (_, ListNil(_)) => None
    /* inner nodes */
    | (0, Parenthesized(p1)) =>
      switch (follow_pat_and_place_cursor(xs, place_cursor, p1)) {
      | None => None
      | Some(zp1) => Some(ParenthesizedZ(zp1))
      }
    | (_, Parenthesized(_)) => None
    | (_, OpSeq(skel, seq)) =>
      switch (OperatorSeq.split(x, seq)) {
      | None => None
      | Some((p, surround)) =>
        switch (follow_pat_and_place_cursor(xs, place_cursor, p)) {
        | None => None
        | Some(zp) => Some(OpSeqZ(skel, zp, surround))
        }
      }
    | (0, Inj(err, side, p1)) =>
      switch (follow_pat_and_place_cursor(xs, place_cursor, p1)) {
      | None => None
      | Some(zp1) => Some(InjZ(err, side, zp1))
      }
    | (_, Inj(_, _, _)) => None
    }
  };

let follow_pat_and_place_before = (steps: steps, p: UHPat.t): option(ZPat.t) =>
  follow_pat_and_place_cursor(steps, p => Some(ZPat.place_before(p)), p);

let follow_pat_and_place_after = (steps: steps, p: UHPat.t): option(ZPat.t) =>
  follow_pat_and_place_cursor(steps, p => Some(ZPat.place_after(p)), p);

let follow_pat = ((steps, cursor): t, p: UHPat.t): option(ZPat.t) =>
  follow_pat_and_place_cursor(steps, ZPat.place_cursor(cursor), p);

exception UHPatNodeNotFound(t, UHPat.t);
let follow_pat_or_fail = (path: t, p: UHPat.t): ZPat.t =>
  switch (follow_pat(path, p)) {
  | None => raise(UHPatNodeNotFound(path, p))
  | Some(zp) => zp
  };

let rec follow_block_and_place_cursor =
        (
          steps: steps,
          pcl: UHExp.line => option(ZExp.zline),
          pce: UHExp.t => option(ZExp.t),
          pcr: UHExp.rule => option(ZExp.zrule),
          pcp: UHPat.t => option(ZPat.t),
          pct: UHTyp.t => option(ZTyp.t),
          Block(lines, e): UHExp.block,
        )
        : option(ZExp.zblock) =>
  switch (steps) {
  | [] => None /* no block level cursor */
  | [x, ...xs] =>
    if (x === List.length(lines)) {
      switch (follow_exp_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, e)) {
      | None => None
      | Some(ze) => Some(BlockZE(lines, ze))
      };
    } else {
      switch (
        follow_lines_and_place_cursor(steps, pcl, pce, pcr, pcp, pct, lines)
      ) {
      | None => None
      | Some(zlines) => Some(BlockZL(zlines, e))
      };
    }
  }
and follow_lines_and_place_cursor =
    (
      steps: steps,
      pcl: UHExp.line => option(ZExp.zline),
      pce: UHExp.t => option(ZExp.t),
      pcr: UHExp.rule => option(ZExp.zrule),
      pcp: UHPat.t => option(ZPat.t),
      pct: UHTyp.t => option(ZTyp.t),
      lines: UHExp.lines,
    )
    : option(ZExp.zlines) =>
  switch (steps) {
  | [] => None
  | [x, ...xs] =>
    switch (ZList.split_at(x, lines)) {
    | None => None
    | Some(split_lines) =>
      ZList.optmap_z(
        follow_line_and_place_cursor(xs, pcl, pce, pcr, pcp, pct),
        split_lines,
      )
    }
  }
and follow_line_and_place_cursor =
    (
      steps: steps,
      pcl: UHExp.line => option(ZExp.zline),
      pce: UHExp.t => option(ZExp.t),
      pcr: UHExp.rule => option(ZExp.zrule),
      pcp: UHPat.t => option(ZPat.t),
      pct: UHTyp.t => option(ZTyp.t),
      line: UHExp.line,
    )
    : option(ZExp.zline) =>
  switch (steps, line) {
  | (_, ExpLine(e)) =>
    switch (follow_exp_and_place_cursor(steps, pcl, pce, pcr, pcp, pct, e)) {
    | None => None
    | Some(ze) => Some(ExpLineZ(ze))
    }
  | ([], _) => pcl(line)
  | (_, EmptyLine) => None
  | ([0, ...xs], LetLine(p, ann, e1)) =>
    switch (follow_pat_and_place_cursor(xs, pcp, p)) {
    | None => None
    | Some(zp) => Some(LetLineZP(zp, ann, e1))
    }
  | ([1, ...xs], LetLine(p, ann, e1)) =>
    switch (ann) {
    | None => None
    | Some(ann_ty) =>
      switch (follow_ty_and_place_cursor(xs, pct, ann_ty)) {
      | None => None
      | Some(zann) => Some(LetLineZA(p, zann, e1))
      }
    }
  | ([2, ...xs], LetLine(p, ann, block)) =>
    switch (follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)) {
    | None => None
    | Some(zblock) => Some(LetLineZE(p, ann, zblock))
    }
  | (_, LetLine(_, _, _)) => None
  }
and follow_exp_and_place_cursor =
    (
      steps: steps,
      pcl: UHExp.line => option(ZExp.zline),
      pce: UHExp.t => option(ZExp.t),
      pcr: UHExp.rule => option(ZExp.zrule),
      pcp: UHPat.t => option(ZPat.t),
      pct: UHTyp.t => option(ZTyp.t),
      e: UHExp.t,
    )
    : option(ZExp.t) =>
  switch (steps) {
  | [] => pce(e)
  | [x, ...xs] =>
    switch (x, e) {
    /* outer nodes */
    | (_, EmptyHole(_))
    | (_, Var(_, _, _))
    | (_, NumLit(_, _))
    | (_, BoolLit(_, _))
    | (_, ListNil(_)) => None
    /* inner nodes */
    | (0, Parenthesized(block)) =>
      switch (
        follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
      ) {
      | None => None
      | Some(zblock) => Some(ParenthesizedZ(zblock))
      }
    | (_, Parenthesized(_)) => None
    | (_, OpSeq(skel, seq)) =>
      switch (OperatorSeq.split(x, seq)) {
      | None => None
      | Some((e, surround)) =>
        switch (follow_exp_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, e)) {
        | Some(ze) => Some(OpSeqZ(skel, ze, surround))
        | None => None
        }
      }
    | (0, Lam(err, p, ann, block)) =>
      switch (follow_pat_and_place_cursor(xs, pcp, p)) {
      | None => None
      | Some(zp) => Some(LamZP(err, zp, ann, block))
      }
    | (1, Lam(err, p, ann, block)) =>
      switch (ann) {
      | None => None
      | Some(ann_ty) =>
        switch (follow_ty_and_place_cursor(xs, pct, ann_ty)) {
        | None => None
        | Some(zann) => Some(LamZA(err, p, zann, block))
        }
      }
    | (2, Lam(err, p, ann, block)) =>
      switch (
        follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
      ) {
      | None => None
      | Some(zblock) => Some(LamZE(err, p, ann, zblock))
      }
    | (_, Lam(_, _, _, _)) => None
    | (0, Inj(err, side, block)) =>
      switch (
        follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
      ) {
      | None => None
      | Some(zblock) => Some(InjZ(err, side, zblock))
      }
    | (_, Inj(_, _, _)) => None
    | (0, Case(err, block, rules, ann)) =>
      switch (
        follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
      ) {
      | None => None
      | Some(zblock) => Some(CaseZE(err, zblock, rules, ann))
      }
    | (x, Case(err, block, rules, ann)) when x === List.length(rules) + 1 =>
      switch (ann) {
      | None => None
      | Some(ty) =>
        switch (follow_ty_and_place_cursor(xs, pct, ty)) {
        | None => None
        | Some(zann) => Some(CaseZA(err, block, rules, zann))
        }
      }
    | (x, Case(err, block, rules, ann)) =>
      switch (ZList.split_at(x - 1, rules)) {
      | None => None
      | Some(split_rules) =>
        switch (
          ZList.optmap_z(
            follow_rule_and_place_cursor(xs, pcl, pce, pcr, pcp, pct),
            split_rules,
          )
        ) {
        | None => None
        | Some(zrules) => Some(CaseZR(err, block, zrules, ann))
        }
      }
    | (n, ApPalette(_, name, serialized_model, splice_info)) =>
      switch (
        ZSpliceInfo.select_opt(splice_info, n, ((ty, block)) =>
          switch (
            follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
          ) {
          | None => None
          | Some(zblock) => Some((ty, zblock))
          }
        )
      ) {
      | None => None
      | Some(zsplice_info) =>
        Some(ApPaletteZ(NotInHole, name, serialized_model, zsplice_info))
      }
    }
  }
and follow_rule_and_place_cursor =
    (
      steps: steps,
      pcl: UHExp.line => option(ZExp.zline),
      pce: UHExp.t => option(ZExp.t),
      pcr: UHExp.rule => option(ZExp.zrule),
      pcp: UHPat.t => option(ZPat.t),
      pct: UHTyp.t => option(ZTyp.t),
      Rule(p, block) as rule: UHExp.rule,
    )
    : option(ZExp.zrule) =>
  switch (steps) {
  | [] => pcr(rule)
  | [0, ...xs] =>
    switch (follow_pat_and_place_cursor(xs, pcp, p)) {
    | None => None
    | Some(zp) => Some(RuleZP(zp, block))
    }
  | [1, ...xs] =>
    switch (follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)) {
    | None => None
    | Some(zblock) => Some(RuleZE(p, zblock))
    }
  | [_, ..._] => None
  };

let follow_block_and_place_before =
    (steps: steps, block: UHExp.block): option(ZExp.zblock) =>
  follow_block_and_place_cursor(
    steps,
    line => Some(ZExp.place_before_line(line)),
    e => Some(ZExp.place_before_exp(e)),
    rule => Some(ZExp.place_before_rule(rule)),
    p => Some(ZPat.place_before(p)),
    uty => Some(ZTyp.place_before(uty)),
    block,
  );
let follow_line_and_place_before =
    (steps: steps, line: UHExp.line): option(ZExp.zline) =>
  follow_line_and_place_cursor(
    steps,
    line => Some(ZExp.place_before_line(line)),
    e => Some(ZExp.place_before_exp(e)),
    rule => Some(ZExp.place_before_rule(rule)),
    p => Some(ZPat.place_before(p)),
    uty => Some(ZTyp.place_before(uty)),
    line,
  );
let follow_exp_and_place_before = (steps: steps, e: UHExp.t): option(ZExp.t) =>
  follow_exp_and_place_cursor(
    steps,
    line => Some(ZExp.place_before_line(line)),
    e => Some(ZExp.place_before_exp(e)),
    rule => Some(ZExp.place_before_rule(rule)),
    p => Some(ZPat.place_before(p)),
    uty => Some(ZTyp.place_before(uty)),
    e,
  );
let follow_rule_and_place_before =
    (steps: steps, rule: UHExp.rule): option(ZExp.zrule) =>
  follow_rule_and_place_cursor(
    steps,
    line => Some(ZExp.place_before_line(line)),
    e => Some(ZExp.place_before_exp(e)),
    rule => Some(ZExp.place_before_rule(rule)),
    p => Some(ZPat.place_before(p)),
    uty => Some(ZTyp.place_before(uty)),
    rule,
  );

let follow_block_and_place_after =
    (steps: steps, block: UHExp.block): option(ZExp.zblock) =>
  follow_block_and_place_cursor(
    steps,
    line => Some(ZExp.place_after_line(line)),
    e => Some(ZExp.place_after_exp(e)),
    rule => Some(ZExp.place_after_rule(rule)),
    p => Some(ZPat.place_after(p)),
    uty => Some(ZTyp.place_after(uty)),
    block,
  );
let follow_line_and_place_after =
    (steps: steps, line: UHExp.line): option(ZExp.zline) =>
  follow_line_and_place_cursor(
    steps,
    line => Some(ZExp.place_after_line(line)),
    e => Some(ZExp.place_after_exp(e)),
    rule => Some(ZExp.place_after_rule(rule)),
    p => Some(ZPat.place_after(p)),
    uty => Some(ZTyp.place_after(uty)),
    line,
  );
let follow_exp_and_place_after = (steps: steps, e: UHExp.t): option(ZExp.t) =>
  follow_exp_and_place_cursor(
    steps,
    line => Some(ZExp.place_after_line(line)),
    e => Some(ZExp.place_after_exp(e)),
    rule => Some(ZExp.place_after_rule(rule)),
    p => Some(ZPat.place_after(p)),
    uty => Some(ZTyp.place_after(uty)),
    e,
  );
let follow_rule_and_place_after =
    (steps: steps, rule: UHExp.rule): option(ZExp.zrule) =>
  follow_rule_and_place_cursor(
    steps,
    line => Some(ZExp.place_after_line(line)),
    e => Some(ZExp.place_after_exp(e)),
    rule => Some(ZExp.place_after_rule(rule)),
    p => Some(ZPat.place_after(p)),
    uty => Some(ZTyp.place_after(uty)),
    rule,
  );

let follow_block =
    ((steps, cursor): t, block: UHExp.block): option(ZExp.zblock) =>
  follow_block_and_place_cursor(
    steps,
    ZExp.place_cursor_line(cursor),
    ZExp.place_cursor_exp(cursor),
    ZExp.place_cursor_rule(cursor),
    ZPat.place_cursor(cursor),
    ZTyp.place_cursor(cursor),
    block,
  );
let follow_lines =
    ((steps, cursor): t, lines: UHExp.lines): option(ZExp.zlines) =>
  follow_lines_and_place_cursor(
    steps,
    ZExp.place_cursor_line(cursor),
    ZExp.place_cursor_exp(cursor),
    ZExp.place_cursor_rule(cursor),
    ZPat.place_cursor(cursor),
    ZTyp.place_cursor(cursor),
    lines,
  );
let follow_line = ((steps, cursor): t, line: UHExp.line): option(ZExp.zline) =>
  follow_line_and_place_cursor(
    steps,
    ZExp.place_cursor_line(cursor),
    ZExp.place_cursor_exp(cursor),
    ZExp.place_cursor_rule(cursor),
    ZPat.place_cursor(cursor),
    ZTyp.place_cursor(cursor),
    line,
  );
let follow_exp = ((steps, cursor): t, e: UHExp.t): option(ZExp.t) =>
  follow_exp_and_place_cursor(
    steps,
    ZExp.place_cursor_line(cursor),
    ZExp.place_cursor_exp(cursor),
    ZExp.place_cursor_rule(cursor),
    ZPat.place_cursor(cursor),
    ZTyp.place_cursor(cursor),
    e,
  );
let follow_rule = ((steps, cursor): t, rule: UHExp.rule): option(ZExp.zrule) =>
  follow_rule_and_place_cursor(
    steps,
    ZExp.place_cursor_line(cursor),
    ZExp.place_cursor_exp(cursor),
    ZExp.place_cursor_rule(cursor),
    ZPat.place_cursor(cursor),
    ZTyp.place_cursor(cursor),
    rule,
  );

exception UHBlockNodeNotFound;
let follow_block_or_fail = (path: t, block: UHExp.block): ZExp.zblock =>
  switch (follow_block(path, block)) {
  | None =>
    JSUtil.log_sexp(sexp_of_t(path));
    JSUtil.log_sexp(UHExp.sexp_of_block(block));
    raise(UHBlockNodeNotFound);
  | Some(zblock) => zblock
  };

exception UHLinesNodeNotFound(t, UHExp.lines);
let follow_lines_or_fail = (path: t, lines: UHExp.lines): ZExp.zlines =>
  switch (follow_lines(path, lines)) {
  | None => raise(UHLinesNodeNotFound(path, lines))
  | Some(zlines) => zlines
  };

exception UHExpNodeNotFound(t, UHExp.t);
let follow_e_or_fail = (path: t, e: UHExp.t): ZExp.t =>
  switch (follow_exp(path, e)) {
  | None => raise(UHExpNodeNotFound(path, e))
  | Some(ze) => ze
  };

type hole_desc =
  | VHole(MetaVar.t)
  | TypHole
  | PatHole(MetaVar.t)
  | ExpHole(MetaVar.t);

type hole_list = list((hole_desc, t));

let rec holes_skel =
        (
          ~holes_tm: ('tm, steps, hole_list) => hole_list,
          ~hole_desc: MetaVar.t => hole_desc,
          ~path_before_tm: 'tm => t,
          ~is_space: 'op => bool,
          ~rev_steps: rev_steps,
          skel: Skel.t('op),
          seq: OperatorSeq.opseq('tm, 'op),
          holes: hole_list,
        )
        : hole_list => {
  switch (skel) {
  | Placeholder(n) =>
    let tm_n = seq |> OperatorSeq.nth_tm(n) |> Opt.get(() => assert(false));
    holes |> holes_tm(tm_n, [n, ...rev_steps]);
  | BinOp(err_status, op, skel1, skel2) =>
    holes
    |> holes_skel(
         ~holes_tm,
         ~hole_desc,
         ~path_before_tm,
         ~is_space,
         ~rev_steps,
         skel2,
         seq,
       )
    |> (
      holes =>
        switch (err_status, op |> is_space) {
        | (NotInHole, _) => holes
        | (InHole(_, u), true) =>
          let tm_before_space =
            seq
            |> OperatorSeq.nth_tm((skel2 |> Skel.leftmost_tm_index) - 1)
            |> Opt.get(() => assert(false));
          [
            (
              hole_desc(u),
              rev_steps
              |> List.rev
              |> append(path_before_tm(tm_before_space)),
            ),
            ...holes,
          ];
        | (InHole(_, u), false) => [
            (
              hole_desc(u),
              (
                rev_steps |> List.rev,
                OnDelim(skel2 |> Skel.leftmost_tm_index, Before),
              ),
            ),
          ]
        }
    )
    |> holes_skel(
         ~holes_tm,
         ~hole_desc,
         ~path_before_tm,
         ~is_space,
         ~rev_steps,
         skel1,
         seq,
       )
  };
};

let rec holes_uty =
        (uty: UHTyp.t, rev_steps: rev_steps, holes: hole_list): hole_list =>
  switch (uty) {
  | Hole => [
      (TypHole, (rev_steps |> List.rev, OnDelim(0, Before))),
      ...holes,
    ]
  | Unit => holes
  | Num => holes
  | Bool => holes
  | Parenthesized(uty1) => holes |> holes_uty(uty1, [0, ...rev_steps])
  | List(uty1) => holes |> holes_uty(uty1, [0, ...rev_steps])
  | OpSeq(skel, seq) =>
    holes
    |> holes_skel(
         ~holes_tm=holes_uty,
         ~path_before_tm=before_typ,
         ~hole_desc=_ => TypHole,
         ~is_space=_ => false,
         ~rev_steps,
         skel,
         seq,
       )
  };

let rec holes_pat =
        (p: UHPat.t, rev_steps: rev_steps, holes: hole_list): hole_list =>
  switch (p) {
  | EmptyHole(u) => [
      (PatHole(u), (rev_steps |> List.rev, OnDelim(0, Before))),
      ...holes,
    ]
  | Wild(InHole(_, u))
  | Var(InHole(_, u), _, _)
  | NumLit(InHole(_, u), _)
  | BoolLit(InHole(_, u), _)
  | ListNil(InHole(_, u))
  | Inj(InHole(_, u), _, _) => [
      (PatHole(u), rev_steps |> List.rev |> append(before_pat(p))),
      ...holes,
    ]
  | Var(_, InVHole(_, u), _) => [
      (PatHole(u), (rev_steps |> List.rev, OnText(0))),
      ...holes,
    ]
  | Var(NotInHole, NotInVHole, _) => holes
  | Wild(NotInHole) => holes
  | NumLit(NotInHole, _) => holes
  | BoolLit(NotInHole, _) => holes
  | ListNil(NotInHole) => holes
  | Parenthesized(p1) => holes |> holes_pat(p1, [0, ...rev_steps])
  | Inj(NotInHole, _, p1) => holes |> holes_pat(p1, [0, ...rev_steps])
  | OpSeq(skel, seq) =>
    holes
    |> holes_skel(
         ~holes_tm=holes_pat,
         ~path_before_tm=before_pat,
         ~hole_desc=u => PatHole(u),
         ~is_space=op => op == UHPat.Space,
         ~rev_steps,
         skel,
         seq,
       )
  };

let rec holes_block =
        (
          Block(lines, e): UHExp.block,
          rev_steps: rev_steps,
          holes: hole_list,
        )
        : hole_list => {
  let len = List.length(lines);
  let holes = holes_exp(e, [len, ...rev_steps], holes);
  holes_lines(lines, 0, rev_steps, holes);
}
and holes_lines =
    (lines: UHExp.lines, offset: int, rev_steps: rev_steps, holes: hole_list)
    : hole_list =>
  fold_right_i(
    ((i, line), holes) =>
      holes_line(line, [i + offset, ...rev_steps], holes),
    lines,
    holes,
  )
and holes_line =
    (line: UHExp.line, rev_steps: rev_steps, holes: hole_list): hole_list =>
  switch (line) {
  | EmptyLine => holes
  | LetLine(p, ann, block) =>
    holes
    |> holes_block(block, [2, ...rev_steps])
    |> (
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...rev_steps])
      | None => (holes => holes)
      }
    )
    |> holes_pat(p, [0, ...rev_steps])
  | ExpLine(e) => holes |> holes_exp(e, rev_steps)
  }
and holes_exp =
    (e: UHExp.t, rev_steps: rev_steps, holes: hole_list): hole_list =>
  switch (e) {
  | EmptyHole(u) => [
      (ExpHole(u), (rev_steps |> List.rev, OnDelim(0, Before))),
      ...holes,
    ]
  | Var(InHole(_, u), _, _)
  | NumLit(InHole(_, u), _)
  | BoolLit(InHole(_, u), _)
  | ListNil(InHole(_, u))
  | Lam(InHole(_, u), _, _, _)
  | Inj(InHole(_, u), _, _)
  | Case(InHole(_, u), _, _, _)
  | ApPalette(InHole(_, u), _, _, _) => [
      (ExpHole(u), rev_steps |> List.rev |> append(before_exp(e))),
      ...holes,
    ]
  | Var(_, InVHole(_, u), _) => [
      (VHole(u), (rev_steps |> List.rev, OnText(0))),
      ...holes,
    ]
  | Var(NotInHole, NotInVHole, _) => holes
  | NumLit(NotInHole, _) => holes
  | BoolLit(NotInHole, _) => holes
  | ListNil(NotInHole) => holes
  | Parenthesized(block) => holes_block(block, [0, ...rev_steps], holes)
  | OpSeq(skel, seq) =>
    holes
    |> holes_skel(
         ~holes_tm=holes_exp,
         ~hole_desc=u => ExpHole(u),
         ~path_before_tm=before_exp,
         ~is_space=op => op == UHExp.Space,
         ~rev_steps,
         skel,
         seq,
       )
  | Inj(NotInHole, _, block) => holes_block(block, [0, ...rev_steps], holes)
  | Lam(NotInHole, p, ann, block) =>
    let holes = holes_block(block, [2, ...rev_steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...rev_steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...rev_steps], holes);
  | Case(NotInHole, block, rules, ann) =>
    let holes =
      switch (ann) {
      | None => holes
      | Some(uty) =>
        holes_uty(uty, [List.length(rules) + 1, ...rev_steps], holes)
      };
    let holes = holes_rules(rules, 1, rev_steps, holes);
    holes_block(block, [0, ...rev_steps], holes);
  | ApPalette(NotInHole, _, _, psi) =>
    let splice_map = psi.splice_map;
    let splice_order = psi.splice_order;
    List.fold_right(
      (i, holes) =>
        switch (NatMap.lookup(splice_map, i)) {
        | None => holes
        | Some((_, block)) => holes_block(block, [i, ...rev_steps], holes)
        },
      splice_order,
      holes,
    );
  }
and holes_rules =
    (rules: UHExp.rules, offset: int, rev_steps: rev_steps, holes: hole_list)
    : hole_list =>
  fold_right_i(
    ((i, rule), holes) =>
      holes_rule(rule, [i + offset, ...rev_steps], holes),
    rules,
    holes,
  )
and holes_rule =
    (Rule(p, block): UHExp.rule, rev_steps: rev_steps, holes: hole_list)
    : hole_list => {
  let holes = holes_block(block, [1, ...rev_steps], holes);
  holes_pat(p, [0, ...rev_steps], holes);
};

/* two hole lists, one for before the cursor, one for after */
type zhole_list = {
  holes_before: hole_list,
  hole_selected: option((hole_desc, t)),
  holes_after: hole_list,
};

let no_holes = {holes_before: [], hole_selected: None, holes_after: []};

let rec _holes_surround =
        (
          ~holes_tm: ('tm, steps, hole_list) => hole_list,
          ~hole_desc: MetaVar.t => hole_desc,
          ~is_space: 'op => bool,
          ~path_before_tm: 'tm => t,
          ~rev_steps: rev_steps,
          ~surrounded_index: int,
          skel: Skel.t('op),
          seq: OperatorSeq.opseq('tm, 'op),
        )
        : (hole_list, hole_list) => {
  switch (skel) {
  | Placeholder(n) =>
    let tm_n = seq |> OperatorSeq.nth_tm(n) |> Opt.get(() => assert(false));
    if (n > surrounded_index) {
      ([], holes_tm(tm_n, [n, ...rev_steps], []));
    } else if (n < surrounded_index) {
      (holes_tm(tm_n, [n, ...rev_steps], []), []);
    } else {
      ([], []);
    };
  | BinOp(err_status, op, skel1, skel2) =>
    let (holes_before1, holes_after1) =
      _holes_surround(
        ~holes_tm,
        ~hole_desc,
        ~is_space,
        ~path_before_tm,
        ~rev_steps,
        ~surrounded_index,
        skel1,
        seq,
      );
    let (holes_before2, holes_after2) =
      _holes_surround(
        ~holes_tm,
        ~hole_desc,
        ~is_space,
        ~path_before_tm,
        ~rev_steps,
        ~surrounded_index,
        skel2,
        seq,
      );
    // holes_after1 is nonempty iff holes_before2 is empty,
    // therefore it's safe to crisscross
    let (holes_before, holes_after) = (
      holes_before1 @ holes_before2,
      holes_after1 @ holes_after2,
    );
    let steps = rev_steps |> List.rev;
    switch (
      err_status,
      op |> is_space,
      surrounded_index < (skel2 |> Skel.leftmost_tm_index),
    ) {
    | (NotInHole, _, _) => (holes_before, holes_after)
    | (InHole(_, u), true, _) =>
      let tm_before_space =
        seq
        |> OperatorSeq.nth_tm((skel2 |> Skel.leftmost_tm_index) - 1)
        |> Opt.get(() => assert(false));
      (
        holes_before
        @ [
          (hole_desc(u), steps |> append(path_before_tm(tm_before_space))),
        ],
        holes_after,
      );
    | (InHole(_, u), false, true) => (
        holes_before
        @ [
          (
            hole_desc(u),
            (steps, OnDelim(skel2 |> Skel.leftmost_tm_index, Before)),
          ),
        ],
        holes_after,
      )
    | (InHole(_, u), false, false) => (
        holes_before,
        [
          (
            hole_desc(u),
            (steps, OnDelim(skel2 |> Skel.leftmost_tm_index, Before)),
          ),
          ...holes_after,
        ],
      )
    };
  };
};

let holes_surround =
    (
      ~holes_tm: ('tm, steps, hole_list) => hole_list,
      ~hole_desc: MetaVar.t => hole_desc,
      ~is_space: 'op => bool,
      ~path_before_tm: 'tm => t,
      ~rev_steps: rev_steps,
      ~erase: 'ztm => 'tm,
      skel: Skel.t('op),
      ztm: 'ztm,
      surround: OperatorSeq.opseq_surround('tm, 'op),
    )
    : (hole_list, hole_list) => {
  let seq = OperatorSeq.opseq_of_exp_and_surround(erase(ztm), surround);
  let surrounded_index = surround |> OperatorSeq.surround_prefix_length;
  _holes_surround(
    ~holes_tm,
    ~hole_desc,
    ~is_space,
    ~path_before_tm,
    ~rev_steps,
    ~surrounded_index,
    skel,
    seq,
  );
};

let holes_OpSeqZ =
    (
      ~holes_tm: ('tm, steps, hole_list) => hole_list,
      ~holes_ztm: ('ztm, steps) => zhole_list,
      ~hole_desc: MetaVar.t => hole_desc,
      ~is_space: 'op => bool,
      ~path_before_tm: 'tm => t,
      ~rev_steps: rev_steps,
      ~erase: 'ztm => 'tm,
      skel: Skel.t('op),
      ztm: 'ztm,
      surround: OperatorSeq.opseq_surround('tm, 'op),
    )
    : zhole_list => {
  let (holes_prefix, holes_suffix) =
    holes_surround(
      ~holes_tm,
      ~hole_desc,
      ~is_space,
      ~path_before_tm,
      ~rev_steps,
      ~erase,
      skel,
      ztm,
      surround,
    );
  let prefix_len = OperatorSeq.surround_prefix_length(surround);
  let {holes_before, hole_selected, holes_after} =
    holes_ztm(ztm, [prefix_len, ...rev_steps]);
  let holes_before = holes_prefix @ holes_before;
  let holes_after = holes_after @ holes_suffix;
  {holes_before, hole_selected, holes_after};
};

let holes_Cursor_bracketed =
    (
      holes_fn: ('t, steps, hole_list) => hole_list,
      k: delim_index,
      bracketed_t: 't,
      rev_steps: rev_steps,
    )
    : zhole_list => {
  let holes_bracketed_t = holes_fn(bracketed_t, [0, ...rev_steps], []);
  switch (k) {
  | 0 => {
      holes_before: holes_bracketed_t,
      hole_selected: None,
      holes_after: [],
    }
  | 1 => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_bracketed_t,
    }
  | _ => no_holes
  };
};

let rec holes_Cursor_OpSeq =
        (
          ~holes_tm: ('tm, steps, hole_list) => hole_list,
          ~hole_desc: MetaVar.t => hole_desc,
          ~op_index: op_index,
          ~rev_steps: rev_steps,
          skel: Skel.t('op),
          seq: OperatorSeq.opseq('tm, 'op),
        )
        : zhole_list => {
  switch (skel) {
  | Placeholder(n) =>
    let tm_n = seq |> OperatorSeq.nth_tm(n) |> Opt.get(_ => assert(false));
    n >= op_index
      ? {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_tm(tm_n, [n, ...rev_steps], []),
      }
      : {
        holes_before: holes_tm(tm_n, [n, ...rev_steps], []),
        hole_selected: None,
        holes_after: [],
      };
  | BinOp(err_status, _, skel1, skel2) =>
    let {
      holes_before: holes_before1,
      hole_selected: hole_selected1,
      holes_after: holes_after1,
    } =
      holes_Cursor_OpSeq(
        ~holes_tm,
        ~hole_desc,
        ~op_index,
        ~rev_steps,
        skel1,
        seq,
      );
    let {
      holes_before: holes_before2,
      hole_selected: hole_selected2,
      holes_after: holes_after2,
    } =
      holes_Cursor_OpSeq(
        ~holes_tm,
        ~hole_desc,
        ~op_index,
        ~rev_steps,
        skel2,
        seq,
      );
    // holes_after1 is nonempty iff holes_before2 is empty,
    // therefore it's safe to crisscross
    let (holes_before, hole_selected, holes_after) = (
      holes_before1 @ holes_before2,
      switch (hole_selected1, hole_selected2) {
      | (None, None) => None
      | (Some(hole), _) => Some(hole)
      | (_, Some(hole)) => Some(hole)
      },
      holes_after1 @ holes_after2,
    );
    let steps = rev_steps |> List.rev;
    switch (err_status) {
    | NotInHole => {holes_before, hole_selected, holes_after}
    | InHole(_, u) =>
      let current_op_index = skel2 |> Skel.leftmost_tm_index;
      let current_hole = (
        hole_desc(u),
        (steps, OnDelim(op_index, Before)),
      );
      if (current_op_index < op_index) {
        {
          holes_before: holes_before @ [current_hole],
          hole_selected,
          holes_after,
        };
      } else if (current_op_index > op_index) {
        {
          holes_before,
          hole_selected,
          holes_after: [current_hole, ...holes_after],
        };
      } else {
        {holes_before, hole_selected: Some(current_hole), holes_after};
      };
    };
  };
};

let rec holes_zty = (zty: ZTyp.t, rev_steps: rev_steps): zhole_list => {
  switch (zty) {
  | CursorT(Staging(_), _) => no_holes
  | CursorT(_, Hole) => {
      holes_before: [],
      hole_selected:
        Some((TypHole, (rev_steps |> List.rev, OnDelim(0, Before)))),
      holes_after: [],
    }
  | CursorT(_, Unit)
  | CursorT(_, Num)
  | CursorT(_, Bool) => {
      holes_before: [],
      hole_selected: None,
      holes_after: [],
    }
  | CursorT(OnDelim(k, _), Parenthesized(uty1))
  | CursorT(OnDelim(k, _), List(uty1)) =>
    holes_Cursor_bracketed(holes_uty, k, uty1, rev_steps)
  | CursorT(OnDelim(k, _), OpSeq(skel, seq)) =>
    holes_Cursor_OpSeq(
      ~holes_tm=holes_uty,
      ~hole_desc=_ => TypHole,
      ~op_index=k,
      ~rev_steps,
      skel,
      seq,
    )
  | CursorT(OnText(_), Parenthesized(_) | List(_) | OpSeq(_, _)) =>
    /* invalid cursor position */
    no_holes
  | ParenthesizedZ(zty1) => holes_zty(zty1, [0, ...rev_steps])
  | ListZ(zty1) => holes_zty(zty1, [0, ...rev_steps])
  | OpSeqZ(skel, zty0, surround) =>
    holes_OpSeqZ(
      ~holes_tm=holes_uty,
      ~holes_ztm=holes_zty,
      ~hole_desc=_ => TypHole,
      ~is_space=_ => false,
      ~path_before_tm=before_typ,
      ~rev_steps,
      ~erase=ZTyp.erase,
      skel,
      zty0,
      surround,
    )
  };
};

let rec holes_zpat = (zp: ZPat.t, rev_steps: rev_steps): zhole_list => {
  switch (zp) {
  | CursorP(Staging(_), _) => no_holes
  | CursorP(_, EmptyHole(u)) => {
      holes_before: [],
      hole_selected:
        Some((PatHole(u), (rev_steps |> List.rev, OnDelim(0, Before)))),
      holes_after: [],
    }
  | CursorP(_, Wild(_))
  | CursorP(_, Var(_, _, _))
  | CursorP(_, NumLit(_, _))
  | CursorP(_, BoolLit(_, _))
  | CursorP(_, ListNil(_)) => {
      holes_before: [],
      hole_selected: None,
      holes_after: [],
    }
  | CursorP(OnDelim(k, _), Parenthesized(p1))
  | CursorP(OnDelim(k, _), Inj(_, _, p1)) =>
    holes_Cursor_bracketed(holes_pat, k, p1, rev_steps)
  | CursorP(OnDelim(k, _), OpSeq(skel, seq)) =>
    holes_Cursor_OpSeq(
      ~holes_tm=holes_pat,
      ~hole_desc=u => PatHole(u),
      ~op_index=k,
      ~rev_steps,
      skel,
      seq,
    )
  | CursorP(OnText(_), Parenthesized(_) | Inj(_, _, _) | OpSeq(_, _)) =>
    /* invalid cursor position */
    no_holes
  | ParenthesizedZ(zp1) => holes_zpat(zp1, [0, ...rev_steps])
  | OpSeqZ(skel, zp1, surround) =>
    holes_OpSeqZ(
      ~holes_tm=holes_pat,
      ~holes_ztm=holes_zpat,
      ~hole_desc=u => PatHole(u),
      ~is_space=op => op == UHPat.Space,
      ~path_before_tm=before_pat,
      ~rev_steps,
      ~erase=ZPat.erase,
      skel,
      zp1,
      surround,
    )
  | InjZ(_, _, zp1) => holes_zpat(zp1, [0, ...rev_steps])
  };
};

let rec holes_zblock = (zblock: ZExp.zblock, rev_steps: rev_steps): zhole_list => {
  switch (zblock) {
  | BlockZL(zlines, e) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zlines(zlines, rev_steps);
    let holes_e = holes_exp(e, [ZList.length(zlines), ...rev_steps], []);
    {holes_before, hole_selected, holes_after: holes_after @ holes_e};
  | BlockZE(lines, ze) =>
    let {holes_before, hole_selected, holes_after} =
      holes_ze(ze, [List.length(lines), ...rev_steps]);
    let holes_lines = holes_lines(lines, 0, rev_steps, []);
    {holes_before: holes_lines @ holes_before, hole_selected, holes_after};
  };
}
and holes_zlines =
    ((prefix, zline, suffix): ZExp.zlines, rev_steps: rev_steps): zhole_list => {
  let holes_prefix = holes_lines(prefix, 0, rev_steps, []);
  let {holes_before, hole_selected, holes_after} =
    holes_zline(zline, [List.length(prefix), ...rev_steps]);
  let holes_suffix =
    holes_lines(suffix, List.length(prefix) + 1, rev_steps, []);
  {
    holes_before: holes_prefix @ holes_before,
    hole_selected,
    holes_after: holes_after @ holes_suffix,
  };
}
and holes_zline = (zli: ZExp.zline, rev_steps: rev_steps): zhole_list =>
  switch (zli) {
  | CursorL(Staging(_), _) => no_holes
  | CursorL(_, EmptyLine) => no_holes
  | CursorL(_, ExpLine(_)) => no_holes /* invalid cursor position */
  | CursorL(cursor, LetLine(p, ann, block)) =>
    let holes_p = holes_pat(p, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) => holes_uty(uty, [1, ...rev_steps], [])
      };
    let holes_block = holes_block(block, [2, ...rev_steps], []);
    switch (cursor) {
    | OnDelim(0, _) => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_p @ holes_ann @ holes_block,
      }
    | OnDelim(1, _) => {
        holes_before: holes_p,
        hole_selected: None,
        holes_after: holes_ann @ holes_block,
      }
    | OnDelim(2, _) => {
        holes_before: holes_p @ holes_ann,
        hole_selected: None,
        holes_after: holes_block,
      }
    | OnDelim(3, _) => {
        holes_before: holes_p @ holes_ann @ holes_block,
        hole_selected: None,
        holes_after: [],
      }
    | _ => no_holes
    };
  | ExpLineZ(ze1) => holes_ze(ze1, rev_steps)
  | LetLineZP(zp, ann, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, ...rev_steps]);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...rev_steps], [])
      | None => []
      };
    let holes_block = holes_block(block, [2, ...rev_steps], []);
    {
      holes_before,
      hole_selected,
      holes_after: holes_after @ holes_ann @ holes_block,
    };
  | LetLineZA(p, zann, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zty(zann, [1, ...rev_steps]);
    let holes_p = holes_pat(p, [0, ...rev_steps], []);
    let holes_block = holes_block(block, [2, ...rev_steps], []);
    {
      holes_before: holes_p @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_block,
    };
  | LetLineZE(p, ann, zblock) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [2, ...rev_steps]);
    let holes_p = holes_pat(p, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...rev_steps], [])
      | None => []
      };
    {
      holes_before: holes_p @ holes_ann @ holes_before,
      hole_selected,
      holes_after,
    };
  }
and holes_ze = (ze: ZExp.t, rev_steps: rev_steps): zhole_list =>
  switch (ze) {
  | CursorE(Staging(_), _) => no_holes
  | CursorE(_, EmptyHole(u)) => {
      holes_before: [],
      hole_selected:
        Some((ExpHole(u), (rev_steps |> List.rev, OnDelim(0, Before)))),
      holes_after: [],
    }
  | CursorE(_, Var(_, _, _))
  | CursorE(_, NumLit(_, _))
  | CursorE(_, BoolLit(_, _))
  | CursorE(_, ListNil(_)) => {
      holes_before: [],
      hole_selected: None,
      holes_after: [],
    }
  | CursorE(OnDelim(k, _), Inj(_, _, block))
  | CursorE(OnDelim(k, _), Parenthesized(block)) =>
    holes_Cursor_bracketed(holes_block, k, block, rev_steps)
  | CursorE(OnDelim(k, _), Lam(_, p, ann, block)) =>
    let holes_p = holes_pat(p, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) => holes_uty(uty, [1, ...rev_steps], [])
      };
    let holes_block = holes_block(block, [2, ...rev_steps], []);
    switch (k) {
    | 0 => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_p @ holes_ann @ holes_block,
      }
    | 1 => {
        holes_before: holes_p,
        hole_selected: None,
        holes_after: holes_ann @ holes_block,
      }
    | 2 => {
        holes_before: holes_p @ holes_ann,
        hole_selected: None,
        holes_after: holes_block,
      }
    | _ => no_holes
    };
  | CursorE(OnDelim(k, _), Case(_, block, rules, ann)) =>
    let holes_block = holes_block(block, [0, ...rev_steps], []);
    let holes_rules = holes_rules(rules, 1, rev_steps, []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) =>
        holes_uty(uty, [List.length(rules) + 1, ...rev_steps], [])
      };
    switch (k) {
    | 0 => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_block @ holes_rules @ holes_ann,
      }
    | 1 => {
        holes_before: holes_block @ holes_rules,
        hole_selected: None,
        holes_after: holes_ann,
      }
    | _ => no_holes
    };
  | CursorE(OnDelim(k, _), OpSeq(skel, seq)) =>
    holes_Cursor_OpSeq(
      ~holes_tm=holes_exp,
      ~hole_desc=u => ExpHole(u),
      ~op_index=k,
      ~rev_steps,
      skel,
      seq,
    )
  | CursorE(
      OnText(_),
      Inj(_, _, _) | Parenthesized(_) | Lam(_, _, _, _) | Case(_, _, _, _) |
      OpSeq(_, _),
    ) =>
    /* invalid cursor position */
    no_holes
  | CursorE(_, ApPalette(_, _, _, _)) => no_holes /* TODO[livelits] */
  | OpSeqZ(skel, ze0, surround) =>
    holes_OpSeqZ(
      ~holes_tm=holes_exp,
      ~holes_ztm=holes_ze,
      ~hole_desc=u => ExpHole(u),
      ~is_space=op => op == UHExp.Space,
      ~path_before_tm=before_exp,
      ~rev_steps,
      ~erase=ZExp.erase,
      skel,
      ze0,
      surround,
    )
  | ParenthesizedZ(zblock) => holes_zblock(zblock, [0, ...rev_steps])
  | LamZP(_, zp, ann, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, ...rev_steps]);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...rev_steps], [])
      | None => []
      };
    let holes_block = holes_block(block, [2, ...rev_steps], []);
    {
      holes_before,
      hole_selected,
      holes_after: holes_after @ holes_ann @ holes_block,
    };
  | LamZA(_, p, zann, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zty(zann, [1, ...rev_steps]);
    let holes_p = holes_pat(p, [0, ...rev_steps], []);
    let holes_block = holes_block(block, [2, ...rev_steps], []);
    {
      holes_before: holes_p @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_block,
    };
  | LamZE(_, p, ann, zblock) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [2, ...rev_steps]);
    let holes_p = holes_pat(p, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...rev_steps], [])
      | None => []
      };
    {
      holes_before: holes_p @ holes_ann @ holes_before,
      hole_selected,
      holes_after,
    };
  | InjZ(_, _, zblock) => holes_zblock(zblock, [0, ...rev_steps])
  | CaseZE(_, zblock, rules, ann) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [0, ...rev_steps]);
    let holes_rules = holes_rules(rules, 1, rev_steps, []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) =>
        holes_uty(uty, [List.length(rules) + 1, ...rev_steps], [])
      };
    {
      holes_before,
      hole_selected,
      holes_after: holes_after @ holes_rules @ holes_ann,
    };
  | CaseZR(_, block, zrules, ann) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zrules(zrules, 1, rev_steps);
    let holes_block = holes_block(block, [0, ...rev_steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) =>
        holes_uty(uty, [ZList.length(zrules) + 1, ...rev_steps], [])
      };
    {
      holes_before: holes_block @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_ann,
    };
  | CaseZA(_, block, rules, zann) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zty(zann, [List.length(rules) + 1, ...rev_steps]);
    let holes_block = holes_block(block, [0, ...rev_steps], []);
    let holes_rules = holes_rules(rules, 1, rev_steps, []);
    {
      holes_before: holes_block @ holes_rules @ holes_before,
      hole_selected,
      holes_after,
    };
  | ApPaletteZ(_, _, _, zpsi) =>
    let zsplice_map = zpsi.zsplice_map;
    let (n, (_, zblock)) = ZNatMap.prj_z_kv(zsplice_map);
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [n, ...rev_steps]);
    let splice_order = zpsi.splice_order;
    let splice_map = ZNatMap.prj_map(zsplice_map);
    let (splices_before, splices_after) =
      GeneralUtil.split_at(splice_order, n);
    let holes_splices_before =
      List.fold_left(
        (holes, n) =>
          switch (NatMap.lookup(splice_map, n)) {
          | None => holes
          | Some((_, block)) =>
            holes @ holes_block(block, [n, ...rev_steps], [])
          },
        [],
        splices_before,
      );
    let holes_splices_after =
      List.fold_left(
        (holes, n) =>
          switch (NatMap.lookup(splice_map, n)) {
          | None => holes
          | Some((_, block)) =>
            holes @ holes_block(block, [n, ...rev_steps], [])
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
and holes_zrules = (zrules: ZExp.zrules, offset: int, rev_steps: rev_steps) => {
  let (prefix, zrule, suffix) = ZList.prj(zrules);
  let holes_prefix = holes_rules(prefix, offset, rev_steps, []);
  let prefix_len = List.length(prefix);
  let {holes_before, hole_selected, holes_after} =
    holes_zrule(zrule, offset, prefix_len, rev_steps);
  let holes_suffix =
    holes_rules(suffix, offset + 1 + prefix_len, rev_steps, []);
  {
    holes_before: holes_prefix @ holes_before,
    hole_selected,
    holes_after: holes_after @ holes_suffix,
  };
}
and holes_zrule =
    (zrule: ZExp.zrule, offset: int, prefix_len: int, rev_steps: rev_steps) =>
  switch (zrule) {
  | CursorR(Staging(_), _) => no_holes
  | CursorR(OnDelim(k, _), Rule(p, block)) =>
    let holes_p = holes_pat(p, [1, prefix_len + offset, ...rev_steps], []);
    let holes_block =
      holes_block(block, [0, prefix_len + offset, ...rev_steps], []);
    switch (k) {
    | 0 => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_p @ holes_block,
      }
    | 1 => {
        holes_before: holes_p,
        hole_selected: None,
        holes_after: holes_block,
      }
    | _ => no_holes
    };
  | CursorR(OnText(_), _) =>
    /* invalid cursor position */
    no_holes
  | RuleZP(zp, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, prefix_len + offset, ...rev_steps]);
    let holes_block =
      holes_block(block, [1, prefix_len + offset, ...rev_steps], []);
    {holes_before, hole_selected, holes_after: holes_after @ holes_block};
  | RuleZE(p, zblock) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [1, prefix_len + offset, ...rev_steps]);
    let holes_p = holes_pat(p, [0, prefix_len + offset, ...rev_steps], []);
    {holes_before: holes_p @ holes_before, hole_selected, holes_after};
  };

let path_to_hole = (hole_list: hole_list, u: MetaVar.t): option(t) =>
  switch (
    List.find_opt(
      ((hole_desc, _)) =>
        switch (hole_desc) {
        | TypeVarHole(u')
        | TypeForallHole(u')
        | ExpHole(u')
        | PatHole(u')
        | VHole(u') => MetaVar.eq(u, u')
        | TypHole => false
        },
      hole_list,
    )
  ) {
  | None => None
  | Some((_, path)) => Some(path)
  };

let path_to_hole_z = (zhole_list: zhole_list, u: MetaVar.t): option(t) => {
  let {holes_before, hole_selected, holes_after} = zhole_list;
  switch (path_to_hole(holes_before, u)) {
  | Some(_) as path => path
  | None =>
    switch (hole_selected) {
    | Some((VHole(u'), path))
    | Some((ExpHole(u'), path))
    | Some((PatHole(u'), path)) =>
      MetaVar.eq(u, u') ? Some(path) : path_to_hole(holes_after, u)
    | Some((TypHole, _))
    | None => path_to_hole(holes_after, u)
    }
  };
};

let opt_steps_to_opt_path =
    (cursor_position: cursor_position, opt_steps: option(steps)): option(t) =>
  switch (opt_steps) {
  | None => None
  | Some(steps) => Some((List.rev(steps), cursor_position))
  };

let next_hole_path = (zhole_list: zhole_list): option(t) => {
  switch (zhole_list.holes_before, zhole_list.holes_after) {
  | ([], []) => None
  | (_, [(_, path), ..._]) => Some(path)
  | ([(_, path), ..._], _) => Some(path)
  };
};

let prev_hole_path = (zhole_list: zhole_list): option(t) => {
  switch (
    zhole_list.holes_before |> List.rev,
    zhole_list.holes_after |> List.rev,
  ) {
  | ([], []) => None
  | ([(_, path), ..._], _) => Some(path)
  | ([], [(_, path), ..._]) => Some(path)
  };
};

let prev_hole_path_zblock = (zblock: ZExp.zblock): option(t) => {
  let holes = holes_zblock(zblock, []);
  prev_hole_path(holes);
};

let next_hole_path_zblock = (zblock: ZExp.zblock): option(t) => {
  let holes = holes_zblock(zblock, []);
  next_hole_path(holes);
};

let rec is_prefix_of = (steps, prefix) =>
  switch (prefix, steps) {
  | ([], _) => true
  | ([_, ..._], []) => false
  | ([prefix_first, ...prefix_rest], [steps_first, ...steps_rest]) =>
    prefix_first == steps_first && prefix_rest |> is_prefix_of(steps_rest)
  };

let rec compare_steps = (steps1, steps2) =>
  switch (steps1, steps2) {
  | ([], []) => 0
  | ([], [_, ..._]) => (-1)
  | ([_, ..._], []) => 1
  | ([step1, ...rest1], [step2, ...rest2]) =>
    if (step1 > step2) {
      1;
    } else if (step1 < step2) {
      (-1);
    } else {
      compare_steps(rest1, rest2);
    }
  };

module StepsMap =
  Map.Make({
    type t = steps;
    let compare = compare_steps;
  });

let rec prune_trivial_suffix_block =
        (~steps_of_first_line, UHExp.Block(leading, conclusion) as block) =>
  switch (steps_of_first_line) {
  | [] => block // should never happen
  | [x] =>
    switch (x == (leading |> List.length), conclusion) {
    | (true, _) => block
    | (false, EmptyHole(_)) =>
      let (prefix, suffix) = leading |> partition_i((i, _) => i < x);
      let (pruned_suffix, _) =
        List.fold_right(
          (line, (pruned_suffix_so_far, stop_pruning)) =>
            switch (stop_pruning, line) {
            | (false, UHExp.EmptyLine) => (pruned_suffix_so_far, false)
            | (_, _) => ([line, ...pruned_suffix_so_far], true)
            },
          suffix,
          ([], false),
        );
      Block(prefix @ pruned_suffix, conclusion);
    | (false, _) => block
    }
  | [x, ...xs] =>
    switch (leading |> ZList.split_at(x)) {
    | None =>
      Block(
        leading,
        prune_trivial_suffix_block__exp(~steps_of_first_line=xs, conclusion),
      )
    | Some((prefix, line, suffix)) =>
      Block(
        prefix
        @ [prune_trivial_suffix_block__line(~steps_of_first_line=xs, line)]
        @ suffix,
        conclusion,
      )
    }
  }
and prune_trivial_suffix_block__line = (~steps_of_first_line, line) =>
  switch (line, steps_of_first_line) {
  | (EmptyLine, _)
  | (_, []) => line
  | (ExpLine(e), _) =>
    ExpLine(prune_trivial_suffix_block__exp(~steps_of_first_line, e))
  | (LetLine(p, ann, def), [2, ...xs]) =>
    LetLine(p, ann, prune_trivial_suffix_block(~steps_of_first_line=xs, def))
  | (LetLine(_, _, _), [_, ..._]) => line
  }
and prune_trivial_suffix_block__exp = (~steps_of_first_line, e) =>
  switch (e, steps_of_first_line) {
  | (
      EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) | ListNil(_) |
      ApPalette(_, _, _, _),
      _,
    )
  | (_, []) => e
  | (Lam(err_status, arg, ann, body), [2, ...xs]) =>
    Lam(
      err_status,
      arg,
      ann,
      prune_trivial_suffix_block(~steps_of_first_line=xs, body),
    )
  | (Inj(err_status, side, body), [0, ...xs]) =>
    Inj(
      err_status,
      side,
      prune_trivial_suffix_block(~steps_of_first_line=xs, body),
    )
  | (Parenthesized(body), [0, ...xs]) =>
    Parenthesized(prune_trivial_suffix_block(~steps_of_first_line=xs, body))
  | (Case(err_status, scrut, rules, ann), [x, ...xs]) =>
    switch (x == 0, rules |> ZList.split_at(x - 1)) {
    | (true, _) =>
      Case(
        err_status,
        prune_trivial_suffix_block(~steps_of_first_line=xs, scrut),
        rules,
        ann,
      )
    | (false, Some((prefix, rule, suffix))) =>
      Case(
        err_status,
        scrut,
        prefix
        @ [prune_trivial_suffix_block__rule(~steps_of_first_line=xs, rule)]
        @ suffix,
        ann,
      )
    | (false, None) => e
    }
  | (OpSeq(skel, seq), [x, ...xs]) =>
    switch (seq |> OperatorSeq.nth_tm(x)) {
    | None => e
    | Some(tm) =>
      let seq =
        OperatorSeq.seq_update_nth(
          x,
          seq,
          prune_trivial_suffix_block__exp(~steps_of_first_line=xs, tm),
        )
        |> Opt.get(() => assert(false));
      OpSeq(skel, seq);
    }
  | (Lam(_, _, _, _) | Inj(_, _, _) | Parenthesized(_), [_, ..._]) => e
  }
and prune_trivial_suffix_block__rule =
    (~steps_of_first_line, Rule(p, clause) as rule) =>
  switch (steps_of_first_line) {
  | [2, ...xs] =>
    Rule(p, prune_trivial_suffix_block(~steps_of_first_line=xs, clause))
  | _ => rule
  };
