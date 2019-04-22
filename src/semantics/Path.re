open Sexplib.Std;
open SemanticsCommon;
open GeneralUtil;

[@deriving (show({with_path: false}), sexp)]
type steps = list(int);

let string_of_steps = GeneralUtil.string_of_list(string_of_int);

[@deriving (show({with_path: false}), sexp)]
type t = (steps, cursor_pos);

let cons' = (step: int, r: t): t => {
  let (steps, side) = r;
  ([step, ...steps], side);
};

let rec _update_cursor = (cursor: cursor_pos, steps: steps): t =>
  switch (steps) {
  | [] => ([], cursor)
  | [x, ...xs] => cons'(x, _update_cursor(cursor, xs))
  };
let update_cursor = (cursor: cursor_pos, (steps, _): t): t =>
  _update_cursor(cursor, steps);

let rec of_ztyp = (zty: ZTyp.t): t =>
  switch (zty) {
  | CursorTO(outer_cursor, _) => ([], O(outer_cursor))
  | CursorTI(inner_cursor, _) => ([], I(inner_cursor))
  | ParenthesizedZ(zty1) => cons'(0, of_ztyp(zty1))
  | ListZ(zty1) => cons'(0, of_ztyp(zty1))
  | OpSeqZ(_, zty1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_ztyp(zty1));
  };

let rec of_zpat = (zp: ZPat.t): t =>
  switch (zp) {
  | CursorPO(outer_cursor, _) => ([], O(outer_cursor))
  | CursorPI(inner_cursor, _) => ([], I(inner_cursor))
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
  | CursorLO(outer_cursor, _) => ([], O(outer_cursor))
  | CursorLI(inner_cursor, _) => ([], I(inner_cursor))
  | ExpLineZ(ze) => of_zexp(ze)
  | LetLineZP(zp, _, _) => cons'(0, of_zpat(zp))
  | LetLineZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | LetLineZE(_, _, zblock) => cons'(2, of_zblock(zblock))
  }
and of_zexp = (ze: ZExp.t): t =>
  switch (ze) {
  | CursorEO(outer_cursor, _) => ([], O(outer_cursor))
  | CursorEI(inner_cursor, _) => ([], I(inner_cursor))
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
  | CursorR(inner_cursor, _) => ([], I(inner_cursor))
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

let rec follow_ty_and_place_cursor =
        (steps: steps, place_cursor: UHTyp.t => option(ZTyp.t), uty: UHTyp.t)
        : option(ZTyp.t) =>
  switch (steps) {
  | [] => place_cursor(uty)
  | [x, ...xs] =>
    switch (uty) {
    | TO(Hole)
    | TO(Unit)
    | TO(Num)
    | TO(Bool) => None
    | TI(Parenthesized(uty1)) =>
      switch (x) {
      | 0 =>
        switch (follow_ty_and_place_cursor(xs, place_cursor, uty1)) {
        | None => None
        | Some(zty) => Some(ParenthesizedZ(zty))
        }
      | _ => None
      }
    | TI(List(uty1)) =>
      switch (x) {
      | 0 =>
        switch (follow_ty_and_place_cursor(xs, place_cursor, uty1)) {
        | None => None
        | Some(zty) => Some(ListZ(zty))
        }
      | _ => None
      }
    | TI(OpSeq(skel, seq)) =>
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
    | (_, PO(_)) => None /* outer nodes have no children */
    | (0, PI(Parenthesized(p1))) =>
      switch (follow_pat_and_place_cursor(xs, place_cursor, p1)) {
      | None => None
      | Some(zp1) => Some(ParenthesizedZ(zp1))
      }
    | (_, PI(Parenthesized(_))) => None
    | (_, PI(OpSeq(skel, seq))) =>
      switch (OperatorSeq.split(x, seq)) {
      | None => None
      | Some((p, surround)) =>
        switch (follow_pat_and_place_cursor(xs, place_cursor, p)) {
        | None => None
        | Some(zp) => Some(OpSeqZ(skel, zp, surround))
        }
      }
    | (0, PI(Inj(err, side, p1))) =>
      switch (follow_pat_and_place_cursor(xs, place_cursor, p1)) {
      | None => None
      | Some(zp1) => Some(InjZ(err, side, zp1))
      }
    | (_, PI(Inj(_, _, _))) => None
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
  | (_, LO(EmptyLine)) => None
  | ([0, ...xs], LI(LetLine(p, ann, e1))) =>
    switch (follow_pat_and_place_cursor(xs, pcp, p)) {
    | None => None
    | Some(zp) => Some(LetLineZP(zp, ann, e1))
    }
  | ([1, ...xs], LI(LetLine(p, ann, e1))) =>
    switch (ann) {
    | None => None
    | Some(ann_ty) =>
      switch (follow_ty_and_place_cursor(xs, pct, ann_ty)) {
      | None => None
      | Some(zann) => Some(LetLineZA(p, zann, e1))
      }
    }
  | ([2, ...xs], LI(LetLine(p, ann, block))) =>
    switch (follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)) {
    | None => None
    | Some(zblock) => Some(LetLineZE(p, ann, zblock))
    }
  | (_, LI(LetLine(_, _, _))) => None
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
    | (_, EO(_)) => None
    | (0, EI(Parenthesized(block))) =>
      switch (
        follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
      ) {
      | None => None
      | Some(zblock) => Some(ParenthesizedZ(zblock))
      }
    | (_, EI(Parenthesized(_))) => None
    | (_, EI(OpSeq(skel, seq))) =>
      switch (OperatorSeq.split(x, seq)) {
      | Some((e, surround)) =>
        switch (follow_exp_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, e)) {
        | Some(ze) => Some(OpSeqZ(skel, ze, surround))
        | None => None
        }
      | None => None
      }
    | (0, EI(Lam(err, p, ann, block))) =>
      switch (follow_pat_and_place_cursor(xs, pcp, p)) {
      | None => None
      | Some(zp) => Some(LamZP(err, zp, ann, block))
      }
    | (1, EI(Lam(err, p, ann, block))) =>
      switch (ann) {
      | None => None
      | Some(ann_ty) =>
        switch (follow_ty_and_place_cursor(xs, pct, ann_ty)) {
        | None => None
        | Some(zann) => Some(LamZA(err, p, zann, block))
        }
      }
    | (2, EI(Lam(err, p, ann, block))) =>
      switch (
        follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
      ) {
      | None => None
      | Some(zblock) => Some(LamZE(err, p, ann, zblock))
      }
    | (_, EI(Lam(_, _, _, _))) => None
    | (0, EI(Inj(err, side, block))) =>
      switch (
        follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
      ) {
      | None => None
      | Some(zblock) => Some(InjZ(err, side, zblock))
      }
    | (_, EI(Inj(_, _, _))) => None
    | (0, EI(Case(err, block, rules, ann))) =>
      switch (
        follow_block_and_place_cursor(xs, pcl, pce, pcr, pcp, pct, block)
      ) {
      | None => None
      | Some(zblock) => Some(CaseZE(err, zblock, rules, ann))
      }
    | (x, EI(Case(err, block, rules, ann)))
        when x === List.length(rules) + 1 =>
      switch (ann) {
      | None => None
      | Some(ty) =>
        switch (follow_ty_and_place_cursor(xs, pct, ty)) {
        | None => None
        | Some(zann) => Some(CaseZA(err, block, rules, zann))
        }
      }
    | (x, EI(Case(err, block, rules, ann))) =>
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
    | (n, EI(ApPalette(_, name, serialized_model, splice_info))) =>
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

exception UHBlockNodeNotFound(t, UHExp.block);
let follow_block_or_fail = (path: t, block: UHExp.block): ZExp.zblock =>
  switch (follow_block(path, block)) {
  | None => raise(UHBlockNodeNotFound(path, block))
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
  | TypeHole
  | ExpHole(MetaVar.t)
  | PatHole(MetaVar.t);

let string_of_hole_desc =
  fun
  | TypeHole => "TypeHole"
  | ExpHole(u) => "ExpHole(" ++ string_of_int(u) ++ ")"
  | PatHole(u) => "PatHole(" ++ string_of_int(u) ++ ")";

type hole_list = list((hole_desc, steps));

let string_of_hole_list = (hole_list: hole_list): string =>
  GeneralUtil.string_of_list(
    GeneralUtil.string_of_pair(string_of_hole_desc, string_of_steps),
    hole_list,
  );

let rec holes_seq =
        (
          seq: OperatorSeq.opseq('tm, 'op),
          holes_tm: ('tm, steps, hole_list) => hole_list,
          offset: int,
          steps: steps,
          holes: hole_list,
        )
        : hole_list =>
  switch (seq) {
  | ExpOpExp(e1, _, e2) =>
    let holes = holes_tm(e2, [offset + 1, ...steps], holes);
    holes_tm(e1, [offset, ...steps], holes);
  | SeqOpExp(seq1, _, e2) =>
    let holes =
      holes_tm(
        e2,
        [offset + OperatorSeq.seq_length(seq1), ...steps],
        holes,
      );
    holes_seq(seq1, holes_tm, offset, steps, holes);
  };

let rec holes_uty = (uty: UHTyp.t, steps: steps, holes: hole_list): hole_list =>
  switch (uty) {
  | TO(Hole) => [(TypeHole, steps), ...holes]
  | TO(Unit) => holes
  | TO(Num) => holes
  | TO(Bool) => holes
  | TI(Parenthesized(uty1)) => holes_uty(uty1, [0, ...steps], holes)
  | TI(List(uty1)) => holes_uty(uty1, [0, ...steps], holes)
  | TI(OpSeq(_, seq)) => holes_seq(seq, holes_uty, 0, steps, holes)
  };

let rec holes_pat = (p: UHPat.t, steps: steps, holes: hole_list): hole_list =>
  switch (p) {
  | PO(EmptyHole(u)) => [(PatHole(u), steps), ...holes]
  | PO(Wild(_)) => holes
  | PO(Var(_, _, _)) => holes
  | PO(NumLit(_, _)) => holes
  | PO(BoolLit(_, _)) => holes
  | PO(ListNil(_)) => holes
  | PI(Parenthesized(p1)) => holes_pat(p1, [0, ...steps], holes)
  | PI(OpSeq(_, seq)) => holes_seq(seq, holes_pat, 0, steps, holes)
  | PI(Inj(_, _, p1)) => holes_pat(p1, [0, ...steps], holes)
  };

let rec holes_block =
        (Block(lines, e): UHExp.block, steps: steps, holes: hole_list)
        : hole_list => {
  let len = List.length(lines);
  let holes = holes_exp(e, [len, ...steps], holes);
  holes_lines(lines, 0, steps, holes);
}
and holes_lines =
    (lines: UHExp.lines, offset: int, steps: steps, holes: hole_list)
    : hole_list =>
  fold_right_i(
    ((i, line), holes) => holes_line(line, [i + offset, ...steps], holes),
    lines,
    holes,
  )
and holes_line = (line: UHExp.line, steps: steps, holes: hole_list): hole_list =>
  switch (line) {
  | LO(EmptyLine) => holes
  | LI(LetLine(p, ann, block)) =>
    let holes = holes_block(block, [2, ...steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...steps], holes);
  | ExpLine(e) => holes_exp(e, [0, ...steps], holes)
  }
and holes_exp = (e: UHExp.t, steps: steps, holes: hole_list): hole_list =>
  switch (e) {
  | EO(EmptyHole(u)) => [(ExpHole(u), steps), ...holes]
  | EO(Var(_, _, _)) => holes
  | EO(NumLit(_, _)) => holes
  | EO(BoolLit(_, _)) => holes
  | EO(ListNil(_)) => holes
  | EI(Parenthesized(block)) => holes_block(block, [0, ...steps], holes)
  | EI(OpSeq(_, seq)) => holes_seq(seq, holes_exp, 0, steps, holes)
  | EI(Inj(_, _, block)) => holes_block(block, [0, ...steps], holes)
  | EI(Lam(_, p, ann, block)) =>
    let holes = holes_block(block, [2, ...steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...steps], holes);
  | EI(Case(_, block, rules, ann)) =>
    let holes =
      switch (ann) {
      | None => holes
      | Some(uty) =>
        holes_uty(uty, [List.length(rules) + 1, ...steps], holes)
      };
    let holes = holes_rules(rules, 1, steps, holes);
    holes_block(block, [0, ...steps], holes);
  | EI(ApPalette(_, _, _, psi)) =>
    let splice_map = psi.splice_map;
    let splice_order = psi.splice_order;
    List.fold_right(
      (i, holes) =>
        switch (NatMap.lookup(splice_map, i)) {
        | None => holes
        | Some((_, block)) => holes_block(block, [i, ...steps], holes)
        },
      splice_order,
      holes,
    );
  }
and holes_rules =
    (rules: UHExp.rules, offset: int, steps: steps, holes: hole_list)
    : hole_list =>
  fold_right_i(
    ((i, rule), holes) => holes_rule(rule, [i + offset, ...steps], holes),
    rules,
    holes,
  )
and holes_rule =
    (Rule(p, block): UHExp.rule, steps: steps, holes: hole_list): hole_list => {
  let holes = holes_block(block, [1, ...steps], holes);
  holes_pat(p, [0, ...steps], holes);
};

/* two hole lists, one for before the cursor, one for after */
type zhole_list = {
  holes_before: hole_list,
  hole_selected: option((hole_desc, steps)),
  holes_after: hole_list,
};

let string_of_zhole_list = ({holes_before, hole_selected, holes_after}) =>
  "{ holes_before: "
  ++ string_of_hole_list(holes_before)
  ++ ", \n"
  ++ "  hole_selected: "
  ++ GeneralUtil.string_of_opt(
       GeneralUtil.string_of_pair(string_of_hole_desc, string_of_steps),
       hole_selected,
     )
  ++ ", \n"
  ++ "  holes_after: "
  ++ string_of_hole_list(holes_after)
  ++ "}\n";

let no_holes = {holes_before: [], hole_selected: None, holes_after: []};

let holes_prefix =
    (
      holes_fn: ('tm, steps, hole_list) => hole_list,
      prefix: OperatorSeq.opseq_prefix('tm, 'op),
      steps: steps,
    )
    : hole_list =>
  switch (prefix) {
  | ExpPrefix(e, _) => holes_fn(e, [0, ...steps], [])
  | SeqPrefix(seq, _) => holes_seq(seq, holes_fn, 0, steps, [])
  };

let holes_suffix =
    (
      holes_fn: ('tm, steps, hole_list) => hole_list,
      suffix: OperatorSeq.opseq_suffix('tm, 'op),
      prefix_len: int,
      steps: steps,
    )
    : hole_list =>
  switch (suffix) {
  | ExpSuffix(_, e) => holes_fn(e, [prefix_len + 1, ...steps], [])
  | SeqSuffix(_, seq) => holes_seq(seq, holes_fn, prefix_len + 1, steps, [])
  };

let holes_surround =
    (
      holes_fn: ('tm, steps, hole_list) => hole_list,
      surround: OperatorSeq.opseq_surround('tm, 'op),
      steps: steps,
    )
    : (hole_list, hole_list) =>
  switch (surround) {
  | EmptyPrefix(suffix) => ([], holes_suffix(holes_fn, suffix, 0, steps))
  | EmptySuffix(prefix) => (holes_prefix(holes_fn, prefix, steps), [])
  | BothNonEmpty(prefix, suffix) =>
    let prefix_len = OperatorSeq.prefix_length(prefix);
    (
      holes_prefix(holes_fn, prefix, steps),
      holes_suffix(holes_fn, suffix, prefix_len, steps),
    );
  };

let holes_OpSeqZ =
    (
      holes_fn: ('tm, steps, hole_list) => hole_list,
      zholes_fn: ('ztm, steps) => zhole_list,
      z0: 'ztm,
      surround: OperatorSeq.opseq_surround('tm, 'op),
      steps: steps,
    )
    : zhole_list => {
  let (holes_prefix, holes_suffix) =
    holes_surround(holes_fn, surround, steps);
  let prefix_len = OperatorSeq.surround_prefix_length(surround);
  let {holes_before, hole_selected, holes_after} =
    zholes_fn(z0, [prefix_len, ...steps]);
  let holes_before = holes_prefix @ holes_before;
  let holes_after = holes_after @ holes_suffix;
  {holes_before, hole_selected, holes_after};
};

let holes_Cursor_bracketed =
    (
      holes_fn: ('t, steps, hole_list) => hole_list,
      inner_cursor: inner_cursor,
      bracketed_t: 't,
      steps: steps,
    )
    : zhole_list => {
  let holes_bracketed_t = holes_fn(bracketed_t, [0, ...steps], []);
  switch (inner_cursor) {
  | ClosingDelimiter(_) => {
      holes_before: holes_bracketed_t,
      hole_selected: None,
      holes_after: [],
    }
  | BeforeChild(0, _) => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_bracketed_t,
    }
  | BeforeChild(_, _) => no_holes
  };
};

let holes_Cursor_OpSeq =
    (
      holes_fn: ('tm, steps, hole_list) => hole_list,
      inner_cursor: inner_cursor,
      skel: Skel.t('op),
      seq: OperatorSeq.opseq('tm, 'op),
      raise_inconsistent:
        (Skel.t('op), OperatorSeq.opseq('tm, 'op)) => zhole_list,
      steps: steps,
    )
    : zhole_list =>
  switch (inner_cursor) {
  | ClosingDelimiter(_) => no_holes
  | BeforeChild(k, _) when k > 0 && k <= OperatorSeq.seq_length(seq) - 1 =>
    switch (OperatorSeq.split(k, seq)) {
    | None => raise_inconsistent(skel, seq)
    | Some((tm_k, surround)) =>
      let (holes_prefix, holes_suffix) =
        holes_surround(holes_fn, surround, steps);
      {
        holes_before: holes_prefix,
        hole_selected: None,
        holes_after: holes_fn(tm_k, [k, ...steps], []) @ holes_suffix,
      };
    }
  | BeforeChild(_, _) => no_holes
  };

let rec holes_zty = (zty: ZTyp.t, steps: steps): zhole_list =>
  switch (zty) {
  | CursorTO(_, utyo) => {
      holes_before: [],
      hole_selected:
        switch (utyo) {
        | Hole => Some((TypeHole, steps))
        | _ => None
        },
      holes_after: [],
    }
  | CursorTI(inner_cursor, utyi) =>
    switch (utyi) {
    | Parenthesized(uty1)
    | List(uty1) =>
      holes_Cursor_bracketed(holes_uty, inner_cursor, uty1, steps)
    | OpSeq(skel, seq) =>
      holes_Cursor_OpSeq(
        holes_uty,
        inner_cursor,
        skel,
        seq,
        (skel, seq) => raise(UHTyp.SkelInconsistentWithOpSeq(skel, seq)),
        steps,
      )
    }
  | ParenthesizedZ(zty1) => holes_zty(zty1, [0, ...steps])
  | ListZ(zty1) => holes_zty(zty1, [0, ...steps])
  | OpSeqZ(_, zty0, surround) =>
    holes_OpSeqZ(holes_uty, holes_zty, zty0, surround, steps)
  };

let rec holes_zpat = (zp: ZPat.t, steps: steps): zhole_list =>
  switch (zp) {
  | CursorPO(_, po) => {
      holes_before: [],
      hole_selected:
        switch (po) {
        | EmptyHole(u) => Some((PatHole(u), steps))
        | _ => None
        },
      holes_after: [],
    }
  | CursorPI(inner_cursor, pi) =>
    switch (pi) {
    | Parenthesized(p1)
    | Inj(_, _, p1) =>
      holes_Cursor_bracketed(holes_pat, inner_cursor, p1, steps)
    | OpSeq(skel, seq) =>
      holes_Cursor_OpSeq(
        holes_pat,
        inner_cursor,
        skel,
        seq,
        (skel, seq) => raise(UHPat.SkelInconsistentWithOpSeq(skel, seq)),
        steps,
      )
    }
  | ParenthesizedZ(zp1) => holes_zpat(zp1, [0, ...steps])
  | OpSeqZ(_, zp1, surround) =>
    holes_OpSeqZ(holes_pat, holes_zpat, zp1, surround, steps)
  | InjZ(_, _, zp1) => holes_zpat(zp1, [0, ...steps])
  };

let rec holes_zblock = (zblock: ZExp.zblock, steps: steps): zhole_list =>
  switch (zblock) {
  | BlockZL(zlines, e) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zlines(zlines, steps);
    let holes_e = holes_exp(e, [ZList.length(zlines), ...steps], []);
    {holes_before, hole_selected, holes_after: holes_after @ holes_e};
  | BlockZE(lines, ze) =>
    let {holes_before, hole_selected, holes_after} =
      holes_ze(ze, [List.length(lines), ...steps]);
    let holes_lines = holes_lines(lines, 0, steps, []);
    {holes_before: holes_lines @ holes_before, hole_selected, holes_after};
  }
and holes_zlines =
    ((prefix, zline, suffix): ZExp.zlines, steps: steps): zhole_list => {
  let holes_prefix = holes_lines(prefix, 0, steps, []);
  let {holes_before, hole_selected, holes_after} =
    holes_zline(zline, [List.length(prefix), ...steps]);
  let holes_suffix = holes_lines(suffix, List.length(prefix) + 1, steps, []);
  {
    holes_before: holes_prefix @ holes_before,
    hole_selected,
    holes_after: holes_after @ holes_suffix,
  };
}
and holes_zline = (zli: ZExp.zline, steps: steps): zhole_list =>
  switch (zli) {
  | CursorLO(_, EmptyLine) => no_holes
  | CursorLI(inner_cursor, LetLine(p, ann, block)) =>
    let holes_p = holes_pat(p, [0, ...steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) => holes_uty(uty, [1, ...steps], [])
      };
    let holes_block = holes_block(block, [2, ...steps], []);
    switch (inner_cursor) {
    | ClosingDelimiter(_) => no_holes
    | BeforeChild(0, _) => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_p @ holes_ann @ holes_block,
      }
    | BeforeChild(1, _) => {
        holes_before: holes_p,
        hole_selected: None,
        holes_after: holes_ann @ holes_block,
      }
    | BeforeChild(2, _) => {
        holes_before: holes_p @ holes_ann,
        hole_selected: None,
        holes_after: holes_block,
      }
    | BeforeChild(_, _) => no_holes
    };
  | ExpLineZ(ze1) => holes_ze(ze1, steps)
  | LetLineZP(zp, ann, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, ...steps]);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], [])
      | None => []
      };
    let holes_block = holes_block(block, [2, ...steps], []);
    {
      holes_before,
      hole_selected,
      holes_after: holes_after @ holes_ann @ holes_block,
    };
  | LetLineZA(p, zann, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zty(zann, [1, ...steps]);
    let holes_p = holes_pat(p, [0, ...steps], []);
    let holes_block = holes_block(block, [2, ...steps], []);
    {
      holes_before: holes_p @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_block,
    };
  | LetLineZE(p, ann, zblock) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [2, ...steps]);
    let holes_p = holes_pat(p, [0, ...steps], []);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], [])
      | None => []
      };
    {
      holes_before: holes_p @ holes_ann @ holes_before,
      hole_selected,
      holes_after,
    };
  }
and holes_ze = (ze: ZExp.t, steps: steps): zhole_list =>
  switch (ze) {
  | CursorEO(_, eo) => {
      holes_before: [],
      hole_selected:
        switch (eo) {
        | EmptyHole(u) => Some((ExpHole(u), steps))
        | _ => None
        },
      holes_after: [],
    }
  | CursorEI(inner_cursor, ei) =>
    switch (ei) {
    | Inj(_, _, block)
    | Parenthesized(block) =>
      holes_Cursor_bracketed(holes_block, inner_cursor, block, steps)
    | Lam(_, p, ann, block) =>
      let holes_p = holes_pat(p, [0, ...steps], []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) => holes_uty(uty, [1, ...steps], [])
        };
      let holes_block = holes_block(block, [2, ...steps], []);
      switch (inner_cursor) {
      | ClosingDelimiter(_) => no_holes
      | BeforeChild(0, _) => {
          holes_before: [],
          hole_selected: None,
          holes_after: holes_p @ holes_ann @ holes_block,
        }
      | BeforeChild(1, _) => {
          holes_before: holes_p,
          hole_selected: None,
          holes_after: holes_ann @ holes_block,
        }
      | BeforeChild(2, _) => {
          holes_before: holes_p @ holes_ann,
          hole_selected: None,
          holes_after: holes_block,
        }
      | BeforeChild(_, _) => no_holes
      };
    | Case(_, block, rules, ann) =>
      let holes_block = holes_block(block, [0, ...steps], []);
      let holes_rules = holes_rules(rules, 1, steps, []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) =>
          holes_uty(uty, [List.length(rules) + 1, ...steps], [])
        };
      switch (inner_cursor) {
      | ClosingDelimiter(_) => {
          holes_before: holes_block @ holes_rules,
          hole_selected: None,
          holes_after: [],
        }
      | BeforeChild(0, _) => {
          holes_before: [],
          hole_selected: None,
          holes_after: holes_block @ holes_rules @ holes_ann,
        }
      | BeforeChild(k, _) =>
        k === List.length(rules) + 1
          ? {
            holes_before: holes_block @ holes_rules,
            hole_selected: None,
            holes_after: holes_ann,
          }
          : no_holes
      };
    | OpSeq(skel, seq) =>
      holes_Cursor_OpSeq(
        holes_exp,
        inner_cursor,
        skel,
        seq,
        (skel, seq) => raise(UHExp.SkelInconsistentWithOpSeq(skel, seq)),
        steps,
      )
    | ApPalette(_, _, _, _) => no_holes /* TODO */
    }
  | OpSeqZ(_, ze0, surround) =>
    holes_OpSeqZ(holes_exp, holes_ze, ze0, surround, steps)
  | ParenthesizedZ(zblock) => holes_zblock(zblock, [0, ...steps])
  | LamZP(_, zp, ann, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, ...steps]);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], [])
      | None => []
      };
    let holes_block = holes_block(block, [2, ...steps], []);
    {
      holes_before,
      hole_selected,
      holes_after: holes_after @ holes_ann @ holes_block,
    };
  | LamZA(_, p, zann, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zty(zann, [1, ...steps]);
    let holes_p = holes_pat(p, [0, ...steps], []);
    let holes_block = holes_block(block, [2, ...steps], []);
    {
      holes_before: holes_p @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_block,
    };
  | LamZE(_, p, ann, zblock) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [2, ...steps]);
    let holes_p = holes_pat(p, [0, ...steps], []);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], [])
      | None => []
      };
    {
      holes_before: holes_p @ holes_ann @ holes_before,
      hole_selected,
      holes_after,
    };
  | InjZ(_, _, zblock) => holes_zblock(zblock, [0, ...steps])
  | CaseZE(_, zblock, rules, ann) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [0, ...steps]);
    let holes_rules = holes_rules(rules, 1, steps, []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) => holes_uty(uty, [List.length(rules) + 1, ...steps], [])
      };
    {
      holes_before,
      hole_selected,
      holes_after: holes_after @ holes_rules @ holes_ann,
    };
  | CaseZR(_, block, zrules, ann) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zrules(zrules, 1, steps);
    let holes_block = holes_block(block, [0, ...steps], []);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) =>
        holes_uty(uty, [ZList.length(zrules) + 1, ...steps], [])
      };
    {
      holes_before: holes_block @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_ann,
    };
  | CaseZA(_, block, rules, zann) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zty(zann, [List.length(rules) + 1, ...steps]);
    let holes_block = holes_block(block, [0, ...steps], []);
    let holes_rules = holes_rules(rules, 1, steps, []);
    {
      holes_before: holes_block @ holes_rules @ holes_before,
      hole_selected,
      holes_after,
    };
  | ApPaletteZ(_, _, _, zpsi) =>
    let zsplice_map = zpsi.zsplice_map;
    let (n, (_, zblock)) = ZNatMap.prj_z_kv(zsplice_map);
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [n, ...steps]);
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
            holes @ holes_block(block, [n, ...steps], [])
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
            holes @ holes_block(block, [n, ...steps], [])
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
and holes_zrules = (zrules: ZExp.zrules, offset: int, steps: steps) => {
  let (prefix, zrule, suffix) = ZList.prj(zrules);
  let holes_prefix = holes_rules(prefix, offset, steps, []);
  let prefix_len = List.length(prefix);
  let {holes_before, hole_selected, holes_after} =
    holes_zrule(zrule, offset, prefix_len, steps);
  let holes_suffix = holes_rules(suffix, offset + 1 + prefix_len, steps, []);
  {
    holes_before: holes_prefix @ holes_before,
    hole_selected,
    holes_after: holes_after @ holes_suffix,
  };
}
and holes_zrule =
    (zrule: ZExp.zrule, offset: int, prefix_len: int, steps: steps) =>
  switch (zrule) {
  | CursorR(inner_cursor, Rule(p, block)) =>
    let holes_p = holes_pat(p, [1, prefix_len + offset, ...steps], []);
    let holes_block =
      holes_block(block, [0, prefix_len + offset, ...steps], []);
    switch (inner_cursor) {
    | ClosingDelimiter(_) => no_holes
    | BeforeChild(0, _) => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_p @ holes_block,
      }
    | BeforeChild(1, _) => {
        holes_before: holes_p,
        hole_selected: None,
        holes_after: holes_block,
      }
    | BeforeChild(_, _) => no_holes
    };
  | RuleZP(zp, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, prefix_len + offset, ...steps]);
    let holes_block =
      holes_block(block, [1, prefix_len + offset, ...steps], []);
    {holes_before, hole_selected, holes_after: holes_after @ holes_block};
  | RuleZE(p, zblock) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [1, prefix_len + offset, ...steps]);
    let holes_p = holes_pat(p, [0, prefix_len + offset, ...steps], []);
    {holes_before: holes_p @ holes_before, hole_selected, holes_after};
  };

let steps_to_hole = (hole_list: hole_list, u: MetaVar.t): option(steps) =>
  switch (
    List.find_opt(
      ((hole_desc, _)) =>
        switch (hole_desc) {
        | ExpHole(u')
        | PatHole(u') => MetaVar.eq(u, u')
        | TypeHole => false
        },
      hole_list,
    )
  ) {
  | None => None
  | Some((_, path)) => Some(path)
  };

let steps_to_hole_z = (zhole_list: zhole_list, u: MetaVar.t): option(steps) => {
  let {holes_before, hole_selected, holes_after} = zhole_list;
  switch (steps_to_hole(holes_before, u)) {
  | Some(_) as steps => steps
  | None =>
    switch (hole_selected) {
    | Some((ExpHole(u'), steps))
    | Some((PatHole(u'), steps)) =>
      MetaVar.eq(u, u') ? Some(steps) : steps_to_hole(holes_after, u)
    | Some((TypeHole, _))
    | None => steps_to_hole(holes_after, u)
    }
  };
};

let opt_steps_to_opt_path =
    (cursor_pos: cursor_pos, opt_steps: option(steps)): option(t) =>
  switch (opt_steps) {
  | None => None
  | Some(steps) => Some((List.rev(steps), cursor_pos))
  };

let path_to_hole = (hole_list: hole_list, u: MetaVar.t): option(t) =>
  opt_steps_to_opt_path(O(Char(0)), steps_to_hole(hole_list, u));

let path_to_hole_z = (zhole_list: zhole_list, u: MetaVar.t): option(t) =>
  opt_steps_to_opt_path(O(Char(0)), steps_to_hole_z(zhole_list, u));

let next_hole_steps = (zhole_list: zhole_list): option(steps) => {
  let holes_after = zhole_list.holes_after;
  switch (holes_after) {
  | [] => None
  | [(_, steps), ..._] => Some(steps)
  };
};

let next_hole_path = (zhole_list: zhole_list): option(t) =>
  opt_steps_to_opt_path(O(Char(0)), next_hole_steps(zhole_list));

let prev_hole_steps = (zhole_list: zhole_list): option(steps) => {
  let holes_before = zhole_list.holes_before;
  switch (List.rev(holes_before)) {
  | [] => None
  | [(_, steps), ..._] => Some(steps)
  };
};

let prev_hole_path = (zhole_list: zhole_list): option(t) =>
  opt_steps_to_opt_path(O(Char(0)), prev_hole_steps(zhole_list));

let prev_hole_path_zblock = (zblock: ZExp.zblock): option(t) => {
  let holes = holes_zblock(zblock, []);
  prev_hole_path(holes);
};

let next_hole_path_zblock = (zblock: ZExp.zblock): option(t) => {
  let holes = holes_zblock(zblock, []);
  next_hole_path(holes);
};

/* Node Positions */

type node_pos =
  | On(cursor_pos)
  | Deeper(int);

let node_positions_ty = (uty: UHTyp.t): list(node_pos) =>
  switch (uty) {
  | TO(utyo) => ZTyp.valid_outer_cursors(utyo) |> List.map(c => On(O(c)))
  | TI(utyi) =>
    let cfd = ZTyp.children_following_delimiters(utyi);
    let child_positions: list(node_pos) =
      ZTyp.children(utyi)
      |> List.map(k => {
           let before_child_positions: list(node_pos) =
             contains(cfd, k)
               ? [
                 On(I(BeforeChild(k, Before))),
                 On(I(BeforeChild(k, After))),
               ]
               : [];
           let child_position: list(node_pos) = [Deeper(k)];
           before_child_positions @ child_position;
         })
      |> List.flatten;
    let closing_delimiter_positions: list(node_pos) =
      ZTyp.has_closing_delimiter(utyi)
        ? [
          On(I(ClosingDelimiter(Before))),
          On(I(ClosingDelimiter(After))),
        ]
        : [];
    child_positions @ closing_delimiter_positions;
  };

let node_positions_pat = (p: UHPat.t): list(node_pos) =>
  switch (p) {
  | PO(po) => ZPat.valid_outer_cursors(po) |> List.map(c => On(O(c)))
  | PI(pi) =>
    let cfd = ZPat.child_indices_following_delimiters(pi);
    let child_positions: list(node_pos) =
      ZPat.children(pi)
      |> List.map(k => {
           let before_child_positions: list(node_pos) =
             contains(cfd, k)
               ? [
                 On(I(BeforeChild(k, Before))),
                 On(I(BeforeChild(k, After))),
               ]
               : [];
           let child_position: list(node_pos) = [Deeper(k)];
           before_child_positions @ child_position;
         })
      |> List.flatten;
    let closing_delimiter_positions: list(node_pos) =
      ZPat.has_closing_delimiter(pi)
        ? [
          On(I(ClosingDelimiter(Before))),
          On(I(ClosingDelimiter(After))),
        ]
        : [];
    child_positions @ closing_delimiter_positions;
  };

let node_positions_line = (line: UHExp.line): list(node_pos) =>
  switch (line) {
  | ExpLine(_) => []
  | LO(lo) => ZExp.valid_outer_cursors_line(lo) |> List.map(c => On(O(c)))
  | LI(li) =>
    let cfd = ZExp.children_following_delimiters_line(li);
    let child_positions: list(node_pos) =
      ZExp.children_line(li)
      |> List.map(k => {
           let before_child_positions: list(node_pos) =
             contains(cfd, k)
               ? [
                 On(I(BeforeChild(k, Before))),
                 On(I(BeforeChild(k, After))),
               ]
               : [];
           let child_position: list(node_pos) = [Deeper(k)];
           before_child_positions @ child_position;
         })
      |> List.flatten;
    let closing_delimiter_positions: list(node_pos) =
      ZExp.has_closing_delimiter_line(li)
        ? [
          On(I(ClosingDelimiter(Before))),
          On(I(ClosingDelimiter(After))),
        ]
        : [];
    child_positions @ closing_delimiter_positions;
  };

let node_positions_exp = (e: UHExp.t): list(node_pos) =>
  switch (e) {
  | EO(eo) => ZExp.valid_outer_cursors_exp(eo) |> List.map(c => On(O(c)))
  | EI(ei) =>
    let cfd = ZExp.children_following_delimiters_exp(ei);
    let child_positions: list(node_pos) =
      ZExp.children_exp(ei)
      |> List.map(k => {
           let before_child_positions: list(node_pos) =
             contains(cfd, k)
               ? [
                 On(I(BeforeChild(k, Before))),
                 On(I(BeforeChild(k, After))),
               ]
               : [];
           let child_position: list(node_pos) = [Deeper(k)];
           before_child_positions @ child_position;
         })
      |> List.flatten;
    let closing_delimiter_positions: list(node_pos) =
      ZExp.has_closing_delimiter_exp(ei)
        ? [
          On(I(ClosingDelimiter(Before))),
          On(I(ClosingDelimiter(After))),
        ]
        : [];
    child_positions @ closing_delimiter_positions;
  };

let node_position_zty = (zty: ZTyp.t): node_pos =>
  switch (zty) {
  | CursorTO(outer_cursor, _) => On(O(outer_cursor))
  | CursorTI(inner_cursor, _) => On(I(inner_cursor))
  | ParenthesizedZ(_) => Deeper(0)
  | ListZ(_) => Deeper(0)
  | OpSeqZ(_, _, surround) =>
    Deeper(OperatorSeq.surround_prefix_length(surround))
  };

let node_position_zpat = (zp: ZPat.t): node_pos =>
  switch (zp) {
  | CursorPO(outer_cursor, _) => On(O(outer_cursor))
  | CursorPI(inner_cursor, _) => On(I(inner_cursor))
  | ParenthesizedZ(_) => Deeper(0)
  | InjZ(_, _, _) => Deeper(0)
  | OpSeqZ(_, _, surround) =>
    Deeper(OperatorSeq.surround_prefix_length(surround))
  };

let node_position_zline = (zline: ZExp.zline): option(node_pos) =>
  switch (zline) {
  | ExpLineZ(_) => None
  | CursorLO(outer_cursor, _) => Some(On(O(outer_cursor)))
  | CursorLI(inner_cursor, _) => Some(On(I(inner_cursor)))
  | LetLineZP(_, _, _) => Some(Deeper(0))
  | LetLineZA(_, _, _) => Some(Deeper(1))
  | LetLineZE(_, _, _) => Some(Deeper(2))
  };

let node_position_zexp = (ze: ZExp.t): node_pos =>
  switch (ze) {
  | CursorEO(outer_cursor, _) => On(O(outer_cursor))
  | CursorEI(inner_cursor, _) => On(I(inner_cursor))
  | ParenthesizedZ(_) => Deeper(0)
  | OpSeqZ(_, _, surround) =>
    Deeper(OperatorSeq.surround_prefix_length(surround))
  | LamZP(_, _, _, _) => Deeper(0)
  | LamZA(_, _, _, _) => Deeper(1)
  | LamZE(_, _, _, _) => Deeper(2)
  | InjZ(_, _, _) => Deeper(0)
  | CaseZE(_, _, _, _) => Deeper(0)
  | CaseZR(_, _, (prefix, _, _), _) => Deeper(List.length(prefix) + 1)
  | CaseZA(_, _, rules, _) => Deeper(List.length(rules) + 1)
  | ApPaletteZ(_, _, _, _) => Deeper(0) /* TODO */
  };

let steps_of_prev_node_pos_ty = (zty: ZTyp.t): option(steps) => {
  let node_position = node_position_zty(zty);
  let node_positions = node_positions_ty(ZTyp.erase(zty));
  switch (elem_before(node_position, node_positions)) {
  | None => None
  | Some(On(_)) => Some([])
  | Some(Deeper(k)) => Some([k])
  };
};

let steps_of_next_node_pos_ty = (zty: ZTyp.t): option(steps) => {
  let node_position = node_position_zty(zty);
  let node_positions = node_positions_ty(ZTyp.erase(zty));
  switch (elem_after(node_position, node_positions)) {
  | None => None
  | Some(On(_)) => Some([])
  | Some(Deeper(k)) => Some([k])
  };
};

let steps_of_prev_node_pos_pat = (zp: ZPat.t): option(steps) => {
  let node_position = node_position_zpat(zp);
  let node_positions = node_positions_pat(ZPat.erase(zp));
  switch (elem_before(node_position, node_positions)) {
  | None => None
  | Some(On(_)) => Some([])
  | Some(Deeper(k)) => Some([k])
  };
};

let steps_of_next_node_pos_pat = (zp: ZPat.t): option(steps) => {
  let node_position = node_position_zpat(zp);
  let node_positions = node_positions_pat(ZPat.erase(zp));
  switch (elem_after(node_position, node_positions)) {
  | None => None
  | Some(On(_)) => Some([])
  | Some(Deeper(k)) => Some([k])
  };
};

let steps_of_prev_node_pos_line = (zline: ZExp.zline): option(steps) =>
  switch (node_position_zline(zline)) {
  | None => None
  | Some(node_position) =>
    let node_positions = node_positions_line(ZExp.erase_line(zline));
    switch (elem_before(node_position, node_positions)) {
    | None => None
    | Some(On(_)) => Some([])
    | Some(Deeper(k)) => Some([k])
    };
  };

let steps_of_next_node_pos_line = (zline: ZExp.zline): option(steps) =>
  switch (node_position_zline(zline)) {
  | None => None
  | Some(node_position) =>
    let node_positions = node_positions_line(ZExp.erase_line(zline));
    switch (elem_after(node_position, node_positions)) {
    | None => None
    | Some(On(_)) => Some([])
    | Some(Deeper(k)) => Some([k])
    };
  };

let steps_of_prev_node_pos_exp = (ze: ZExp.t): option(steps) => {
  let node_position = node_position_zexp(ze);
  let node_positions = node_positions_exp(ZExp.erase(ze));
  switch (elem_before(node_position, node_positions)) {
  | None => None
  | Some(On(_)) => Some([])
  | Some(Deeper(k)) => Some([k])
  };
};

let steps_of_next_node_pos_exp = (ze: ZExp.t): option(steps) => {
  let node_position = node_position_zexp(ze);
  let node_positions = node_positions_exp(ZExp.erase(ze));
  switch (elem_after(node_position, node_positions)) {
  | None => None
  | Some(On(_)) => Some([])
  | Some(Deeper(k)) => Some([k])
  };
};
