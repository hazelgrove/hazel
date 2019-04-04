open SemanticsCommon;
open GeneralUtil;

[@deriving show({with_path: false})]
type steps = list(int);

let string_of_steps = GeneralUtil.string_of_list(string_of_int);

[@deriving show({with_path: false})]
type t = (steps, ZExp.cursor_pos);

let cons' = (step: int, r: t): t => {
  let (steps, side) = r;
  ([step, ...steps], side);
};

let rec of_ztyp = (zty: ZTyp.t): t =>
  switch (zty) {
  | CursorT(cursor_pos, _) => ([], cursor_pos)
  | ParenthesizedZ(zty1) => cons'(0, of_ztyp(zty1))
  | ListZ(zty1) => cons'(0, of_ztyp(zty1))
  | OpSeqZ(_, zty1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_ztyp(zty1));
  };

let rec of_zpat = (zp: ZPat.t): t =>
  switch (zp) {
  | CursorP(cursor_pos, _) => ([], cursor_pos)
  | Deeper(_, zp') => of_zpat'(zp')
  | ParenthesizedZ(zp1) => cons'(0, of_zpat(zp1))
  | OpSeqZ(_, zp1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zpat(zp1));
  }
and of_zpat' = (zp': ZPat.t'): t =>
  switch (zp') {
  | InjZ(_, zp1) => cons'(0, of_zpat(zp1))
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
  | CursorL(side, _) => ([], side)
  | DeeperL(zline') => of_zline'(zline')
  }
and of_zline' = (zline': ZExp.zline'): t =>
  switch (zline') {
  | ExpLineZ(ze) => cons'(0, of_zexp(ze))
  | LetLineZP(zp, _, _) => cons'(0, of_zpat(zp))
  | LetLineZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | LetLineZE(_, _, zblock) => cons'(2, of_zblock(zblock))
  }
and of_zexp = (ze: ZExp.t): t =>
  switch (ze) {
  | CursorE(cursor_pos, _) => ([], cursor_pos)
  | DeeperE(_, ze') => of_zexp'(ze')
  | ParenthesizedZ(zblock) => cons'(0, of_zblock(zblock))
  | OpSeqZ(_, ze, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zexp(ze));
  }
and of_zexp' = (ze': ZExp.t'): t =>
  switch (ze') {
  | LamZP(zp, _, _) => cons'(0, of_zpat(zp))
  | LamZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | LamZE(_, _, zblock) => cons'(2, of_zblock(zblock))
  | InjZ(_, zblock) => cons'(0, of_zblock(zblock))
  | CaseZE(zblock, _, _) => cons'(0, of_zblock(zblock))
  | CaseZR(_, zrules, _) =>
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons'(prefix_len + 1, of_zrule(zrule));
  | CaseZA(_, rules, zann) => cons'(List.length(rules) + 1, of_ztyp(zann))
  | ApPaletteZ(_, _, zpsi) =>
    let zhole_map = zpsi.zsplice_map;
    let (n, (_, zblock)) = ZNatMap.prj_z_kv(zhole_map);
    cons'(n, of_zblock(zblock));
  }
and of_zrule = (zrule: ZExp.zrule): t =>
  switch (zrule) {
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

let rec follow_ty = (path: t, uty: UHTyp.t): option(ZTyp.t) =>
  switch (path) {
  | ([], cursor_pos) => Some(CursorT(cursor_pos, uty))
  | ([x, ...xs], cursor_pos) =>
    switch (uty) {
    | Hole
    | Unit
    | Num
    | Bool => None
    | Parenthesized(uty1) =>
      switch (x) {
      | 0 =>
        switch (follow_ty((xs, cursor_pos), uty1)) {
        | Some(zty) => Some(ParenthesizedZ(zty))
        | None => None
        }
      | _ => None
      }
    | List(uty1) =>
      switch (x) {
      | 0 =>
        switch (follow_ty((xs, cursor_pos), uty1)) {
        | None => None
        | Some(zty) => Some(ListZ(zty))
        }
      | _ => None
      }
    | OpSeq(skel, seq) =>
      switch (OperatorSeq.split(x, seq)) {
      | Some((uty_n, surround)) =>
        switch (follow_ty((xs, cursor_pos), uty_n)) {
        | Some(zty_n) => Some(OpSeqZ(skel, zty_n, surround))
        | None => None
        }
      | None => None
      }
    }
  };

exception UHTypeNodeNotFound(t, UHTyp.t);
let follow_ty_or_fail = (path: t, uty: UHTyp.t): ZTyp.t =>
  switch (follow_ty(path, uty)) {
  | None => raise(UHTypeNodeNotFound(path, uty))
  | Some(zty) => zty
  };

let rec follow_pat = (path: t, p: UHPat.t): option(ZPat.t) =>
  switch (path) {
  | ([], cursor_pos) => Some(CursorP(cursor_pos, p))
  | ([x, ...xs], cursor_pos) =>
    switch (p) {
    | EmptyHole(_) => None
    | Parenthesized(p1) =>
      switch (x) {
      | 0 =>
        switch (follow_pat((xs, cursor_pos), p1)) {
        | None => None
        | Some(zp1) => Some(ParenthesizedZ(zp1))
        }
      | _ => None
      }
    | OpSeq(skel, seq) =>
      switch (OperatorSeq.split(x, seq)) {
      | None => None
      | Some((p, surround)) =>
        switch (follow_pat((xs, cursor_pos), p)) {
        | Some(zp) => Some(OpSeqZ(skel, zp, surround))
        | None => None
        }
      }
    | Pat(err_status, p') =>
      switch (x, p') {
      | (_, Wild)
      | (_, Var(_))
      | (_, NumLit(_))
      | (_, BoolLit(_))
      | (_, ListNil) => None
      | (0, Inj(side, p1)) =>
        switch (follow_pat((xs, cursor_pos), p1)) {
        | None => None
        | Some(zp1) => Some(Deeper(err_status, InjZ(side, zp1)))
        }
      | (_, Inj(_, _)) => None
      }
    }
  };

exception UHPatNodeNotFound(t, UHPat.t);
let follow_pat_or_fail = (path: t, p: UHPat.t): ZPat.t =>
  switch (follow_pat(path, p)) {
  | None => raise(UHPatNodeNotFound(path, p))
  | Some(zp) => zp
  };

let rec follow_block = (path: t, block: UHExp.block): option(ZExp.zblock) =>
  switch (path) {
  | ([], _) => None /* no block level cursor */
  | ([x, ...xs], cursor_pos) =>
    let Block(lines, e) = block;
    if (x === List.length(lines)) {
      switch (follow_exp((xs, cursor_pos), e)) {
      | None => None
      | Some(ze) => Some(BlockZE(lines, ze))
      };
    } else {
      switch (ZList.split_at(x, lines)) {
      | None => None
      | Some(split_lines) =>
        switch (ZList.optmap_z(follow_line((xs, cursor_pos)), split_lines)) {
        | None => None
        | Some(zlines) => Some(BlockZL(zlines, e))
        }
      };
    };
  }
and follow_line = (path: t, line: UHExp.line): option(ZExp.zline) =>
  switch (path, line) {
  | (([], cursor_pos), _) => Some(CursorL(cursor_pos, line))
  | (_, EmptyLine) => None
  | (([0, ...xs], cursor_pos), ExpLine(e)) =>
    switch (follow_exp((xs, cursor_pos), e)) {
    | None => None
    | Some(ze) => Some(DeeperL(ExpLineZ(ze)))
    }
  | (_, ExpLine(_)) => None
  | (([0, ...xs], cursor_pos), LetLine(p, ann, e1)) =>
    switch (follow_pat((xs, cursor_pos), p)) {
    | None => None
    | Some(zp) => Some(DeeperL(LetLineZP(zp, ann, e1)))
    }
  | (([1, ...xs], cursor_pos), LetLine(p, ann, e1)) =>
    switch (ann) {
    | None => None
    | Some(ann_ty) =>
      switch (follow_ty((xs, cursor_pos), ann_ty)) {
      | None => None
      | Some(zann) => Some(DeeperL(LetLineZA(p, zann, e1)))
      }
    }
  | (([2, ...xs], cursor_pos), LetLine(p, ann, block)) =>
    switch (follow_block((xs, cursor_pos), block)) {
    | None => None
    | Some(zblock) => Some(DeeperL(LetLineZE(p, ann, zblock)))
    }
  | (_, LetLine(_, _, _)) => None
  }
and follow_exp = (path: t, e: UHExp.t): option(ZExp.t) =>
  switch (path) {
  | ([], cursor_pos) => Some(CursorE(cursor_pos, e))
  | ([x, ...xs], cursor_pos) =>
    switch (e) {
    | EmptyHole(_) => None
    | Parenthesized(block) =>
      switch (x) {
      | 0 =>
        switch (follow_block((xs, cursor_pos), block)) {
        | Some(zblock) => Some(ParenthesizedZ(zblock))
        | None => None
        }
      | _ => None
      }
    | OpSeq(skel, seq) =>
      switch (OperatorSeq.split(x, seq)) {
      | Some((e, surround)) =>
        switch (follow_exp((xs, cursor_pos), e)) {
        | Some(ze) => Some(OpSeqZ(skel, ze, surround))
        | None => None
        }
      | None => None
      }
    | Tm(err_status, e) =>
      switch (x, e) {
      | (_, Var(_, _)) => None
      | (0, Lam(p, ann, block)) =>
        switch (follow_pat((xs, cursor_pos), p)) {
        | None => None
        | Some(zp) => Some(DeeperE(err_status, LamZP(zp, ann, block)))
        }
      | (1, Lam(p, ann, block)) =>
        switch (ann) {
        | None => None
        | Some(ann_ty) =>
          switch (follow_ty((xs, cursor_pos), ann_ty)) {
          | None => None
          | Some(zann) => Some(DeeperE(err_status, LamZA(p, zann, block)))
          }
        }
      | (2, Lam(p, ann, block)) =>
        switch (follow_block((xs, cursor_pos), block)) {
        | None => None
        | Some(zblock) => Some(DeeperE(err_status, LamZE(p, ann, zblock)))
        }
      | (_, Lam(_, _, _)) => None
      | (_, NumLit(_)) => None
      | (_, BoolLit(_)) => None
      | (0, Inj(side, block)) =>
        switch (follow_block((xs, cursor_pos), block)) {
        | None => None
        | Some(zblock) => Some(DeeperE(err_status, InjZ(side, zblock)))
        }
      | (_, Inj(_, _)) => None
      | (_, ListNil) => None
      | (0, Case(block, rules, ann)) =>
        switch (follow_block((xs, cursor_pos), block)) {
        | None => None
        | Some(zblock) =>
          Some(DeeperE(err_status, CaseZE(zblock, rules, ann)))
        }
      | (x, Case(block, rules, ann)) when x === List.length(rules) + 1 =>
        switch (ann) {
        | None => None
        | Some(ty) =>
          switch (follow_ty((xs, cursor_pos), ty)) {
          | None => None
          | Some(zann) =>
            Some(DeeperE(err_status, CaseZA(block, rules, zann)))
          }
        }
      | (x, Case(block, rules, ann)) =>
        switch (ZList.split_at(x - 1, rules)) {
        | None => None
        | Some(split_rules) =>
          switch (ZList.optmap_z(follow_rule((xs, cursor_pos)), split_rules)) {
          | None => None
          | Some(zrules) =>
            Some(DeeperE(err_status, CaseZR(block, zrules, ann)))
          }
        }
      | (n, ApPalette(name, serialized_model, splice_info)) =>
        switch (
          ZSpliceInfo.select_opt(splice_info, n, ((ty, block)) =>
            switch (follow_block((xs, cursor_pos), block)) {
            | None => None
            | Some(zblock) => Some((ty, zblock))
            }
          )
        ) {
        | None => None
        | Some(zsplice_info) =>
          Some(
            DeeperE(
              NotInHole,
              ApPaletteZ(name, serialized_model, zsplice_info),
            ),
          )
        }
      }
    }
  }
and follow_rule = (path: t, rule: UHExp.rule): option(ZExp.zrule) =>
  switch (rule) {
  | Rule(p, block) =>
    switch (path) {
    | ([], _) => None
    | ([0, ...xs], cursor_pos) =>
      switch (follow_pat((xs, cursor_pos), p)) {
      | None => None
      | Some(zp) => Some(RuleZP(zp, block))
      }
    | ([1, ...xs], cursor_pos) =>
      switch (follow_block((xs, cursor_pos), block)) {
      | None => None
      | Some(zblock) => Some(RuleZE(p, zblock))
      }
    | ([_, ..._], _) => None
    }
  };

exception UHBlockNodeNotFound(t, UHExp.block);
let follow_block_or_fail = (path: t, block: UHExp.block): ZExp.zblock =>
  switch (follow_block(path, block)) {
  | None => raise(UHBlockNodeNotFound(path, block))
  | Some(zblock) => zblock
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
  | Parenthesized(uty1) => holes_uty(uty1, [0, ...steps], holes)
  | Hole => [(TypeHole, steps), ...holes]
  | Unit => holes
  | Num => holes
  | Bool => holes
  | List(uty1) => holes_uty(uty1, [0, ...steps], holes)
  | OpSeq(_, seq) => holes_seq(seq, holes_uty, 0, steps, holes)
  };

let rec holes_pat = (p: UHPat.t, steps: steps, holes: hole_list): hole_list =>
  switch (p) {
  | Parenthesized(p1) => holes_pat(p1, [0, ...steps], holes)
  | EmptyHole(u) => [(PatHole(u), steps), ...holes]
  | Pat(_, Wild) => holes
  | Pat(_, Var(_)) => holes
  | Pat(_, NumLit(_)) => holes
  | Pat(_, BoolLit(_)) => holes
  | Pat(_, ListNil) => holes
  | Pat(_, Inj(_, p1)) => holes_pat(p1, [0, ...steps], holes)
  | OpSeq(_, seq) => holes_seq(seq, holes_pat, 0, steps, holes)
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
  | EmptyLine => holes
  | ExpLine(e) => holes_exp(e, [0, ...steps], holes)
  | LetLine(p, ann, block) =>
    let holes = holes_block(block, [2, ...steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...steps], holes);
  }
and holes_exp = (e: UHExp.t, steps: steps, holes: hole_list): hole_list =>
  switch (e) {
  | Parenthesized(block) => holes_block(block, [0, ...steps], holes)
  | EmptyHole(u) => [(ExpHole(u), steps), ...holes]
  | OpSeq(_, seq) => holes_seq(seq, holes_exp, 0, steps, holes)
  | Tm(_, Var(_, _)) => holes
  | Tm(_, NumLit(_)) => holes
  | Tm(_, BoolLit(_)) => holes
  | Tm(_, Inj(_, block)) => holes_block(block, [0, ...steps], holes)
  | Tm(_, ListNil) => holes
  | Tm(_, Lam(p, ann, block)) =>
    let holes = holes_block(block, [2, ...steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...steps], holes);
  | Tm(_, Case(block, rules, ann)) =>
    let holes =
      switch (ann) {
      | None => holes
      | Some(uty) =>
        holes_uty(uty, [List.length(rules) + 1, ...steps], holes)
      };
    let holes = holes_rules(rules, 1, steps, holes);
    holes_block(block, [0, ...steps], holes);
  | Tm(_, ApPalette(_, _, psi)) =>
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

let rec holes_zty = (zty: ZTyp.t, steps: steps): zhole_list =>
  switch (zty) {
  | CursorT(cursor_pos, uty) =>
    switch (cursor_pos, uty) {
    | (_, Hole) => {
        holes_before: [],
        hole_selected: Some((TypeHole, steps)),
        holes_after: [],
      }
    | (Before, _) => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_uty(uty, steps, []),
      }
    | (After, _) => {
        holes_before: holes_uty(uty, steps, []),
        hole_selected: None,
        holes_after: [],
      }
    | (In(_), _) =>
      switch (uty) {
      | Parenthesized(_)
      | Hole
      | Unit
      | Num
      | Bool
      | OpSeq(_, _) => no_holes
      | List(uty1) => {
          holes_before: [],
          hole_selected: None,
          holes_after: holes_uty(uty1, steps, []),
        }
      }
    }
  | ParenthesizedZ(zty1) => holes_zty(zty1, [0, ...steps])
  | ListZ(zty1) => holes_zty(zty1, [0, ...steps])
  | OpSeqZ(_, zty0, surround) =>
    holes_OpSeqZ(holes_uty, holes_zty, zty0, surround, steps)
  };

let rec holes_zpat = (zp: ZPat.t, steps: steps): zhole_list =>
  switch (zp) {
  | CursorP(cursor_pos, p) =>
    switch (cursor_pos, p) {
    | (_, EmptyHole(u)) => {
        holes_before: [],
        hole_selected: Some((PatHole(u), steps)),
        holes_after: [],
      }
    | (Before, _) => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_pat(p, steps, []),
      }
    | (After, _) => {
        holes_before: holes_pat(p, steps, []),
        hole_selected: None,
        holes_after: [],
      }
    | (In(_), _) =>
      switch (p) {
      | EmptyHole(_) => no_holes
      | OpSeq(_, _) => no_holes
      | Parenthesized(_) => no_holes
      | Pat(_, p') =>
        switch (p') {
        | Wild
        | Var(_)
        | NumLit(_)
        | BoolLit(_)
        | ListNil => no_holes
        | Inj(_, p1) => {
            holes_before: [],
            hole_selected: None,
            holes_after: holes_pat(p1, [0, ...steps], []),
          }
        }
      }
    }
  | ParenthesizedZ(zp1) => holes_zpat(zp1, [0, ...steps])
  | Deeper(_, InjZ(_, zp1)) => holes_zpat(zp1, [0, ...steps])
  | OpSeqZ(_, zp1, surround) =>
    holes_OpSeqZ(holes_pat, holes_zpat, zp1, surround, steps)
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
  | CursorL(Before, li) => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_line(li, steps, []),
    }
  | CursorL(In(_), EmptyLine)
  | CursorL(In(_), ExpLine(_)) => no_holes
  | CursorL(In(_), LetLine(_, _, _) as li) => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_line(li, steps, []),
    }
  | CursorL(After, li) => {
      holes_before: holes_line(li, steps, []),
      hole_selected: None,
      holes_after: [],
    }
  | DeeperL(zli') => holes_zline'(zli', steps)
  }
and holes_zline' = (zli': ZExp.zline', steps: steps): zhole_list =>
  switch (zli') {
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
  | OpSeqZ(_, ze0, surround) =>
    holes_OpSeqZ(holes_exp, holes_ze, ze0, surround, steps)
  | ParenthesizedZ(zblock) => holes_zblock(zblock, [0, ...steps])
  | CursorE(cursor_pos, e) =>
    switch (cursor_pos, e) {
    | (_, EmptyHole(u)) => {
        holes_before: [],
        hole_selected: Some((ExpHole(u), steps)),
        holes_after: [],
      }
    | (Before, _) => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_exp(e, steps, []),
      }
    | (After, _) => {
        holes_before: holes_exp(e, steps, []),
        hole_selected: None,
        holes_after: [],
      }
    | (In(k), _) =>
      switch (e) {
      | OpSeq(_, _) => no_holes
      | Parenthesized(_) => no_holes
      | EmptyHole(_) => no_holes
      | Tm(_, ue') =>
        switch (ue') {
        | NumLit(_)
        | BoolLit(_)
        | ListNil
        | Var(_, _) => no_holes
        | Inj(_, _)
        | Lam(_, _, _) => {
            holes_before: [],
            hole_selected: None,
            holes_after: holes_exp(e, steps, []),
          }
        | Case(block, rules, ann) =>
          switch (k) {
          | 0 => {
              holes_before: [],
              hole_selected: None,
              holes_after: holes_exp(e, steps, []),
            }
          | 1 =>
            let holes_rules = holes_rules(rules, 1, steps, []);
            let holes_before =
              holes_block(block, [0, ...steps], holes_rules);
            let holes_after =
              switch (ann) {
              | None => []
              | Some(uty) =>
                holes_uty(uty, [List.length(rules) + 1, ...steps], [])
              };
            {holes_before, hole_selected: None, holes_after};
          | _ => no_holes
          }
        | ApPalette(_, _, _) => no_holes
        }
      }
    }
  | DeeperE(_, ze) =>
    switch (ze) {
    | LamZP(zp, ann, block) =>
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
    | LamZA(p, zann, block) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zty(zann, [1, ...steps]);
      let holes_p = holes_pat(p, [0, ...steps], []);
      let holes_block = holes_block(block, [2, ...steps], []);
      {
        holes_before: holes_p @ holes_before,
        hole_selected,
        holes_after: holes_after @ holes_block,
      };
    | LamZE(p, ann, zblock) =>
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
    | InjZ(_, zblock) => holes_zblock(zblock, [0, ...steps])
    | CaseZE(zblock, rules, ann) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zblock(zblock, [0, ...steps]);
      let holes_rules = holes_rules(rules, 1, steps, []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) =>
          holes_uty(uty, [List.length(rules) + 1, ...steps], [])
        };
      {
        holes_before,
        hole_selected,
        holes_after: holes_after @ holes_rules @ holes_ann,
      };
    | CaseZR(block, zrules, ann) =>
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
    | CaseZA(block, rules, zann) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zty(zann, [List.length(rules) + 1, ...steps]);
      let holes_block = holes_block(block, [0, ...steps], []);
      let holes_rules = holes_rules(rules, 1, steps, []);
      {
        holes_before: holes_block @ holes_rules @ holes_before,
        hole_selected,
        holes_after,
      };
    | ApPaletteZ(_, _, zpsi) =>
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
  }
and holes_zrules = (zrules: ZExp.zrules, offset: int, steps: steps) => {
  let (prefix, zrule, suffix) = ZList.prj(zrules);
  let holes_prefix = holes_rules(prefix, offset, steps, []);
  let prefix_len = List.length(prefix);
  let {holes_before, hole_selected, holes_after} =
    holes_zrule(zrule, prefix_len, steps);
  let holes_suffix = holes_rules(suffix, offset + 1 + prefix_len, steps, []);
  {
    holes_before: holes_prefix @ holes_before,
    hole_selected,
    holes_after: holes_after @ holes_suffix,
  };
}
and holes_zrule = (zrule: ZExp.zrule, prefix_len: int, steps: steps) =>
  switch (zrule) {
  | RuleZP(zp, block) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, prefix_len + 1, ...steps]);
    let holes_block = holes_block(block, [1, prefix_len + 1, ...steps], []);
    {holes_before, hole_selected, holes_after: holes_after @ holes_block};
  | RuleZE(p, zblock) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zblock(zblock, [1, prefix_len + 1, ...steps]);
    let holes_p = holes_pat(p, [0, prefix_len + 1, ...steps], []);
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
  opt_steps_to_opt_path(Before, steps_to_hole(hole_list, u));

let path_to_hole_z = (zhole_list: zhole_list, u: MetaVar.t): option(t) =>
  opt_steps_to_opt_path(Before, steps_to_hole_z(zhole_list, u));

let next_hole_steps = (zhole_list: zhole_list): option(steps) => {
  let holes_after = zhole_list.holes_after;
  switch (holes_after) {
  | [] => None
  | [(_, steps), ..._] => Some(steps)
  };
};

let next_hole_path = (zhole_list: zhole_list): option(t) =>
  opt_steps_to_opt_path(Before, next_hole_steps(zhole_list));

let prev_hole_steps = (zhole_list: zhole_list): option(steps) => {
  let holes_before = zhole_list.holes_before;
  switch (List.rev(holes_before)) {
  | [] => None
  | [(_, steps), ..._] => Some(steps)
  };
};

let prev_hole_path = (zhole_list: zhole_list): option(t) =>
  opt_steps_to_opt_path(Before, prev_hole_steps(zhole_list));

let prev_hole_path_zblock = (zblock: ZExp.zblock): option(t) => {
  let holes = holes_zblock(zblock, []);
  prev_hole_path(holes);
};

let next_hole_path_zblock = (zblock: ZExp.zblock): option(t) => {
  let holes = holes_zblock(zblock, []);
  next_hole_path(holes);
};
