open SemanticsCommon;
open GeneralUtil;

[@deriving show({with_path: false})]
type steps = list(int);

let string_of_steps = GeneralUtil.string_of_list(string_of_int);

[@deriving show({with_path: false})]
type t = (steps, cursor_pos);

let cons' = (step: int, r: t): t => {
  let (steps, side) = r;
  ([step, ...steps], side);
};

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

let rec follow_ty = (path: t, uty: UHTyp.t): option(ZTyp.t) =>
  switch (path) {
  | ([], cursor) =>
    switch (cursor, uty) {
    | (O(outer_cursor), TO(utyo)) => Some(CursorTO(outer_cursor, utyo))
    | (I(inner_cursor), TI(utyi)) => Some(CursorTI(inner_cursor, utyi))
    | (O(_), TI(_))
    | (I(_), TO(_)) => None
    }
  | ([x, ...xs], cursor_pos) =>
    switch (uty) {
    | TO(Hole)
    | TO(Unit)
    | TO(Num)
    | TO(Bool) => None
    | TI(Parenthesized(uty1)) =>
      switch (x) {
      | 0 =>
        switch (follow_ty((xs, cursor_pos), uty1)) {
        | Some(zty) => Some(ParenthesizedZ(zty))
        | None => None
        }
      | _ => None
      }
    | TI(List(uty1)) =>
      switch (x) {
      | 0 =>
        switch (follow_ty((xs, cursor_pos), uty1)) {
        | None => None
        | Some(zty) => Some(ListZ(zty))
        }
      | _ => None
      }
    | TI(OpSeq(skel, seq)) =>
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
  | ([], cursor) =>
    switch (cursor, p) {
    | (O(outer_cursor), PO(po)) => Some(CursorPO(outer_cursor, po))
    | (I(inner_cursor), PI(pi)) => Some(CursorPI(inner_cursor, pi))
    | (O(_), PI(_))
    | (I(_), PO(_)) => None
    }
  | ([x, ...xs], cursor_pos) =>
    switch (x, p) {
    | (_, PO(_)) => None /* outer nodes have no children */
    | (0, PI(Parenthesized(p1))) =>
      switch (follow_pat((xs, cursor_pos), p1)) {
      | None => None
      | Some(zp1) => Some(ParenthesizedZ(zp1))
      }
    | (_, PI(Parenthesized(_))) => None
    | (_, PI(OpSeq(skel, seq))) =>
      switch (OperatorSeq.split(x, seq)) {
      | None => None
      | Some((p, surround)) =>
        switch (follow_pat((xs, cursor_pos), p)) {
        | None => None
        | Some(zp) => Some(OpSeqZ(skel, zp, surround))
        }
      }
    | (0, PI(Inj(err, side, p1))) =>
      switch (follow_pat((xs, cursor_pos), p1)) {
      | None => None
      | Some(zp1) => Some(InjZ(err, side, zp1))
      }
    | (_, PI(Inj(_, _, _))) => None
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
  | (_, ExpLine(e)) =>
    switch (follow_exp(path, e)) {
    | None => None
    | Some(ze) => Some(ExpLineZ(ze))
    }
  | (([], O(outer_cursor)), LO(lo)) => Some(CursorLO(outer_cursor, lo))
  | (([], I(inner_cursor)), LI(li)) => Some(CursorLI(inner_cursor, li))
  | (([], O(_)), LI(_))
  | (([], I(_)), LO(_)) => None
  | (_, LO(EmptyLine)) => None
  | (([0, ...xs], cursor_pos), LI(LetLine(p, ann, e1))) =>
    switch (follow_pat((xs, cursor_pos), p)) {
    | None => None
    | Some(zp) => Some(LetLineZP(zp, ann, e1))
    }
  | (([1, ...xs], cursor_pos), LI(LetLine(p, ann, e1))) =>
    switch (ann) {
    | None => None
    | Some(ann_ty) =>
      switch (follow_ty((xs, cursor_pos), ann_ty)) {
      | None => None
      | Some(zann) => Some(LetLineZA(p, zann, e1))
      }
    }
  | (([2, ...xs], cursor_pos), LI(LetLine(p, ann, block))) =>
    switch (follow_block((xs, cursor_pos), block)) {
    | None => None
    | Some(zblock) => Some(LetLineZE(p, ann, zblock))
    }
  | (_, LI(LetLine(_, _, _))) => None
  }
and follow_exp = (path: t, e: UHExp.t): option(ZExp.t) =>
  switch (path) {
  | ([], cursor) =>
    switch (cursor, e) {
    /* TODO consider coming back and checking valid cursor positions */
    | (O(outer_cursor), EO(eo)) => Some(CursorEO(outer_cursor, eo))
    | (I(inner_cursor), EI(ei)) => Some(CursorEI(inner_cursor, ei))
    | (O(_), EI(_))
    | (I(_), EO(_)) => None
    }
  | ([x, ...xs], cursor_pos) =>
    switch (x, e) {
    | (_, EO(_)) => None
    | (0, EI(Parenthesized(block))) =>
      switch (follow_block((xs, cursor_pos), block)) {
      | Some(zblock) => Some(ParenthesizedZ(zblock))
      | None => None
      }
    | (_, EI(Parenthesized(_))) => None
    | (_, EI(OpSeq(skel, seq))) =>
      switch (OperatorSeq.split(x, seq)) {
      | Some((e, surround)) =>
        switch (follow_exp((xs, cursor_pos), e)) {
        | Some(ze) => Some(OpSeqZ(skel, ze, surround))
        | None => None
        }
      | None => None
      }
    | (0, EI(Lam(err, p, ann, block))) =>
      switch (follow_pat((xs, cursor_pos), p)) {
      | None => None
      | Some(zp) => Some(LamZP(err, zp, ann, block))
      }
    | (1, EI(Lam(err, p, ann, block))) =>
      switch (ann) {
      | None => None
      | Some(ann_ty) =>
        switch (follow_ty((xs, cursor_pos), ann_ty)) {
        | None => None
        | Some(zann) => Some(LamZA(err, p, zann, block))
        }
      }
    | (2, EI(Lam(err, p, ann, block))) =>
      switch (follow_block((xs, cursor_pos), block)) {
      | None => None
      | Some(zblock) => Some(LamZE(err, p, ann, zblock))
      }
    | (_, EI(Lam(_, _, _, _))) => None
    | (0, EI(Inj(err, side, block))) =>
      switch (follow_block((xs, cursor_pos), block)) {
      | None => None
      | Some(zblock) => Some(InjZ(err, side, zblock))
      }
    | (_, EI(Inj(_, _, _))) => None
    | (0, EI(Case(err, block, rules, ann))) =>
      switch (follow_block((xs, cursor_pos), block)) {
      | None => None
      | Some(zblock) => Some(CaseZE(err, zblock, rules, ann))
      }
    | (x, EI(Case(err, block, rules, ann)))
        when x === List.length(rules) + 1 =>
      switch (ann) {
      | None => None
      | Some(ty) =>
        switch (follow_ty((xs, cursor_pos), ty)) {
        | None => None
        | Some(zann) => Some(CaseZA(err, block, rules, zann))
        }
      }
    | (x, EI(Case(err, block, rules, ann))) =>
      switch (ZList.split_at(x - 1, rules)) {
      | None => None
      | Some(split_rules) =>
        switch (ZList.optmap_z(follow_rule((xs, cursor_pos)), split_rules)) {
        | None => None
        | Some(zrules) => Some(CaseZR(err, block, zrules, ann))
        }
      }
    | (n, EI(ApPalette(_, name, serialized_model, splice_info))) =>
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
        Some(ApPaletteZ(NotInHole, name, serialized_model, zsplice_info))
      }
    }
  }
and follow_rule = (path: t, rule: UHExp.rule): option(ZExp.zrule) =>
  switch (rule) {
  | Rule(p, block) =>
    switch (path) {
    | ([], I(inner_cursor)) => Some(CursorR(inner_cursor, rule))
    | ([], O(_)) => None
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

type slot_holes = list(option(hole_list));

let slot_holes_OpSeq = (holes_fn: ('tm, steps, hole_list) => hole_list, seq: OperatorSeq.opseq('tm, _), steps: steps) =>
  List.mapi((n, tm_n) => Some(holes_fn(tm_n, [n, ...steps], [])), OperatorSeq.tms(seq));

let slot_holes_typ = (utyi: UHTyp.t_inner, steps: steps): slot_holes =>
  switch (utyi) {
  | Parenthesized(uty1)
  | List(uty1) => [None, Some(holes_uty(uty1, [0, ...steps], [])), None]
  | OpSeq(_, seq) => slot_holes_OpSeq(holes_uty, seq, steps)
  };
  
let slot_holes_pat = (pi: UHPat.t_inner, steps: steps): slot_holes =>
  switch (pi) {
  | Parenthesized(p1)
  | Inj(p1) => [None, Some(holes_pat(p1, [0, ...steps])), None]
  | OpSeq(_, seq) => slot_holes_OpSeq(holes_pat, seq, steps)
  };
  
let slot_holes_exp = (ei: UHExp.t_inner, steps: steps): slot_holes =>
  switch (ei) {
  | Lam(_, p, ann, block) => [
      None,
      Some(holes_pat(p, [0, ...steps], [])),
      Some(switch (ann) {
        | None => []
        | Some(uty) => holes_uty(uty, [1, ...steps], [])
      }),
      Some(holes_block(block, [2, ...steps], []))
    ]
  | Inj(_, _, block) => [None, Some(holes_block(block, [0, ...steps], [])), None]
  | Case(_, block, rules, ann) =>
    let holes_block = holes_block(block, [0, ...steps], []);
    let holes_rules =
      List.mapi((i, Rule(p, clause)) => holes_pat(p, [0, i, ...steps], []) @ holes_block(clause, [1, i, ...steps], []), rules);
    let holes_ann =
      switch (ann) {
      | None => []
      | Some(uty) => holes_uty(uty, [List.length(rules) + 1, ...steps], [])
      };
    [None, Some(holes_block) /* TODO */];
  }


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
  | AfterChild(0)
  | BeforeChild(1) => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_bracketed_t,
    }
  | AfterChild(1)
  | BeforeChild(2) => {
      holes_before: holes_bracketed_t,
      hole_selected: None,
      holes_after: [],
    }
  | AfterChild(_)
  | BeforeChild(_) => no_holes
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
  | AfterChild(k) when k >= 0 && k < OperatorSeq.seq_length(seq) - 1 =>
    switch (OperatorSeq.split(k, seq)) {
    | None => raise_inconsistent(skel, seq)
    | Some((tm_k, surround)) =>
      let (holes_prefix, holes_suffix) =
        holes_surround(holes_fn, surround, steps);
      {
        holes_before: holes_prefix @ holes_fn(tm_k, [k, ...steps], []),
        hole_selected: None,
        holes_after: holes_suffix,
      };
    }
  | AfterChild(_) => no_holes
  | BeforeChild(k) when k > 0 && k <= OperatorSeq.seq_length(seq) - 1 =>
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
  | BeforeChild(_) => no_holes
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
    | AfterChild(0)
    | BeforeChild(1) => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_p @ holes_ann @ holes_block,
      }
    | AfterChild(1)
    | BeforeChild(2) => {
        holes_before: holes_p,
        hole_selected: None,
        holes_after: holes_ann @ holes_block,
      }
    | AfterChild(2)
    | BeforeChild(3) => {
        holes_before: holes_p @ holes_ann,
        hole_selected: None,
        holes_after: holes_block
      }
    | AfterChild(_)
    | BeforeChild(_) => no_holes
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
      holes_after: []
    }
  | CursorEI(inner_cursor, ei) =>
    switch (ei) {
    | Lam(_, p, ann, block) =>
      let holes_p = holes_pat(p, [0, ...steps], []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) => holes_uty(uty, [1, ...steps], [])
        };
      let holes_block = holes_block(block, [2, ...steps], []);
      switch (inner_cursor) {
      | AfterChild(0)
      | BeforeChild(1) => {
          holes_before: [],
          hole_selected: None,
          holes_after: holes_p @ holes_ann @ holes_block
        }
      | AfterChild(1)
      | BeforeChild(2) =>
      }
    }
  
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
