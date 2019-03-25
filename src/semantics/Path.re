open SemanticsCommon;
open HazelUtil;

[@deriving show({with_path: false})]
type steps = list(int);

let string_of_steps = HazelUtil.string_of_list(string_of_int);

[@deriving show({with_path: false})]
type t = (steps, ZExp.cursor_side);

let cons' = (step: int, r: t): t => {
  let (steps, side) = r;
  ([step, ...steps], side);
};

let rec of_ztyp = (zty: ZTyp.t): t =>
  switch (zty) {
  | CursorT(cursor_side, _) => ([], cursor_side)
  | ParenthesizedZ(zty1) => cons'(0, of_ztyp(zty1))
  | ListZ(zty1) => cons'(0, of_ztyp(zty1))
  | OpSeqZ(_, zty1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_ztyp(zty1));
  };

let rec of_zpat = (zp: ZPat.t): t =>
  switch (zp) {
  | CursorP(cursor_side, _) => ([], cursor_side)
  | Deeper(_, zp') => of_zpat'(zp')
  | ParenthesizedZ(zp1) => cons'(0, of_zpat(zp1))
  }
and of_zpat' = (zp': ZPat.t'): t =>
  switch (zp') {
  | InjZ(_, zp1) => cons'(0, of_zpat(zp1))
  | OpSeqZ(_, zp1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zpat(zp1));
  };

let rec of_zexp = (ze: ZExp.t): t =>
  switch (ze) {
  | CursorE(cursor_side, _) => ([], cursor_side)
  | Deeper(_, ze') => of_zexp'(ze')
  | ParenthesizedZ(ze1) => cons'(0, of_zexp(ze1))
  }
and of_zexp' = (ze: ZExp.t'): t =>
  switch (ze) {
  | LineItemZL(zli, _) => cons'(0, of_zline_item(zli))
  | LineItemZE(_, ze) => cons'(1, of_zexp(ze))
  | LamZP(zp, _, _) => cons'(0, of_zpat(zp))
  | LamZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | LamZE(_, _, ze') => cons'(2, of_zexp(ze'))
  | InjZ(_, ze') => cons'(0, of_zexp(ze'))
  | CaseZE(ze1, _, _) => cons'(0, of_zexp(ze1))
  | CaseZR(_, zrules, _) =>
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons'(prefix_len + 1, of_zrule(zrule));
  | CaseZA(_, rules, zann) => cons'(List.length(rules) + 1, of_ztyp(zann))
  | OpSeqZ(_, ze', surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zexp(ze'));
  | ApPaletteZ(_, _, zpsi) =>
    let zhole_map = zpsi.zsplice_map;
    let (n, (_, ze)) = ZNatMap.prj_z_kv(zhole_map);
    cons'(n, of_zexp(ze));
  }
and of_zline_item = (zli: ZExp.zline_item): t =>
  switch (zli) {
  | CursorL(side, _) => ([], side)
  | DeeperL(zli') => of_zline_item'(zli')
  }
and of_zline_item' = (zli': ZExp.zline_item'): t =>
  switch (zli') {
  | ExpLineZ(ze) => cons'(0, of_zexp(ze))
  | LetLineZP(zp, _, _) => cons'(0, of_zpat(zp))
  | LetLineZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | LetLineZE(_, _, ze) => cons'(2, of_zexp(ze))
  }
and of_zrule = (zrule: ZExp.zrule): t =>
  switch (zrule) {
  | RuleZP(zp, _) => cons'(0, of_zpat(zp))
  | RuleZE(_, ze) => cons'(1, of_zexp(ze))
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
  | ([], cursor_side) => Some(CursorT(cursor_side, uty))
  | ([x, ...xs], cursor_side) =>
    switch (uty) {
    | Hole
    | Unit
    | Num
    | Bool => None
    | Parenthesized(uty1) =>
      switch (x) {
      | 0 =>
        switch (follow_ty((xs, cursor_side), uty1)) {
        | Some(zty) => Some(ParenthesizedZ(zty))
        | None => None
        }
      | _ => None
      }
    | List(uty1) =>
      switch (x) {
      | 0 =>
        switch (follow_ty((xs, cursor_side), uty1)) {
        | None => None
        | Some(zty) => Some(ListZ(zty))
        }
      | _ => None
      }
    | OpSeq(skel, seq) =>
      switch (OperatorSeq.split(x, seq)) {
      | Some((uty_n, surround)) =>
        switch (follow_ty((xs, cursor_side), uty_n)) {
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
  | ([], cursor_side) => Some(CursorP(cursor_side, p))
  | ([x, ...xs], cursor_side) =>
    switch (p) {
    | Parenthesized(p1) =>
      switch (x) {
      | 0 =>
        switch (follow_pat((xs, cursor_side), p1)) {
        | None => None
        | Some(zp1) => Some(ParenthesizedZ(zp1))
        }
      | _ => None
      }
    | Pat(err_status, p') =>
      switch (x, p') {
      | (_, EmptyHole(_))
      | (_, Wild)
      | (_, Var(_))
      | (_, NumLit(_))
      | (_, BoolLit(_))
      | (_, ListNil) => None
      | (0, Inj(side, p1)) =>
        switch (follow_pat((xs, cursor_side), p1)) {
        | None => None
        | Some(zp1) => Some(Deeper(err_status, InjZ(side, zp1)))
        }
      | (_, Inj(_, _)) => None
      | (n, OpSeq(skel, seq)) =>
        switch (OperatorSeq.split(n, seq)) {
        | None => None
        | Some((p, surround)) =>
          switch (follow_pat((xs, cursor_side), p)) {
          | Some(zp) => Some(Deeper(err_status, OpSeqZ(skel, zp, surround)))
          | None => None
          }
        }
      }
    }
  };

exception UHPatNodeNotFound(t, UHPat.t);
let follow_pat_or_fail = (path: t, p: UHPat.t): ZPat.t =>
  switch (follow_pat(path, p)) {
  | None => raise(UHPatNodeNotFound(path, p))
  | Some(zp) => zp
  };

let rec follow_e = (path: t, e: UHExp.t): option(ZExp.t) =>
  switch (path) {
  | ([], cursor_side) => Some(CursorE(cursor_side, e))
  | ([x, ...xs], cursor_side) =>
    switch (e) {
    | Parenthesized(e1) =>
      switch (x) {
      | 0 =>
        switch (follow_e((xs, cursor_side), e1)) {
        | Some(ze1) => Some(ParenthesizedZ(ze1))
        | None => None
        }
      | _ => None
      }
    | Tm(err_status, e) =>
      switch (x, e) {
      | (_, EmptyHole(_)) => None
      | (_, Var(_, _)) => None
      | (0, LineItem(li, e1)) =>
        switch (follow_line_item((xs, cursor_side), li)) {
        | None => None
        | Some(zli) => Some(Deeper(err_status, LineItemZL(zli, e1)))
        }
      | (1, LineItem(li, e1)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | None => None
        | Some(ze1) => Some(Deeper(err_status, LineItemZE(li, ze1)))
        }
      | (_, LineItem(_, _)) => None
      | (0, Lam(p, ann, e1)) =>
        switch (follow_pat((xs, cursor_side), p)) {
        | None => None
        | Some(zp) => Some(Deeper(err_status, LamZP(zp, ann, e1)))
        }
      | (1, Lam(p, ann, e1)) =>
        switch (ann) {
        | None => None
        | Some(ann_ty) =>
          switch (follow_ty((xs, cursor_side), ann_ty)) {
          | None => None
          | Some(zann) => Some(Deeper(err_status, LamZA(p, zann, e1)))
          }
        }
      | (2, Lam(p, ann, e1)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | None => None
        | Some(ze) => Some(Deeper(err_status, LamZE(p, ann, ze)))
        }
      | (_, Lam(_, _, _)) => None
      | (_, NumLit(_)) => None
      | (_, BoolLit(_)) => None
      | (0, Inj(side, e1)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | Some(ze) => Some(Deeper(err_status, InjZ(side, ze)))
        | None => None
        }
      | (_, Inj(_, _)) => None
      | (_, ListNil) => None
      | (0, Case(e1, rules, ann)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | Some(ze) => Some(Deeper(err_status, CaseZE(ze, rules, ann)))
        | None => None
        }
      | (x, Case(e1, rules, ann)) when x === List.length(rules) + 1 =>
        switch (ann) {
        | None => None
        | Some(ty) =>
          switch (follow_ty((xs, cursor_side), ty)) {
          | None => None
          | Some(zann) => Some(Deeper(err_status, CaseZA(e1, rules, zann)))
          }
        }
      | (x, Case(e1, rules, ann)) =>
        switch (ZList.split_at(x - 1, rules)) {
        | None => None
        | Some(split_rules) =>
          switch (
            ZList.optmap_z(follow_rule((xs, cursor_side)), split_rules)
          ) {
          | None => None
          | Some(zrules) =>
            Some(Deeper(err_status, CaseZR(e1, zrules, ann)))
          }
        }
      | (n, OpSeq(skel, seq)) =>
        switch (OperatorSeq.split(n, seq)) {
        | Some((e, surround)) =>
          switch (follow_e((xs, cursor_side), e)) {
          | Some(ze) => Some(Deeper(err_status, OpSeqZ(skel, ze, surround)))
          | None => None
          }
        | None => None
        }
      | (n, ApPalette(name, serialized_model, splice_info)) =>
        switch (
          ZSpliceInfo.select_opt(splice_info, n, ((ty, e)) =>
            switch (follow_e((xs, cursor_side), e)) {
            | None => None
            | Some(ze) => Some((ty, ze))
            }
          )
        ) {
        | None => None
        | Some(zsplice_info) =>
          Some(
            Deeper(
              NotInHole,
              ApPaletteZ(name, serialized_model, zsplice_info),
            ),
          )
        }
      }
    }
  }
and follow_line_item =
    (path: t, li: UHExp.line_item): option(ZExp.zline_item) =>
  switch (path, li) {
  | (([], cursor_side), li) => Some(CursorL(cursor_side, li))
  | (_, EmptyLine) => None
  | (([0, ...xs], cursor_side), ExpLine(e)) =>
    switch (follow_e((xs, cursor_side), e)) {
    | None => None
    | Some(ze) => Some(DeeperL(ExpLineZ(ze)))
    }
  | (_, ExpLine(_)) => None
  | (([0, ...xs], cursor_side), LetLine(p, ann, e1)) =>
    switch (follow_pat((xs, cursor_side), p)) {
    | None => None
    | Some(zp) => Some(DeeperL(LetLineZP(zp, ann, e1)))
    }
  | (([1, ...xs], cursor_side), LetLine(p, ann, e1)) =>
    switch (ann) {
    | None => None
    | Some(ann_ty) =>
      switch (follow_ty((xs, cursor_side), ann_ty)) {
      | None => None
      | Some(zann) => Some(DeeperL(LetLineZA(p, zann, e1)))
      }
    }
  | (([2, ...xs], cursor_side), LetLine(p, ann, e1)) =>
    switch (follow_e((xs, cursor_side), e1)) {
    | None => None
    | Some(ze1) => Some(DeeperL(LetLineZE(p, ann, ze1)))
    }
  | (_, LetLine(_, _, _)) => None
  }
and follow_rule = (path: t, rule: UHExp.rule): option(ZExp.zrule) =>
  switch (rule) {
  | Rule(p, e) =>
    switch (path) {
    | ([], _) => None
    | ([0, ...xs], cursor_side) =>
      switch (follow_pat((xs, cursor_side), p)) {
      | None => None
      | Some(zp) => Some(RuleZP(zp, e))
      }
    | ([1, ...xs], cursor_side) =>
      switch (follow_e((xs, cursor_side), e)) {
      | None => None
      | Some(ze) => Some(RuleZE(p, ze))
      }
    | ([_, ..._], _) => None
    }
  };

exception UHExpNodeNotFound(t, UHExp.t);
let follow_e_or_fail = (path: t, e: UHExp.t): ZExp.t =>
  switch (follow_e(path, e)) {
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
  HazelUtil.string_of_list(
    HazelUtil.string_of_pair(string_of_hole_desc, string_of_steps),
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
  | Pat(_, EmptyHole(u)) => [(PatHole(u), steps), ...holes]
  | Pat(_, Wild) => holes
  | Pat(_, Var(_)) => holes
  | Pat(_, NumLit(_)) => holes
  | Pat(_, BoolLit(_)) => holes
  | Pat(_, ListNil) => holes
  | Pat(_, Inj(_, p1)) => holes_pat(p1, [0, ...steps], holes)
  | Pat(_, OpSeq(_, seq)) => holes_seq(seq, holes_pat, 0, steps, holes)
  };

let rec holes_e = (e: UHExp.t, steps: steps, holes: hole_list): hole_list =>
  switch (e) {
  | Parenthesized(e1) => holes_e(e1, [0, ...steps], holes)
  | Tm(_, EmptyHole(u)) => [(ExpHole(u), steps), ...holes]
  | Tm(_, Var(_, _)) => holes
  | Tm(_, NumLit(_)) => holes
  | Tm(_, BoolLit(_)) => holes
  | Tm(_, Inj(_, e1)) => holes_e(e1, [0, ...steps], holes)
  | Tm(_, ListNil) => holes
  | Tm(_, Lam(p, ann, e1)) =>
    let holes = holes_e(e1, [2, ...steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...steps], holes);
  | Tm(_, LineItem(li, e2)) =>
    let holes = holes_e(e2, [1, ...steps], holes);
    holes_line_item(li, [0, ...steps], holes);
  | Tm(_, Case(e1, rules, ann)) =>
    let holes =
      switch (ann) {
      | None => holes
      | Some(uty) =>
        holes_uty(uty, [List.length(rules) + 1, ...steps], holes)
      };
    let holes = holes_rules(rules, 0, steps, holes);
    holes_e(e1, [0, ...steps], holes);
  | Tm(_, OpSeq(_, seq)) => holes_seq(seq, holes_e, 0, steps, holes)
  | Tm(_, ApPalette(_, _, psi)) =>
    let splice_map = psi.splice_map;
    let splice_order = psi.splice_order;
    List.fold_right(
      (i, holes) =>
        switch (NatMap.lookup(splice_map, i)) {
        | None => holes
        | Some((_, e)) => holes_e(e, [i, ...steps], holes)
        },
      splice_order,
      holes,
    );
  }
and holes_line_item =
    (li: UHExp.line_item, steps: steps, holes: hole_list): hole_list =>
  switch (li) {
  | EmptyLine => holes
  | ExpLine(e1) => holes_e(e1, steps, holes)
  | LetLine(p, ann, e1) =>
    let holes = holes_e(e1, [2, ...steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...steps], holes);
  }
and holes_rules =
    (rules: UHExp.rules, offset: int, steps: steps, holes: hole_list)
    : hole_list => {
  let (_, holes) =
    List.fold_right(
      (UHExp.Rule(p, e), (i, holes)) => {
        let holes = holes_e(e, [1, i, ...steps], holes);
        let holes = holes_pat(p, [0, i, ...steps], holes);
        (i - 1, holes);
      },
      rules,
      (List.length(rules) + offset, holes),
    );
  holes;
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
  ++ HazelUtil.string_of_opt(
       HazelUtil.string_of_pair(string_of_hole_desc, string_of_steps),
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
  | CursorT(cursor_side, uty) =>
    switch (cursor_side, uty) {
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
  | CursorP(cursor_side, p) =>
    switch (cursor_side, p) {
    | (_, Pat(_, EmptyHole(u))) => {
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
      | Parenthesized(_) => no_holes
      | Pat(_, p') =>
        switch (p') {
        | Wild
        | Var(_)
        | NumLit(_)
        | BoolLit(_)
        | ListNil
        | OpSeq(_, _)
        | EmptyHole(_) => no_holes
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
  | Deeper(_, OpSeqZ(_, zp1, surround)) =>
    holes_OpSeqZ(holes_pat, holes_zpat, zp1, surround, steps)
  };

let rec holes_ze = (ze: ZExp.t, steps: steps): zhole_list =>
  switch (ze) {
  | ParenthesizedZ(ze1) => holes_ze(ze1, [0, ...steps])
  | CursorE(cursor_side, e) =>
    switch (cursor_side, e) {
    | (_, Tm(_, EmptyHole(u))) => {
        holes_before: [],
        hole_selected: Some((ExpHole(u), steps)),
        holes_after: [],
      }
    | (Before, _) => {
        holes_before: [],
        hole_selected: None,
        holes_after: holes_e(e, steps, []),
      }
    | (After, _) => {
        holes_before: holes_e(e, steps, []),
        hole_selected: None,
        holes_after: [],
      }
    | (In(k), _) =>
      switch (e) {
      | Parenthesized(_) => no_holes
      | Tm(_, ue') =>
        switch (ue') {
        | NumLit(_)
        | BoolLit(_)
        | ListNil
        | Var(_, _)
        | EmptyHole(_)
        | OpSeq(_, _) => no_holes
        | Inj(_, _)
        | LineItem(_, _)
        | Lam(_, _, _) => {
            holes_before: [],
            hole_selected: None,
            holes_after: holes_e(e, steps, []),
          }
        | Case(e1, rules, ann) =>
          switch (k) {
          | 0 => {
              holes_before: [],
              hole_selected: None,
              holes_after: holes_e(e, steps, []),
            }
          | 1 =>
            let holes_rules = holes_rules(rules, 0, steps, []);
            let holes_before = holes_e(e1, [0, ...steps], holes_rules);
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
  | Deeper(_, ze) =>
    switch (ze) {
    | LineItemZL(zli, e2) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zline_item(zli, [0, ...steps]);
      let holes_e2 = holes_e(e2, [1, ...steps], []);
      {holes_before, hole_selected, holes_after: holes_after @ holes_e2};
    | LineItemZE(li, ze2) =>
      let {holes_before, hole_selected, holes_after} =
        holes_ze(ze2, [1, ...steps]);
      let holes_li = holes_line_item(li, [0, ...steps], []);
      {holes_before: holes_li @ holes_before, hole_selected, holes_after};
    | LamZP(zp, ann, e1) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zpat(zp, [0, ...steps]);
      let holes_ann =
        switch (ann) {
        | Some(uty) => holes_uty(uty, [1, ...steps], [])
        | None => []
        };
      let holes_e1 = holes_e(e1, [2, ...steps], []);
      {
        holes_before,
        hole_selected,
        holes_after: holes_after @ holes_ann @ holes_e1,
      };
    | LamZA(p, zann, e1) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zty(zann, [1, ...steps]);
      let holes_p = holes_pat(p, [0, ...steps], []);
      let holes_e1 = holes_e(e1, [2, ...steps], []);
      {
        holes_before: holes_p @ holes_before,
        hole_selected,
        holes_after: holes_after @ holes_e1,
      };
    | LamZE(p, ann, ze1) =>
      let {holes_before, hole_selected, holes_after} =
        holes_ze(ze1, [2, ...steps]);
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
    | InjZ(_, ze1) => holes_ze(ze1, [0, ...steps])
    | CaseZE(ze1, rules, ann) =>
      let {holes_before, hole_selected, holes_after} =
        holes_ze(ze1, [0, ...steps]);
      let holes_rules = holes_rules(rules, 0, steps, []);
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
    | CaseZR(e1, zrules, ann) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zrules(zrules, steps);
      let holes_e1 = holes_e(e1, [0, ...steps], []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) =>
          holes_uty(uty, [ZList.length(zrules) + 1, ...steps], [])
        };
      {
        holes_before: holes_e1 @ holes_before,
        hole_selected,
        holes_after: holes_after @ holes_ann,
      };
    | CaseZA(e1, rules, zann) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zty(zann, [List.length(rules) + 1, ...steps]);
      let holes_e1 = holes_e(e1, [0, ...steps], []);
      let holes_rules = holes_rules(rules, 0, steps, []);
      {
        holes_before: holes_e1 @ holes_rules @ holes_before,
        hole_selected,
        holes_after,
      };
    | OpSeqZ(_, ze0, surround) =>
      holes_OpSeqZ(holes_e, holes_ze, ze0, surround, steps)
    | ApPaletteZ(_, _, zpsi) =>
      let zsplice_map = zpsi.zsplice_map;
      let (n, (_ty, ze)) = ZNatMap.prj_z_kv(zsplice_map);
      let {holes_before, hole_selected, holes_after} =
        holes_ze(ze, [n, ...steps]);
      let splice_order = zpsi.splice_order;
      let splice_map = ZNatMap.prj_map(zsplice_map);
      let (splices_before, splices_after) =
        HazelUtil.split_at(splice_order, n);
      let holes_splices_before =
        List.fold_left(
          (holes, n) =>
            switch (NatMap.lookup(splice_map, n)) {
            | None => holes
            | Some((_, e)) => holes @ holes_e(e, [n, ...steps], [])
            },
          [],
          splices_before,
        );
      let holes_splices_after =
        List.fold_left(
          (holes, n) =>
            switch (NatMap.lookup(splice_map, n)) {
            | None => holes
            | Some((_, e)) => holes @ holes_e(e, [n, ...steps], [])
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
and holes_zline_item = (zli: ZExp.zline_item, steps: steps): zhole_list =>
  switch (zli) {
  | CursorL(Before, li) => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_line_item(li, steps, []),
    }
  | CursorL(In(_), EmptyLine)
  | CursorL(In(_), ExpLine(_)) => no_holes
  | CursorL(In(_), LetLine(_, _, _) as li) => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_line_item(li, steps, []),
    }
  | CursorL(After, li) => {
      holes_before: holes_line_item(li, steps, []),
      hole_selected: None,
      holes_after: [],
    }
  | DeeperL(zli') => holes_zline_item'(zli', steps)
  }
and holes_zline_item' = (zli': ZExp.zline_item', steps: steps): zhole_list =>
  switch (zli') {
  | ExpLineZ(ze1) => holes_ze(ze1, steps)
  | LetLineZP(zp, ann, e1) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, ...steps]);
    let holes_ann =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], [])
      | None => []
      };
    let holes_e1 = holes_e(e1, [2, ...steps], []);
    {
      holes_before,
      hole_selected,
      holes_after: holes_after @ holes_ann @ holes_e1,
    };
  | LetLineZA(p, zann, e1) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zty(zann, [1, ...steps]);
    let holes_p = holes_pat(p, [0, ...steps], []);
    let holes_e1 = holes_e(e1, [2, ...steps], []);
    {
      holes_before: holes_p @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_e1,
    };
  | LetLineZE(p, ann, ze1) =>
    let {holes_before, hole_selected, holes_after} =
      holes_ze(ze1, [2, ...steps]);
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
and holes_zrules = (zrules: ZExp.zrules, steps: steps) => {
  let (prefix, zrule, suffix) = ZList.prj(zrules);
  let holes_prefix = holes_rules(prefix, 0, steps, []);
  let prefix_len = List.length(prefix);
  let {holes_before, hole_selected, holes_after} =
    holes_zrule(zrule, prefix_len, steps);
  let holes_suffix = holes_rules(suffix, prefix_len + 1, steps, []);
  {
    holes_before: holes_prefix @ holes_before,
    hole_selected,
    holes_after: holes_after @ holes_suffix,
  };
}
and holes_zrule = (zrule: ZExp.zrule, prefix_len: int, steps: steps) =>
  switch (zrule) {
  | RuleZP(zp, e1) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, prefix_len + 1, ...steps]);
    let holes_e1 = holes_e(e1, [1, prefix_len + 1, ...steps], []);
    {holes_before, hole_selected, holes_after: holes_after @ holes_e1};
  | RuleZE(p, ze1) =>
    let {holes_before, hole_selected, holes_after} =
      holes_ze(ze1, [1, prefix_len + 1, ...steps]);
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
    (cursor_side: cursor_side, opt_steps: option(steps)): option(t) =>
  switch (opt_steps) {
  | None => None
  | Some(steps) => Some((List.rev(steps), cursor_side))
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
