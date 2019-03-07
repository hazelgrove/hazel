open SemanticsCommon;
open HazelUtil;

[@deriving show({with_path: false})]
type steps = list(nat);

let string_of_steps = HazelUtil.string_of_list(string_of_int);

[@deriving show({with_path: false})]
type t = (steps, ZExp.cursor_side);

let cons' = (step: nat, r: t): t => {
  let (steps, side) = r;
  ([step, ...steps], side);
};

let rec of_ztyp = (zty: ZTyp.t): t =>
  switch (zty) {
  | ZTyp.CursorT(cursor_side, _) => ([], cursor_side)
  | ZTyp.ParenthesizedZ(zty1) => cons'(0, of_ztyp(zty1))
  | ZTyp.ListZ(zty1) => cons'(0, of_ztyp(zty1))
  | ZTyp.OpSeqZ(_, zty1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_ztyp(zty1));
  };

let rec of_zpat = (zp: ZPat.t): t =>
  switch (zp) {
  | ZPat.CursorP(cursor_side, _) => ([], cursor_side)
  | ZPat.Deeper(_, zp') => of_zpat'(zp')
  | ZPat.ParenthesizedZ(zp1) => cons'(0, of_zpat(zp1))
  }
and of_zpat' = (zp': ZPat.t'): t =>
  switch (zp') {
  | ZPat.InjZ(_, zp1) => cons'(0, of_zpat(zp1))
  | ZPat.OpSeqZ(_, zp1, surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zpat(zp1));
  };

let rec of_zexp = (ze: ZExp.t): t =>
  switch (ze) {
  | ZExp.CursorE(cursor_side, _) => ([], cursor_side)
  | ZExp.Deeper(_, ze') => of_zexp'(ze')
  | ZExp.ParenthesizedZ(ze1) => cons'(0, of_zexp(ze1))
  }
and of_zexp' = (ze: ZExp.t'): t =>
  switch (ze) {
  | ZExp.LineItemZL(zli, _) => cons'(0, of_zline_item(zli))
  | ZExp.LineItemZE(_, ze) => cons'(1, of_zexp(ze))
  | ZExp.LamZP(zp, _, _) => cons'(0, of_zpat(zp))
  | ZExp.LamZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | ZExp.LamZE(_, ann, ze') => cons'(2, of_zexp(ze'))
  | ZExp.InjZ(_, ze') => cons'(0, of_zexp(ze'))
  | ZExp.CaseZE(ze1, _, _) => cons'(0, of_zexp(ze1))
  | ZExp.CaseZR(_, zrules, _) =>
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons'(prefix_len + 1, of_zrule(zrule));
  | ZExp.CaseZA(_, rules, zann) =>
    cons'(List.length(rules) + 1, of_ztyp(zann))
  | ZExp.OpSeqZ(_, ze', surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zexp(ze'));
  | ZExp.ApPaletteZ(_, _, zpsi) =>
    let zhole_map = zpsi.zsplice_map;
    let (n, (_, ze)) = ZNatMap.prj_z_kv(zhole_map);
    cons'(n, of_zexp(ze));
  }
and of_zline_item = (zli: ZExp.zline_item): t =>
  switch (zli) {
  | ZExp.CursorL(side, _) => ([], side)
  | ZExp.DeeperL(zli') => of_zline_item'(zli')
  }
and of_zline_item' = (zli': ZExp.zline_item'): t =>
  switch (zli') {
  | ZExp.ExpLineZ(ze) => cons'(0, of_zexp(ze))
  | ZExp.LetLineZP(zp, _, _) => cons'(0, of_zpat(zp))
  | ZExp.LetLineZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | ZExp.LetLineZE(_, _, ze) => cons'(2, of_zexp(ze))
  }
and of_zrule = (zrule: ZExp.zrule): t =>
  switch (zrule) {
  | ZExp.RuleZP(zp, _) => cons'(0, of_zpat(zp))
  | ZExp.RuleZE(_, ze) => cons'(1, of_zexp(ze))
  };

let of_OpSeqZ = (ze: ZExp.t, surround: ZExp.opseq_surround) => {
  let n = OperatorSeq.surround_prefix_length(surround);
  cons'(n, of_zexp(ze));
};

let of_OpSeqZ_pat = (zp: ZPat.t, surround: ZPat.opseq_surround) => {
  let n = OperatorSeq.surround_prefix_length(surround);
  cons'(n, of_zpat(zp));
};

let rec follow_ty = (path: t, uty: UHTyp.t): option(ZTyp.t) =>
  switch (path) {
  | ([], cursor_side) => Some(ZTyp.CursorT(cursor_side, uty))
  | ([x, ...xs], cursor_side) =>
    switch (uty) {
    | UHTyp.Hole
    | UHTyp.Unit
    | UHTyp.Num
    | UHTyp.Bool => None
    | UHTyp.Parenthesized(uty1) =>
      switch (x) {
      | 0 =>
        switch (follow_ty((xs, cursor_side), uty1)) {
        | Some(zty) => Some(ZTyp.ParenthesizedZ(zty))
        | None => None
        }
      | _ => None
      }
    | UHTyp.List(uty1) =>
      switch (x) {
      | 0 =>
        switch (follow_ty((xs, cursor_side), uty1)) {
        | None => None
        | Some(zty) => Some(ZTyp.ListZ(zty))
        }
      | _ => None
      }
    | UHTyp.OpSeq(skel, seq) =>
      switch (OperatorSeq.split(x, seq)) {
      | Some((uty_n, surround)) =>
        switch (follow_ty((xs, cursor_side), uty_n)) {
        | Some(zty_n) => Some(ZTyp.OpSeqZ(skel, zty_n, surround))
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
  | ([], cursor_side) => Some(ZPat.CursorP(cursor_side, p))
  | ([x, ...xs], cursor_side) =>
    switch (p) {
    | UHPat.Parenthesized(p1) =>
      switch (x) {
      | 0 =>
        switch (follow_pat((xs, cursor_side), p1)) {
        | None => None
        | Some(zp1) => Some(ZPat.ParenthesizedZ(zp1))
        }
      | _ => None
      }
    | UHPat.Pat(err_status, p') =>
      switch (x, p') {
      | (_, UHPat.EmptyHole(_))
      | (_, UHPat.Wild)
      | (_, UHPat.Var(_))
      | (_, UHPat.NumLit(_))
      | (_, UHPat.BoolLit(_))
      | (_, UHPat.ListNil) => None
      | (0, UHPat.Inj(side, p1)) =>
        switch (follow_pat((xs, cursor_side), p1)) {
        | None => None
        | Some(zp1) => Some(ZPat.Deeper(err_status, ZPat.InjZ(side, zp1)))
        }
      | (_, UHPat.Inj(_, _)) => None
      | (n, UHPat.OpSeq(skel, seq)) =>
        switch (OperatorSeq.split(n, seq)) {
        | None => None
        | Some((p, surround)) =>
          switch (follow_pat((xs, cursor_side), p)) {
          | Some(zp) =>
            Some(ZPat.Deeper(err_status, ZPat.OpSeqZ(skel, zp, surround)))
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
  | ([], cursor_side) => Some(ZExp.CursorE(cursor_side, e))
  | ([x, ...xs], cursor_side) =>
    switch (e) {
    | UHExp.Parenthesized(e1) =>
      switch (x) {
      | 0 =>
        switch (follow_e((xs, cursor_side), e1)) {
        | Some(ze1) => Some(ZExp.ParenthesizedZ(ze1))
        | None => None
        }
      | _ => None
      }
    | UHExp.Tm(err_status, e) =>
      switch (x, e) {
      | (_, UHExp.EmptyHole(_)) => None
      | (_, UHExp.Var(_, _)) => None
      | (0, UHExp.LineItem(li, e1)) =>
        switch (follow_line_item((xs, cursor_side), li)) {
        | None => None
        | Some(zli) =>
          Some(ZExp.Deeper(err_status, ZExp.LineItemZL(zli, e1)))
        }
      | (1, UHExp.LineItem(li, e1)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | None => None
        | Some(ze1) =>
          Some(ZExp.Deeper(err_status, ZExp.LineItemZE(li, ze1)))
        }
      | (_, UHExp.LineItem(_, _)) => None
      | (0, UHExp.Lam(p, ann, e1)) =>
        switch (follow_pat((xs, cursor_side), p)) {
        | None => None
        | Some(zp) => Some(ZExp.Deeper(err_status, ZExp.LamZP(zp, ann, e1)))
        }
      | (1, UHExp.Lam(p, ann, e1)) =>
        switch (ann) {
        | None => None
        | Some(ann_ty) =>
          switch (follow_ty((xs, cursor_side), ann_ty)) {
          | None => None
          | Some(zann) =>
            Some(ZExp.Deeper(err_status, ZExp.LamZA(p, zann, e1)))
          }
        }
      | (2, UHExp.Lam(p, ann, e1)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | None => None
        | Some(ze) => Some(ZExp.Deeper(err_status, ZExp.LamZE(p, ann, ze)))
        }
      | (_, UHExp.Lam(_, _, _)) => None
      | (_, UHExp.NumLit(_)) => None
      | (_, UHExp.BoolLit(_)) => None
      | (0, UHExp.Inj(side, e1)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | Some(ze) => Some(ZExp.Deeper(err_status, ZExp.InjZ(side, ze)))
        | None => None
        }
      | (_, UHExp.Inj(_, _)) => None
      | (_, UHExp.ListNil) => None
      | (0, UHExp.Case(e1, rules, ann)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | Some(ze) =>
          Some(ZExp.Deeper(err_status, ZExp.CaseZE(ze, rules, ann)))
        | None => None
        }
      | (x, UHExp.Case(e1, rules, ann)) when x === List.length(rules) + 1 =>
        switch (ann) {
        | None => None
        | Some(ty) =>
          switch (follow_ty((xs, cursor_side), ty)) {
          | None => None
          | Some(zann) =>
            Some(ZExp.Deeper(err_status, ZExp.CaseZA(e1, rules, zann)))
          }
        }
      | (x, UHExp.Case(e1, rules, ann)) =>
        switch (ZList.split_at(x - 1, rules)) {
        | None => None
        | Some(split_rules) =>
          switch (
            ZList.optmap_z(follow_rule((xs, cursor_side)), split_rules)
          ) {
          | None => None
          | Some(zrules) =>
            Some(ZExp.Deeper(err_status, ZExp.CaseZR(e1, zrules, ann)))
          }
        }
      | (n, UHExp.OpSeq(skel, seq)) =>
        switch (OperatorSeq.split(n, seq)) {
        | Some((e, surround)) =>
          switch (follow_e((xs, cursor_side), e)) {
          | Some(ze) =>
            Some(ZExp.Deeper(err_status, ZExp.OpSeqZ(skel, ze, surround)))
          | None => None
          }
        | None => None
        }
      | (n, UHExp.ApPalette(name, serialized_model, splice_info)) =>
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
            ZExp.Deeper(
              NotInHole,
              ZExp.ApPaletteZ(name, serialized_model, zsplice_info),
            ),
          )
        }
      }
    }
  }
and follow_line_item =
    (path: t, li: UHExp.line_item): option(ZExp.zline_item) =>
  switch (path, li) {
  | (([], cursor_side), li) => Some(ZExp.CursorL(cursor_side, li))
  | (_, UHExp.EmptyLine) => None
  | (([0, ...xs], cursor_side), UHExp.ExpLine(e)) =>
    switch (follow_e((xs, cursor_side), e)) {
    | None => None
    | Some(ze) => Some(ZExp.DeeperL(ZExp.ExpLineZ(ze)))
    }
  | (_, UHExp.ExpLine(_)) => None
  | (([0, ...xs], cursor_side), UHExp.LetLine(p, ann, e1)) =>
    switch (follow_pat((xs, cursor_side), p)) {
    | None => None
    | Some(zp) => Some(ZExp.DeeperL(ZExp.LetLineZP(zp, ann, e1)))
    }
  | (([1, ...xs], cursor_side), UHExp.LetLine(p, ann, e1)) =>
    switch (ann) {
    | None => None
    | Some(ann_ty) =>
      switch (follow_ty((xs, cursor_side), ann_ty)) {
      | None => None
      | Some(zann) => Some(ZExp.DeeperL(ZExp.LetLineZA(p, zann, e1)))
      }
    }
  | (([2, ...xs], cursor_side), UHExp.LetLine(p, ann, e1)) =>
    switch (follow_e((xs, cursor_side), e1)) {
    | None => None
    | Some(ze1) => Some(ZExp.DeeperL(ZExp.LetLineZE(p, ann, ze1)))
    }
  | (_, UHExp.LetLine(_, _, _)) => None
  }
and follow_rule = (path: t, rule: UHExp.rule): option(ZExp.zrule) =>
  switch (rule) {
  | UHExp.Rule(p, e) =>
    switch (path) {
    | ([], _) => None
    | ([0, ...xs], cursor_side) =>
      switch (follow_pat((xs, cursor_side), p)) {
      | None => None
      | Some(zp) => Some(ZExp.RuleZP(zp, e))
      }
    | ([1, ...xs], cursor_side) =>
      switch (follow_e((xs, cursor_side), e)) {
      | None => None
      | Some(ze) => Some(ZExp.RuleZE(p, ze))
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

let string_of_hole_list = hole_list =>
  HazelUtil.string_of_list(
    HazelUtil.string_of_pair(string_of_hole_desc, string_of_steps),
    hole_list,
  );

let rec holes_seq = (seq, holes_tm, offset, steps, holes): hole_list =>
  switch (seq) {
  | OperatorSeq.ExpOpExp(e1, _, e2) =>
    let holes = holes_tm(e2, [offset + 1, ...steps], holes);
    holes_tm(e1, [offset, ...steps], holes);
  | OperatorSeq.SeqOpExp(seq1, op, e2) =>
    let holes =
      holes_tm(
        e2,
        [offset + OperatorSeq.seq_length(seq1), ...steps],
        holes,
      );
    holes_seq(seq1, holes_tm, offset, steps, holes);
  };

let rec holes_uty = (uty, steps, holes): hole_list =>
  switch (uty) {
  | UHTyp.Parenthesized(uty1) => holes_uty(uty1, [0, ...steps], holes)
  | UHTyp.Hole => [(TypeHole, steps), ...holes]
  | UHTyp.Unit => holes
  | UHTyp.Num => holes
  | UHTyp.Bool => holes
  | UHTyp.List(uty1) => holes_uty(uty1, [0, ...steps], holes)
  | UHTyp.OpSeq(_, seq) => holes_seq(seq, holes_uty, 0, steps, holes)
  };

let rec holes_pat = (p, steps, holes): hole_list =>
  switch (p) {
  | UHPat.Parenthesized(p1) => holes_pat(p1, [0, ...steps], holes)
  | UHPat.Pat(_, UHPat.EmptyHole(u)) => [(PatHole(u), steps), ...holes]
  | UHPat.Pat(_, UHPat.Wild) => holes
  | UHPat.Pat(_, UHPat.Var(_)) => holes
  | UHPat.Pat(_, UHPat.NumLit(_)) => holes
  | UHPat.Pat(_, UHPat.BoolLit(_)) => holes
  | UHPat.Pat(_, UHPat.ListNil) => holes
  | UHPat.Pat(_, UHPat.Inj(_, p1)) => holes_pat(p1, [0, ...steps], holes)
  | UHPat.Pat(_, UHPat.OpSeq(skel, seq)) =>
    holes_seq(seq, holes_pat, 0, steps, holes)
  };

let rec holes_e = (e, steps, holes): hole_list =>
  switch (e) {
  | UHExp.Parenthesized(e1) => holes_e(e1, [0, ...steps], holes)
  | UHExp.Tm(_, UHExp.EmptyHole(u)) => [(ExpHole(u), steps), ...holes]
  | UHExp.Tm(_, UHExp.Var(_, _)) => holes
  | UHExp.Tm(_, UHExp.NumLit(_)) => holes
  | UHExp.Tm(_, UHExp.BoolLit(_)) => holes
  | UHExp.Tm(_, UHExp.Inj(_, e1)) => holes_e(e1, [0, ...steps], holes)
  | UHExp.Tm(_, UHExp.ListNil) => holes
  | UHExp.Tm(_, UHExp.Lam(p, ann, e1)) =>
    let holes = holes_e(e1, [2, ...steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...steps], holes);
  | UHExp.Tm(_, UHExp.LineItem(li, e2)) =>
    let holes = holes_e(e2, [1, ...steps], holes);
    holes_line_item(li, [0, ...steps], holes);
  | UHExp.Tm(_, UHExp.Case(e1, rules, ann)) =>
    let holes =
      switch (ann) {
      | None => holes
      | Some(uty) =>
        holes_uty(uty, [List.length(rules) + 1, ...steps], holes)
      };
    let holes = holes_rules(rules, 0, steps, holes);
    holes_e(e1, [0, ...steps], holes);
  | UHExp.Tm(_, UHExp.OpSeq(skel, seq)) =>
    holes_seq(seq, holes_e, 0, steps, holes)
  | UHExp.Tm(_, UHExp.ApPalette(_, _, psi)) =>
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
and holes_line_item = (li, steps, holes) =>
  switch (li) {
  | UHExp.EmptyLine => holes
  | UHExp.ExpLine(e1) => holes_e(e1, steps, holes)
  | UHExp.LetLine(p, ann, e1) =>
    let holes = holes_e(e1, [2, ...steps], holes);
    let holes =
      switch (ann) {
      | Some(uty) => holes_uty(uty, [1, ...steps], holes)
      | None => holes
      };
    holes_pat(p, [0, ...steps], holes);
  }
and holes_rules = (rules, offset, steps, holes) => {
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

let holes_prefix = (holes_fn, prefix, steps): hole_list =>
  switch (prefix) {
  | OperatorSeq.ExpPrefix(e, _) => holes_fn(e, [0, ...steps], [])
  | OperatorSeq.SeqPrefix(seq, _) => holes_seq(seq, holes_fn, 0, steps, [])
  };

let holes_suffix = (holes_fn, suffix, prefix_len, steps): hole_list =>
  switch (suffix) {
  | OperatorSeq.ExpSuffix(_, e) =>
    holes_fn(e, [prefix_len + 1, ...steps], [])
  | OperatorSeq.SeqSuffix(_, seq) =>
    holes_seq(seq, holes_fn, prefix_len + 1, steps, [])
  };

let holes_surround = (holes_fn, surround, steps): (hole_list, hole_list) =>
  switch (surround) {
  | OperatorSeq.EmptyPrefix(suffix) => (
      [],
      holes_suffix(holes_fn, suffix, 0, steps),
    )
  | OperatorSeq.EmptySuffix(prefix) => (
      holes_prefix(holes_fn, prefix, steps),
      [],
    )
  | OperatorSeq.BothNonEmpty(prefix, suffix) =>
    let prefix_len = OperatorSeq.prefix_length(prefix);
    (
      holes_prefix(holes_fn, prefix, steps),
      holes_suffix(holes_fn, suffix, prefix_len, steps),
    );
  };

let holes_OpSeqZ = (holes_fn, zholes_fn, z0, surround, steps) => {
  let (holes_prefix, holes_suffix) =
    holes_surround(holes_fn, surround, steps);
  let prefix_len = OperatorSeq.surround_prefix_length(surround);
  let {holes_before, hole_selected, holes_after} =
    zholes_fn(z0, [prefix_len, ...steps]);
  let holes_before = holes_prefix @ holes_before;
  let holes_after = holes_after @ holes_suffix;
  {holes_before, hole_selected, holes_after};
};

let rec holes_zty = (zty, steps): zhole_list =>
  switch (zty) {
  | ZTyp.CursorT(cursor_side, uty) =>
    switch (cursor_side, uty) {
    | (_, UHTyp.Hole) => {
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
      | UHTyp.Parenthesized(_)
      | UHTyp.Hole
      | UHTyp.Unit
      | UHTyp.Num
      | UHTyp.Bool
      | UHTyp.OpSeq(_, _) => no_holes
      | UHTyp.List(uty1) => {
          holes_before: [],
          hole_selected: None,
          holes_after: holes_uty(uty1, steps, []),
        }
      }
    }
  | ZTyp.ParenthesizedZ(zty1) => holes_zty(zty1, [0, ...steps])
  | ZTyp.ListZ(zty1) => holes_zty(zty1, [0, ...steps])
  | ZTyp.OpSeqZ(_, zty0, surround) =>
    holes_OpSeqZ(holes_uty, holes_zty, zty0, surround, steps)
  };

let rec holes_zpat = (zp, steps): zhole_list =>
  switch (zp) {
  | ZPat.CursorP(cursor_side, p) =>
    switch (cursor_side, p) {
    | (_, UHPat.Pat(_, UHPat.EmptyHole(u))) => {
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
      | UHPat.Parenthesized(_) => no_holes
      | UHPat.Pat(_, p') =>
        switch (p') {
        | UHPat.Wild
        | UHPat.Var(_)
        | UHPat.NumLit(_)
        | UHPat.BoolLit(_)
        | UHPat.ListNil
        | UHPat.OpSeq(_, _)
        | UHPat.EmptyHole(_) => no_holes
        | UHPat.Inj(_, p1) => {
            holes_before: [],
            hole_selected: None,
            holes_after: holes_pat(p1, [0, ...steps], []),
          }
        }
      }
    }
  | ZPat.ParenthesizedZ(zp1) => holes_zpat(zp1, [0, ...steps])
  | ZPat.Deeper(_, ZPat.InjZ(_, zp1)) => holes_zpat(zp1, [0, ...steps])
  | ZPat.Deeper(_, ZPat.OpSeqZ(_, zp1, surround)) =>
    holes_OpSeqZ(holes_pat, holes_zpat, zp1, surround, steps)
  };

let rec holes_ze = (ze, steps): zhole_list =>
  switch (ze) {
  | ZExp.ParenthesizedZ(ze1) => holes_ze(ze1, [0, ...steps])
  | ZExp.CursorE(cursor_side, e) =>
    switch (cursor_side, e) {
    | (_, UHExp.Tm(_, UHExp.EmptyHole(u))) => {
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
      | UHExp.Parenthesized(_) => no_holes
      | UHExp.Tm(err, ue') =>
        switch (ue') {
        | UHExp.NumLit(_)
        | UHExp.BoolLit(_)
        | UHExp.ListNil
        | UHExp.Var(_, _)
        | UHExp.EmptyHole(_)
        | UHExp.OpSeq(_, _) => no_holes
        | UHExp.Inj(_, _)
        | UHExp.LineItem(_, _)
        | UHExp.Lam(_, _, _) => {
            holes_before: [],
            hole_selected: None,
            holes_after: holes_e(e, steps, []),
          }
        | UHExp.Case(e1, rules, ann) =>
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
        | UHExp.ApPalette(_, _, _) => no_holes
        }
      }
    }
  | ZExp.Deeper(_, ze) =>
    switch (ze) {
    | ZExp.LineItemZL(zli, e2) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zline_item(zli, [0, ...steps]);
      let holes_e2 = holes_e(e2, [1, ...steps], []);
      {holes_before, hole_selected, holes_after: holes_after @ holes_e2};
    | ZExp.LineItemZE(li, ze2) =>
      let {holes_before, hole_selected, holes_after} =
        holes_ze(ze2, [1, ...steps]);
      let holes_li = holes_line_item(li, [0, ...steps], []);
      {holes_before: holes_li @ holes_before, hole_selected, holes_after};
    | ZExp.LamZP(zp, ann, e1) =>
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
    | ZExp.LamZA(p, zann, e1) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zty(zann, [1, ...steps]);
      let holes_p = holes_pat(p, [0, ...steps], []);
      let holes_e1 = holes_e(e1, [2, ...steps], []);
      {
        holes_before: holes_p @ holes_before,
        hole_selected,
        holes_after: holes_after @ holes_e1,
      };
    | ZExp.LamZE(p, ann, ze1) =>
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
    | ZExp.InjZ(_, ze1) => holes_ze(ze1, [0, ...steps])
    | ZExp.CaseZE(ze1, rules, ann) =>
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
    | ZExp.CaseZR(e1, zrules, ann) =>
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
    | ZExp.CaseZA(e1, rules, zann) =>
      let {holes_before, hole_selected, holes_after} =
        holes_zty(zann, [List.length(rules) + 1, ...steps]);
      let holes_e1 = holes_e(e1, [0, ...steps], []);
      let holes_rules = holes_rules(rules, 0, steps, []);
      {
        holes_before: holes_e1 @ holes_rules @ holes_before,
        hole_selected,
        holes_after,
      };
    | ZExp.OpSeqZ(_, ze0, surround) =>
      holes_OpSeqZ(holes_e, holes_ze, ze0, surround, steps)
    | ZExp.ApPaletteZ(_, _, zpsi) =>
      let zsplice_map = zpsi.zsplice_map;
      let (n, (ty, ze)) = ZNatMap.prj_z_kv(zsplice_map);
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
and holes_zline_item = (zli, steps) =>
  switch (zli) {
  | ZExp.CursorL(Before, li) => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_line_item(li, steps, []),
    }
  | ZExp.CursorL(In(_), UHExp.EmptyLine)
  | ZExp.CursorL(In(_), UHExp.ExpLine(_)) => no_holes
  | ZExp.CursorL(In(_), UHExp.LetLine(_, _, _) as li) => {
      holes_before: [],
      hole_selected: None,
      holes_after: holes_line_item(li, steps, []),
    }
  | ZExp.CursorL(After, li) => {
      holes_before: holes_line_item(li, steps, []),
      hole_selected: None,
      holes_after: [],
    }
  | ZExp.DeeperL(zli') => holes_zline_item'(zli', steps)
  }
and holes_zline_item' = (zli', steps) =>
  switch (zli') {
  | ZExp.ExpLineZ(ze1) => holes_ze(ze1, steps)
  | ZExp.LetLineZP(zp, ann, e1) =>
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
  | ZExp.LetLineZA(p, zann, e1) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zty(zann, [1, ...steps]);
    let holes_p = holes_pat(p, [0, ...steps], []);
    let holes_e1 = holes_e(e1, [2, ...steps], []);
    {
      holes_before: holes_p @ holes_before,
      hole_selected,
      holes_after: holes_after @ holes_e1,
    };
  | ZExp.LetLineZE(p, ann, ze1) =>
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
and holes_zrules = (zrules, steps) => {
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
and holes_zrule = (zrule, prefix_len, steps) =>
  switch (zrule) {
  | ZExp.RuleZP(zp, e1) =>
    let {holes_before, hole_selected, holes_after} =
      holes_zpat(zp, [0, prefix_len + 1, ...steps]);
    let holes_e1 = holes_e(e1, [1, prefix_len + 1, ...steps], []);
    {holes_before, hole_selected, holes_after: holes_after @ holes_e1};
  | ZExp.RuleZE(p, ze1) =>
    let {holes_before, hole_selected, holes_after} =
      holes_ze(ze1, [1, prefix_len + 1, ...steps]);
    let holes_p = holes_pat(p, [0, prefix_len + 1, ...steps], []);
    {holes_before: holes_p @ holes_before, hole_selected, holes_after};
  };

let rec steps_to_hole = (hole_list, u): option(steps) =>
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

let rec steps_to_hole_z = (zhole_list, u): option(steps) => {
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

let opt_steps_to_opt_path = cursor_side =>
  fun
  | None => None
  | Some(steps) => Some((List.rev(steps), cursor_side));

let path_to_hole = (hole_list, u): option(t) =>
  opt_steps_to_opt_path(Before, steps_to_hole(hole_list, u));

let path_to_hole_z = (zhole_list, u): option(t) =>
  opt_steps_to_opt_path(Before, steps_to_hole_z(zhole_list, u));

let next_hole_steps = (zhole_list): option(steps) => {
  let holes_after = zhole_list.holes_after;
  switch (holes_after) {
  | [] => None
  | [(_, steps), ..._] => Some(steps)
  };
};

let next_hole_path = zhole_list =>
  opt_steps_to_opt_path(Before, next_hole_steps(zhole_list));

let prev_hole_steps = (zhole_list): option(steps) => {
  let holes_before = zhole_list.holes_before;
  switch (List.rev(holes_before)) {
  | [] => None
  | [(_, steps), ..._] => Some(steps)
  };
};

let prev_hole_path = zhole_list =>
  opt_steps_to_opt_path(Before, prev_hole_steps(zhole_list));
