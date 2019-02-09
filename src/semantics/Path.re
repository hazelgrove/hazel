open Util;

type nat = int;

type t = (list(nat), ZExp.cursor_side);

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
  /* | ZPat.ListLitZ zps ->
     let prefix_length = ZList.prefix_length zps in
     let zp0 = ZList.prj_z zps in
     cons' prefix_length (of_zpat zp0) */
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
  | ZExp.AscZ1(ze', _) => cons'(0, of_zexp(ze'))
  | ZExp.AscZ2(_, zty) => cons'(1, of_ztyp(zty))
  | ZExp.LineItemZL(zli, _) => cons'(0, of_zline_item(zli))
  | ZExp.LineItemZE(_, ze) => cons'(1, of_zexp(ze))
  | ZExp.LamZP(zp, _, _) => cons'(0, of_zpat(zp))
  | ZExp.LamZA(_, zann, _) => cons'(1, of_ztyp(zann))
  | ZExp.LamZE(_, ann, ze') => cons'(2, of_zexp(ze'))
  | ZExp.InjZ(_, ze') => cons'(0, of_zexp(ze'))
  /* | ZExp.ListLitZ zes ->
     let prefix_length = ZList.prefix_length zes in
     let ze0 = ZList.prj_z zes in
     cons' prefix_length (of_zexp ze0) */
  | ZExp.CaseZE(ze1, _) => cons'(0, of_zexp(ze1))
  | ZExp.CaseZR(_, zrules) =>
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons'(prefix_len + 1, of_zrule(zrule));
  | ZExp.OpSeqZ(_, ze', surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    cons'(n, of_zexp(ze'));
  | ZExp.ApPaletteZ(_, _, zholedata) =>
    let (_, zholemap) = zholedata;
    let (_, tz) = zholemap;
    let (n, tz') = tz;
    let (_, ze') = tz';
    cons'(n, of_zexp(ze'));
  }
and of_zline_item = (zli: ZExp.zline_item): t =>
  switch (zli) {
  | ZExp.EmptyLineZ => ([], Before)
  | ZExp.ExpLineZ(ze) => of_zexp(ze)
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
      /* | (n, UHPat.ListLit ps) ->
         begin match ZList.split_at n ps with
         | None -> None
         | Some psz ->
           begin match ZList.optmap_z (follow_pat (xs, cursor_side)) psz with
           | None -> None
           | Some zps ->
             Some (ZPat.Deeper err_status (ZPat.ListLitZ zps))
           end
         end*/
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
      | (0, UHExp.Asc(e1, ty)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | Some(ze) => Some(ZExp.Deeper(err_status, ZExp.AscZ1(ze, ty)))
        | None => None
        }
      | (1, UHExp.Asc(e1, ty)) =>
        switch (follow_ty((xs, cursor_side), ty)) {
        | Some(ztau) => Some(ZExp.Deeper(err_status, ZExp.AscZ2(e1, ztau)))
        | None => None
        }
      | (_, UHExp.Asc(_, _)) => None
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
      /* | (n, UHExp.ListLit es) ->
         begin match ZList.split_at n es with
         | None -> None
         | Some esz ->
           begin match ZList.optmap_z (follow_e (xs, cursor_side)) esz with
           | None -> None
           | Some zes ->
             Some (ZExp.Deeper err_status (ZExp.ListLitZ zes))
           end
         end*/
      | (0, UHExp.Case(e1, rules)) =>
        switch (follow_e((xs, cursor_side), e1)) {
        | Some(ze) => Some(ZExp.Deeper(err_status, ZExp.CaseZE(ze, rules)))
        | None => None
        }
      | (x, UHExp.Case(e1, rules)) =>
        switch (ZList.split_at(x - 1, rules)) {
        | None => None
        | Some(split_rules) =>
          switch (
            ZList.optmap_z(follow_rule((xs, cursor_side)), split_rules)
          ) {
          | None => None
          | Some(zrules) =>
            Some(ZExp.Deeper(err_status, ZExp.CaseZR(e1, zrules)))
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
      | (hole_ref, UHExp.ApPalette(name, serialized_model, hole_data)) =>
        let (next_hole_ref, holemap) = hole_data;
        switch (NatMap.drop(holemap, hole_ref)) {
        | None => None
        | Some((holemap', te)) =>
          let (ty, e') = te;
          switch (follow_e((xs, cursor_side), e')) {
          | None => None
          | Some(ze) =>
            let zholemap = (holemap', (hole_ref, (ty, ze)));
            let zholedata = (next_hole_ref, zholemap);
            Some(
              ZExp.Deeper(
                NotInHole,
                ZExp.ApPaletteZ(name, serialized_model, zholedata),
              ),
            );
          };
        };
      }
    }
  }
and follow_line_item =
    (path: t, li: UHExp.line_item): option(ZExp.zline_item) =>
  switch (path, li) {
  | (([], _), UHExp.EmptyLine) => Some(ZExp.EmptyLineZ)
  | (_, UHExp.EmptyLine) => None
  | (_, UHExp.ExpLine(e)) =>
    switch (follow_e(path, e)) {
    | None => None
    | Some(ze) => Some(ZExp.ExpLineZ(ze))
    }
  | (([0, ...xs], cursor_side), UHExp.LetLine(p, ann, e1)) =>
    switch (follow_pat((xs, cursor_side), p)) {
    | None => None
    | Some(zp) => Some(ZExp.LetLineZP(zp, ann, e1))
    }
  | (([1, ...xs], cursor_side), UHExp.LetLine(p, ann, e1)) =>
    switch (ann) {
    | None => None
    | Some(ann_ty) =>
      switch (follow_ty((xs, cursor_side), ann_ty)) {
      | None => None
      | Some(zann) => Some(ZExp.LetLineZA(p, zann, e1))
      }
    }
  | (([2, ...xs], cursor_side), UHExp.LetLine(p, ann, e1)) =>
    switch (follow_e((xs, cursor_side), e1)) {
    | None => None
    | Some(ze1) => Some(ZExp.LetLineZE(p, ann, ze1))
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

let cons_opt = (n: nat, x: option(list(nat))): option(list(nat)) =>
  switch (x) {
  | None => None
  | Some(xs) => Some([n, ...xs])
  };

let cons_opt2 =
    (
      n1: nat,
      x1: option(list(nat)),
      n2: nat,
      x2: unit => option(list(nat)),
    )
    : option(list(nat)) =>
  switch (x1) {
  | Some(xs) => Some([n1, ...xs])
  | None =>
    switch (x2()) {
    | Some(xs) => Some([n2, ...xs])
    | None => None
    }
  };

let cons_opt3 =
    (
      n1: nat,
      x1: option(list(nat)),
      n2: nat,
      x2: unit => option(list(nat)),
      n3: nat,
      x3: unit => option(list(nat)),
    )
    : option(list(nat)) =>
  switch (x1) {
  | Some(xs) => Some([n1, ...xs])
  | None =>
    switch (x2()) {
    | Some(xs) => Some([n2, ...xs])
    | None =>
      switch (x3()) {
      | Some(xs) => Some([n3, ...xs])
      | None => None
      }
    }
  };

let rec steps_to_hole_pat = (p: UHPat.t, u: MetaVar.t): option(list(nat)) =>
  switch (p) {
  | UHPat.Pat(_, UHPat.EmptyHole(u')) =>
    if (MetaVar.eq(u, u')) {
      Some([]);
    } else {
      None;
    }
  | UHPat.Parenthesized(p1) => cons_opt(0, steps_to_hole_pat(p1, u))
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, UHPat.Var(_))
  | UHPat.Pat(_, UHPat.NumLit(_))
  | UHPat.Pat(_, UHPat.BoolLit(_))
  | UHPat.Pat(_, UHPat.ListNil) => None
  /* | UHPat.Pat _ (UHPat.ListLit ps) ->
     Util.findmapi ps (fun i p ->
       begin match steps_to_hole_pat p u with
       | None -> None
       | Some ns -> Some (i :: ns)
       end */
  | UHPat.Pat(_, UHPat.Inj(_, p1)) => cons_opt(0, steps_to_hole_pat(p1, u))
  | UHPat.Pat(_, UHPat.OpSeq(skel, seq)) => steps_to_hole_seq_pat(seq, u)
  }
and steps_to_hole_seq_pat =
    (seq: UHPat.opseq, u: MetaVar.t): option(list(nat)) =>
  switch (seq) {
  | OperatorSeq.ExpOpExp(p1, _, p2) =>
    cons_opt2(0, steps_to_hole_pat(p1, u), 1, _ => steps_to_hole_pat(p2, u))
  | OperatorSeq.SeqOpExp(seq1, op, p1) =>
    switch (steps_to_hole_seq_pat(seq1, u)) {
    | Some(steps) as path => path
    | None =>
      cons_opt(OperatorSeq.seq_length(seq1), steps_to_hole_pat(p1, u))
    }
  };

let rec steps_to_hole = (e: UHExp.t, u: MetaVar.t): option(list(nat)) =>
  switch (e) {
  | UHExp.Tm(_, UHExp.EmptyHole(u')) =>
    if (MetaVar.eq(u, u')) {
      Some([]);
    } else {
      None;
    }
  | UHExp.Parenthesized(e1) => cons_opt(0, steps_to_hole(e1, u))
  | UHExp.Tm(_, UHExp.Var(_, _))
  | UHExp.Tm(_, UHExp.NumLit(_))
  | UHExp.Tm(_, UHExp.BoolLit(_)) => None
  | UHExp.Tm(_, UHExp.Asc(e1, _))
  | UHExp.Tm(_, UHExp.Inj(_, e1)) => cons_opt(0, steps_to_hole(e1, u))
  | UHExp.Tm(_, UHExp.ListNil) => None
  /* | UHExp.Tm _ (UHExp.ListLit es) ->
     Util.findmapi es (fun i e ->
       begin match steps_to_hole e u with
       | None -> None
       | Some ns -> Some (i :: ns)
       end */
  | UHExp.Tm(_, UHExp.Lam(p, _, e1)) =>
    cons_opt2(0, steps_to_hole_pat(p, u), 2, _ => steps_to_hole(e1, u))
  | UHExp.Tm(_, UHExp.LineItem(li, e2)) =>
    let li_steps =
      switch (li) {
      | UHExp.EmptyLine => None
      | UHExp.ExpLine(e1) => steps_to_hole(e1, u)
      | UHExp.LetLine(p, ann, e1) =>
        cons_opt2(0, steps_to_hole_pat(p, u), 2, _ => steps_to_hole(e1, u))
      };
    cons_opt2(0, li_steps, 1, _ => steps_to_hole(e2, u));
  | UHExp.Tm(_, UHExp.Case(e1, rules)) =>
    switch (steps_to_hole(e1, u)) {
    | Some(steps) => Some([0, ...steps])
    | None =>
      Util.findmapi(rules, (i, rule) =>
        switch (rule) {
        | UHExp.Rule(p, e) =>
          switch (steps_to_hole_pat(p, u)) {
          | Some(steps) => Some([i + 1, 0, ...steps])
          | None =>
            switch (steps_to_hole(e, u)) {
            | Some(steps) => Some([i + 1, 1, ...steps])
            | None => None
            }
          }
        }
      )
    }
  | UHExp.Tm(_, UHExp.OpSeq(skel, seq)) => steps_to_hole_seq(seq, u)
  | UHExp.Tm(_, UHExp.ApPalette(_, _, holedata)) =>
    let (_, holemap) = holedata;
    NatMap.fold(
      holemap,
      (c, v) =>
        switch (c) {
        | Some(_) => c
        | None =>
          let (_, te) = v;
          let (_, e) = te;
          steps_to_hole(e, u);
        },
      None,
    );
  }
and steps_to_hole_seq = (seq: UHExp.opseq, u: MetaVar.t): option(list(nat)) =>
  switch (seq) {
  | OperatorSeq.ExpOpExp(e1, _, e2) =>
    cons_opt2(0, steps_to_hole(e1, u), 1, _ => steps_to_hole(e2, u))
  | OperatorSeq.SeqOpExp(seq1, op, e1) =>
    switch (steps_to_hole_seq(seq1, u)) {
    | Some(steps) as path => path
    | None => cons_opt(OperatorSeq.seq_length(seq1), steps_to_hole(e1, u))
    }
  };

let path_to_hole = (e: UHExp.t, u: MetaVar.t): option(t) =>
  switch (steps_to_hole(e, u)) {
  | Some(steps) => Some((steps, Before))
  | None => None
  };

let rec first_hole_steps_ty = (uty: UHTyp.t): option(list(nat)) =>
  switch (uty) {
  | UHTyp.Parenthesized(uty') => cons_opt(0, first_hole_steps_ty(uty'))
  | UHTyp.Unit
  | UHTyp.Num
  | UHTyp.Bool => None
  | UHTyp.Hole => Some([])
  | UHTyp.List(uty1) => cons_opt(0, first_hole_steps_ty(uty1))
  | UHTyp.OpSeq(_, opseq) => first_hole_steps_ty_opseq(opseq, 0)
  }
/* return an optional path of the first hole in opseq starting and the nth term */
and first_hole_steps_ty_opseq =
    (opseq: UHTyp.opseq, n: nat): option(list(nat)) =>
  if (OperatorSeq.seq_length(opseq) <= n) {
    None;
  } else {
    switch (OperatorSeq.seq_nth(n, opseq)) {
    | None => None /* degenerate case */
    | Some(uty') =>
      switch (first_hole_steps_ty(uty')) {
      | Some(ns) => Some([n, ...ns])
      | None => first_hole_steps_ty_opseq(opseq, n + 1)
      }
    };
  };

let rec first_hole_steps_pat = (p: UHPat.t): option(list(nat)) =>
  switch (p) {
  | UHPat.Parenthesized(p1) => cons_opt(0, first_hole_steps_pat(p1))
  | UHPat.Pat(_, UHPat.EmptyHole(_)) => Some([])
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, UHPat.Var(_))
  | UHPat.Pat(_, UHPat.NumLit(_))
  | UHPat.Pat(_, UHPat.BoolLit(_)) => None
  | UHPat.Pat(_, UHPat.Inj(_, p1)) => cons_opt(0, first_hole_steps_pat(p1))
  | UHPat.Pat(_, UHPat.ListNil) => None
  /* | UHPat.Pat _ (UHPat.ListLit ps) ->
     Util.findmapi ps (fun i p ->
       begin match first_hole_steps_pat p with
       | None -> None
       | Some ns -> Some (i :: ns)
       end */
  | UHPat.Pat(_, UHPat.OpSeq(_, seq)) => first_hole_steps_opseq_pat(seq, 0)
  }
and first_hole_steps_opseq_pat =
    (opseq: UHPat.opseq, n: nat): option(list(nat)) =>
  if (OperatorSeq.seq_length(opseq) <= n) {
    None;
  } else {
    switch (OperatorSeq.seq_nth(n, opseq)) {
    | None => None
    | Some(ue) =>
      switch (first_hole_steps_pat(ue)) {
      | Some(ns) => Some([n, ...ns])
      | None => first_hole_steps_opseq_pat(opseq, n + 1)
      }
    };
  };

let rec first_hole_steps = (ue: UHExp.t): option(list(nat)) =>
  switch (ue) {
  | UHExp.Parenthesized(ue1) => cons_opt(0, first_hole_steps(ue1))
  | UHExp.Tm(_, ue') =>
    switch (ue') {
    | UHExp.EmptyHole(_) => Some([])
    | UHExp.Asc(ue1, uty) =>
      cons_opt2(0, first_hole_steps(ue1), 1, _ => first_hole_steps_ty(uty))
    | UHExp.Var(_, _) => None
    | UHExp.LineItem(li, e2) =>
      cons_opt2(0, first_hole_steps_line_item(li), 1, _ =>
        first_hole_steps(e2)
      )
    | UHExp.Lam(p, ann, e1) =>
      switch (first_hole_steps_pat(p)) {
      | Some(ns) => Some([0, ...ns])
      | None =>
        switch (ann) {
        | Some(uty) =>
          cons_opt2(1, first_hole_steps_ty(uty), 2, _ =>
            first_hole_steps(e1)
          )
        | None => cons_opt(2, first_hole_steps(e1))
        }
      }
    | UHExp.NumLit(_) => None
    | UHExp.BoolLit(_) => None
    | UHExp.ListNil => None
    /* | UHExp.ListLit es ->
       Util.findmapi es (fun i e ->
         begin match first_hole_steps e with
         | None -> None
         | Some ns -> Some (i :: ns)
         end */
    | UHExp.Inj(_, e1) => cons_opt(0, first_hole_steps(e1))
    | UHExp.Case(e1, rules) =>
      switch (first_hole_steps(e1)) {
      | Some(ns) => Some([0, ...ns])
      | None => first_hole_steps_rules(rules)
      }
    | UHExp.OpSeq(_, opseq) => first_hole_steps_opseq(opseq, 0)
    | UHExp.ApPalette(_, _, _) => None /* TODO figure out tab order protocol */
    }
  }
and first_hole_steps_line_item = li =>
  switch (li) {
  | UHExp.EmptyLine => None
  | UHExp.ExpLine(e1) => first_hole_steps(e1)
  | UHExp.LetLine(p, ann, e1) =>
    switch (ann) {
    | Some(ann_ty) =>
      cons_opt3(
        0,
        first_hole_steps_pat(p),
        1,
        _ => first_hole_steps_ty(ann_ty),
        2,
        _ => first_hole_steps(e1),
      )
    | None =>
      cons_opt2(0, first_hole_steps_pat(p), 2, _ => first_hole_steps(e1))
    }
  }
and first_hole_steps_rules = (rules: UHExp.rules): option(list(nat)) =>
  Util.findmapi(rules, (i, rule) =>
    switch (rule) {
    | UHExp.Rule(p, e) =>
      switch (first_hole_steps_pat(p)) {
      | Some(ns) => Some([i + 1, 0, ...ns])
      | None =>
        switch (first_hole_steps(e)) {
        | Some(ns) => Some([i + 1, 1, ...ns])
        | None => None
        }
      }
    }
  )
/* return an optional path of the first hole in opseq starting and the nth term )*/
and first_hole_steps_opseq = (opseq: UHExp.opseq, n: nat): option(list(nat)) =>
  if (OperatorSeq.seq_length(opseq) < n) {
    None;
  } else {
    switch (OperatorSeq.seq_nth(n, opseq)) {
    | None => None
    | Some(ue) =>
      switch (first_hole_steps(ue)) {
      | Some(ns) => Some([n, ...ns])
      | None => first_hole_steps_opseq(opseq, n + 1)
      }
    };
  };

let rec next_hole_steps_ty = (zty: ZTyp.t): option(list(nat)) =>
  switch (zty) {
  | ZTyp.CursorT(cursor_side, uty) =>
    switch (cursor_side, uty) {
    | (_, UHTyp.Hole) => None
    | (Before, _) => first_hole_steps_ty(uty)
    | (After, _) => None
    | (In(_), _) => None
    }
  | ZTyp.ParenthesizedZ(zty') => cons_opt(0, next_hole_steps_ty(zty'))
  | ZTyp.ListZ(zty1) => cons_opt(0, next_hole_steps_ty(zty1))
  | ZTyp.OpSeqZ(_, zty', surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    switch (next_hole_steps_ty(zty')) {
    | Some(ns) => Some([n, ...ns])
    | None =>
      let uty' = ZTyp.erase(zty');
      let opseq = OperatorSeq.opseq_of_exp_and_surround(uty', surround);
      first_hole_steps_ty_opseq(opseq, n + 1);
    };
  };

let rec next_hole_path_ty = (zty: ZTyp.t): option(t) =>
  switch (next_hole_steps_ty(zty)) {
  | None => None
  | Some(path) => Some((path, Before))
  };

let rec next_hole_steps_pat = (zp: ZPat.t): option(list(nat)) =>
  switch (zp) {
  | ZPat.ParenthesizedZ(zp1) => cons_opt(0, next_hole_steps_pat(zp1))
  | ZPat.CursorP(cursor_side, p) =>
    switch (cursor_side, p) {
    | (_, UHPat.Pat(_, UHPat.EmptyHole(_))) => None
    | (After, _) => None
    | (Before, _) => first_hole_steps_pat(p)
    | (In(k), _) =>
      switch (p) {
      | UHPat.Parenthesized(_) => None
      | UHPat.Pat(err, p') =>
        switch (p') {
        | UHPat.Wild
        | UHPat.Var(_)
        | UHPat.NumLit(_)
        | UHPat.BoolLit(_)
        | UHPat.ListNil
        /* | UHPat.ListLit _ */
        | UHPat.OpSeq(_, _) => None
        | UHPat.Inj(_, p1) => first_hole_steps_pat(p1)
        | UHPat.EmptyHole(_) => None
        }
      }
    }
  | ZPat.Deeper(_, ZPat.InjZ(_, zp1)) =>
    cons_opt(0, next_hole_steps_pat(zp1))
  /* | ZPat.Deeper _ (ZPat.ListLitZ zps) ->
     let prefix_length = ZList.prefix_length zps in
     let zp0 = ZList.prj_z zps in
     begin match next_hole_steps_pat zp0 with
     | Some ns ->
       Some (prefix_length :: ns)
     | None ->
       let suffix = ZList.prj_suffix zps in
       Util.findmapi suffix (fun i p ->
         begin match first_hole_steps_pat p with
         | None -> None
         | Some ns -> Some (cons (prefix_length + i + 1) ns)
         end
     end*/
  | ZPat.Deeper(_, ZPat.OpSeqZ(_, zp1, surround)) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    switch (next_hole_steps_pat(zp1)) {
    | Some(ns) => Some([n, ...ns])
    | None =>
      let p = ZPat.erase(zp1);
      let opseq = OperatorSeq.opseq_of_exp_and_surround(p, surround);
      first_hole_steps_opseq_pat(opseq, n + 1);
    };
  };

let rec next_hole_path_pat = (zp: ZPat.t): option(t) =>
  switch (next_hole_steps_pat(zp)) {
  | None => None
  | Some(path) => Some((path, Before))
  };

let rec next_hole_steps = (ze: ZExp.t): option(list(nat)) =>
  switch (ze) {
  | ZExp.CursorE(cursor_side, ue) =>
    switch (cursor_side, ue) {
    | (_, UHExp.Tm(_, UHExp.EmptyHole(_))) => None
    | (After, _) => None
    | (Before, _) => first_hole_steps(ue)
    | (In(k), _) =>
      switch (ue) {
      | UHExp.Parenthesized(_) => None
      | UHExp.Tm(err, ue') =>
        switch (ue') {
        | UHExp.Asc(_, uty) => cons_opt(1, first_hole_steps_ty(uty))
        | UHExp.Var(_, _) => None
        | UHExp.LineItem(_, _) => None
        | UHExp.Lam(_, _, _) => first_hole_steps(ue)
        | UHExp.NumLit(_)
        | UHExp.BoolLit(_)
        | UHExp.ListNil =>
          /* | UHExp.ListLit _ */

          None
        | UHExp.Inj(_, ue'') => first_hole_steps(ue)
        | UHExp.Case(e1, rules) =>
          switch (k) {
          | 0 => first_hole_steps(ue)
          | 1 => None
          | _ => None
          }
        | UHExp.EmptyHole(_) => None
        | UHExp.OpSeq(_, _) => None
        | UHExp.ApPalette(_, _, _) => None /* TODO(move, into, palette, holes) */
        }
      }
    }
  | ZExp.Deeper(_, ze') =>
    switch (ze') {
    | ZExp.AscZ1(ze'', uty) =>
      cons_opt2(0, next_hole_steps(ze''), 1, _ => first_hole_steps_ty(uty))
    | ZExp.AscZ2(_, zty) => cons_opt(1, next_hole_steps_ty(zty))
    | ZExp.LineItemZL(zli, e1) =>
      switch (next_hole_steps_line_item(zli)) {
      | Some(ns) => Some([0, ...ns])
      | None => cons_opt(1, first_hole_steps(e1))
      }
    | ZExp.LineItemZE(li, ze1) => cons_opt(1, next_hole_steps(ze1))
    | ZExp.LamZP(zp, ann, e1) =>
      switch (next_hole_steps_pat(zp)) {
      | Some(ns) => Some([0, ...ns])
      | None =>
        switch (ann) {
        | Some(uty) =>
          cons_opt2(1, first_hole_steps_ty(uty), 2, _ =>
            first_hole_steps(e1)
          )
        | None => cons_opt(2, first_hole_steps(e1))
        }
      }
    | ZExp.LamZA(_, zann, e1) =>
      cons_opt2(1, next_hole_steps_ty(zann), 2, _ => first_hole_steps(e1))
    | ZExp.LamZE(_, _, ze1) => cons_opt(2, next_hole_steps(ze1))
    | ZExp.InjZ(_, ze'') => cons_opt(0, next_hole_steps(ze''))
    /* | ZExp.ListLitZ zes ->
       let prefix_length = ZList.prefix_length zes in
       let ze0 = ZList.prj_z zes in
       begin match next_hole_steps ze0 with
       | Some ns ->
         Some (prefix_length :: ns)
       | None ->
         let suffix = ZList.prj_suffix zes in
         Util.findmapi suffix (fun i e ->
           begin match first_hole_steps e with
           | None -> None
           | Some ns -> Some (cons (prefix_length + i + 1) ns)
           end
       end*/
    | ZExp.CaseZE(ze1, rules) =>
      switch (next_hole_steps(ze1)) {
      | Some(ns) => Some([0, ...ns])
      | None => first_hole_steps_rules(rules)
      }
    | ZExp.CaseZR(_, zrules) =>
      let zr = ZList.prj_z(zrules);
      let prefix_len = List.length(ZList.prj_prefix(zrules));
      switch (zr) {
      | ZExp.RuleZP(zp, e) =>
        switch (next_hole_steps_pat(zp)) {
        | Some(ns) => Some([prefix_len + 1, 0, ...ns])
        | None =>
          switch (first_hole_steps(e)) {
          | Some(ns) => Some([prefix_len + 1, 1, ...ns])
          | None =>
            let suffix = ZList.prj_suffix(zrules);
            switch (first_hole_steps_rules(suffix)) {
            | Some([offset, ...ns]) => Some([prefix_len + offset + 1, ...ns])
            | Some([]) => None /* should never happen */
            | None => None
            };
          }
        }
      | ZExp.RuleZE(_, ze) =>
        switch (next_hole_steps(ze)) {
        | Some(ns) => Some([prefix_len + 1, 1, ...ns])
        | None =>
          let suffix = ZList.prj_suffix(zrules);
          switch (first_hole_steps_rules(suffix)) {
          | Some([offset, ...ns]) => Some([prefix_len + offset + 1, ...ns])
          | Some([]) => None /* should never happen */
          | None => None
          };
        }
      };
    | ZExp.OpSeqZ(_, ze'', surround) =>
      let n = OperatorSeq.surround_prefix_length(surround);
      switch (next_hole_steps(ze'')) {
      | Some(ns) => Some([n, ...ns])
      | None =>
        let ue'' = ZExp.erase(ze'');
        let opseq = OperatorSeq.opseq_of_exp_and_surround(ue'', surround);
        first_hole_steps_opseq(opseq, n + 1);
      };
    | ZExp.ApPaletteZ(_, _, _) => None /* TODO(figure, out, tab, order) protocol */
    }
  | ZExp.ParenthesizedZ(ze') => cons_opt(0, next_hole_steps(ze'))
  }
and next_hole_steps_line_item = (zli: ZExp.zline_item) =>
  switch (zli) {
  | EmptyLineZ => None
  | ExpLineZ(ze) => next_hole_steps(ze)
  | LetLineZP(zp, ann, e1) =>
    switch (ann) {
    | Some(ann_ty) =>
      cons_opt3(
        0,
        next_hole_steps_pat(zp),
        1,
        _ => first_hole_steps_ty(ann_ty),
        2,
        _ => first_hole_steps(e1),
      )
    | None =>
      cons_opt2(0, next_hole_steps_pat(zp), 2, _ => first_hole_steps(e1))
    }
  | LetLineZA(p, zann, e1) =>
    cons_opt2(1, next_hole_steps_ty(zann), 2, _ => first_hole_steps(e1))
  | LetLineZE(p, ann, ze1) => cons_opt(2, next_hole_steps(ze1))
  };

let rec next_hole_path = (ze: ZExp.t): option(t) =>
  switch (next_hole_steps(ze)) {
  | None => None
  | Some(path) => Some((path, Before))
  };

let rec last_hole_steps_ty = (uty: UHTyp.t): option(list(nat)) =>
  switch (uty) {
  | UHTyp.Hole => Some([])
  | UHTyp.Parenthesized(uty') => cons_opt(0, last_hole_steps_ty(uty'))
  | UHTyp.Unit
  | UHTyp.Num
  | UHTyp.Bool => None
  | UHTyp.List(uty1) => cons_opt(0, last_hole_steps_ty(uty1))
  | UHTyp.OpSeq(_, opseq) => last_hole_steps_ty_opseq(opseq, 0)
  }
/* return an optional path of the last hole in opseq starting and the mth term from the end
   (e.g., the 0th and 1st term from the endof `1 + 2 + 3` are 3 and 2 respectively) */
and last_hole_steps_ty_opseq =
    (opseq: UHTyp.opseq, m: nat): option(list(nat)) => {
  let l = OperatorSeq.seq_length(opseq);
  if (l < m) {
    None;
  } else {
    let n = l - m - 1;
    switch (OperatorSeq.seq_nth(n, opseq)) {
    | None => None /* degenerate case */
    | Some(uty') =>
      switch (last_hole_steps_ty(uty')) {
      | Some(ns) => Some([n, ...ns])
      | None => last_hole_steps_ty_opseq(opseq, m + 1)
      }
    };
  };
};

let rec last_hole_steps_pat = (p: UHPat.t): option(list(nat)) =>
  switch (p) {
  | UHPat.Parenthesized(p1) => cons_opt(0, last_hole_steps_pat(p1))
  | UHPat.Pat(_, UHPat.EmptyHole(_)) => Some([])
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, UHPat.Var(_))
  | UHPat.Pat(_, UHPat.NumLit(_))
  | UHPat.Pat(_, UHPat.BoolLit(_)) => None
  | UHPat.Pat(_, UHPat.Inj(_, p1)) => cons_opt(0, last_hole_steps_pat(p1))
  | UHPat.Pat(_, UHPat.ListNil) => None
  /* | UHPat.Pat _ (UHPat.ListLit ps) ->
     let num_elts = List.length ps in
     Util.findmapi ps (fun i p ->
       begin match last_hole_steps_pat p with
       | None -> None
       | Some ns -> Some (cons (num_elts - i - 1) ns)
       end */
  | UHPat.Pat(_, UHPat.OpSeq(_, opseq)) =>
    last_hole_steps_opseq_pat(opseq, 0)
  }
and last_hole_steps_opseq_pat =
    (opseq: UHPat.opseq, m: nat): option(list(nat)) => {
  let l = OperatorSeq.seq_length(opseq);
  if (l < m) {
    None;
  } else {
    let n = l - m - 1;
    switch (OperatorSeq.seq_nth(n, opseq)) {
    | None => None
    | Some(ue) =>
      switch (last_hole_steps_pat(ue)) {
      | Some(ns) => Some([n, ...ns])
      | None => last_hole_steps_opseq_pat(opseq, m + 1)
      }
    };
  };
};

let rec last_hole_steps = (ue: UHExp.t): option(list(nat)) =>
  switch (ue) {
  | UHExp.Parenthesized(ue') => cons_opt(0, last_hole_steps(ue'))
  | UHExp.Tm(_, ue') =>
    switch (ue') {
    | UHExp.EmptyHole(_) => Some([])
    | UHExp.Asc(ue0, uty1) =>
      cons_opt2(1, last_hole_steps_ty(uty1), 0, _ => last_hole_steps(ue0))
    | UHExp.Var(_, _) => None
    | UHExp.LineItem(li, e1) =>
      cons_opt2(1, last_hole_steps(e1), 0, _ =>
        last_hole_steps_line_item(li)
      )
    | UHExp.Lam(p, ann, e1) =>
      switch (last_hole_steps(e1)) {
      | Some(ns) => Some([2, ...ns])
      | None =>
        switch (ann) {
        | Some(uty1) =>
          cons_opt2(1, last_hole_steps_ty(uty1), 0, _ =>
            last_hole_steps_pat(p)
          )
        | None => cons_opt(0, last_hole_steps_pat(p))
        }
      }
    | UHExp.NumLit(_)
    | UHExp.BoolLit(_) => None
    | UHExp.Inj(_, ue0) => cons_opt(0, last_hole_steps(ue0))
    | UHExp.ListNil => None
    /* | UHExp.ListLit es ->
       let num_elts = List.length es in
       Util.findmapi es (fun i e ->
         begin match last_hole_steps e with
         | None -> None
         | Some ns -> Some (cons (num_elts - i - 1) ns)
         end */
    | UHExp.Case(e1, rules) =>
      switch (last_hole_steps_rules(rules)) {
      | Some(ns) as result => result
      | None => cons_opt(0, last_hole_steps(e1))
      }
    | UHExp.OpSeq(_, opseq) => last_hole_steps_opseq(opseq, 0)
    | UHExp.ApPalette(_, _, _) => None /* TODO(figure, out, tab, order) protocol */
    }
  }
and last_hole_steps_line_item = li =>
  switch (li) {
  | UHExp.EmptyLine => None
  | UHExp.ExpLine(e) => last_hole_steps(e)
  | UHExp.LetLine(p, ann, e1) =>
    switch (ann) {
    | Some(ann_ty) =>
      cons_opt3(
        2,
        last_hole_steps(e1),
        1,
        _ => last_hole_steps_ty(ann_ty),
        0,
        _ => last_hole_steps_pat(p),
      )
    | None =>
      cons_opt2(2, last_hole_steps(e1), 0, _ => last_hole_steps_pat(p))
    }
  }
and last_hole_steps_rules = (rules: UHExp.rules): option(list(nat)) => {
  let n_rules = List.length(rules);
  Util.findmapi(List.rev(rules), (i, rule) =>
    switch (rule) {
    | UHExp.Rule(p, e) =>
      switch (last_hole_steps(e)) {
      | Some(ns) => Some([n_rules - i, 1, ...ns])
      | None =>
        switch (last_hole_steps_pat(p)) {
        | Some(ns) => Some([n_rules - i, 0, ...ns])
        | None => None
        }
      }
    }
  );
}
/* return an optional path of the last hole in opseq starting and the mth term from the end
   (e.g., the 0th and 1st term from the endof `1 + 2 + 3` are 3 and 2 respectively) */
and last_hole_steps_opseq = (opseq: UHExp.opseq, m: nat): option(list(nat)) => {
  let l = OperatorSeq.seq_length(opseq);
  if (l < m) {
    None;
  } else {
    let n = l - m - 1;
    switch (OperatorSeq.seq_nth(n, opseq)) {
    | None => None
    | Some(ue) =>
      switch (last_hole_steps(ue)) {
      | Some(ns) => Some([n, ...ns])
      | None => last_hole_steps_opseq(opseq, m + 1)
      }
    };
  };
};

let rec prev_hole_steps_ty = (zty: ZTyp.t): option(list(nat)) =>
  switch (zty) {
  | ZTyp.CursorT(cursor_side, uty) =>
    switch (cursor_side, uty) {
    | (_, UHTyp.Hole) => None
    | (Before, _) => None
    | (After, _) => last_hole_steps_ty(uty)
    | (In(_), _) => None
    }
  | ZTyp.ParenthesizedZ(zty') => cons_opt(0, prev_hole_steps_ty(zty'))
  | ZTyp.ListZ(zty1) => cons_opt(0, prev_hole_steps_ty(zty1))
  | ZTyp.OpSeqZ(_, zty', surround) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    switch (prev_hole_steps_ty(zty')) {
    | Some(ns) => Some([n, ...ns])
    | None =>
      let uty' = ZTyp.erase(zty');
      let opseq = OperatorSeq.opseq_of_exp_and_surround(uty', surround);
      let m = OperatorSeq.surround_suffix_length(surround);
      last_hole_steps_ty_opseq(opseq, m + 1);
    };
  };

let rec prev_hole_path_ty = (zty: ZTyp.t): option(t) =>
  switch (prev_hole_steps_ty(zty)) {
  | None => None
  | Some(path) => Some((path, Before))
  };

let rec prev_hole_steps_pat = (zp: ZPat.t): option(list(nat)) =>
  switch (zp) {
  | ZPat.ParenthesizedZ(zp1) => cons_opt(0, prev_hole_steps_pat(zp1))
  | ZPat.CursorP(cursor_side, p) =>
    switch (cursor_side, p) {
    | (_, UHPat.Pat(_, UHPat.EmptyHole(_))) => None
    | (Before, _) => None
    | (After, _) => last_hole_steps_pat(p)
    | (In(k), _) =>
      switch (p) {
      | UHPat.Parenthesized(_) => None
      | UHPat.Pat(err, p') =>
        switch (p') {
        | UHPat.EmptyHole(_) => None
        | UHPat.Wild
        | UHPat.Var(_)
        | UHPat.NumLit(_)
        | UHPat.BoolLit(_)
        | UHPat.ListNil =>
          /* | UHPat.ListLit _ */

          None
        | UHPat.Inj(_, p1) => None
        | UHPat.OpSeq(_, _) => None
        }
      }
    }
  | ZPat.Deeper(_, ZPat.InjZ(_, zp1)) =>
    cons_opt(0, prev_hole_steps_pat(zp1))
  /* | ZPat.Deeper _ (ZPat.ListLitZ ((prefix, zp0), _)) ->
     let prefix_length = List.length prefix in
     begin match prev_hole_steps_pat zp0 with
     | Some ns -> Some (prefix_length :: ns)
     | None -> last_hole_steps_pat (UHPat.Pat NotInHole (UHPat.ListLit prefix))
     end*/
  | ZPat.Deeper(_, ZPat.OpSeqZ(_, zp1, surround)) =>
    let n = OperatorSeq.surround_prefix_length(surround);
    switch (prev_hole_steps_pat(zp1)) {
    | Some(ns) => Some([n, ...ns])
    | None =>
      let ue_n = ZPat.erase(zp1);
      let opseq = OperatorSeq.opseq_of_exp_and_surround(ue_n, surround);
      let m = OperatorSeq.surround_suffix_length(surround);
      last_hole_steps_opseq_pat(opseq, m + 1);
    };
  };

let rec prev_hole_path_pat = (zp: ZPat.t): option(t) =>
  switch (prev_hole_steps_pat(zp)) {
  | None => None
  | Some(path) => Some((path, Before))
  };

let rec prev_hole_steps = (ze: ZExp.t): option(list(nat)) =>
  switch (ze) {
  | ZExp.CursorE(cursor_side, ue) =>
    switch (cursor_side, ue) {
    | (_, UHExp.Tm(_, UHExp.EmptyHole(_))) => None
    | (After, _) => last_hole_steps(ue)
    | (Before, _) => None
    | (In(k), _) =>
      switch (ue) {
      | UHExp.Parenthesized(_) => None /* cannot be In(Parenthesized, term) */
      | UHExp.Tm(err, ue') =>
        switch (ue') {
        | UHExp.Asc(ue'', _) => cons_opt(0, last_hole_steps(ue''))
        | UHExp.Var(_, _) => None
        | UHExp.LineItem(_, _) => None
        | UHExp.Lam(_, _, _) => None
        | UHExp.NumLit(_)
        | UHExp.BoolLit(_)
        | UHExp.ListNil =>
          /* | UHExp.ListLit _ */

          None
        | UHExp.Inj(_, _) => None
        | UHExp.Case(_, _) =>
          switch (k) {
          | 0 => None
          | 1 => last_hole_steps(ue)
          | _ => None
          }
        | UHExp.EmptyHole(_) => None
        | UHExp.OpSeq(_, _) => None
        | UHExp.ApPalette(_, _, _) => None /* TODO */
        }
      }
    }
  | ZExp.Deeper(_, ze') =>
    switch (ze') {
    | ZExp.AscZ1(ze0, _) => cons_opt(0, prev_hole_steps(ze0))
    | ZExp.AscZ2(ue0, zty1) =>
      cons_opt2(1, prev_hole_steps_ty(zty1), 0, _ => last_hole_steps(ue0))
    | ZExp.LineItemZL(zli, e1) =>
      cons_opt(0, prev_hole_steps_line_item(zli))
    | ZExp.LineItemZE(li, ze1) =>
      cons_opt2(1, prev_hole_steps(ze1), 0, _ =>
        last_hole_steps_line_item(li)
      )
    | ZExp.LamZP(zp, _, _) => prev_hole_steps_pat(zp)
    | ZExp.LamZA(p, zann, _) =>
      cons_opt2(1, prev_hole_steps_ty(zann), 0, _ => last_hole_steps_pat(p))
    | ZExp.LamZE(p, ann, ze1) =>
      switch (prev_hole_steps(ze1)) {
      | Some(ns) => Some([2, ...ns])
      | None =>
        switch (ann) {
        | Some(uty1) =>
          cons_opt2(1, last_hole_steps_ty(uty1), 0, _ =>
            last_hole_steps_pat(p)
          )
        | None => cons_opt(0, last_hole_steps_pat(p))
        }
      }
    | ZExp.InjZ(_, ze0) => cons_opt(0, prev_hole_steps(ze0))
    /* | ZExp.ListLitZ ((prefix, ze0), _) ->
       let prefix_length = List.length prefix in
       begin match prev_hole_steps ze0 with
       | Some ns -> Some (prefix_length :: ns)
       | None -> last_hole_steps (UHExp.Tm NotInHole (UHExp.ListLit prefix))
       end*/
    | ZExp.CaseZE(ze, rules) => cons_opt(0, prev_hole_steps(ze))
    | ZExp.CaseZR(e1, zrules) =>
      let zr = ZList.prj_z(zrules);
      let prefix = ZList.prj_prefix(zrules);
      let prefix_len = List.length(prefix);
      switch (zr) {
      | ZExp.RuleZP(zp, e) =>
        switch (prev_hole_steps_pat(zp)) {
        | Some(ns) => Some([prefix_len + 1, 0, ...ns])
        | None =>
          switch (last_hole_steps_rules(prefix)) {
          | Some(ns) => Some(ns)
          | None => cons_opt(0, last_hole_steps(e1))
          }
        }
      | ZExp.RuleZE(p, ze) =>
        switch (prev_hole_steps(ze)) {
        | Some(ns) => Some([prefix_len + 1, 1, ...ns])
        | None =>
          switch (last_hole_steps_pat(p)) {
          | Some(ns) => Some([prefix_len + 1, 0, ...ns])
          | None =>
            switch (last_hole_steps_rules(prefix)) {
            | Some(ns) => Some(ns)
            | None => cons_opt(0, last_hole_steps(e1))
            }
          }
        }
      };
    | ZExp.OpSeqZ(_, ze_n, surround) =>
      let n = OperatorSeq.surround_prefix_length(surround);
      switch (prev_hole_steps(ze_n)) {
      | Some(ns) => Some([n, ...ns])
      | None =>
        let ue_n = ZExp.erase(ze_n);
        let opseq = OperatorSeq.opseq_of_exp_and_surround(ue_n, surround);
        let m = OperatorSeq.surround_suffix_length(surround);
        last_hole_steps_opseq(opseq, m + 1);
      };
    | ZExp.ApPaletteZ(_, _, _) => None /* TODO(figure, out, tab, order) protocol */
    }
  | ZExp.ParenthesizedZ(ze0) => cons_opt(0, prev_hole_steps(ze0))
  }
and prev_hole_steps_line_item = (zli: ZExp.zline_item) =>
  switch (zli) {
  | EmptyLineZ => None
  | ExpLineZ(ze) => prev_hole_steps(ze)
  | LetLineZP(zp, ann, e1) => cons_opt(0, prev_hole_steps_pat(zp))
  | LetLineZA(p, zann, e1) =>
    cons_opt2(1, prev_hole_steps_ty(zann), 0, _ => last_hole_steps_pat(p))
  | LetLineZE(p, ann, ze1) =>
    switch (ann) {
    | Some(ann_ty) =>
      cons_opt3(
        2,
        prev_hole_steps(ze1),
        1,
        _ => last_hole_steps_ty(ann_ty),
        0,
        _ => last_hole_steps_pat(p),
      )
    | None =>
      cons_opt2(2, prev_hole_steps(ze1), 0, _ => last_hole_steps_pat(p))
    }
  };

let rec prev_hole_path = (ze: ZExp.t): option(t) =>
  switch (prev_hole_steps(ze)) {
  | None => None
  | Some(path) => Some((path, Before))
  };