open Sexplib.Std;

/*
 module Steps : {
   type t;
   let prepend_step : ChildIndex.t => t => t;
   let append_step : t => ChildIndex.t => t;
   let to_list : t => list(ChildIndex.t);
 } = {
   type t = list(ChildIndex.t);

   let prepend_step = (step, steps) => [step, ...steps];
   let append_step = (steps, step) => steps ++ [step];
   let to_list = steps => steps;
 }
 */

[@deriving sexp]
type steps = list(ChildIndex.t);
[@deriving sexp]
type rev_steps = steps;

[@deriving sexp]
type t = (steps, CursorPosition.t);

let cons' = (step: int, (steps, cursor): t): t => {
  ([step, ...steps], cursor);
};

let _of_zopseq =
    (
      ~of_zoperand: 'zoperand => t,
      ZOpSeq(_, zseq): ZOpSeq.t(_, _, 'zoperand, _),
    )
    : t =>
  switch (zseq) {
  | ZOperand(zoperand, (prefix, _)) =>
    cons'(Seq.length_of_affix(prefix), of_zoperand(zoperand))
  | ZOperator((cursor, _), (prefix, suffix)) =>
    let length = Seq.length(prefix) + Seq.length(suffix);
    ([length + Seq.length(prefix) - 1], cursor);
  };

[@deriving sexp]
type hole_desc =
  | TypHole
  | PatHole(MetaVar.t)
  | ExpHole(MetaVar.t);

[@deriving sexp]
type hole_list = list((hole_desc, steps));

/* two hole lists, one for before the cursor, one for after */
[@deriving sexp]
type zhole_list = {
  holes_before: hole_list,
  hole_selected: option((hole_desc, steps)),
  holes_after: hole_list,
};

let mk_zholes =
    (~holes_before=[], ~hole_selected=None, ~holes_after=[], ()): zhole_list => {
  holes_before,
  hole_selected,
  holes_after,
};
let no_holes = mk_zholes();

let prev_hole_steps = (zhole_list: zhole_list): option(steps) => {
  switch (
    zhole_list.holes_before |> List.rev,
    zhole_list.holes_after |> List.rev,
  ) {
  | ([], []) => None
  | ([(_, steps), ..._], _) => Some(steps)
  | ([], [(_, steps), ..._]) => Some(steps)
  };
};

let next_hole_steps = (zhole_list: zhole_list): option(steps) => {
  switch (zhole_list.holes_before, zhole_list.holes_after) {
  | ([], []) => None
  | (_, [(_, steps), ..._]) => Some(steps)
  | ([(_, steps), ..._], _) => Some(steps)
  };
};

let _follow_opseq =
    (
      ~follow_operand: (t, 'operand) => option('zoperand),
      ~follow_operator: (t, 'operator) => option('zoperator),
      (steps, cursor): t,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
    )
    : option(ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (steps) {
  | [] => None
  | [x, ...xs] =>
    switch (
      seq |> Seq.opt_split_nth_operand(x),
      seq |> Seq.opt_split_nth_operator(x - Seq.length(seq)),
    ) {
    | (None, None) => None
    | (Some((operand, surround)), _) =>
      operand
      |> follow_operand((xs, cursor))
      |> OptUtil.map(zoperand =>
           ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, surround))
         )
    | (_, Some((operator, surround))) =>
      operator
      |> follow_operator((xs, cursor))
      |> OptUtil.map(zoperator =>
           ZOpSeq.ZOpSeq(skel, ZOperator(zoperator, surround))
         )
    }
  };

let _follow_steps_opseq =
    (
      ~follow_steps_operand:
         (~side: Side.t, steps, 'operand) => option('zoperand),
      ~follow_steps_operator:
         (~side: Side.t, steps, 'operator) => option('zoperator),
      ~side: Side.t,
      steps: steps,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
    )
    : option(ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (steps) {
  | [] => None
  | [x, ...xs] =>
    switch (
      seq |> Seq.opt_split_nth_operand(x),
      seq |> Seq.opt_split_nth_operator(x - Seq.length(seq)),
    ) {
    | (None, None) => None
    | (Some((operand, surround)), _) =>
      operand
      |> follow_steps_operand(~side, xs)
      |> OptUtil.map(zoperand =>
           ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, surround))
         )
    | (_, Some((operator, surround))) =>
      operator
      |> follow_steps_operator(~side, xs)
      |> OptUtil.map(zoperator =>
           ZOpSeq.ZOpSeq(skel, ZOperator(zoperator, surround))
         )
    }
  };

let holes_err =
    (
      ~hole_desc: MetaVar.t => hole_desc,
      err: ErrStatus.t,
      rev_steps: rev_steps,
      hs: hole_list,
    ) =>
  switch (err) {
  | NotInHole => hs
  | InHole(_, u) => [(hole_desc(u), rev_steps |> List.rev), ...hs]
  };

let holes_verr =
    (
      ~hole_desc: MetaVar.t => hole_desc,
      verr: VarErrStatus.t,
      rev_steps: rev_steps,
      hs: hole_list,
    ) =>
  switch (verr) {
  | NotInVarHole => hs
  | InVarHole(_, u) => [(hole_desc(u), rev_steps |> List.rev), ...hs]
  };

let holes_skel =
    (
      ~holes_operand: ('operand, steps, hole_list) => hole_list,
      ~hole_desc: MetaVar.t => hole_desc,
      ~is_space: 'operator => bool,
      ~rev_steps: rev_steps,
      skel: Skel.t('operator),
      seq: Seq.t('operand, 'operator),
      hs: hole_list,
    )
    : hole_list => {
  let rec go = (skel: Skel.t(_), hs) =>
    switch (skel) {
    | Placeholder(n) =>
      hs |> holes_operand(seq |> Seq.nth_operand(n), [n, ...rev_steps])
    | BinOp(err, op, skel1, skel2) when op |> is_space =>
      // If this skel is rooted at a Space, then we know
      // that all subskels are rooted at a Space.
      // We cannot place cursor on a Space, so make the
      // path to this skel hole the path to the first term
      // of the skel because that term determines how the
      // skel is typed. Make this hole come first before
      // any holes found in subskels.
      let hs = hs |> go(skel2) |> go(skel1);
      switch (err) {
      | NotInHole => hs
      | InHole(_, u) =>
        let step = skel1 |> Skel.leftmost_tm_index;
        [(hole_desc(u), [step, ...rev_steps] |> List.rev), ...hs];
      };
    | BinOp(err, _, skel1, skel2) =>
      let hs = hs |> go(skel2);
      let hs =
        switch (err) {
        | NotInHole => hs
        | InHole(_, u) =>
          let step = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
          [(hole_desc(u), [step, ...rev_steps] |> List.rev), ...hs];
        };
      hs |> go(skel1);
    };
  go(skel, hs);
};

let holes_opseq =
    (
      ~holes_operand: ('operand, steps, hole_list) => hole_list,
      ~hole_desc: MetaVar.t => hole_desc,
      ~is_space: 'operator => bool,
      ~rev_steps: rev_steps,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
      hs: hole_list,
    )
    : hole_list =>
  holes_skel(
    ~holes_operand,
    ~hole_desc,
    ~is_space,
    ~rev_steps,
    skel,
    seq,
    hs,
  );

let _holes_zopseq =
    (
      ~holes_operand: ('operand, rev_steps, hole_list) => hole_list,
      ~holes_zoperand: ('zoperand, rev_steps) => zhole_list,
      ~hole_desc: MetaVar.t => hole_desc,
      ~is_space: 'operator => bool,
      ~rev_steps: rev_steps,
      ~erase_zopseq:
         ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
         OpSeq.t('operand, 'operator),
      ZOpSeq(skel, zseq) as zopseq:
        ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : zhole_list => {
  let OpSeq(_, seq) = zopseq |> erase_zopseq;
  let holes_skel = skel =>
    holes_skel(
      ~holes_operand,
      ~hole_desc,
      ~is_space,
      ~rev_steps,
      skel,
      seq,
      [],
    );
  switch (zseq) {
  | ZOperator(_, (prefix, _)) =>
    let preceding_operand_index = Seq.length(prefix) - 1;
    let rec go: Skel.t(_) => zhole_list = (
      fun
      | Placeholder(_) =>
        // We defer to holes_skel once we have determined that a skel
        // does not contain the cursor, should never hit this case.
        failwith("holes_zopseq/ZOperator: unexpected Placeholder")
      | BinOp(err, op, skel1, skel2) => {
          // We defer to holes_skel once we have determined that a skel
          // does not contain the cursor. Since Space has highest precedence
          // and we cannot place cursor on a Space, we know that the entirety
          // of a skel rooted at Space cannot contain the cursor. Should
          // have deferred to holes_skel before hitting this case.
          assert(!is_space(op));
          let n = skel1 |> Skel.rightmost_tm_index;
          let binop_hole =
            switch (err) {
            | NotInHole => None
            | InHole(_, u) =>
              let step = n + Seq.length(seq);
              Some((hole_desc(u), [step, ...rev_steps] |> List.rev));
            };
          if (n == preceding_operand_index) {
            mk_zholes(
              ~holes_before=holes_skel(skel1),
              ~hole_selected=binop_hole,
              ~holes_after=holes_skel(skel2),
              (),
            );
          } else {
            // convert option to list
            let binop_holes =
              binop_hole |> OptUtil.map_default(~default=[], hole => [hole]);
            if (n < preceding_operand_index) {
              let holes1 = holes_skel(skel1);
              let zholes2 = go(skel2);
              mk_zholes(
                ~holes_before=holes1 @ binop_holes @ zholes2.holes_before,
                ~hole_selected=zholes2.hole_selected,
                ~holes_after=zholes2.holes_after,
                (),
              );
            } else {
              let zholes1 = go(skel1);
              let holes2 = holes_skel(skel2);
              mk_zholes(
                ~holes_before=zholes1.holes_before,
                ~hole_selected=zholes1.hole_selected,
                ~holes_after=zholes1.holes_after @ binop_holes @ holes2,
                (),
              );
            };
          };
        }
    );
    go(skel);
  | ZOperand(zoperand, (prefix, _)) =>
    let zoperand_index = Seq.length_of_affix(prefix);
    let rec go: Skel.t(_) => zhole_list = (
      fun
      | Placeholder(n) => {
          // We defer to holes_skel once we have determined that a skel
          // does not contain the cursor, should never hit Placeholder
          // corresponding to operand other than zoperand.
          assert(n == zoperand_index);
          holes_zoperand(zoperand, [zoperand_index, ...rev_steps]);
        }
      | BinOp(err, op, skel1, skel2) when op |> is_space => {
          // If this skel is rooted at a Space, then we know
          // that all subskels are rooted at a Space.
          // We cannot place cursor on a Space, so make the
          // path to this skel hole the path to the first term
          // of the skel because that term determines how the
          // skel is typed. Make this hole come first before
          // any holes found in subskels.
          let binop_holes =
            switch (err) {
            | NotInHole => []
            | InHole(_, u) =>
              let step = skel1 |> Skel.leftmost_tm_index;
              [(hole_desc(u), [step, ...rev_steps] |> List.rev)];
            };
          if (zoperand_index <= Skel.rightmost_tm_index(skel1)) {
            let zholes1 = go(skel1);
            let holes2 = holes_skel(skel2);
            mk_zholes(
              ~holes_before=binop_holes @ zholes1.holes_before,
              ~hole_selected=zholes1.hole_selected,
              ~holes_after=zholes1.holes_after @ holes2,
              (),
            );
          } else {
            let holes1 = holes_skel(skel1);
            let zholes2 = go(skel2);
            mk_zholes(
              ~holes_before=binop_holes @ holes1 @ zholes2.holes_before,
              ~hole_selected=zholes2.hole_selected,
              ~holes_after=zholes2.holes_after,
              (),
            );
          };
        }
      | BinOp(err, _, skel1, skel2) => {
          let n = skel1 |> Skel.rightmost_tm_index;
          let binop_holes =
            switch (err) {
            | NotInHole => []
            | InHole(_, u) =>
              let step = n + Seq.length(seq);
              [(hole_desc(u), [step, ...rev_steps] |> List.rev)];
            };
          if (zoperand_index <= n) {
            let zholes1 = go(skel1);
            let holes2 = holes_skel(skel2);
            mk_zholes(
              ~holes_before=zholes1.holes_before,
              ~hole_selected=zholes1.hole_selected,
              ~holes_after=zholes1.holes_after @ binop_holes @ holes2,
              (),
            );
          } else {
            let holes1 = holes_skel(skel1);
            let zholes2 = go(skel2);
            mk_zholes(
              ~holes_before=holes1 @ binop_holes @ zholes2.holes_before,
              ~hole_selected=zholes2.hole_selected,
              ~holes_after=zholes2.holes_after,
              (),
            );
          };
        }
    );
    go(skel);
  };
};

module Typ = {
  let rec of_z = (zty: ZTyp.t): t => of_zopseq(zty)
  and of_zopseq = zopseq => _of_zopseq(~of_zoperand, zopseq)
  and of_zoperand =
    fun
    | CursorT(cursor, _) => ([], cursor)
    | ParenthesizedZ(zbody) => cons'(0, of_z(zbody))
    | ListZ(zbody) => cons'(0, of_z(zbody));

  let rec follow = (path: t, uty: UHTyp.t): option(ZTyp.t) =>
    follow_opseq(path, uty)
  and follow_opseq = (path: t, opseq: UHTyp.opseq): option(ZTyp.zopseq) =>
    _follow_opseq(~follow_operand, ~follow_operator, path, opseq)
  and follow_operand =
      ((steps, cursor): t, operand: UHTyp.operand): option(ZTyp.zoperand) =>
    switch (steps) {
    | [] => operand |> ZTyp.place_cursor_operand(cursor)
    | [x, ...xs] =>
      switch (operand) {
      | Hole
      | Unit
      | Num
      | Bool => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          body
          |> follow((xs, cursor))
          |> OptUtil.map(zbody => ZTyp.ParenthesizedZ(zbody))
        | _ => None
        }
      | List(body) =>
        switch (x) {
        | 0 =>
          body
          |> follow((xs, cursor))
          |> OptUtil.map(zbody => ZTyp.ListZ(zbody))
        | _ => None
        }
      }
    }
  and follow_operator =
      ((steps, cursor): t, operator: UHTyp.operator): option(ZTyp.zoperator) =>
    switch (steps) {
    | [] => operator |> ZTyp.place_cursor_operator(cursor)
    | [_, ..._] => None
    };

  let rec follow_steps =
          (~side: Side.t=Before, steps: steps, uty: UHTyp.t): option(ZTyp.t) =>
    follow_steps_opseq(~side, steps, uty)
  and follow_steps_opseq =
      (~side: Side.t, steps: steps, opseq: UHTyp.opseq): option(ZTyp.zopseq) =>
    _follow_steps_opseq(
      ~follow_steps_operand,
      ~follow_steps_operator,
      ~side,
      steps,
      opseq,
    )
  and follow_steps_operand =
      (~side: Side.t, steps: steps, operand: UHTyp.operand)
      : option(ZTyp.zoperand) =>
    switch (steps) {
    | [] =>
      switch (side) {
      | Before => Some(operand |> ZTyp.place_before_operand)
      | After => Some(operand |> ZTyp.place_after_operand)
      }
    | [x, ...xs] =>
      switch (operand) {
      | Hole
      | Unit
      | Num
      | Bool => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          body
          |> follow_steps(~side, xs)
          |> OptUtil.map(zbody => ZTyp.ParenthesizedZ(zbody))
        | _ => None
        }
      | List(body) =>
        switch (x) {
        | 0 =>
          body
          |> follow_steps(~side, xs)
          |> OptUtil.map(zbody => ZTyp.ListZ(zbody))
        | _ => None
        }
      }
    }
  and follow_steps_operator =
      (~side: Side.t, steps: steps, operator: UHTyp.operator)
      : option(ZTyp.zoperator) =>
    switch (steps) {
    | [_, ..._] => None
    | [] =>
      switch (side) {
      | Before => operator |> ZTyp.place_before_operator
      | After => operator |> ZTyp.place_after_operator
      }
    };

  let hole_desc = _ => TypHole;
  let is_space = _ => false;

  let rec holes =
          (uty: UHTyp.t, rev_steps: rev_steps, hs: hole_list): hole_list =>
    hs |> holes_opseq(~holes_operand, ~hole_desc, ~is_space, ~rev_steps, uty)
  and holes_operand =
      (operand: UHTyp.operand, rev_steps: rev_steps, hs: hole_list): hole_list =>
    switch (operand) {
    | Hole => [(TypHole, rev_steps |> List.rev), ...hs]
    | Unit
    | Num
    | Bool => hs
    | Parenthesized(body)
    | List(body) => hs |> holes(body, [0, ...rev_steps])
    };

  let rec holes_z = (zty: ZTyp.t, rev_steps: rev_steps): zhole_list =>
    holes_zopseq(zty, rev_steps)
  and holes_zopseq = (zopseq: ZTyp.zopseq, rev_steps: rev_steps): zhole_list =>
    _holes_zopseq(
      ~holes_operand,
      ~holes_zoperand,
      ~hole_desc,
      ~is_space,
      ~rev_steps,
      ~erase_zopseq=ZTyp.erase_zopseq,
      zopseq,
    )
  and holes_zoperand =
      (zoperand: ZTyp.zoperand, rev_steps: rev_steps): zhole_list =>
    switch (zoperand) {
    | CursorT(_, Hole) =>
      mk_zholes(~hole_selected=Some((TypHole, rev_steps |> List.rev)), ())
    | CursorT(_, Unit | Num | Bool) => no_holes
    | CursorT(OnDelim(k, _), Parenthesized(body) | List(body)) =>
      let holes = holes(body, [0, ...rev_steps], []);
      switch (k) {
      | 0 => mk_zholes(~holes_before=holes, ())
      | 1 => mk_zholes(~holes_after=holes, ())
      | _ => no_holes
      };
    | CursorT(OnOp(_) | OnText(_), Parenthesized(_) | List(_)) =>
      /* invalid cursor position */
      no_holes
    | ParenthesizedZ(zbody)
    | ListZ(zbody) => holes_z(zbody, [0, ...rev_steps])
    };
};

module Pat = {
  let rec of_z = (zp: ZPat.t): t => of_zopseq(zp)
  and of_zopseq = (zopseq: ZPat.zopseq): t =>
    _of_zopseq(~of_zoperand, zopseq)
  and of_zoperand =
    fun
    | CursorP(cursor, _) => ([], cursor)
    | ParenthesizedZ(zbody)
    | InjZ(_, _, zbody) => cons'(0, of_z(zbody));

  let rec follow = (path: t, p: UHPat.t): option(ZPat.t) =>
    follow_opseq(path, p)
  and follow_opseq = (path: t, opseq: UHPat.opseq): option(ZPat.zopseq) =>
    _follow_opseq(~follow_operand, ~follow_operator, path, opseq)
  and follow_operand =
      ((steps, cursor): t, operand: UHPat.operand): option(ZPat.zoperand) =>
    switch (steps) {
    | [] => operand |> ZPat.place_cursor_operand(cursor)
    | [x, ...xs] =>
      switch (operand) {
      | EmptyHole(_)
      | Wild(_)
      | Var(_, _, _)
      | NumLit(_, _)
      | BoolLit(_, _)
      | ListNil(_) => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          body
          |> follow((xs, cursor))
          |> OptUtil.map(zbody => ZPat.ParenthesizedZ(zbody))
        | _ => None
        }
      | Inj(err, side, body) =>
        switch (x) {
        | 0 =>
          body
          |> follow((xs, cursor))
          |> OptUtil.map(zbody => ZPat.InjZ(err, side, zbody))
        | _ => None
        }
      }
    }
  and follow_operator =
      ((steps, cursor): t, operator: UHPat.operator): option(ZPat.zoperator) =>
    switch (steps) {
    | [] => operator |> ZPat.place_cursor_operator(cursor)
    | [_, ..._] => None
    };

  let rec follow_steps =
          (~side: Side.t=Before, steps: steps, p: UHPat.t): option(ZPat.t) =>
    follow_steps_opseq(~side, steps, p)
  and follow_steps_opseq =
      (~side: Side.t, steps: steps, opseq: UHPat.opseq): option(ZPat.zopseq) =>
    _follow_steps_opseq(
      ~follow_steps_operand,
      ~follow_steps_operator,
      ~side,
      steps,
      opseq,
    )
  and follow_steps_operand =
      (~side: Side.t, steps: steps, operand: UHPat.operand)
      : option(ZPat.zoperand) =>
    switch (steps) {
    | [] =>
      switch (side) {
      | Before => Some(operand |> ZPat.place_before_operand)
      | After => Some(operand |> ZPat.place_after_operand)
      }
    | [x, ...xs] =>
      switch (operand) {
      | EmptyHole(_)
      | Wild(_)
      | Var(_, _, _)
      | NumLit(_, _)
      | BoolLit(_, _)
      | ListNil(_) => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          body
          |> follow_steps(~side, xs)
          |> OptUtil.map(zbody => ZPat.ParenthesizedZ(zbody))
        | _ => None
        }
      | Inj(err, inj_side, body) =>
        switch (x) {
        | 0 =>
          body
          |> follow_steps(~side, xs)
          |> OptUtil.map(zbody => ZPat.InjZ(err, inj_side, zbody))
        | _ => None
        }
      }
    }
  and follow_steps_operator =
      (~side: Side.t, steps: steps, operator: UHPat.operator)
      : option(ZPat.zoperator) =>
    switch (steps) {
    | [] =>
      switch (side) {
      | Before => operator |> ZPat.place_before_operator
      | After => operator |> ZPat.place_after_operator
      }
    | [_, ..._] => None
    };

  exception UHPatNodeNotFound(t, UHPat.t);
  let follow_or_fail = (path: t, p: UHPat.t): ZPat.t =>
    switch (follow(path, p)) {
    | None => raise(UHPatNodeNotFound(path, p))
    | Some(zp) => zp
    };

  let hole_desc = (u: MetaVar.t): hole_desc => PatHole(u);

  let rec holes = (p: UHPat.t, rev_steps: rev_steps, hs: hole_list): hole_list =>
    hs
    |> holes_opseq(
         ~holes_operand,
         ~hole_desc,
         ~is_space=UHPat.is_Space,
         ~rev_steps,
         p,
       )
  and holes_operand =
      (operand: UHPat.operand, rev_steps: rev_steps, hs: hole_list): hole_list =>
    switch (operand) {
    | EmptyHole(u)
    | Wild(InHole(_, u))
    | Var(InHole(_, u), _, _)
    | Var(_, InVarHole(_, u), _)
    | NumLit(InHole(_, u), _)
    | BoolLit(InHole(_, u), _)
    | ListNil(InHole(_, u)) => [
        (PatHole(u), rev_steps |> List.rev),
        ...hs,
      ]
    | Var(NotInHole, NotInVarHole, _)
    | Wild(NotInHole)
    | NumLit(NotInHole, _)
    | BoolLit(NotInHole, _)
    | ListNil(NotInHole) => hs
    | Parenthesized(body) => hs |> holes(body, [0, ...rev_steps])
    | Inj(err, _, body) =>
      let body_holes = hs |> holes(body, [0, ...rev_steps]);
      switch (err) {
      | NotInHole => body_holes
      | InHole(_, u) => [
          (PatHole(u), rev_steps |> List.rev),
          ...body_holes,
        ]
      };
    };

  let rec holes_z = (zp: ZPat.t, rev_steps: rev_steps): zhole_list =>
    holes_zopseq(zp, rev_steps)
  and holes_zopseq = (zopseq: ZPat.zopseq, rev_steps: rev_steps): zhole_list =>
    _holes_zopseq(
      ~holes_operand,
      ~holes_zoperand,
      ~hole_desc,
      ~is_space=UHPat.is_Space,
      ~rev_steps,
      ~erase_zopseq=ZPat.erase_zopseq,
      zopseq,
    )
  and holes_zoperand =
      (zoperand: ZPat.zoperand, rev_steps: rev_steps): zhole_list =>
    switch (zoperand) {
    | CursorP(OnOp(_), _) => no_holes
    | CursorP(_, EmptyHole(u)) =>
      mk_zholes(
        ~hole_selected=Some((PatHole(u), rev_steps |> List.rev)),
        (),
      )
    | CursorP(_, Var(err, verr, _)) =>
      switch (err, verr) {
      | (NotInHole, NotInVarHole) => no_holes
      | (InHole(_, u), _)
      | (_, InVarHole(_, u)) =>
        mk_zholes(
          ~hole_selected=Some((PatHole(u), rev_steps |> List.rev)),
          (),
        )
      }
    | CursorP(_, Wild(err))
    | CursorP(_, NumLit(err, _))
    | CursorP(_, BoolLit(err, _))
    | CursorP(_, ListNil(err)) =>
      switch (err) {
      | NotInHole => no_holes
      | InHole(_, u) =>
        mk_zholes(
          ~hole_selected=Some((PatHole(u), rev_steps |> List.rev)),
          (),
        )
      }
    | CursorP(OnDelim(k, _), Parenthesized(body)) =>
      let body_holes = holes(body, [0, ...rev_steps], []);
      switch (k) {
      | 0 => mk_zholes(~holes_before=body_holes, ())
      | 1 => mk_zholes(~holes_after=body_holes, ())
      | _ => no_holes
      };
    | CursorP(OnDelim(k, _), Inj(err, _, body)) =>
      let body_holes = holes(body, [0, ...rev_steps], []);
      let hole_selected =
        switch (err) {
        | NotInHole => None
        | InHole(_, u) => Some((PatHole(u), rev_steps |> List.rev))
        };
      switch (k) {
      | 0 => mk_zholes(~holes_before=body_holes, ~hole_selected, ())
      | 1 => mk_zholes(~hole_selected, ~holes_after=body_holes, ())
      | _ => no_holes
      };
    | CursorP(OnText(_), Parenthesized(_) | Inj(_, _, _)) =>
      // invalid cursor position
      no_holes
    | ParenthesizedZ(zbody) => holes_z(zbody, [0, ...rev_steps])
    | InjZ(err, _, zbody) =>
      let zbody_holes = holes_z(zbody, [0, ...rev_steps]);
      switch (err) {
      | NotInHole => zbody_holes
      | InHole(_, u) => {
          ...zbody_holes,
          holes_before: [
            (PatHole(u), rev_steps |> List.rev),
            ...zbody_holes.holes_before,
          ],
        }
      };
    };
};

module Exp = {
  let rec of_z = (ze: ZExp.t): t => of_zblock(ze)
  and of_zblock = (zblock: ZExp.zblock): t => {
    let prefix_len = ZList.prefix_length(zblock);
    let zline = ZList.prj_z(zblock);
    cons'(prefix_len, of_zline(zline));
  }
  and of_zline = (zline: ZExp.zline): t =>
    switch (zline) {
    | CursorL(cursor, _) => ([], cursor)
    | LetLineZP(zp, _, _) => cons'(0, Pat.of_z(zp))
    | LetLineZA(_, zann, _) => cons'(1, Typ.of_z(zann))
    | LetLineZE(_, _, zdef) => cons'(2, of_z(zdef))
    | ExpLineZ(zopseq) => of_zopseq(zopseq)
    }
  and of_zopseq = (zopseq: ZExp.zopseq): t =>
    _of_zopseq(~of_zoperand, zopseq)
  and of_zoperand = (zoperand: ZExp.zoperand): t =>
    switch (zoperand) {
    | CursorE(cursor, _) => ([], cursor)
    | ParenthesizedZ(zbody) => cons'(0, of_z(zbody))
    | LamZP(_, zp, _, _) => cons'(0, Pat.of_z(zp))
    | LamZA(_, _, zann, _) => cons'(1, Typ.of_z(zann))
    | LamZE(_, _, _, zdef) => cons'(2, of_z(zdef))
    | InjZ(_, _, zbody) => cons'(0, of_z(zbody))
    | CaseZE(_, zscrut, _, _) => cons'(0, of_z(zscrut))
    | CaseZR(_, _, zrules, _) =>
      let prefix_len = List.length(ZList.prj_prefix(zrules));
      let zrule = ZList.prj_z(zrules);
      cons'(prefix_len + 1, of_zrule(zrule));
    | CaseZA(_, _, rules, zann) =>
      cons'(List.length(rules) + 1, Typ.of_z(zann))
    | ApPaletteZ(_, _, _, zpsi) =>
      let zhole_map = zpsi.zsplice_map;
      let (n, (_, ze)) = ZNatMap.prj_z_kv(zhole_map);
      cons'(n, of_z(ze));
    }
  and of_zrule = (zrule: ZExp.zrule): t =>
    switch (zrule) {
    | CursorR(cursor, _) => ([], cursor)
    | RuleZP(zp, _) => cons'(0, Pat.of_z(zp))
    | RuleZE(_, zclause) => cons'(1, of_z(zclause))
    };

  let rec follow = (path: t, e: UHExp.t): option(ZExp.t) =>
    follow_block(path, e)
  and follow_block =
      ((steps, cursor): t, block: UHExp.block): option(ZExp.zblock) =>
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
      ((steps, cursor) as path: t, line: UHExp.line): option(ZExp.zline) =>
    switch (steps, line) {
    | (_, ExpLine(opseq)) =>
      follow_opseq(path, opseq)
      |> OptUtil.map(zopseq => ZExp.ExpLineZ(zopseq))
    | ([], EmptyLine | LetLine(_, _, _)) =>
      line |> ZExp.place_cursor_line(cursor)
    | ([_, ..._], EmptyLine) => None
    | ([x, ...xs], LetLine(p, ann, def)) =>
      switch (x) {
      | 0 =>
        p
        |> Pat.follow((xs, cursor))
        |> OptUtil.map(zp => ZExp.LetLineZP(zp, ann, def))
      | 1 =>
        switch (ann) {
        | None => None
        | Some(ann) =>
          ann
          |> Typ.follow((xs, cursor))
          |> OptUtil.map(zann => ZExp.LetLineZA(p, zann, def))
        }
      | 2 =>
        def
        |> follow((xs, cursor))
        |> OptUtil.map(zdef => ZExp.LetLineZE(p, ann, zdef))
      | _ => None
      }
    }
  and follow_opseq = (path: t, opseq: UHExp.opseq): option(ZExp.zopseq) =>
    _follow_opseq(~follow_operand, ~follow_operator, path, opseq)
  and follow_operator =
      ((steps, cursor): t, operator: UHExp.operator): option(ZExp.zoperator) =>
    switch (steps) {
    | [] => operator |> ZExp.place_cursor_operator(cursor)
    | [_, ..._] => None
    }
  and follow_operand =
      ((steps, cursor): t, operand: UHExp.operand): option(ZExp.zoperand) =>
    switch (steps) {
    | [] => operand |> ZExp.place_cursor_operand(cursor)
    | [x, ...xs] =>
      switch (operand) {
      | EmptyHole(_)
      | Var(_, _, _)
      | NumLit(_, _)
      | BoolLit(_, _)
      | ListNil(_) => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          body
          |> follow((xs, cursor))
          |> OptUtil.map(zbody => ZExp.ParenthesizedZ(zbody))
        | _ => None
        }
      | Lam(err, p, ann, body) =>
        switch (x) {
        | 0 =>
          p
          |> Pat.follow((xs, cursor))
          |> OptUtil.map(zp => ZExp.LamZP(err, zp, ann, body))
        | 1 =>
          switch (ann) {
          | None => None
          | Some(ann) =>
            ann
            |> Typ.follow((xs, cursor))
            |> OptUtil.map(zann => ZExp.LamZA(err, p, zann, body))
          }
        | 2 =>
          body
          |> follow((xs, cursor))
          |> OptUtil.map(zbody => ZExp.LamZE(err, p, ann, zbody))
        | _ => None
        }
      | Inj(err, side, body) =>
        switch (x) {
        | 0 =>
          body
          |> follow((xs, cursor))
          |> OptUtil.map(zbody => ZExp.InjZ(err, side, zbody))
        | _ => None
        }
      | Case(err, scrut, rules, ann) =>
        switch (x) {
        | 0 =>
          scrut
          |> follow((xs, cursor))
          |> OptUtil.map(zscrut => ZExp.CaseZE(err, zscrut, rules, ann))
        | _ when x == List.length(rules) + 1 =>
          switch (ann) {
          | None => None
          | Some(ann) =>
            ann
            |> Typ.follow((xs, cursor))
            |> OptUtil.map(zann => ZExp.CaseZA(err, scrut, rules, zann))
          }
        | _ =>
          switch (ZList.split_at(x - 1, rules)) {
          | None => None
          | Some(split_rules) =>
            split_rules
            |> ZList.optmap_z(follow_rule((xs, cursor)))
            |> OptUtil.map(zrules => ZExp.CaseZR(err, scrut, zrules, ann))
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
  and follow_rule =
      ((steps, cursor): t, Rule(p, clause) as rule: UHExp.rule)
      : option(ZExp.zrule) =>
    switch (steps) {
    | [] => rule |> ZExp.place_cursor_rule(cursor)
    | [x, ...xs] =>
      switch (x) {
      | 0 =>
        p
        |> Pat.follow((xs, cursor))
        |> OptUtil.map(zp => ZExp.RuleZP(zp, clause))
      | 1 =>
        clause
        |> follow((xs, cursor))
        |> OptUtil.map(zclause => ZExp.RuleZE(p, zclause))
      | _ => None
      }
    };

  let rec follow_steps =
          (~side: Side.t=Before, steps: steps, e: UHExp.t): option(ZExp.t) =>
    follow_steps_block(~side, steps, e)
  and follow_steps_block =
      (~side: Side.t, steps: steps, block: UHExp.block): option(ZExp.zblock) =>
    switch (steps) {
    | [] => None // no block level cursor
    | [x, ...xs] =>
      switch (ZList.split_at(x, block)) {
      | None => None
      | Some(split_lines) =>
        split_lines |> ZList.optmap_z(follow_steps_line(~side, xs))
      }
    }
  and follow_steps_line =
      (~side: Side.t, steps: steps, line: UHExp.line): option(ZExp.zline) =>
    switch (steps, line) {
    | (_, ExpLine(opseq)) =>
      follow_steps_opseq(~side, steps, opseq)
      |> OptUtil.map(zopseq => ZExp.ExpLineZ(zopseq))
    | ([], EmptyLine | LetLine(_, _, _)) =>
      switch (side) {
      | Before => Some(line |> ZExp.place_before_line)
      | After => Some(line |> ZExp.place_after_line)
      }
    | ([_, ..._], EmptyLine) => None
    | ([x, ...xs], LetLine(p, ann, def)) =>
      switch (x) {
      | 0 =>
        p
        |> Pat.follow_steps(~side, xs)
        |> OptUtil.map(zp => ZExp.LetLineZP(zp, ann, def))
      | 1 =>
        switch (ann) {
        | None => None
        | Some(ann) =>
          ann
          |> Typ.follow_steps(~side, xs)
          |> OptUtil.map(zann => ZExp.LetLineZA(p, zann, def))
        }
      | 2 =>
        def
        |> follow_steps(~side, xs)
        |> OptUtil.map(zdef => ZExp.LetLineZE(p, ann, zdef))
      | _ => None
      }
    }
  and follow_steps_opseq =
      (~side: Side.t, steps: steps, opseq: UHExp.opseq): option(ZExp.zopseq) =>
    _follow_steps_opseq(
      ~follow_steps_operand,
      ~follow_steps_operator,
      ~side,
      steps,
      opseq,
    )
  and follow_steps_operator =
      (~side: Side.t, steps: steps, operator: UHExp.operator)
      : option(ZExp.zoperator) =>
    switch (steps) {
    | [_, ..._] => None
    | [] =>
      switch (side) {
      | Before => operator |> ZExp.place_before_operator
      | After => operator |> ZExp.place_after_operator
      }
    }
  and follow_steps_operand =
      (~side: Side.t, steps: steps, operand: UHExp.operand)
      : option(ZExp.zoperand) =>
    switch (steps) {
    | [] =>
      switch (side) {
      | Before => Some(operand |> ZExp.place_before_operand)
      | After => Some(operand |> ZExp.place_after_operand)
      }
    | [x, ...xs] =>
      switch (operand) {
      | EmptyHole(_)
      | Var(_, _, _)
      | NumLit(_, _)
      | BoolLit(_, _)
      | ListNil(_) => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          body
          |> follow_steps(~side, xs)
          |> OptUtil.map(zbody => ZExp.ParenthesizedZ(zbody))
        | _ => None
        }
      | Lam(err, p, ann, body) =>
        switch (x) {
        | 0 =>
          p
          |> Pat.follow_steps(~side, xs)
          |> OptUtil.map(zp => ZExp.LamZP(err, zp, ann, body))
        | 1 =>
          switch (ann) {
          | None => None
          | Some(ann) =>
            ann
            |> Typ.follow_steps(~side, xs)
            |> OptUtil.map(zann => ZExp.LamZA(err, p, zann, body))
          }
        | 2 =>
          body
          |> follow_steps(~side, xs)
          |> OptUtil.map(zbody => ZExp.LamZE(err, p, ann, zbody))
        | _ => None
        }
      | Inj(err, inj_side, body) =>
        switch (x) {
        | 0 =>
          body
          |> follow_steps(~side, xs)
          |> OptUtil.map(zbody => ZExp.InjZ(err, inj_side, zbody))
        | _ => None
        }
      | Case(err, scrut, rules, ann) =>
        switch (x) {
        | 0 =>
          scrut
          |> follow_steps(~side, xs)
          |> OptUtil.map(zscrut => ZExp.CaseZE(err, zscrut, rules, ann))
        | _ when x == List.length(rules) + 1 =>
          switch (ann) {
          | None => None
          | Some(ann) =>
            ann
            |> Typ.follow_steps(~side, xs)
            |> OptUtil.map(zann => ZExp.CaseZA(err, scrut, rules, zann))
          }
        | _ =>
          switch (ZList.split_at(x - 1, rules)) {
          | None => None
          | Some(split_rules) =>
            split_rules
            |> ZList.optmap_z(follow_steps_rule(~side, xs))
            |> OptUtil.map(zrules => ZExp.CaseZR(err, scrut, zrules, ann))
          }
        }
      | ApPalette(err, name, serialized_model, splice_info) =>
        switch (
          ZSpliceInfo.select_opt(splice_info, x, ((ty, e)) =>
            switch (follow_steps(~side, xs, e)) {
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
  and follow_steps_rule =
      (~side: Side.t, steps: steps, Rule(p, clause) as rule: UHExp.rule)
      : option(ZExp.zrule) =>
    switch (steps) {
    | [] =>
      switch (side) {
      | Before => Some(rule |> ZExp.place_before_rule)
      | After => Some(rule |> ZExp.place_after_rule)
      }
    | [x, ...xs] =>
      switch (x) {
      | 0 =>
        p
        |> Pat.follow_steps(~side, xs)
        |> OptUtil.map(zp => ZExp.RuleZP(zp, clause))
      | 1 =>
        clause
        |> follow_steps(~side, xs)
        |> OptUtil.map(zclause => ZExp.RuleZE(p, zclause))
      | _ => None
      }
    };

  exception UHExpNodeNotFound;
  let follow_or_fail = (path: t, e: UHExp.t): ZExp.t =>
    switch (follow(path, e)) {
    | None => raise(UHExpNodeNotFound)
    | Some(ze) => ze
    };

  let follow_operand_or_fail =
      (path: t, operand: UHExp.operand): ZExp.zoperand =>
    switch (follow_operand(path, operand)) {
    | None => raise(UHExpNodeNotFound)
    | Some(zoperand) => zoperand
    };

  let hole_desc = (u: MetaVar.t): hole_desc => ExpHole(u);
  let holes_err = holes_err(~hole_desc);
  let holes_verr = holes_verr(~hole_desc);

  let rec holes = (e: UHExp.t, rev_steps: rev_steps, hs: hole_list): hole_list =>
    hs |> holes_block(e, rev_steps)
  and holes_block =
      (block: UHExp.block, rev_steps: rev_steps, hs: hole_list): hole_list =>
    hs
    |> ListUtil.fold_right_i(
         ((i, line), hs) => hs |> holes_line(line, [i, ...rev_steps]),
         block,
       )
  and holes_line =
      (line: UHExp.line, rev_steps: rev_steps, hs: hole_list): hole_list =>
    switch (line) {
    | EmptyLine => hs
    | LetLine(p, ann, def) =>
      hs
      |> holes(def, [2, ...rev_steps])
      |> (
        switch (ann) {
        | None => (hs => hs)
        | Some(ann) => Typ.holes(ann, [1, ...rev_steps])
        }
      )
      |> Pat.holes(p, [0, ...rev_steps])
    | ExpLine(opseq) =>
      hs
      |> holes_opseq(
           ~holes_operand,
           ~hole_desc,
           ~is_space=UHExp.is_Space,
           ~rev_steps,
           opseq,
         )
    }
  and holes_operand =
      (operand: UHExp.operand, rev_steps: rev_steps, hs: hole_list): hole_list =>
    switch (operand) {
    | EmptyHole(u) => [(ExpHole(u), rev_steps |> List.rev), ...hs]
    | Var(err, verr, _) =>
      hs |> holes_verr(verr, rev_steps) |> holes_err(err, rev_steps)
    | NumLit(err, _)
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
        | Some(ann) => Typ.holes(ann, [1, ...rev_steps])
        }
      )
      |> Pat.holes(p, [0, ...rev_steps])
      |> holes_err(err, rev_steps)
    | Case(err, scrut, rules, ann) =>
      hs
      |> (
        switch (ann) {
        | None => (hs => hs)
        | Some(ann) =>
          Typ.holes(ann, [1 + List.length(rules), ...rev_steps])
        }
      )
      |> ListUtil.fold_right_i(
           ((i, rule), hs) =>
             hs |> holes_rule(rule, [1 + i, ...rev_steps]),
           rules,
         )
      |> holes(scrut, [0, ...rev_steps])
      |> holes_err(err, rev_steps)
    | ApPalette(err, _, _, psi) =>
      let splice_map = psi.splice_map;
      let splice_order = psi.splice_order;
      List.fold_right(
        (i, hs) =>
          switch (NatMap.lookup(splice_map, i)) {
          | None => hs
          | Some((_, e)) => hs |> holes(e, [i, ...rev_steps])
          },
        splice_order,
        hs,
      )
      |> holes_err(err, rev_steps);
    }
  and holes_rule =
      (Rule(p, clause): UHExp.rule, rev_steps: rev_steps, hs: hole_list)
      : hole_list => {
    hs
    |> holes(clause, [1, ...rev_steps])
    |> Pat.holes(p, [0, ...rev_steps]);
  };

  let rec holes_z = (ze: ZExp.t, rev_steps: rev_steps): zhole_list =>
    holes_zblock(ze, rev_steps)
  and holes_zblock =
      ((prefix, zline, suffix): ZExp.zblock, rev_steps: rev_steps)
      : zhole_list => {
    let holes_prefix =
      ListUtil.fold_right_i(
        ((i, line), hs) => hs |> holes_line(line, [i, ...rev_steps]),
        prefix,
        [],
      );
    let {holes_before, hole_selected, holes_after} =
      holes_zline(zline, [List.length(prefix), ...rev_steps]);
    let holes_suffix =
      ListUtil.fold_right_i(
        ((i, line), hs) =>
          hs
          |> holes_line(line, [List.length(prefix) + 1 + i, ...rev_steps]),
        suffix,
        [],
      );
    mk_zholes(
      ~holes_before=holes_prefix @ holes_before,
      ~hole_selected,
      ~holes_after=holes_after @ holes_suffix,
      (),
    );
  }
  and holes_zline = (zline: ZExp.zline, rev_steps: rev_steps): zhole_list =>
    switch (zline) {
    | CursorL(OnOp(_), _) => no_holes
    | CursorL(_, EmptyLine) => no_holes
    | CursorL(_, ExpLine(_)) => no_holes /* invalid cursor position */
    | CursorL(cursor, LetLine(p, ann, def)) =>
      let holes_p = Pat.holes(p, [0, ...rev_steps], []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) => Typ.holes(uty, [1, ...rev_steps], [])
        };
      let holes_def = holes(def, [2, ...rev_steps], []);
      switch (cursor) {
      | OnDelim(0, _) =>
        mk_zholes(~holes_after=holes_p @ holes_ann @ holes_def, ())
      | OnDelim(1, _) =>
        mk_zholes(
          ~holes_before=holes_p,
          ~holes_after=holes_ann @ holes_def,
          (),
        )
      | OnDelim(2, _) =>
        mk_zholes(
          ~holes_before=holes_p @ holes_ann,
          ~holes_after=holes_def,
          (),
        )
      | OnDelim(3, _) =>
        mk_zholes(~holes_before=holes_p @ holes_ann @ holes_def, ())
      | _ => no_holes
      };
    | ExpLineZ(zopseq) => holes_zopseq(zopseq, rev_steps)
    | LetLineZP(zp, ann, body) =>
      let {holes_before, hole_selected, holes_after} =
        Pat.holes_z(zp, [0, ...rev_steps]);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(ann) => Typ.holes(ann, [1, ...rev_steps], [])
        };
      let holes_body = holes(body, [2, ...rev_steps], []);
      mk_zholes(
        ~holes_before,
        ~hole_selected,
        ~holes_after=holes_after @ holes_ann @ holes_body,
        (),
      );
    | LetLineZA(p, zann, body) =>
      let holes_p = Pat.holes(p, [0, ...rev_steps], []);
      let {holes_before, hole_selected, holes_after} =
        Typ.holes_z(zann, [1, ...rev_steps]);
      let holes_body = holes(body, [2, ...rev_steps], []);
      mk_zholes(
        ~holes_before=holes_p @ holes_before,
        ~hole_selected,
        ~holes_after=holes_after @ holes_body,
        (),
      );
    | LetLineZE(p, ann, zbody) =>
      let holes_p = Pat.holes(p, [0, ...rev_steps], []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(ann) => Typ.holes(ann, [1, ...rev_steps], [])
        };
      let {holes_before, hole_selected, holes_after} =
        holes_z(zbody, [2, ...rev_steps]);
      mk_zholes(
        ~holes_before=holes_p @ holes_ann @ holes_before,
        ~hole_selected,
        ~holes_after,
        (),
      );
    }
  and holes_zopseq = (zopseq: ZExp.zopseq, rev_steps: rev_steps): zhole_list =>
    _holes_zopseq(
      ~holes_operand,
      ~holes_zoperand,
      ~hole_desc,
      ~is_space=UHExp.is_Space,
      ~rev_steps,
      ~erase_zopseq=ZExp.erase_zopseq,
      zopseq,
    )
  and holes_zoperand =
      (zoperand: ZExp.zoperand, rev_steps: rev_steps): zhole_list =>
    switch (zoperand) {
    | CursorE(OnOp(_), _) => no_holes
    | CursorE(_, EmptyHole(u)) =>
      mk_zholes(
        ~hole_selected=Some((ExpHole(u), rev_steps |> List.rev)),
        (),
      )
    | CursorE(_, Var(err, verr, _)) =>
      switch (err, verr) {
      | (NotInHole, NotInVarHole) => no_holes
      | (InHole(_, u), _)
      | (_, InVarHole(_, u)) =>
        mk_zholes(
          ~hole_selected=Some((ExpHole(u), rev_steps |> List.rev)),
          (),
        )
      }
    | CursorE(_, NumLit(err, _))
    | CursorE(_, BoolLit(err, _))
    | CursorE(_, ListNil(err)) =>
      switch (err) {
      | NotInHole => no_holes
      | InHole(_, u) =>
        mk_zholes(
          ~hole_selected=Some((ExpHole(u), rev_steps |> List.rev)),
          (),
        )
      }
    | CursorE(OnDelim(k, _), Parenthesized(body)) =>
      let body_holes = holes(body, [0, ...rev_steps], []);
      switch (k) {
      | 0 => mk_zholes(~holes_before=body_holes, ())
      | 1 => mk_zholes(~holes_after=body_holes, ())
      | _ => no_holes
      };
    | CursorE(OnDelim(k, _), Inj(err, _, body)) =>
      let hole_selected =
        switch (err) {
        | NotInHole => None
        | InHole(_, u) => Some((ExpHole(u), rev_steps |> List.rev))
        };
      let body_holes = holes(body, [0, ...rev_steps], []);
      switch (k) {
      | 0 => mk_zholes(~holes_before=body_holes, ~hole_selected, ())
      | 1 => mk_zholes(~hole_selected, ~holes_after=body_holes, ())
      | _ => no_holes
      };
    | CursorE(OnDelim(k, _), Lam(err, p, ann, body)) =>
      let hole_selected =
        switch (err) {
        | NotInHole => None
        | InHole(_, u) => Some((ExpHole(u), rev_steps |> List.rev))
        };
      let holes_p = Pat.holes(p, [0, ...rev_steps], []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) => Typ.holes(uty, [1, ...rev_steps], [])
        };
      let holes_body = holes(body, [2, ...rev_steps], []);
      switch (k) {
      | 0 =>
        mk_zholes(
          ~hole_selected,
          ~holes_after=holes_p @ holes_ann @ holes_body,
          (),
        )
      | 1 =>
        mk_zholes(
          ~holes_before=holes_p,
          ~hole_selected,
          ~holes_after=holes_ann @ holes_body,
          (),
        )
      | 2 =>
        mk_zholes(
          ~holes_before=holes_p @ holes_ann,
          ~hole_selected,
          ~holes_after=holes_body,
          (),
        )
      | _ => no_holes
      };
    | CursorE(OnDelim(k, _), Case(err, scrut, rules, ann)) =>
      let hole_selected =
        switch (err) {
        | NotInHole => None
        | InHole(_, u) => Some((ExpHole(u), rev_steps |> List.rev))
        };
      let holes_scrut = holes(scrut, [0, ...rev_steps], []);
      let holes_rules =
        ListUtil.fold_right_i(
          ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
          rules,
          [],
        );
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(ann) =>
          Typ.holes(ann, [List.length(rules) + 1, ...rev_steps], [])
        };
      switch (k) {
      | 0 =>
        mk_zholes(
          ~holes_after=holes_scrut @ holes_rules @ holes_ann,
          ~hole_selected,
          (),
        )
      | 1 =>
        mk_zholes(
          ~holes_before=holes_scrut @ holes_rules,
          ~hole_selected,
          ~holes_after=holes_ann,
          (),
        )
      | _ => no_holes
      };
    | CursorE(
        OnText(_),
        Inj(_, _, _) | Parenthesized(_) | Lam(_, _, _, _) | Case(_, _, _, _),
      ) =>
      /* invalid cursor position */
      no_holes
    | CursorE(_, ApPalette(_, _, _, _)) => no_holes /* TODO[livelits] */
    | ParenthesizedZ(zbody) => holes_z(zbody, [0, ...rev_steps])
    | LamZP(err, zp, ann, body) =>
      let holes_err =
        switch (err) {
        | NotInHole => []
        | InHole(_, u) => [(ExpHole(u), rev_steps |> List.rev)]
        };
      let {holes_before, hole_selected, holes_after} =
        Pat.holes_z(zp, [0, ...rev_steps]);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(ann) => Typ.holes(ann, [1, ...rev_steps], [])
        };
      let holes_body = holes(body, [2, ...rev_steps], []);
      mk_zholes(
        ~holes_before=holes_err @ holes_before,
        ~hole_selected,
        ~holes_after=holes_after @ holes_ann @ holes_body,
        (),
      );
    | LamZA(err, p, zann, body) =>
      let holes_err =
        switch (err) {
        | NotInHole => []
        | InHole(_, u) => [(ExpHole(u), rev_steps |> List.rev)]
        };
      let holes_p = Pat.holes(p, [0, ...rev_steps], []);
      let {holes_before, hole_selected, holes_after} =
        Typ.holes_z(zann, [1, ...rev_steps]);
      let holes_body = holes(body, [2, ...rev_steps], []);
      mk_zholes(
        ~holes_before=holes_err @ holes_p @ holes_before,
        ~hole_selected,
        ~holes_after=holes_after @ holes_body,
        (),
      );
    | LamZE(err, p, ann, zbody) =>
      let holes_err =
        switch (err) {
        | NotInHole => []
        | InHole(_, u) => [(ExpHole(u), rev_steps |> List.rev)]
        };
      let holes_p = Pat.holes(p, [0, ...rev_steps], []);
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) => Typ.holes(uty, [1, ...rev_steps], [])
        };
      let {holes_before, hole_selected, holes_after} =
        holes_z(zbody, [2, ...rev_steps]);
      mk_zholes(
        ~holes_before=holes_err @ holes_p @ holes_ann @ holes_before,
        ~hole_selected,
        ~holes_after,
        (),
      );
    | InjZ(err, _, zbody) =>
      let holes_err =
        switch (err) {
        | NotInHole => []
        | InHole(_, u) => [(ExpHole(u), rev_steps |> List.rev)]
        };
      let {holes_before, hole_selected, holes_after} =
        holes_z(zbody, [0, ...rev_steps]);
      mk_zholes(
        ~holes_before=holes_err @ holes_before,
        ~hole_selected,
        ~holes_after,
        (),
      );
    | CaseZE(err, zscrut, rules, ann) =>
      let holes_err =
        switch (err) {
        | NotInHole => []
        | InHole(_, u) => [(ExpHole(u), rev_steps |> List.rev)]
        };
      let {holes_before, hole_selected, holes_after} =
        holes_z(zscrut, [0, ...rev_steps]);
      let holes_rules =
        ListUtil.fold_right_i(
          ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
          rules,
          [],
        );
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) =>
          Typ.holes(uty, [List.length(rules) + 1, ...rev_steps], [])
        };
      mk_zholes(
        ~holes_before=holes_err @ holes_before,
        ~hole_selected,
        ~holes_after=holes_after @ holes_rules @ holes_ann,
        (),
      );
    | CaseZR(err, scrut, (prefix, zrule, suffix), ann) =>
      let holes_err =
        switch (err) {
        | NotInHole => []
        | InHole(_, u) => [(ExpHole(u), rev_steps |> List.rev)]
        };
      let holes_scrut = holes(scrut, [0, ...rev_steps], []);
      let holes_prefix =
        ListUtil.fold_right_i(
          ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
          prefix,
          [],
        );
      let {holes_before, hole_selected, holes_after} =
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
      let holes_ann =
        switch (ann) {
        | None => []
        | Some(uty) =>
          Typ.holes(
            uty,
            [
              1 + List.length(prefix) + 1 + List.length(suffix),
              ...rev_steps,
            ],
            [],
          )
        };
      {
        holes_before: holes_err @ holes_scrut @ holes_before @ holes_prefix,
        hole_selected,
        holes_after: holes_suffix @ holes_after @ holes_ann,
      };
    | CaseZA(err, scrut, rules, zann) =>
      let holes_err =
        switch (err) {
        | NotInHole => []
        | InHole(_, u) => [(ExpHole(u), rev_steps |> List.rev)]
        };
      let holes_scrut = holes(scrut, [0, ...rev_steps], []);
      let holes_rules =
        ListUtil.fold_right_i(
          ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
          rules,
          [],
        );
      let {holes_before, hole_selected, holes_after} =
        Typ.holes_z(zann, [1 + List.length(rules), ...rev_steps]);
      {
        holes_before: holes_err @ holes_scrut @ holes_rules @ holes_before,
        hole_selected,
        holes_after,
      };
    | ApPaletteZ(_, _, _, zpsi) =>
      let zsplice_map = zpsi.zsplice_map;
      let (n, (_, ze)) = ZNatMap.prj_z_kv(zsplice_map);
      let {holes_before, hole_selected, holes_after} =
        holes_z(ze, [n, ...rev_steps]);
      let splice_order = zpsi.splice_order;
      let splice_map = ZNatMap.prj_map(zsplice_map);
      let (splices_before, splices_after) =
        ListUtil.split_at(splice_order, n);
      let holes_splices_before =
        List.fold_left(
          (hs, n) =>
            switch (NatMap.lookup(splice_map, n)) {
            | None => hs
            | Some((_, e)) => hs @ holes(e, [n, ...rev_steps], [])
            },
          [],
          splices_before,
        );
      let holes_splices_after =
        List.fold_left(
          (hs, n) =>
            switch (NatMap.lookup(splice_map, n)) {
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
  and holes_zrule = (zrule: ZExp.zrule, rev_steps: rev_steps) =>
    switch (zrule) {
    | CursorR(OnOp(_) | OnText(_), _) =>
      // invalid cursor position
      no_holes
    | CursorR(OnDelim(k, _), Rule(p, clause)) =>
      let holes_p = Pat.holes(p, [1, ...rev_steps], []);
      let holes_clause = holes(clause, [0, ...rev_steps], []);
      switch (k) {
      | 0 => mk_zholes(~holes_after=holes_p @ holes_clause, ())
      | 1 => mk_zholes(~holes_before=holes_p, ~holes_after=holes_clause, ())
      | _ => no_holes
      };
    | RuleZP(zp, clause) =>
      let zholes_p = Pat.holes_z(zp, [0, ...rev_steps]);
      let holes_clause = holes(clause, [1, ...rev_steps], []);
      {...zholes_p, holes_after: zholes_p.holes_after @ holes_clause};
    | RuleZE(p, zclause) =>
      let holes_p = Pat.holes(p, [0, ...rev_steps], []);
      let zholes_clause = holes_z(zclause, [1, ...rev_steps]);
      {...zholes_clause, holes_before: holes_p @ zholes_clause.holes_before};
    };

  let prev_hole_steps_z = (ze: ZExp.t): option(steps) => {
    let holes = holes_z(ze, []);
    prev_hole_steps(holes);
  };
  let prev_hole_steps_zline = (zline: ZExp.zline): option(steps) => {
    let holes = holes_zline(zline, []);
    prev_hole_steps(holes);
  };

  let next_hole_steps_z = (ze: ZExp.t): option(steps) => {
    let holes = holes_z(ze, []);
    next_hole_steps(holes);
  };
  let next_hole_steps_zline = (zline: ZExp.zline): option(steps) => {
    let holes = holes_zline(zline, []);
    next_hole_steps(holes);
  };
};

let append = ((appendee_steps, appendee_cursor): t, steps): t => (
  steps @ appendee_steps,
  appendee_cursor,
);

let steps_to_hole = (hole_list: hole_list, u: MetaVar.t): option(steps) =>
  switch (
    List.find_opt(
      ((hole_desc, _)) =>
        switch (hole_desc) {
        | ExpHole(u')
        | PatHole(u') => MetaVar.eq(u, u')
        | TypHole => false
        },
      hole_list,
    )
  ) {
  | None => None
  | Some((_, steps)) => Some(steps)
  };

let steps_to_hole_z = (zhole_list: zhole_list, u: MetaVar.t): option(steps) => {
  let {holes_before, hole_selected, holes_after} = zhole_list;
  switch (steps_to_hole(holes_before, u)) {
  | Some(_) as res => res
  | None =>
    switch (hole_selected) {
    | Some((ExpHole(u'), path))
    | Some((PatHole(u'), path)) =>
      MetaVar.eq(u, u') ? Some(path) : steps_to_hole(holes_after, u)
    | Some((TypHole, _))
    | None => steps_to_hole(holes_after, u)
    }
  };
};

let opt_steps_to_opt_path =
    (cursor: CursorPosition.t, opt_steps: option(steps)): option(t) =>
  switch (opt_steps) {
  | None => None
  | Some(steps) => Some((List.rev(steps), cursor))
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
