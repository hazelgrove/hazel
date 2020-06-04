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
[@deriving sexp]
type rev_t = (CursorPosition.t, rev_steps);

let rev = ((cursor, rev_steps): rev_t): t => (
  rev_steps |> List.rev,
  cursor,
);

let cons' = (step: int, (steps, cursor): t): t => {
  ([step, ...steps], cursor);
};

let of_zopseq_ =
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

let follow_opseq_ =
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

let of_steps_opseq_ =
    (
      ~of_steps_operand: (steps, ~side: Side.t, 'operand) => option(t),
      ~of_steps_operator: (steps, ~side: Side.t, 'operator) => option(t),
      steps: steps,
      ~side: Side.t,
      OpSeq(_, seq): OpSeq.t('operand, 'operator),
    )
    : option(t) =>
  switch (steps) {
  | [] => None
  | [x, ...xs] =>
    switch (
      seq |> Seq.opt_split_nth_operand(x),
      seq |> Seq.opt_split_nth_operator(x - Seq.length(seq)),
    ) {
    | (None, None) => None
    | (Some((operand, _)), _) =>
      let path = operand |> of_steps_operand(xs, ~side);
      path |> OptUtil.map(path => cons'(x, path));
    | (_, Some((operator, _))) =>
      operator
      |> of_steps_operator(xs, ~side)
      |> OptUtil.map(path => cons'(x, path))
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

let holes_case_err =
    (
      ~hole_desc: MetaVar.t => hole_desc,
      err: CaseErrStatus.t,
      rev_steps: rev_steps,
      hs: hole_list,
    ) =>
  switch (err) {
  | StandardErrStatus(err) => holes_err(~hole_desc, err, rev_steps, hs)
  | InconsistentBranches(_, u) => [
      (hole_desc(u), rev_steps |> List.rev),
      ...hs,
    ]
  };

let holes_skel_ =
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
  holes_skel_(
    ~holes_operand,
    ~hole_desc,
    ~is_space,
    ~rev_steps,
    skel,
    seq,
    hs,
  );

let holes_zopseq_ =
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
    holes_skel_(
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
  and of_zopseq = zopseq => of_zopseq_(~of_zoperand, zopseq)
  and of_zoperand =
    fun
    | CursorT(cursor, _) => ([], cursor)
    | ParenthesizedZ(zbody) => cons'(0, of_z(zbody))
    | ListZ(zbody) => cons'(0, of_z(zbody))
  and of_zoperator =
    fun
    | (cursor, _) => ([], cursor);

  let rec follow = (path: t, uty: UHTyp.t): option(ZTyp.t) =>
    follow_opseq(path, uty)
  and follow_opseq = (path: t, opseq: UHTyp.opseq): option(ZTyp.zopseq) =>
    follow_opseq_(~follow_operand, ~follow_operator, path, opseq)
  and follow_operand =
      ((steps, cursor): t, operand: UHTyp.operand): option(ZTyp.zoperand) =>
    switch (steps) {
    | [] => operand |> ZTyp.place_cursor_operand(cursor)
    | [x, ...xs] =>
      switch (operand) {
      | Hole
      | Unit
      | Int
      | Float
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

  let rec of_steps =
          (steps: steps, ~side: Side.t=Before, uty: UHTyp.t): option(t) =>
    of_steps_opseq(steps, ~side, uty)
  and of_steps_opseq =
      (steps: steps, ~side: Side.t, opseq: UHTyp.opseq): option(t) =>
    of_steps_opseq_(
      ~of_steps_operand,
      ~of_steps_operator,
      steps,
      ~side,
      opseq,
    )
  and of_steps_operand =
      (steps: steps, ~side: Side.t, operand: UHTyp.operand): option(t) =>
    switch (steps) {
    | [] =>
      let place_cursor =
        switch (side) {
        | Before => ZTyp.place_before_operand
        | After => ZTyp.place_after_operand
        };
      Some(of_zoperand(place_cursor(operand)));
    | [x, ...xs] =>
      switch (operand) {
      | Hole
      | Unit
      | Int
      | Float
      | Bool => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          of_steps(xs, ~side, body) |> Option.map(path => cons'(0, path))
        | _ => None
        }
      | List(body) =>
        switch (x) {
        | 0 =>
          body |> of_steps(xs, ~side) |> OptUtil.map(path => cons'(0, path))
        | _ => None
        }
      }
    }
  and of_steps_operator =
      (steps: steps, ~side: Side.t, operator: UHTyp.operator): option(t) =>
    switch (steps) {
    | [_, ..._] => None
    | [] =>
      let place_cursor =
        switch (side) {
        | Before => ZTyp.place_before_operator
        | After => ZTyp.place_after_operator
        };
      switch (place_cursor(operator)) {
      | Some(zty) => Some(of_zoperator(zty))
      | _ => None
      };
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
    | Int
    | Float
    | Bool => hs
    | Parenthesized(body)
    | List(body) => hs |> holes(body, [0, ...rev_steps])
    };

  let rec holes_z = (zty: ZTyp.t, rev_steps: rev_steps): zhole_list =>
    holes_zopseq(zty, rev_steps)
  and holes_zopseq = (zopseq: ZTyp.zopseq, rev_steps: rev_steps): zhole_list =>
    holes_zopseq_(
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
    | CursorT(_, Unit | Int | Float | Bool) => no_holes
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
    of_zopseq_(~of_zoperand, zopseq)
  and of_zoperand =
    fun
    | CursorP(cursor, _) => ([], cursor)
    | ParenthesizedZ(zbody)
    | InjZ(_, _, zbody) => cons'(0, of_z(zbody))
  and of_zoperator =
    fun
    | (cursor, _) => ([], cursor);

  let rec follow = (path: t, p: UHPat.t): option(ZPat.t) =>
    follow_opseq(path, p)
  and follow_opseq = (path: t, opseq: UHPat.opseq): option(ZPat.zopseq) =>
    follow_opseq_(~follow_operand, ~follow_operator, path, opseq)
  and follow_operand =
      ((steps, cursor): t, operand: UHPat.operand): option(ZPat.zoperand) =>
    switch (steps) {
    | [] => operand |> ZPat.place_cursor_operand(cursor)
    | [x, ...xs] =>
      switch (operand) {
      | EmptyHole(_)
      | Wild(_)
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

  let rec of_steps =
          (steps: steps, ~side: Side.t=Before, p: UHPat.t): option(t) =>
    of_steps_opseq(steps, ~side, p)
  and of_steps_opseq =
      (steps: steps, ~side: Side.t, opseq: UHPat.opseq): option(t) =>
    of_steps_opseq_(
      ~of_steps_operand,
      ~of_steps_operator,
      steps,
      ~side,
      opseq,
    )
  and of_steps_operand =
      (steps: steps, ~side: Side.t, operand: UHPat.operand): option(t) =>
    switch (steps) {
    | [] =>
      let place_cursor =
        switch (side) {
        | Before => ZPat.place_before_operand
        | After => ZPat.place_after_operand
        };
      Some(of_zoperand(place_cursor(operand)));
    | [x, ...xs] =>
      switch (operand) {
      | EmptyHole(_)
      | Wild(_)
      | Var(_, _, _)
      | IntLit(_, _)
      | FloatLit(_, _)
      | BoolLit(_, _)
      | ListNil(_) => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          body |> of_steps(xs, ~side) |> OptUtil.map(path => cons'(0, path))
        | _ => None
        }
      | Inj(_, _, body) =>
        switch (x) {
        | 0 =>
          body |> of_steps(xs, ~side) |> OptUtil.map(path => cons'(0, path))
        | _ => None
        }
      }
    }
  and of_steps_operator =
      (steps: steps, ~side: Side.t, operator: UHPat.operator): option(t) =>
    switch (steps) {
    | [] =>
      let place_cursor =
        switch (side) {
        | Before => ZPat.place_before_operator
        | After => ZPat.place_after_operator
        };
      switch (place_cursor(operator)) {
      | Some(zop) => Some(of_zoperator(zop))
      | _ => None
      };
    | [_, ..._] => None
    };

  let hole_desc = (u: MetaVar.t): hole_desc => PatHole(u);

  let rec holes = (p: UHPat.t, rev_steps: rev_steps, hs: hole_list): hole_list =>
    hs
    |> holes_opseq(
         ~holes_operand,
         ~hole_desc,
         ~is_space=Operators.Pat.is_Space,
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
    | IntLit(InHole(_, u), _)
    | FloatLit(InHole(_, u), _)
    | BoolLit(InHole(_, u), _)
    | ListNil(InHole(_, u)) => [
        (PatHole(u), rev_steps |> List.rev),
        ...hs,
      ]
    | Var(NotInHole, NotInVarHole, _)
    | Wild(NotInHole)
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
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
    holes_zopseq_(
      ~holes_operand,
      ~holes_zoperand,
      ~hole_desc,
      ~is_space=Operators.Pat.is_Space,
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
    | CursorP(_, IntLit(err, _))
    | CursorP(_, FloatLit(err, _))
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
    of_zopseq_(~of_zoperand, zopseq)
  and of_zoperand = (zoperand: ZExp.zoperand): t =>
    switch (zoperand) {
    | CursorE(cursor, _) => ([], cursor)
    | ParenthesizedZ(zbody) => cons'(0, of_z(zbody))
    | LamZP(_, zp, _, _) => cons'(0, Pat.of_z(zp))
    | LamZA(_, _, zann, _) => cons'(1, Typ.of_z(zann))
    | LamZE(_, _, _, zdef) => cons'(2, of_z(zdef))
    | InjZ(_, _, zbody) => cons'(0, of_z(zbody))
    | CaseZE(_, zscrut, _) => cons'(0, of_z(zscrut))
    | CaseZR(_, _, zrules) =>
      let prefix_len = List.length(ZList.prj_prefix(zrules));
      let zrule = ZList.prj_z(zrules);
      cons'(prefix_len + 1, of_zrule(zrule));
    | ApPaletteZ(_, _, _, zpsi) =>
      let zhole_map = zpsi.zsplice_map;
      let (n, (_, ze)) = ZNatMap.prj_z_kv(zhole_map);
      cons'(n, of_z(ze));
    }
  and of_zoperator = (zoperator: ZExp.zoperator): t => {
    let (cursor, _) = zoperator;
    ([], cursor);
  }
  and of_zrules = (zrules: ZExp.zrules): t => {
    let prefix_len = List.length(ZList.prj_prefix(zrules));
    let zrule = ZList.prj_z(zrules);
    cons'(prefix_len, of_zrule(zrule));
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
    follow_opseq_(~follow_operand, ~follow_operator, path, opseq)
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
      | IntLit(_, _)
      | FloatLit(_, _)
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
      | Case(err, scrut, rules) =>
        switch (x) {
        | 0 =>
          scrut
          |> follow((xs, cursor))
          |> OptUtil.map(zscrut => ZExp.CaseZE(err, zscrut, rules))
        | _ =>
          switch (ZList.split_at(x - 1, rules)) {
          | None => None
          | Some(split_rules) =>
            split_rules
            |> ZList.optmap_z(follow_rule((xs, cursor)))
            |> OptUtil.map(zrules => ZExp.CaseZR(err, scrut, zrules))
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
      ((steps, cursor): t, rules: UHExp.rules): option(ZExp.zrules) =>
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

  let rec of_steps =
          (steps: steps, ~side: Side.t=Before, e: UHExp.t): option(t) =>
    of_steps_block(steps, ~side, e)
  and of_steps_block =
      (steps: steps, ~side: Side.t, block: UHExp.block): option(t) =>
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
        z |> of_steps_line(xs, ~side) |> OptUtil.map(path => cons'(x, path));
      }
    }
  and of_steps_line =
      (steps: steps, ~side: Side.t, line: UHExp.line): option(t) =>
    switch (steps, line) {
    | (_, ExpLine(opseq)) => of_steps_opseq(steps, ~side, opseq)
    | ([], EmptyLine | LetLine(_, _, _)) =>
      let place_cursor =
        switch (side) {
        | Before => ZExp.place_before_line
        | After => ZExp.place_after_line
        };
      Some(of_zline(place_cursor(line)));
    | ([_, ..._], EmptyLine) => None
    | ([x, ...xs], LetLine(p, ann, def)) =>
      switch (x) {
      | 0 =>
        p |> Pat.of_steps(xs, ~side) |> OptUtil.map(path => cons'(0, path))
      | 1 =>
        switch (ann) {
        | None => None
        | Some(ann) =>
          ann
          |> Typ.of_steps(xs, ~side)
          |> OptUtil.map(path => cons'(1, path))
        }
      | 2 =>
        def |> of_steps(xs, ~side) |> OptUtil.map(path => cons'(2, path))
      | _ => None
      }
    }
  and of_steps_opseq =
      (steps: steps, ~side: Side.t, opseq: UHExp.opseq): option(t) =>
    of_steps_opseq_(
      ~of_steps_operand,
      ~of_steps_operator,
      steps,
      ~side,
      opseq,
    )
  and of_steps_operator =
      (steps: steps, ~side: Side.t, operator: UHExp.operator): option(t) =>
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
      (steps: steps, ~side: Side.t, operand: UHExp.operand): option(t) =>
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
      | Var(_, _, _)
      | IntLit(_, _)
      | FloatLit(_, _)
      | BoolLit(_, _)
      | ListNil(_) => None
      | Parenthesized(body) =>
        switch (x) {
        | 0 =>
          body |> of_steps(xs, ~side) |> OptUtil.map(path => cons'(0, path))
        | _ => None
        }
      | Lam(_, p, ann, body) =>
        switch (x) {
        | 0 =>
          p |> Pat.of_steps(xs, ~side) |> OptUtil.map(path => cons'(0, path))
        | 1 =>
          switch (ann) {
          | None => None
          | Some(ann) =>
            ann
            |> Typ.of_steps(xs, ~side)
            |> OptUtil.map(path => cons'(1, path))
          }
        | 2 =>
          body |> of_steps(xs, ~side) |> OptUtil.map(path => cons'(2, path))
        | _ => None
        }
      | Inj(_, _, body) =>
        switch (x) {
        | 0 =>
          body |> of_steps(xs, ~side) |> OptUtil.map(path => cons'(2, path))
        | _ => None
        }
      | Case(_, scrut, rules) =>
        switch (x) {
        | 0 =>
          scrut |> of_steps(~side, xs) |> OptUtil.map(path => cons'(0, path))
        | _ =>
          switch (ZList.split_at(x - 1, rules)) {
          | None => None
          | Some(split_rules) =>
            let (_, z, _) = split_rules;
            z
            |> of_steps_rule(xs, ~side)
            |> OptUtil.map(path => cons'(x, path));
          }
        }
      | ApPalette(_, _, _, splice_info) =>
        let splice_map = splice_info.splice_map;
        switch (NatMap.drop(splice_map, x)) {
        | None => None
        | Some((_, ty_e)) =>
          let (_, e) = ty_e;
          e |> of_steps(xs, ~side) |> OptUtil.map(path => cons'(x, path));
        };
      }
    }
  and of_steps_rule =
      (steps: steps, ~side: Side.t, rule: UHExp.rule): option(t) =>
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
        p |> Pat.of_steps(~side, xs) |> OptUtil.map(path => cons'(0, path))
      | 1 =>
        clause |> of_steps(~side, xs) |> OptUtil.map(path => cons'(1, path))
      | _ => None
      };
    };

  let hole_desc = (u: MetaVar.t): hole_desc => ExpHole(u);
  let holes_err = holes_err(~hole_desc);
  let holes_case_err = holes_case_err(~hole_desc);
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
           ~is_space=Operators.Exp.is_Space,
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
        | Some(ann) => Typ.holes(ann, [1, ...rev_steps])
        }
      )
      |> Pat.holes(p, [0, ...rev_steps])
      |> holes_err(err, rev_steps)
    | Case(err, scrut, rules) =>
      hs
      |> ListUtil.fold_right_i(
           ((i, rule), hs) =>
             hs |> holes_rule(rule, [1 + i, ...rev_steps]),
           rules,
         )
      |> holes(scrut, [0, ...rev_steps])
      |> holes_case_err(err, rev_steps)
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
    holes_zopseq_(
      ~holes_operand,
      ~holes_zoperand,
      ~hole_desc,
      ~is_space=Operators.Exp.is_Space,
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
    | CursorE(_, IntLit(err, _))
    | CursorE(_, FloatLit(err, _))
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
    | CursorE(OnDelim(k, _), Case(err, scrut, rules)) =>
      let hole_selected =
        switch (err) {
        | StandardErrStatus(NotInHole) => None
        | StandardErrStatus(InHole(_, u))
        | InconsistentBranches(_, u) =>
          Some((ExpHole(u), rev_steps |> List.rev))
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
        mk_zholes(~holes_after=holes_scrut @ holes_rules, ~hole_selected, ())
      | 1 =>
        mk_zholes(
          ~holes_before=holes_scrut @ holes_rules,
          ~hole_selected,
          ~holes_after=[],
          (),
        )
      | _ => no_holes
      };
    | CursorE(OnText(_), Inj(_) | Parenthesized(_) | Lam(_) | Case(_)) =>
      /* invalid cursor position */
      no_holes
    | CursorE(_, ApPalette(_)) => no_holes /* TODO[livelits] */
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
    | CaseZE(err, zscrut, rules) =>
      let holes_err =
        switch (err) {
        | StandardErrStatus(NotInHole) => []
        | StandardErrStatus(InHole(_, u))
        | InconsistentBranches(_, u) => [
            (ExpHole(u), rev_steps |> List.rev),
          ]
        };
      let {holes_before, hole_selected, holes_after} =
        holes_z(zscrut, [0, ...rev_steps]);
      let holes_rules =
        ListUtil.fold_right_i(
          ((i, rule), hs) => hs |> holes_rule(rule, [1 + i, ...rev_steps]),
          rules,
          [],
        );
      mk_zholes(
        ~holes_before=holes_err @ holes_before,
        ~hole_selected,
        ~holes_after=holes_after @ holes_rules,
        (),
      );
    | CaseZR(err, scrut, (prefix, zrule, suffix)) =>
      let holes_err =
        switch (err) {
        | StandardErrStatus(NotInHole) => []
        | StandardErrStatus(InHole(_, u))
        | InconsistentBranches(_, u) => [
            (ExpHole(u), rev_steps |> List.rev),
          ]
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
      {
        holes_before: holes_err @ holes_scrut @ holes_prefix @ holes_before,
        hole_selected,
        holes_after: holes_after @ holes_suffix,
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
      let holes_p = Pat.holes(p, [0, ...rev_steps], []);
      let holes_clause = holes(clause, [1, ...rev_steps], []);
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
