open CursorPath;

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

let mk_zholes =
    (~holes_before=[], ~hole_selected=None, ~holes_after=[], ()): zhole_list => {
  holes_before,
  hole_selected,
  holes_after,
};
let no_holes = mk_zholes();

let prev_hole_steps = (zhole_list: zhole_list): option(steps) => {
  switch (
    List.rev(zhole_list.holes_before),
    List.rev(zhole_list.holes_after),
  ) {
  | ([], []) => None
  | ([hi, ..._], _)
  | ([], [hi, ..._]) => Some(get_steps(~to_fpos_for_aps=true, hi))
  };
};

let next_hole_steps = (zhole_list: zhole_list): option(steps) => {
  switch (zhole_list.holes_before, zhole_list.holes_after) {
  | ([], []) => None
  | (_, [hi, ..._])
  | ([hi, ..._], _) => Some(get_steps(~to_fpos_for_aps=true, hi))
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
      Seq.opt_split_nth_operand(x, seq),
      Seq.opt_split_nth_operator(x - Seq.length(seq), seq),
    ) {
    | (None, None) => None
    | (Some((operand, surround)), _) =>
      operand
      |> follow_operand((xs, cursor))
      |> Option.map(zoperand =>
           ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, surround))
         )
    | (_, Some((operator, surround))) =>
      operator
      |> follow_operator((xs, cursor))
      |> Option.map(zoperator =>
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
      Seq.opt_split_nth_operand(x, seq),
      Seq.opt_split_nth_operator(x - Seq.length(seq), seq),
    ) {
    | (None, None) => None
    | (Some((operand, _)), _) =>
      let path = operand |> of_steps_operand(xs, ~side);
      path |> Option.map(path => cons'(x, path));
    | (_, Some((operator, _))) =>
      operator
      |> of_steps_operator(xs, ~side)
      |> Option.map(path => cons'(x, path))
    }
  };

let holes_err =
    (
      ~hole_sort: MetaVar.t => hole_sort,
      err: ErrStatus.t,
      rev_steps: rev_steps,
      hs: hole_list,
    ) =>
  switch (err) {
  | NotInHole => hs
  | InHole(_, u) => [
      mk_hole_sort(hole_sort(u), List.rev(rev_steps)),
      ...hs,
    ]
  };

let holes_verr =
    (
      ~hole_sort: MetaVar.t => hole_sort,
      verr: VarErrStatus.t,
      rev_steps: rev_steps,
      hs: hole_list,
    ) =>
  switch (verr) {
  | NotInVarHole => hs
  | InVarHole(_, u) => [
      mk_hole_sort(hole_sort(u), List.rev(rev_steps)),
      ...hs,
    ]
  };

let holes_case_err =
    (
      ~hole_sort: MetaVar.t => hole_sort,
      err: CaseErrStatus.t,
      rev_steps: rev_steps,
      hs: hole_list,
    ) =>
  switch (err) {
  | StandardErrStatus(err) => holes_err(~hole_sort, err, rev_steps, hs)
  | InconsistentBranches(_, u) => [
      mk_hole_sort(hole_sort(u), List.rev(rev_steps)),
      ...hs,
    ]
  };

let holes_skel_ =
    (
      ~holes_operand: ('operand, steps, hole_list) => hole_list,
      ~hole_sort: MetaVar.t => hole_sort,
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
    | BinOp(err, op, skel1, skel2) =>
      let hs = hs |> go(skel2);
      let hs =
        switch (err) {
        | NotInHole => hs
        | InHole(_, u) =>
          // If this skel is rooted at a Space, then we know
          // that all subskels are rooted at a Space.
          // We cannot place cursor on a Space, so make the
          // path to this skel hole the path to the first term
          // of the skel because that term determines how the
          // skel is typed. Make this hole come first before
          // any holes found in subskels. But we need the actual
          // path as well for error hole decorations
          let step = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
          let steps = List.rev([step, ...rev_steps]);
          let ap_steps =
            is_space(op)
              ? List.rev([Skel.leftmost_tm_index(skel1), ...rev_steps])
              : steps;
          [mk_hole_sort_ap(hole_sort(u), steps, ~ap_steps), ...hs];
        };
      hs |> go(skel1);
    };
  go(skel, hs);
};

let holes_opseq =
    (
      ~holes_operand: ('operand, steps, hole_list) => hole_list,
      ~hole_sort: MetaVar.t => hole_sort,
      ~is_space: 'operator => bool,
      ~rev_steps: rev_steps,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
      hs: hole_list,
    )
    : hole_list =>
  holes_skel_(
    ~holes_operand,
    ~hole_sort,
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
      ~hole_sort: MetaVar.t => hole_sort,
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
      ~hole_sort,
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
              let steps = List.rev([step, ...rev_steps]);
              Some(mk_hole_sort(hole_sort(u), steps));
            };
          if (n == preceding_operand_index) {
            mk_zholes(
              ~holes_before=holes_skel(skel1),
              ~hole_selected=binop_hole,
              ~holes_after=holes_skel(skel2),
              (),
            );
          } else {
            let binop_holes = Option.to_list(binop_hole);
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
              let step = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
              let steps = List.rev([step, ...rev_steps]);
              let ap_steps =
                is_space(op)
                  ? List.rev([Skel.leftmost_tm_index(skel1), ...rev_steps])
                  : steps;
              [mk_hole_sort_ap(hole_sort(u), steps, ~ap_steps)];
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
      | BinOp(err, _op, skel1, skel2) => {
          let n = skel1 |> Skel.rightmost_tm_index;
          let binop_holes =
            switch (err) {
            | NotInHole => []
            | InHole(_, u) =>
              let step = n + Seq.length(seq);
              let steps = List.rev([step, ...rev_steps]);
              [mk_hole_sort(hole_sort(u), steps)];
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

let steps_to_hole = (hole_list: hole_list, u: MetaVar.t): option(steps) =>
  switch (
    List.find_opt(
      hole_info =>
        switch (CursorPath.get_sort(hole_info)) {
        | ExpHole(u', _)
        | PatHole(u', _) => MetaVar.eq(u, u')
        | Assert(_, _)
        | TypHole => false
        },
      hole_list,
    )
  ) {
  | None => None
  | Some(hi) => Some(get_steps(~to_fpos_for_aps=true, hi))
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
