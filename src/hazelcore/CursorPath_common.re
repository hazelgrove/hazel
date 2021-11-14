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

let mk_zhooks =
    (~hooks_before=[], ~hook_selected=None, ~hooks_after=[], ()): zhook_list => {
  hooks_before,
  hook_selected,
  hooks_after,
};
let no_hooks = mk_zhooks();

let prev_hook_steps = (zhook_list: zhook_list): option(steps) => {
  switch (
    List.rev(zhook_list.hooks_before),
    List.rev(zhook_list.hooks_after),
  ) {
  | ([], []) => None
  | ([hi, ..._], _)
  | ([], [hi, ..._]) => Some(get_steps(~to_fpos_for_aps=true, hi))
  };
};

let next_hook_steps = (zhook_list: zhook_list): option(steps) => {
  switch (zhook_list.hooks_before, zhook_list.hooks_after) {
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

let hooks_err =
    (
      ~hook: MetaVar.t => hook,
      err: ErrStatus.t,
      rev_steps: rev_steps,
      hs: hook_list,
    ) =>
  switch (err) {
  | NotInHole => hs
  | InHole(_, u) => [mk_hook(hook(u), List.rev(rev_steps)), ...hs]
  };

let hooks_verr =
    (
      ~hook: MetaVar.t => hook,
      verr: VarErrStatus.t,
      rev_steps: rev_steps,
      hs: hook_list,
    ) =>
  switch (verr) {
  | NotInVarHole => hs
  | InVarHole(_, u) => [mk_hook(hook(u), List.rev(rev_steps)), ...hs]
  };

let hooks_case_err =
    (
      ~hook: MetaVar.t => hook,
      err: CaseErrStatus.t,
      rev_steps: rev_steps,
      hs: hook_list,
    ) =>
  switch (err) {
  | StandardErrStatus(err) => hooks_err(~hook, err, rev_steps, hs)
  | InconsistentBranches(_, u) => [
      mk_hook(hook(u), List.rev(rev_steps)),
      ...hs,
    ]
  };

let hooks_skel_ =
    (
      ~hooks_operand: ('operand, steps, hook_list) => hook_list,
      ~hook: MetaVar.t => hook,
      ~is_space: 'operator => bool,
      ~rev_steps: rev_steps,
      skel: Skel.t('operator),
      seq: Seq.t('operand, 'operator),
      hs: hook_list,
    )
    : hook_list => {
  let rec go = (skel: Skel.t(_), hs) =>
    switch (skel) {
    | Placeholder(n) =>
      hs |> hooks_operand(seq |> Seq.nth_operand(n), [n, ...rev_steps])
    | BinOp(err, op, skel1, skel2) =>
      let hs = hs |> go(skel2);
      let hs =
        switch (err) {
        | NotInHole => hs
        | InHole(_, u) =>
          // If this skel is rooted at a Space, then we know
          // that all subskels are rooted at a Space.
          // We cannot place cursor on a Space, so make the
          // path to this skel hook the path to the first term
          // of the skel because that term determines how the
          // skel is typed. Make this hook come first before
          // any hooks found in subskels. But we need the actual
          // path as well for error hook decorations
          let step = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
          let steps = List.rev([step, ...rev_steps]);
          let ap_steps =
            is_space(op)
              ? List.rev([Skel.leftmost_tm_index(skel1), ...rev_steps])
              : steps;
          [mk_hook_ap(hook(u), steps, ~ap_steps), ...hs];
        };
      hs |> go(skel1);
    };
  go(skel, hs);
};

let hooks_opseq =
    (
      ~hooks_operand: ('operand, steps, hook_list) => hook_list,
      ~hook: MetaVar.t => hook,
      ~is_space: 'operator => bool,
      ~rev_steps: rev_steps,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
      hs: hook_list,
    )
    : hook_list =>
  hooks_skel_(~hooks_operand, ~hook, ~is_space, ~rev_steps, skel, seq, hs);

let hooks_zopseq_ =
    (
      ~hooks_operand: ('operand, rev_steps, hook_list) => hook_list,
      ~hooks_zoperand: ('zoperand, rev_steps) => zhook_list,
      ~hook: MetaVar.t => hook,
      ~is_space: 'operator => bool,
      ~rev_steps: rev_steps,
      ~erase_zopseq:
         ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
         OpSeq.t('operand, 'operator),
      ZOpSeq(skel, zseq) as zopseq:
        ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : zhook_list => {
  let OpSeq(_, seq) = zopseq |> erase_zopseq;
  let hooks_skel = skel =>
    hooks_skel_(~hooks_operand, ~hook, ~is_space, ~rev_steps, skel, seq, []);
  switch (zseq) {
  | ZOperator(_, (prefix, _)) =>
    let preceding_operand_index = Seq.length(prefix) - 1;
    let rec go: Skel.t(_) => zhook_list = (
      fun
      | Placeholder(_) =>
        // We defer to hooks_skel once we have determined that a skel
        // does not contain the cursor, should never hit this case.
        failwith("hooks_zopseq/ZOperator: unexpected Placeholder")
      | BinOp(err, op, skel1, skel2) => {
          // We defer to hooks_skel once we have determined that a skel
          // does not contain the cursor. Since Space has highest precedence
          // and we cannot place cursor on a Space, we know that the entirety
          // of a skel rooted at Space cannot contain the cursor. Should
          // have deferred to hooks_skel before hitting this case.
          assert(!is_space(op));
          let n = skel1 |> Skel.rightmost_tm_index;
          let binop_hook =
            switch (err) {
            | NotInHole => None
            | InHole(_, u) =>
              let step = n + Seq.length(seq);
              let steps = List.rev([step, ...rev_steps]);
              Some(mk_hook(hook(u), steps));
            };
          if (n == preceding_operand_index) {
            mk_zhooks(
              ~hooks_before=hooks_skel(skel1),
              ~hook_selected=binop_hook,
              ~hooks_after=hooks_skel(skel2),
              (),
            );
          } else {
            let binop_hooks = Option.to_list(binop_hook);
            if (n < preceding_operand_index) {
              let hooks1 = hooks_skel(skel1);
              let zhooks2 = go(skel2);
              mk_zhooks(
                ~hooks_before=hooks1 @ binop_hooks @ zhooks2.hooks_before,
                ~hook_selected=zhooks2.hook_selected,
                ~hooks_after=zhooks2.hooks_after,
                (),
              );
            } else {
              let zhooks1 = go(skel1);
              let hooks2 = hooks_skel(skel2);
              mk_zhooks(
                ~hooks_before=zhooks1.hooks_before,
                ~hook_selected=zhooks1.hook_selected,
                ~hooks_after=zhooks1.hooks_after @ binop_hooks @ hooks2,
                (),
              );
            };
          };
        }
    );
    go(skel);
  | ZOperand(zoperand, (prefix, _)) =>
    let zoperand_index = Seq.length_of_affix(prefix);
    let rec go: Skel.t(_) => zhook_list = (
      fun
      | Placeholder(n) => {
          // We defer to hooks_skel once we have determined that a skel
          // does not contain the cursor, should never hit Placeholder
          // corresponding to operand other than zoperand.
          assert(n == zoperand_index);
          hooks_zoperand(zoperand, [zoperand_index, ...rev_steps]);
        }
      | BinOp(err, op, skel1, skel2) when op |> is_space => {
          // If this skel is rooted at a Space, then we know
          // that all subskels are rooted at a Space.
          // We cannot place cursor on a Space, so make the
          // path to this skel hook the path to the first term
          // of the skel because that term determines how the
          // skel is typed. Make this hook come first before
          // any hooks found in subskels.
          let binop_hooks =
            switch (err) {
            | NotInHole => []
            | InHole(_, u) =>
              let step = Skel.rightmost_tm_index(skel1) + Seq.length(seq);
              let steps = List.rev([step, ...rev_steps]);
              let ap_steps =
                is_space(op)
                  ? List.rev([Skel.leftmost_tm_index(skel1), ...rev_steps])
                  : steps;
              [mk_hook_ap(hook(u), steps, ~ap_steps)];
            };
          if (zoperand_index <= Skel.rightmost_tm_index(skel1)) {
            let zhooks1 = go(skel1);
            let hooks2 = hooks_skel(skel2);
            mk_zhooks(
              ~hooks_before=binop_hooks @ zhooks1.hooks_before,
              ~hook_selected=zhooks1.hook_selected,
              ~hooks_after=zhooks1.hooks_after @ hooks2,
              (),
            );
          } else {
            let hooks1 = hooks_skel(skel1);
            let zhooks2 = go(skel2);
            mk_zhooks(
              ~hooks_before=binop_hooks @ hooks1 @ zhooks2.hooks_before,
              ~hook_selected=zhooks2.hook_selected,
              ~hooks_after=zhooks2.hooks_after,
              (),
            );
          };
        }
      | BinOp(err, _op, skel1, skel2) => {
          let n = skel1 |> Skel.rightmost_tm_index;
          let binop_hooks =
            switch (err) {
            | NotInHole => []
            | InHole(_, u) =>
              let step = n + Seq.length(seq);
              let steps = List.rev([step, ...rev_steps]);
              [mk_hook(hook(u), steps)];
            };
          if (zoperand_index <= n) {
            let zhooks1 = go(skel1);
            let hooks2 = hooks_skel(skel2);
            mk_zhooks(
              ~hooks_before=zhooks1.hooks_before,
              ~hook_selected=zhooks1.hook_selected,
              ~hooks_after=zhooks1.hooks_after @ binop_hooks @ hooks2,
              (),
            );
          } else {
            let hooks1 = hooks_skel(skel1);
            let zhooks2 = go(skel2);
            mk_zhooks(
              ~hooks_before=hooks1 @ binop_hooks @ zhooks2.hooks_before,
              ~hook_selected=zhooks2.hook_selected,
              ~hooks_after=zhooks2.hooks_after,
              (),
            );
          };
        }
    );
    go(skel);
  };
};

let steps_to_hook = (hook_list: hook_list, u: MetaVar.t): option(steps) =>
  switch (
    List.find_opt(
      hook_info =>
        switch (CursorPath.get_hook(hook_info)) {
        | ExpHole(u', _)
        | PatHole(u', _) => MetaVar.eq(u, u')
        | KeywordHook(_)
        | TypHole => false
        },
      hook_list,
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
