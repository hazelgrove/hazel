open Sexplib.Std;

[@deriving sexp]
type uses_list = list(CursorPath.steps);

let rec binds_var = (x: Var.t, p: UHPat.t): bool => binds_var_opseq(x, p)
and binds_var_opseq = (x, OpSeq(_, seq): UHPat.opseq): bool =>
  seq |> Seq.operands |> List.exists(binds_var_operand(x))
and binds_var_operand = (x, operand: UHPat.operand): bool =>
  switch (operand) {
  | EmptyHole(_)
  | Wild(_)
  | Var(_, InVarHole(_), _, _)
  | Var(InHole(_), _, _, _)
  | NumLit(_)
  | BoolLit(_)
  | ListNil(_)
  | Inj(InHole(_), _, _) => false
  | Var(NotInHole, NotInVarHole, _, y) => x == y
  | Parenthesized(body) => binds_var(x, body)
  | Inj(NotInHole, _, body) => binds_var(x, body)
  };

let rec find_uses =
        (~steps: CursorPath.steps, x: Var.t, e: UHExp.t): uses_list =>
  find_uses_block(~steps, x, e)
and find_uses_block =
    (~offset=0, ~steps=[], x: Var.t, block: UHExp.block): uses_list => {
  let (uses, _) =
    block
    |> ListUtil.fold_left_i(
         ((uses_so_far, shadowed), (i, line)) =>
           if (shadowed) {
             (uses_so_far, shadowed);
           } else {
             let (line_uses, shadowed) =
               find_uses_line(~steps=steps @ [offset + i], x, line);
             (line_uses @ uses_so_far, shadowed);
           },
         ([], false),
       );
  uses;
}
and find_uses_line = (~steps, x: Var.t, line: UHExp.line): (uses_list, bool) =>
  switch (line) {
  | ExpLine(opseq) => (find_uses_opseq(~steps, x, opseq), false)
  | EmptyLine => ([], false)
  | LetLine(p, _, def) => (
      find_uses(~steps=steps @ [2], x, def),
      binds_var(x, p),
    )
  }
and find_uses_opseq =
    (~steps, x: Var.t, OpSeq(_, seq): UHExp.opseq): uses_list =>
  seq
  |> Seq.operands
  |> List.mapi((i, operand) =>
       find_uses_operand(~steps=steps @ [i], x, operand)
     )
  |> List.concat
and find_uses_operand = (~steps, x: Var.t, operand: UHExp.operand): uses_list =>
  switch (operand) {
  | EmptyHole(_)
  | Var(_, InVarHole(_), _)
  | NumLit(_)
  | BoolLit(_)
  | ListNil(_)
  | Lam(InHole(_), _, _, _)
  | Inj(InHole(_), _, _)
  | Case(InHole(_), _, _, _)
  | ApPalette(_) => []
  | Var(_, NotInVarHole, y) => x == y ? [steps] : []
  | Lam(NotInHole, p, _, body) =>
    binds_var(x, p) ? [] : find_uses(~steps=steps @ [2], x, body)
  | Inj(NotInHole, _, body) => find_uses(~steps=steps @ [0], x, body)
  | Case(NotInHole, scrut, rules, _) =>
    let scrut_uses = find_uses(~steps=steps @ [0], x, scrut);
    let rules_uses =
      rules
      |> List.mapi((i, rule) =>
           find_uses_rule(~steps=steps @ [1 + i], x, rule)
         )
      |> List.concat;
    scrut_uses @ rules_uses;
  | Parenthesized(body) => find_uses(~steps=steps @ [0], x, body)
  }
and find_uses_rule =
    (~steps, x: Var.t, Rule(p, clause): UHExp.rule): uses_list =>
  binds_var(x, p) ? [] : find_uses(~steps=steps @ [1], x, clause);

let rec ana_var = (p: UHPat.t, block: UHExp.block): UHPat.t =>
  ana_var_opseq(p, block)
and ana_var_opseq =
    (OpSeq(skel, seq): UHPat.opseq, block: UHExp.block): UHPat.opseq =>
  OpSeq(skel, ana_var_seq(seq, block))
and ana_var_seq = (seq: UHPat.seq, block: UHExp.block): UHPat.seq =>
  switch (seq) {
  | S(operand, E) => S(ana_var_operand(operand, block), E)
  | S(operand, A(op, seq)) =>
    S(ana_var_operand(operand, block), A(op, ana_var_seq(seq, block)))
  }
and ana_var_operand =
    (operand: UHPat.operand, block: UHExp.block): UHPat.operand =>
  switch (operand) {
  | EmptyHole(_)
  | Wild(_)
  | Var(_, InVarHole(_), _, _)
  | Var(InHole(_), _, _, _)
  | NumLit(_)
  | BoolLit(_)
  | ListNil(_)
  | Inj(InHole(_), _, _) => operand
  | Var(NotInHole, NotInVarHole, _, x) =>
    Var(
      NotInHole,
      NotInVarHole,
      if (UHExp.is_complete_block(block)) {
        switch (find_uses_block(x, block)) {
        | [] => VarWarnStatus.CritUnused
        | [_, ..._] => VarWarnStatus.NoWarning
        };
      } else {
        VarWarnStatus.NoWarning;
      },
      x,
    )
  | Parenthesized(p) => Parenthesized(ana_var(p, block))
  | Inj(NotInHole, side, p) => Inj(NotInHole, side, ana_var(p, block))
  };

let rec reduce = (f: ('b, 'a, list('a)) => 'b, acc: 'b, xs: list('a)): 'b => {
  switch (xs) {
  | [] => acc
  | [x, ...xs] => reduce(f, f(acc, x, xs), xs)
  };
};

let ana_var_block = (lines: UHExp.block): UHExp.block => {
  let rev_lines =
    reduce(
      (acc: UHExp.block, line: UHExp.line, left: UHExp.block) => {
        let analyzed_line: UHExp.line =
          switch (line) {
          | LetLine(p, ann, block) => LetLine(ana_var(p, left), ann, block)
          | _ => line
          };
        [analyzed_line, ...acc];
      },
      [],
      lines,
    );
  List.rev(rev_lines);
};

let ana_var_zblock = zblock => {
  let path = CursorPath.Exp.of_z(zblock);
  let block = ZExp.erase(zblock);
  let block = ana_var_block(block);
  let zblock = CursorPath.Exp.follow_or_fail(path, block);
  zblock;
};

let rec go_to_next_usage = (~found=false, cur_steps, uses) =>
  switch (found, uses) {
  | (_, []) => None
  | (false, [use, ...uses]) =>
    go_to_next_usage(~found=use == cur_steps, cur_steps, uses)
  | (true, [use, ..._]) => Some(use)
  };

let go_to_prev_usage = (cur_steps, uses) =>
  go_to_next_usage(cur_steps, List.rev(uses));
