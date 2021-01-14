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
  | InvalidText(_)
  | Var(_, InVarHole(_), _)
  | Var(InHole(_), _, _)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil(_)
  | Inj(InHole(_), _, _) => false
  | Var(NotInHole, NotInVarHole, y) => x == y
  | Parenthesized(body) => binds_var(x, body)
  | Inj(NotInHole, _, body) => binds_var(x, body)
  };

let rec find_uses =
        (~steps: CursorPath.steps, x: Var.t, e: UHExp.t): uses_list =>
  find_uses_block(~steps, x, e)
and find_uses_block =
    (~offset=0, ~steps, x: Var.t, block: UHExp.block): uses_list => {
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
  | CommentLine(_) => ([], false)
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
  | InvalidText(_)
  | Var(_, InVarHole(_), _)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil(_)
  | Lam(InHole(_), _, _, _)
  | Inj(InHole(_), _, _)
  | Case(StandardErrStatus(InHole(_)), _, _)
  | If(StandardErrStatus(InHole(_)), _, _, _)
  | ApPalette(_) => []
  | Var(_, NotInVarHole, y) => x == y ? [steps] : []
  | Lam(NotInHole, p, _, body) =>
    binds_var(x, p) ? [] : find_uses(~steps=steps @ [2], x, body)
  | Inj(NotInHole, _, body) => find_uses(~steps=steps @ [0], x, body)
  | Case(
      StandardErrStatus(NotInHole) | InconsistentBranches(_),
      scrut,
      rules,
    ) =>
    let scrut_uses = find_uses(~steps=steps @ [0], x, scrut);
    let rules_uses =
      rules
      |> List.mapi((i, rule) =>
           find_uses_rule(~steps=steps @ [1 + i], x, rule)
         )
      |> List.concat;
    scrut_uses @ rules_uses;
  | If(StandardErrStatus(NotInHole) | InconsistentBranches(_), t1, t2, t3) =>
    let t1_rules = find_uses(~steps=steps @ [0], x, t1);
    let t2_rules = find_uses(~steps=steps @ [1], x, t2);
    let t3_rules = find_uses(~steps=steps @ [2], x, t3);
    t1_rules @ t2_rules @ t3_rules;
  | Parenthesized(body) => find_uses(~steps=steps @ [0], x, body)
  }
and find_uses_rule =
    (~steps, x: Var.t, Rule(p, clause): UHExp.rule): uses_list =>
  binds_var(x, p) ? [] : find_uses(~steps=steps @ [1], x, clause);
