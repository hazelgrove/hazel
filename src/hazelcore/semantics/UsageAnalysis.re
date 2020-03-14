open Sexplib.Std;
open GeneralUtil;

[@deriving sexp]
type uses_list = list(CursorPath.steps);

let rec binds_var = (x: Var.t, p: UHPat.t): bool =>
  switch (p) {
  | EmptyHole(_)
  | Wild(_)
  | Var(_, InVarHole(_, _), _, _)
  | Var(InHole(_, _), _, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_)
  | Inj(InHole(_, _), _, _) => false
  | Var(NotInHole, NotInVarHole, _, y) => x == y
  | Parenthesized(e) => binds_var(x, e)
  | OpSeq(_, ExpOpExp(e1, _, e2)) => binds_var(x, e1) || binds_var(x, e2)
  | OpSeq(skel, SeqOpExp(seq1, _, e1)) =>
    binds_var(x, e1) || binds_var(x, OpSeq(skel, seq1))
  | Inj(NotInHole, _, e) => binds_var(x, e)
  };

let rec find_uses_block =
        (x: Var.t, ~index=0, ~steps=[], block: UHExp.block): uses_list => {
  let UHExp.Block(lines, e) = block;
  let (uses, shadowed) = find_uses_lines(x, lines, steps, index, false);
  shadowed
    ? uses
    : uses @ find_uses_exp(x, e, steps @ [index + List.length(lines)]);
}
and find_uses_lines =
    (x: Var.t, lines: UHExp.lines, steps, index, shadowed): (uses_list, bool) =>
  if (shadowed) {
    ([], shadowed);
  } else {
    switch (lines) {
    | [] => ([], false)
    | [line, ...lines] =>
      let (line_uses, shadowed) = find_uses_line(x, line, steps @ [index]);
      let (lines_uses, shadowed) =
        find_uses_lines(x, lines, steps, index + 1, shadowed);
      (line_uses @ lines_uses, shadowed);
    };
  }
and find_uses_line = (x: Var.t, line: UHExp.line, steps): (uses_list, bool) =>
  switch (line) {
  | ExpLine(e) => (find_uses_exp(x, e, steps), false)
  | EmptyLine => ([], false)
  | LetLine(p, _, block) => (
      find_uses_block(x, block, ~steps=steps @ [2], ~index=0),
      binds_var(x, p),
    )
  }
and find_uses_exp = (x: Var.t, e: UHExp.t, steps): uses_list =>
  switch (e) {
  | EmptyHole(_)
  | Var(_, InVarHole(_, _), _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_)
  | Lam(InHole(_, _), _, _, _)
  | Inj(InHole(_, _), _, _)
  | Case(InHole(_, _), _, _, _)
  | ApPalette(_, _, _, _) => []
  | Var(_, NotInVarHole, y) => x == y ? [steps] : []
  | Lam(NotInHole, p, _, block) =>
    binds_var(x, p)
      ? [] : find_uses_block(x, block, ~steps=steps @ [2], ~index=0)
  | Inj(NotInHole, _, block) =>
    find_uses_block(x, block, ~steps=steps @ [0], ~index=0)
  | Case(NotInHole, block, rules, _) =>
    find_uses_block(x, block, ~steps=steps @ [0], ~index=0)
    @ find_uses_rules(x, rules, steps, 1)
  | Parenthesized(block) =>
    find_uses_block(x, block, ~steps=steps @ [0], ~index=0)
  | OpSeq(_, opseq) =>
    let (uses, _) = find_uses_opseq(x, opseq, steps);
    uses;
  }
and find_uses_rules =
    (x: Var.t, rules: UHExp.rules, steps, prefix_length): uses_list =>
  switch (rules) {
  | [] => []
  | [rule, ...rules] =>
    let UHExp.Rule(p, block) = rule;
    let uses =
      binds_var(x, p)
        ? []
        : find_uses_block(
            x,
            block,
            ~steps=steps @ [prefix_length, 1],
            ~index=0,
          );
    uses @ find_uses_rules(x, rules, steps, prefix_length + 1);
  }
and find_uses_opseq = (x: Var.t, opseq: UHExp.opseq, steps): (uses_list, int) =>
  switch (opseq) {
  | ExpOpExp(e1, _, e2) => (
      find_uses_exp(x, e1, steps @ [0]) @ find_uses_exp(x, e2, steps @ [1]),
      2,
    )
  | SeqOpExp(seq, _, e) =>
    let (uses, length) = find_uses_opseq(x, seq, steps);
    (uses @ find_uses_exp(x, e, steps @ [length]), length + 1);
  };

type var_warns_deferrable('a) = ListDeferrable.t(VarWarnStatus.t, 'a, Var.t);
include ListDeferrable;

let rec reduce = (f: ('b, 'a, list('a)) => 'b, acc: 'b, xs: list('a)): 'b => {
  switch (xs) {
  | [] => acc
  | [x, ...xs] => reduce(f, f(acc, x, xs), xs)
  };
};

let rec ana_var_pat = (p: UHPat.t, block: UHExp.block): UHPat.t =>
  switch (p) {
  | Var(err, NotInVarHole, _, x) =>
    Var(
      err,
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
  | OpSeq(skel, opseq) => OpSeq(skel, ana_var_pat_seq(opseq, block))
  | _ => p
  }
and ana_var_pat_seq = (seq: UHPat.opseq, block: UHExp.block): UHPat.opseq =>
  switch (seq) {
  | ExpOpExp(tm1, op, tm2) =>
    ExpOpExp(ana_var_pat(tm1, block), op, ana_var_pat(tm2, block))
  | SeqOpExp(seq, op, tm) =>
    SeqOpExp(ana_var_pat_seq(seq, block), op, ana_var_pat(tm, block))
  };

let ana_var_block = (Block(lines, e): UHExp.block): UHExp.block => {
  let rev_lines =
    reduce(
      (acc_lines: UHExp.lines, line: UHExp.line, lines: UHExp.lines) => {
        let analyzed_line =
          switch (line) {
          | LetLine(p, ann, block) =>
            UHExp.LetLine(ana_var_pat(p, Block(lines, e)), ann, block)
          | _ => line
          };
        [analyzed_line, ...acc_lines];
      },
      [],
      lines,
    );
  Block(List.rev(rev_lines), e);
};

let ana_var_zblock = zblock => {
  let path = CursorPath.of_zblock(zblock);
  let block = ZExp.erase_block(zblock);
  let block = ana_var_block(block);
  let zblock = CursorPath.follow_block_or_fail(path, block);
  zblock;
};
