open Sexplib.Std;
open GeneralUtil;

[@deriving sexp]
type op =
  | Space
  | Plus
  | Minus
  | Times
  | LessThan
  | GreaterThan
  | Equals
  | Comma
  | Cons
  | And
  | Or;

let is_Space =
  fun
  | Space => true
  | _ => false;

[@deriving sexp]
type skel_t = Skel.t(op);

[@deriving sexp]
type block =
  | Block(lines, t)
and lines = list(line)
and line =
  | ExpLine(t)
  | EmptyLine
  | LetLine(UHPat.t, option(UHTyp.t), block)
and t =
  /* outer nodes */
  | EmptyHole(MetaVar.t)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | NumLit(ErrStatus.t, int)
  | BoolLit(ErrStatus.t, bool)
  | StringLit(ErrStatus.t, string)
  | ListNil(ErrStatus.t)
  /* inner nodes */
  | Lam(ErrStatus.t, UHPat.t, option(UHTyp.t), block)
  | Inj(ErrStatus.t, InjSide.t, block)
  | Case(ErrStatus.t, block, rules, option(UHTyp.t))
  | Parenthesized(block)
  | OpSeq(skel_t, opseq) /* invariant: skeleton is consistent with opseq */
  | ApPalette(ErrStatus.t, PaletteName.t, SerializedModel.t, splice_info)
and opseq = OperatorSeq.opseq(t, op)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, block)
and splice_info = SpliceInfo.t(block)
and splice_map = SpliceInfo.splice_map(block);

exception SkelInconsistentWithOpSeq(skel_t, opseq);

let letline = (p: UHPat.t, ~ann: option(UHTyp.t)=?, block: block): line =>
  LetLine(p, ann, block);

let var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      x: Var.t,
    )
    : t =>
  Var(err, var_err, x);

let numlit = (~err: ErrStatus.t=NotInHole, n: int): t => NumLit(err, n);

let lam =
    (
      ~err: ErrStatus.t=NotInHole,
      p: UHPat.t,
      ~ann: option(UHTyp.t)=?,
      body: block,
    )
    : t =>
  Lam(err, p, ann, body);

let case =
    (
      ~err: ErrStatus.t=NotInHole,
      ~ann: option(UHTyp.t)=?,
      scrut: block,
      rules: rules,
    )
    : t =>
  Case(err, scrut, rules, ann);

let listnil = (~err: ErrStatus.t=NotInHole, ()): t => ListNil(err);

let wrap_in_block = (e: t): block => Block([], e);

let prune_empty_hole_line = (li: line): line =>
  switch (li) {
  | ExpLine(EmptyHole(_)) => EmptyLine
  | ExpLine(_)
  | EmptyLine
  | LetLine(_, _, _) => li
  };
let prune_empty_hole_lines: lines => lines = List.map(prune_empty_hole_line);

let rec get_tuple = (skel1: skel_t, skel2: skel_t): ListMinTwo.t(skel_t) =>
  switch (skel2) {
  | BinOp(_, Comma, skel21, skel22) =>
    Cons(skel1, get_tuple(skel21, skel22))
  | BinOp(_, _, _, _)
  | Placeholder(_) => Pair(skel1, skel2)
  };

let rec make_tuple = (err: ErrStatus.t, skels: ListMinTwo.t(skel_t)): skel_t =>
  switch (skels) {
  | Pair(skel1, skel2) => BinOp(err, Comma, skel1, skel2)
  | Cons(skel1, skels) =>
    BinOp(err, Comma, skel1, make_tuple(NotInHole, skels))
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (EmptyHole(u), u_gen);
};

let is_EmptyHole = (e: t): bool =>
  switch (e) {
  | EmptyHole(_) => true
  | _ => false
  };

let empty_rule = (u_gen: MetaVarGen.t): (rule, MetaVarGen.t) => {
  let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
  let (e, u_gen) = new_EmptyHole(u_gen);
  let block = wrap_in_block(e);
  let rule = Rule(p, block);
  (rule, u_gen);
};

let prepend_leading_line = (line, Block(lines, e)) =>
  Block([line, ...lines], e);

let append_concluding_exp = (new_conclusion, Block(lines, conclusion)) =>
  Block(lines @ [ExpLine(conclusion)], new_conclusion);

let block_to_lines = (Block(lines, e)) => lines @ [ExpLine(e)];

/**
 * Bidelimited expressions are those that do not need to
 * be wrapped in parentheses in an opseq. In most cases,
 * this means those expressions that don't have subexpressions
 * at the outer left/right edges in the concrete syntax.
 * In the ostensibly bidelimited case of case...end expressions,
 * however, we still require explicit parenthesization in an
 * opseq. This is because, in our edit actions, we require that
 * let and case expressions be constructed only at the beginning
 * of a line or parenthesized expression -- hence, constructing
 * a case expression in the middle of an opseq requires first
 * constructing parentheses around the desired scrutinee within
 * the opseq. For consistency, we require that case expressions
 * always be parenthesized in an opseq.
 */
let bidelimited = (e: t): bool =>
  switch (e) {
  /* bidelimited */
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | StringLit(_, _)
  | ListNil(_)
  | Inj(_, _, _)
  | ApPalette(_, _, _, _)
  | Parenthesized(_) => true
  /* non-bidelimited */
  | Case(_, _, _, _)
  | Lam(_, _, _, _)
  | OpSeq(_, _) => false
  };

/* if e is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = (e: t): t =>
  if (bidelimited(e)) {
    e;
  } else {
    Parenthesized(wrap_in_block(e));
  };

let rec get_err_status_block = (Block(_, e): block): ErrStatus.t =>
  get_err_status_t(e)
and get_err_status_t = (e: t): ErrStatus.t =>
  switch (e) {
  | EmptyHole(_) => NotInHole
  | Var(err, _, _)
  | NumLit(err, _)
  | BoolLit(err, _)
  | StringLit(err, _)
  | ListNil(err)
  | Lam(err, _, _, _)
  | Inj(err, _, _)
  | Case(err, _, _, _)
  | ApPalette(err, _, _, _) => err
  | Parenthesized(block) => get_err_status_block(block)
  | OpSeq(BinOp(err, _, _, _), _) => err
  | OpSeq(Placeholder(n) as skel, seq) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(e_n) => get_err_status_t(e_n)
    }
  };

let rec set_err_status_block =
        (err: ErrStatus.t, Block(lines, e): block): block =>
  Block(lines, set_err_status_t(err, e))
/* put e in the specified hole */
and set_err_status_t = (err: ErrStatus.t, e: t): t =>
  switch (e) {
  | EmptyHole(_) => e
  | Var(_, var_err, x) => Var(err, var_err, x)
  | NumLit(_, n) => NumLit(err, n)
  | BoolLit(_, b) => BoolLit(err, b)
  | StringLit(_, s) => StringLit(err, s)
  | ListNil(_) => ListNil(err)
  | Lam(_, p, ann, block) => Lam(err, p, ann, block)
  | Inj(_, inj_side, block) => Inj(err, inj_side, block)
  | Case(_, block, rules, ann) => Case(err, block, rules, ann)
  | ApPalette(_, name, model, si) => ApPalette(err, name, model, si)
  | Parenthesized(block) => Parenthesized(set_err_status_block(err, block))
  | OpSeq(skel, seq) =>
    let (skel, seq) = set_err_status_opseq(err, skel, seq);
    OpSeq(skel, seq);
  }
and set_err_status_opseq =
    (err: ErrStatus.t, skel: skel_t, seq: opseq): (skel_t, opseq) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(en) =>
      let en = set_err_status_t(err, en);
      switch (OperatorSeq.seq_update_nth(n, seq, en)) {
      | None => raise(SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq)
      };
    }
  | BinOp(_, op, skel1, skel2) => (BinOp(err, op, skel1, skel2), seq)
  };

let is_inconsistent = e =>
  switch (e |> get_err_status_t) {
  | InHole(TypeInconsistent, _) => true
  | _ => false
  };

let rec make_block_inconsistent =
        (u_gen: MetaVarGen.t, block: block): (block, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  let block = set_err_status_block(InHole(TypeInconsistent, u), block);
  (block, u_gen);
}
/* put e in a new hole, if it is not already in a hole */
and make_t_inconsistent = (u_gen: MetaVarGen.t, e: t): (t, MetaVarGen.t) =>
  switch (e) {
  /* already in hole */
  | EmptyHole(_)
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | StringLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Lam(InHole(TypeInconsistent, _), _, _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(InHole(TypeInconsistent, _), _, _, _)
  | ApPalette(InHole(TypeInconsistent, _), _, _, _) => (e, u_gen)
  /* not in hole */
  | Var(NotInHole | InHole(WrongLength, _), _, _)
  | NumLit(NotInHole | InHole(WrongLength, _), _)
  | BoolLit(NotInHole | InHole(WrongLength, _), _)
  | StringLit(NotInHole | InHole(WrongLength, _), _)
  | ListNil(NotInHole | InHole(WrongLength, _))
  | Lam(NotInHole | InHole(WrongLength, _), _, _, _)
  | Inj(NotInHole | InHole(WrongLength, _), _, _)
  | Case(NotInHole | InHole(WrongLength, _), _, _, _)
  | ApPalette(NotInHole | InHole(WrongLength, _), _, _, _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let e = set_err_status_t(InHole(TypeInconsistent, u), e);
    (e, u_gen);
  /* err in constructor args */
  | Parenthesized(block) =>
    let (block, u_gen) = make_block_inconsistent(u_gen, block);
    (Parenthesized(block), u_gen);
  | OpSeq(skel, seq) =>
    let (skel, seq, u_gen) = make_opseq_inconsistent(u_gen, skel, seq);
    (OpSeq(skel, seq), u_gen);
  }
/* put skel in a new hole, if it is not already in a hole */
and make_opseq_inconsistent =
    (u_gen: MetaVarGen.t, skel: skel_t, seq: opseq)
    : (skel_t, opseq, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(en) =>
      let (en, u_gen) = make_t_inconsistent(u_gen, en);
      switch (OperatorSeq.seq_update_nth(n, seq, en)) {
      | None => raise(SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq, u_gen)
      };
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) => (skel, seq, u_gen)
  | BinOp(NotInHole, op, skel1, skel2)
  | BinOp(InHole(WrongLength, _), op, skel1, skel2) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (BinOp(InHole(TypeInconsistent, u), op, skel1, skel2), seq, u_gen);
  };

let rec drop_outer_parentheses = (e: t): block =>
  switch (e) {
  | Parenthesized(Block([], e)) => drop_outer_parentheses(e)
  | Parenthesized(block) => block
  | _ => Block([], e)
  };

let child_indices_line =
  fun
  | EmptyLine => []
  | ExpLine(_) => []
  | LetLine(_, None, _) => [0, 2]
  | LetLine(_, Some(_), _) => [0, 1, 2];
let child_indices_exp =
  fun
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | StringLit(_, _)
  | ListNil(_) => []
  | Lam(_, _, None, _) => [0, 2]
  | Lam(_, _, Some(_), _) => [0, 1, 2]
  | Case(_, _, rules, None) => range(List.length(rules) + 1)
  | Case(_, _, rules, Some(_)) => range(List.length(rules) + 2)
  | Inj(_, _, _) => [0]
  | Parenthesized(_) => [0]
  | OpSeq(_, seq) => range(OperatorSeq.seq_length(seq))
  | ApPalette(_, _, _, _) => [];
let child_indices_rule =
  fun
  | Rule(_, _) => [0, 1];

let num_lines_in_block = (Block(leading, _)) => List.length(leading) + 1;

[@deriving sexp]
type exp_or_block =
  | E(t)
  | B(block);

let rec first_contiguous_empty_lines = lines =>
  switch (lines) {
  | [] => ([], [])
  | [EmptyLine, ...rest] =>
    let (empty_lines, rest) = first_contiguous_empty_lines(rest);
    ([EmptyLine, ...empty_lines], rest);
  | [_, ..._] => ([], lines)
  };

let last_contiguous_empty_lines = lines => {
  let (empty_lines, rev_prefix) =
    lines |> List.rev |> first_contiguous_empty_lines;
  (List.rev(rev_prefix), empty_lines);
};

let first_line_and_trailing_contiguous_empty_lines = lines =>
  switch (lines) {
  | [] => None
  | [line, ...rest] =>
    let (empty_lines, rest) = first_contiguous_empty_lines(rest);
    Some((line, empty_lines, rest));
  };

let rec first_contiguous_empty_lines_and_nonempty_line =
        (lines): (lines, option(line), lines) =>
  switch (lines) {
  | [] => ([], None, [])
  | [EmptyLine, ...lines] =>
    let (empty_lines, nonempty_line, rest) =
      first_contiguous_empty_lines_and_nonempty_line(lines);
    ([EmptyLine, ...empty_lines], nonempty_line, rest);
  | [nonempty_line, ...rest] => ([], Some(nonempty_line), rest)
  };

let last_nonempty_line_and_trailing_contiguous_empty_lines = lines => {
  List.fold_right(
    (line, (prefix, nonempty_line, empty_lines)) =>
      switch (line, nonempty_line) {
      | (EmptyLine, None) => ([], None, [EmptyLine, ...empty_lines])
      | (_, None) => ([], Some(line), empty_lines)
      | (_, Some(_)) => ([line, ...prefix], nonempty_line, empty_lines)
      },
    lines,
    ([], None, []),
  );
};

let last_line_and_leading_contiguous_empty_lines = lines =>
  switch (lines |> split_last) {
  | None => None
  | Some((prefix, last)) =>
    let (prefix, empty_lines) = last_contiguous_empty_lines(prefix);
    Some((prefix, empty_lines, last));
  };

let shift_line_to_prefix =
    (~u_gen: MetaVarGen.t, prefix: lines, Block(leading, conclusion))
    : option((lines, block, MetaVarGen.t)) =>
  switch (
    leading |> first_line_and_trailing_contiguous_empty_lines,
    conclusion,
  ) {
  | (None, EmptyHole(_)) => None
  | (None, _) =>
    let (hole, u_gen) = u_gen |> new_EmptyHole;
    Some((prefix @ [ExpLine(conclusion)], hole |> wrap_in_block, u_gen));
  | (Some((next_line, empty_lines, leading_rest)), _) =>
    Some((
      prefix @ [next_line, ...empty_lines],
      Block(leading_rest, conclusion),
      u_gen,
    ))
  };

let shift_line_from_prefix =
    (~u_gen: MetaVarGen.t, prefix: lines, Block(leading, conclusion))
    : option((lines, block, MetaVarGen.t)) =>
  switch (prefix |> last_nonempty_line_and_trailing_contiguous_empty_lines) {
  | (_, None, empty_lines) =>
    Some(([], Block(empty_lines @ leading, conclusion), u_gen))
  | (new_prefix, Some(nonempty_line), empty_lines) =>
    switch (nonempty_line, leading, conclusion) {
    | (ExpLine(e), [], EmptyHole(_)) =>
      Some((new_prefix, e |> wrap_in_block, u_gen))
    | (_, _, _) =>
      Some((
        new_prefix,
        Block([nonempty_line] @ empty_lines @ leading, conclusion),
        u_gen,
      ))
    }
  };

let shift_line_from_suffix_block =
    (
      ~is_node_terminal: bool,
      ~u_gen: MetaVarGen.t,
      suffix_block: option(block),
      Block(leading, conclusion),
    )
    : option((block, option(block), MetaVarGen.t)) =>
  switch (suffix_block) {
  | None => assert(false)
  | Some(Block(suffix_leading, suffix_conclusion)) =>
    switch (
      suffix_leading |> first_contiguous_empty_lines_and_nonempty_line,
      suffix_conclusion,
    ) {
    | ((_, None, _), EmptyHole(_)) => None
    | ((empty_lines, None, _), _) =>
      switch (is_node_terminal, conclusion) {
      | (false, EmptyHole(_) as recycled_hole) =>
        Some((
          Block(leading @ empty_lines, suffix_conclusion),
          Some(recycled_hole |> wrap_in_block),
          u_gen,
        ))
      | (false, _) =>
        let (hole, u_gen) = u_gen |> new_EmptyHole;
        Some((
          Block(
            leading @ empty_lines @ [ExpLine(conclusion)],
            suffix_conclusion,
          ),
          Some(hole |> wrap_in_block),
          u_gen,
        ));
      | (true, EmptyHole(_)) =>
        Some((Block(leading @ empty_lines, suffix_conclusion), None, u_gen))
      | (true, _) =>
        Some((
          Block(leading @ [ExpLine(conclusion)], suffix_conclusion),
          None,
          u_gen,
        ))
      }
    | (
        (empty_lines, Some(LetLine(_, _, _) as let_line), []),
        EmptyHole(_) as let_line_hole,
      ) =>
      switch (is_node_terminal, conclusion) {
      | (false, EmptyHole(_)) =>
        Some((
          Block(leading @ empty_lines @ [let_line], conclusion),
          Some(let_line_hole |> wrap_in_block),
          u_gen,
        ))
      | (false, _) =>
        let (hole, u_gen) = u_gen |> new_EmptyHole;
        Some((
          Block(
            leading @ [ExpLine(conclusion)] @ empty_lines @ [let_line],
            hole,
          ),
          Some(let_line_hole |> wrap_in_block),
          u_gen,
        ));
      | (true, EmptyHole(_)) =>
        Some((
          Block(leading @ empty_lines @ [let_line], let_line_hole),
          None,
          u_gen,
        ))
      | (true, _) =>
        Some((
          Block(
            leading @ [ExpLine(conclusion)] @ empty_lines @ [let_line],
            let_line_hole,
          ),
          None,
          u_gen,
        ))
      }
    | ((empty_lines, Some(nonempty_line), rest), _) =>
      let new_suffix_block = Some(Block(rest, suffix_conclusion));
      let (new_block, u_gen) =
        switch (conclusion, nonempty_line) {
        | (_, EmptyLine) => assert(false)
        | (EmptyHole(_), ExpLine(e)) => (
            Block(leading @ empty_lines, e),
            u_gen,
          )
        | (EmptyHole(_) as recycled_hole, LetLine(_, _, _) as let_line) => (
            Block(leading @ empty_lines @ [let_line], recycled_hole),
            u_gen,
          )
        | (_, LetLine(_, _, _) as let_line) =>
          let (hole, u_gen) = u_gen |> new_EmptyHole;
          (
            Block(
              leading @ [ExpLine(conclusion)] @ empty_lines @ [let_line],
              hole,
            ),
            u_gen,
          );
        | (_, ExpLine(e)) => (
            Block(leading @ [ExpLine(conclusion)] @ empty_lines, e),
            u_gen,
          )
        };
      Some((new_block, new_suffix_block, u_gen));
    }
  };

let shift_line_to_suffix_block =
    (
      ~u_gen: MetaVarGen.t,
      suffix_block: option(block),
      Block(leading, conclusion),
    )
    // return type should be option((block, block, MetaVarGen.t))
    // but I've already coupled this type with that of
    // shift_line_from_suffix block via their use in Action.re
    // TODO decouple
    : option((block, option(block), MetaVarGen.t)) => {
  switch (leading |> last_line_and_leading_contiguous_empty_lines, conclusion) {
  | (None, EmptyHole(_)) => None
  | (None, _) =>
    switch (suffix_block) {
    | None =>
      let (hole, u_gen) = u_gen |> new_EmptyHole;
      let new_block = hole |> wrap_in_block;
      Some((new_block, Some(conclusion |> wrap_in_block), u_gen));
    | Some(Block([], EmptyHole(_) as recycled_hole)) =>
      Some((
        recycled_hole |> wrap_in_block,
        Some(conclusion |> wrap_in_block),
        u_gen,
      ))
    | Some(Block(suffix_leading, suffix_conclusion)) =>
      let (hole, u_gen) = u_gen |> new_EmptyHole;
      let new_block = hole |> wrap_in_block;
      Some((
        new_block,
        Some(
          Block(
            [ExpLine(conclusion), ...suffix_leading],
            suffix_conclusion,
          ),
        ),
        u_gen,
      ));
    }
  | (Some((_, _, EmptyLine)), EmptyHole(_)) => assert(false)
  | (
      Some((prefix, empty_lines, LetLine(_, _, _) as last_line)),
      EmptyHole(_),
    ) =>
    switch (prefix |> split_last) {
    | None
    | Some((_, LetLine(_, _, _))) =>
      let (hole, u_gen) = u_gen |> new_EmptyHole;
      Some((
        // recycle existing hole if prefix
        // does not have a conclusion
        Block(prefix, conclusion),
        Some(
          switch (suffix_block) {
          | None => Block(empty_lines @ [last_line], hole)
          | Some(Block(suffix_leading, suffix_conclusion)) =>
            Block(
              empty_lines @ [last_line] @ suffix_leading,
              suffix_conclusion,
            )
          },
        ),
        u_gen,
      ));
    | Some((prefix_prefix, ExpLine(e))) =>
      Some((
        Block(prefix_prefix, e),
        Some(
          switch (suffix_block) {
          | None =>
            // if prefix does have a conclusion
            // and therefore does not need its
            // existing hole, recycle here
            Block(empty_lines @ [last_line], conclusion)
          | Some(Block(suffix_leading, suffix_conclusion)) =>
            Block(
              empty_lines @ [last_line] @ suffix_leading,
              suffix_conclusion,
            )
          },
        ),
        u_gen,
      ))
    | Some((_, EmptyLine)) => assert(false)
    }
  | (Some((_, _, _)), _) =>
    let (leading_prefix, empty_lines) =
      leading |> last_contiguous_empty_lines;
    switch (suffix_block) {
    | None =>
      switch (leading_prefix |> split_last) {
      | None
      | Some((_, LetLine(_, _, _))) =>
        let (hole, u_gen) = u_gen |> new_EmptyHole;
        Some((
          Block(leading_prefix, hole),
          Some(Block(empty_lines, conclusion)),
          u_gen,
        ));
      | Some((leading_prefix_prefix, ExpLine(e))) =>
        Some((
          Block(leading_prefix_prefix, e),
          Some(Block(empty_lines, conclusion)),
          u_gen,
        ))
      | Some((_, EmptyLine)) => assert(false)
      }
    | Some(Block(suffix_leading, suffix_conclusion)) =>
      switch (leading_prefix |> split_last, suffix_leading, suffix_conclusion) {
      | (Some((_, EmptyLine)), _, _) => assert(false)
      | (
          None | Some((_, LetLine(_, _, _))),
          [],
          EmptyHole(_) as recycled_hole,
        ) =>
        Some((
          Block(leading_prefix, recycled_hole),
          Some(Block(empty_lines, conclusion)),
          u_gen,
        ))
      | (None | Some((_, LetLine(_, _, _))), _, _) =>
        let (hole, u_gen) = u_gen |> new_EmptyHole;
        Some((
          Block(leading_prefix, hole),
          Some(
            Block(
              empty_lines @ [ExpLine(conclusion)] @ suffix_leading,
              suffix_conclusion,
            ),
          ),
          u_gen,
        ));
      | (Some((leading_prefix_prefix, ExpLine(e))), _, _) =>
        Some((
          Block(leading_prefix_prefix, e),
          switch (suffix_leading, suffix_conclusion) {
          | ([], EmptyHole(_)) => Some(Block(empty_lines, conclusion))
          | (_, _) =>
            Some(
              Block(
                empty_lines @ [ExpLine(conclusion)] @ suffix_leading,
                suffix_conclusion,
              ),
            )
          },
          u_gen,
        ))
      }
    };
  };
};

let favored_child_of_line: line => option((ChildIndex.t, block)) =
  fun
  | EmptyLine
  | ExpLine(_) => None
  | LetLine(_, _, def) => Some((2, def));

let favored_child_of_exp: t => option((ChildIndex.t, block)) =
  fun
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | StringLit(_, _)
  | ListNil(_)
  | OpSeq(_, _)
  | ApPalette(_, _, _, _) => None
  | Lam(_, _, _, block) => Some((2, block))
  | Inj(_, _, block)
  | Case(_, block, _, _)
  | Parenthesized(block) => Some((0, block));

let has_concluding_let_line =
  fun
  | Block(leading, conclusion) =>
    switch (leading |> split_last, conclusion) {
    | (Some((_, LetLine(_, _, _))), EmptyHole(_)) => true
    | (_, _) => false
    };

let rec is_multi_line =
  fun
  | Block(lines, e) as block =>
    if (lines |> List.exists(is_multi_line_line) || is_multi_line_exp(e)) {
      true;
    } else if (List.length(lines) == 1 && has_concluding_let_line(block)) {
      false;
    } else {
      List.length(lines) > 0 || is_multi_line_exp(e);
    }
and is_multi_line_line =
  fun
  | EmptyLine => false
  | ExpLine(e) => is_multi_line_exp(e)
  | LetLine(_, _, def) => is_multi_line(def)
and is_multi_line_exp =
  fun
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | StringLit(_, _)
  | ListNil(_)
  | ApPalette(_, _, _, _) => false
  | Lam(_, _, _, body) => is_multi_line(body)
  | Inj(_, _, body) => is_multi_line(body)
  | Case(_, _, _, _) => true
  | Parenthesized(body) => is_multi_line(body)
  | OpSeq(_, seq) =>
    seq |> OperatorSeq.tms |> List.exists(is_multi_line_exp);

let is_trivial_block =
  fun
  | Block([], EmptyHole(_)) => true
  | _ => false;
