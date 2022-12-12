open Sexplib.Std;

[@deriving sexp]
type operator = Operators_Exp.t;

[@deriving sexp]
type t = block
and block = list(line)
and line =
  | EmptyLine
  | CommentLine(string)
  | LetLine(UHPat.t, t)
  | ExpLine(opseq)
and opseq = OpSeq.t(operand, operator)
and operand =
  | EmptyHole(MetaVar.t)
  | InvalidText(MetaVar.t, string)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | IntLit(ErrStatus.t, string)
  | FloatLit(ErrStatus.t, string)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  | Fun(ErrStatus.t, UHPat.t, t)
  | Inj(ErrStatus.t, InjSide.t, t)
  | Case(CaseErrStatus.t, t, rules)
  | Parenthesized(t)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

type affix = Seq.affix(operand, operator);

let letline = (p: UHPat.t, def: t): line => LetLine(p, def);

let var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      x: Var.t,
    )
    : operand =>
  Var(err, var_err, x);

let intlit = (~err: ErrStatus.t=NotInHole, n: string): operand =>
  IntLit(err, n);

let floatlit = (~err: ErrStatus.t=NotInHole, f: string): operand =>
  FloatLit(err, f);

let boollit = (~err: ErrStatus.t=NotInHole, b: bool): operand =>
  BoolLit(err, b);

let lam = (~err: ErrStatus.t=NotInHole, p: UHPat.t, body: t): operand =>
  Fun(err, p, body);

let case =
    (
      ~err: CaseErrStatus.t=StandardErrStatus(NotInHole),
      scrut: t,
      rules: rules,
    )
    : operand =>
  Case(err, scrut, rules);

let listnil = (~err: ErrStatus.t=NotInHole, ()): operand => ListNil(err);

module Line = {
  let prune_empty_hole = (line: line): line =>
    switch (line) {
    | ExpLine(OpSeq(_, S(EmptyHole(_), E))) => EmptyLine
    | CommentLine(_)
    | ExpLine(_)
    | EmptyLine
    | LetLine(_) => line
    };

  let get_opseq =
    fun
    | EmptyLine
    | CommentLine(_)
    | LetLine(_) => None
    | ExpLine(opseq) => Some(opseq);
  let force_get_opseq = line =>
    line
    |> get_opseq
    |> OptUtil.get(_ => failwith("force_get_opseq: expected ExpLine"));
};

module Lines = {
  let prune_empty_holes = (lines: list(line)): list(line) =>
    lines |> List.map(Line.prune_empty_hole);
};

module Block = {
  let wrap' = (opseq: opseq): block => [ExpLine(opseq)];
  let wrap = (operand: operand): block => wrap'(OpSeq.wrap(operand));

  let num_lines: block => int = List.length;

  let prune_empty_hole_lines = (block: block): block =>
    switch (block |> ListUtil.split_last_opt) {
    | None => block
    | Some((leading, last)) => (leading |> Lines.prune_empty_holes) @ [last]
    };

  let split_conclusion = (block: block): option((list(line), opseq)) =>
    switch (block |> ListUtil.split_last_opt) {
    | None => None
    | Some((leading, last)) =>
      switch (last |> Line.get_opseq) {
      | None => None
      | Some(opseq) => Some((leading, opseq))
      }
    };
  let force_split_conclusion = (block: block): (list(line), opseq) =>
    switch (block |> split_conclusion) {
    | None => failwith("force_split_conclusion: unconcluded block")
    | Some((leading, conclusion)) => (leading, conclusion)
    };

  let join_conclusion = (leading: list(line), conclusion: opseq): block =>
    leading @ [ExpLine(conclusion)];
};

let rec get_tuple_elements: skel => list(skel) =
  fun
  | BinOp(_, Comma, skel1, skel2) =>
    get_tuple_elements(skel1) @ get_tuple_elements(skel2)
  | skel => [skel];

let rec mk_tuple = (~err: ErrStatus.t=NotInHole, elements: list(skel)): skel =>
  switch (elements) {
  | [] => failwith("mk_tuple: expected at least 1 element")
  | [skel] => skel
  | [skel, ...skels] => BinOp(err, Comma, skel, mk_tuple(skels))
  };

let new_InvalidText = (id_gen: IDGen.t, t: string): (operand, IDGen.t) => {
  let (u, id_gen) = IDGen.next_hole(id_gen);
  (InvalidText(u, t), id_gen);
} /* helper function for constructing a new empty hole */;

let new_EmptyHole = (id_gen: IDGen.t): (operand, IDGen.t) => {
  let (u, id_gen) = id_gen |> IDGen.next_hole;
  (EmptyHole(u), id_gen);
};

let is_EmptyHole =
  fun
  | EmptyHole(_) => true
  | _ => false;

let empty_rule = (id_gen: IDGen.t): (rule, IDGen.t) => {
  let (p, id_gen) = UHPat.new_EmptyHole(id_gen);
  let (e, id_gen) = new_EmptyHole(id_gen);
  let rule = Rule(OpSeq.wrap(p), Block.wrap(e));
  (rule, id_gen);
};

let rec get_err_status = (e: t): ErrStatus.t => get_err_status_block(e)
and get_err_status_block = block => {
  let (_, conclusion) = block |> Block.force_split_conclusion;
  conclusion |> get_err_status_opseq;
}
and get_err_status_opseq = opseq =>
  OpSeq.get_err_status(~get_err_status_operand, opseq)
and get_err_status_operand =
  fun
  | EmptyHole(_) => NotInHole
  | InvalidText(_, _) => NotInHole
  | Var(err, _, _)
  | IntLit(err, _)
  | FloatLit(err, _)
  | BoolLit(err, _)
  | ListNil(err)
  | Fun(err, _, _)
  | Inj(err, _, _)
  | Case(StandardErrStatus(err), _, _) => err
  | Case(InconsistentBranches(_), _, _) => NotInHole
  | Parenthesized(e) => get_err_status(e) /* put e in the specified hole */;

let rec set_err_status = (err: ErrStatus.t, e: t): t =>
  e |> set_err_status_block(err)
and set_err_status_block = (err: ErrStatus.t, block: block): block => {
  let (leading, conclusion) = block |> Block.force_split_conclusion;
  Block.join_conclusion(leading, conclusion |> set_err_status_opseq(err));
}
and set_err_status_opseq = (err, opseq) =>
  OpSeq.set_err_status(~set_err_status_operand, err, opseq)
and set_err_status_operand = (err, operand) =>
  switch (operand) {
  | EmptyHole(_) => operand
  | InvalidText(_, _) => operand
  | Var(_, var_err, x) => Var(err, var_err, x)
  | IntLit(_, n) => IntLit(err, n)
  | FloatLit(_, f) => FloatLit(err, f)
  | BoolLit(_, b) => BoolLit(err, b)
  | ListNil(_) => ListNil(err)
  | Fun(_, p, def) => Fun(err, p, def)
  | Inj(_, inj_side, body) => Inj(err, inj_side, body)
  | Case(_, scrut, rules) => Case(StandardErrStatus(err), scrut, rules)
  | Parenthesized(body) => Parenthesized(body |> set_err_status(err))
  } /* put e in a new hole, if it is not already in a hole */;

let rec mk_inconsistent = (id_gen: IDGen.t, e: t): (t, IDGen.t) =>
  mk_inconsistent_block(id_gen, e)
and mk_inconsistent_block = (id_gen: IDGen.t, block: block): (block, IDGen.t) => {
  let (leading, conclusion) = block |> Block.force_split_conclusion;
  let (conclusion, id_gen) = conclusion |> mk_inconsistent_opseq(id_gen);
  (Block.join_conclusion(leading, conclusion), id_gen);
}
and mk_inconsistent_opseq = (id_gen, opseq) =>
  OpSeq.mk_inconsistent(~mk_inconsistent_operand, id_gen, opseq)
and mk_inconsistent_operand = (id_gen, operand) =>
  switch (operand) {
  /* already in hole */
  | EmptyHole(_)
  | InvalidText(_, _)
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Fun(InHole(TypeInconsistent, _), _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent, _)), _, _) => (
      operand,
      id_gen,
    ) /* not in hole */
  | Var(NotInHole | InHole(WrongLength, _), _, _)
  | IntLit(NotInHole | InHole(WrongLength, _), _)
  | FloatLit(NotInHole | InHole(WrongLength, _), _)
  | BoolLit(NotInHole | InHole(WrongLength, _), _)
  | ListNil(NotInHole | InHole(WrongLength, _))
  | Fun(NotInHole | InHole(WrongLength, _), _, _)
  | Inj(NotInHole | InHole(WrongLength, _), _, _)
  | Case(
      StandardErrStatus(NotInHole | InHole(WrongLength, _)) |
      InconsistentBranches(_, _),
      _,
      _,
    ) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    let operand =
      operand |> set_err_status_operand(InHole(TypeInconsistent, u));
    (operand, id_gen /* err in constructor args */);
  | Parenthesized(body) =>
    let (body, id_gen) = body |> mk_inconsistent(id_gen);
    (Parenthesized(body), id_gen);
  };

let text_operand = (id_gen: IDGen.t, shape: TextShape.t): (operand, IDGen.t) =>
  switch (shape) {
  | Underscore => (var("_"), id_gen)
  | IntLit(n) => (intlit(n), id_gen)
  | FloatLit(f) => (floatlit(f), id_gen)
  | BoolLit(b) => (boollit(b), id_gen)
  | Var(x) => (var(x), id_gen)
  | ExpandingKeyword(kw) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    (
      var(~var_err=InVarHole(Free, u), kw |> ExpandingKeyword.to_string),
      id_gen,
    );
  | InvalidTextShape(t) => new_InvalidText(id_gen, t)
  };

let associate =
  Skel.mk(Operators_Exp.precedence, Operators_Exp.associativity);

let mk_OpSeq = OpSeq.mk(~associate);

let rec is_complete_line = (l: line): bool => {
  switch (l) {
  | EmptyLine
  | CommentLine(_) => true
  | LetLine(pat, body) => UHPat.is_complete(pat) && is_complete(body)
  | ExpLine(body) => OpSeq.is_complete(is_complete_operand, body)
  };
}
and is_complete_block = (b: block): bool => {
  b |> List.for_all(l => is_complete_line(l));
}
and is_complete_rule = (rule: rule): bool => {
  switch (rule) {
  | Rule(pat, body) => UHPat.is_complete(pat) && is_complete(body)
  };
}
and is_complete_rules = (rules: rules): bool => {
  rules |> List.for_all(l => is_complete_rule(l));
}
and is_complete_operand = (operand: 'operand): bool => {
  switch (operand) {
  | EmptyHole(_) => false
  | InvalidText(_, _) => false
  | Var(InHole(_), _, _) => false
  | Var(NotInHole, InVarHole(_), _) => false
  | Var(NotInHole, NotInVarHole, _) => true
  | IntLit(InHole(_), _) => false
  | IntLit(NotInHole, _) => true
  | FloatLit(InHole(_), _) => false
  | FloatLit(NotInHole, _) => true
  | BoolLit(InHole(_), _) => false
  | BoolLit(NotInHole, _) => true
  | ListNil(InHole(_)) => false
  | ListNil(NotInHole) => true
  | Fun(InHole(_), _, _) => false
  | Fun(NotInHole, pat, body) => UHPat.is_complete(pat) && is_complete(body)
  | Inj(InHole(_), _, _) => false
  | Inj(NotInHole, _, body) => is_complete(body)
  | Case(StandardErrStatus(InHole(_)) | InconsistentBranches(_), _, _) =>
    false
  | Case(StandardErrStatus(NotInHole), body, rules) =>
    is_complete(body) && is_complete_rules(rules)
  | Parenthesized(body) => is_complete(body)
  };
}
and is_complete = (exp: t): bool => {
  is_complete_block(exp);
};
